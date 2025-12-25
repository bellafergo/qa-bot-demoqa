# services/chat_service.py
from __future__ import annotations

import logging
import time
from typing import Any, Dict, List, Optional

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core import chat_helpers as H
from services import store
from services.evidence_service import upload_screenshot_b64
from runner import execute_test

logger = logging.getLogger("vanya.chat_service")


# ============================================================
# SYSTEM PROMPTS
# ============================================================

SYSTEM_PROMPT_LEAD = """Eres Vanya, QA Lead / SDET experta en Retail y E-commerce.
Tu objetivo es evitar defectos que afecten conversión, ingresos o experiencia.

Reglas:
- Señala riesgos CRÍTICOS en checkout, pagos, promociones, stock y performance.
- Prioriza acciones (P0 / P1 / P2).
- Pide solo la información mínima necesaria.
- Sé clara, directa y orientada a negocio.
"""

SYSTEM_PROMPT_AUTOMATION = """Eres Vanya, QA Automation / SDET.
Tu misión es ejecutar pruebas web de forma robusta.

Reglas:
- Devuelve pasos Playwright claros y estables.
- Usa selectores robustos (data-testid > id > role/text).
- Espera visibilidad antes de interactuar.
- No expliques: ejecuta.
"""


# ============================================================
# OpenAI client
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


# ============================================================
# Confidence helper
# ============================================================
def _confidence(mode: str, prompt: str, base_url: Optional[str]) -> Dict[str, Any]:
    score = 0.55
    if mode == "execute":
        score = 0.65 + (0.15 if base_url else -0.15)
    elif mode == "doc":
        score = 0.60
    else:
        score = 0.55

    if len(prompt or "") < 25:
        score -= 0.10

    score = max(0.10, min(0.95, score))
    label = "high" if score >= 0.80 else "medium" if score >= 0.55 else "low"
    return {"confidence_0_1": round(score, 2), "confidence_label": label}


# ============================================================
# Persona selector
# ============================================================
def _persona(prompt: str, mode: str) -> str:
    p = H.low(prompt)
    if "modo automation" in p or "modo automatizacion" in p:
        return "automation"
    if "modo lead" in p:
        return "lead"
    return "automation" if mode == "execute" else "lead"


def _system_prompt(persona: str) -> str:
    return SYSTEM_PROMPT_AUTOMATION if persona == "automation" else SYSTEM_PROMPT_LEAD


# ============================================================
# Execute answer renderer (estable)
# ============================================================
def _render_execute_answer(result: Dict[str, Any], evidence_url: Optional[str] = None) -> str:
    ok = bool(result.get("ok", True))
    status = result.get("status") or ("ok" if ok else "error")
    msg = (result.get("message") or result.get("detail") or "").strip()

    if evidence_url:
        if msg:
            return f"✅ Ejecutado ({status}). {msg}\nEvidence: {evidence_url}"
        return f"✅ Ejecutado ({status}).\nEvidence: {evidence_url}"

    runner_url = result.get("screenshot_url") or result.get("evidence_url")
    if runner_url:
        if msg:
            return f"✅ Ejecutado ({status}). {msg}\nEvidence: {runner_url}"
        return f"✅ Ejecutado ({status}).\nEvidence: {runner_url}"

    if msg:
        return f"✅ Ejecutado ({status}). {msg}"
    return f"✅ Ejecutado ({status})."


# ============================================================
# MAIN ENTRY (called by routes/chat.py)
# ============================================================
def handle_chat_run(req: Any) -> Dict[str, Any]:
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Prompt vacío")

    # ----------------------------
    # Session
    # ----------------------------
    session_id, session = H.get_session(getattr(req, "session_id", None))

    # ----------------------------
    # Thread
    # ----------------------------
    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    store.add_message(thread_id, "user", prompt, meta={"source": "chat"})

    # ----------------------------
    # Intent
    # ----------------------------
    if H.wants_doc(prompt):
        mode = "doc"
    elif H.wants_execute(prompt, session):
        mode = "execute"
    else:
        mode = "advise"

    persona = _persona(prompt, mode)

    # ----------------------------
    # Memory (last N messages)
    # ----------------------------
    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    messages: List[Dict[str, str]] = [{"role": "system", "content": _system_prompt(persona)}]
    for m in history_msgs:
        role = (m.get("role") or "assistant").strip()
        content = (m.get("content") or "").strip()
        if content:
            messages.append({"role": role, "content": content})

    # ============================================================
    # EXECUTE MODE
    # ============================================================
    if mode == "execute":
        base_url = H.pick_base_url(req, session, prompt)
        if not base_url:
            answer = (
                "Para ejecutar necesito:\n"
                "- URL (o dime “la misma”)\n"
                "- Qué validar (botón / campo / texto esperado)\n"
                "- Credenciales (si aplica)"
            )
            store.add_message(thread_id, "assistant", answer, meta={"mode": "execute"})
            return {
                "mode": "execute",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("execute", prompt, None),
            }

        # --- 1) Pedimos steps al LLM ---
        client = _client()
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages
            + [
                {
                    "role": "user",
                    "content": (
                        f"URL base: {base_url}\n"
                        f"Genera pasos Playwright para validar:\n{prompt}\n"
                        "Devuelve SOLO JSON con {\"steps\": [...]} o SOLO la lista de steps."
                    ),
                }
            ],
            temperature=settings.EXEC_TEMPERATURE,
            max_tokens=settings.EXEC_MAX_TOKENS,
        )

        raw = (resp.choices[0].message.content or "").strip()
        steps = H.extract_steps_from_text(raw)

        if not steps:
            answer = "No pude generar pasos ejecutables. Dime el botón/campo exacto y el texto esperado."
            store.add_message(thread_id, "assistant", answer, meta={"mode": "execute"})
            return {
                "mode": "execute",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("execute", prompt, base_url),
            }

        # goto + last_url
        H.ensure_goto(steps, base_url)
        H.update_last_url(session, steps, fallback=base_url)

        # --- 2) Runner ---
        started = time.time()
        result = execute_test(
            steps=steps,
            base_url=base_url,
            headless=bool(getattr(req, "headless", True)),
        )
        duration_ms = int((time.time() - started) * 1000)

        # --- 3) Evidence: screenshot_b64 -> Cloudinary url (si está disponible) ---
        evidence_url: Optional[str] = None
        cloud_public_id: Optional[str] = None

        evidence_id = (result.get("evidence_id") or f"ev_{thread_id}_{int(time.time())}").strip()

        try:
            screenshot_b64 = result.get("screenshot_b64") or result.get("screenshotBase64")
            if screenshot_b64 and settings.HAS_CLOUDINARY:
                uploaded = upload_screenshot_b64(
                    screenshot_b64,
                    evidence_id=evidence_id,
                    folder="vanya/evidence",
                )
                evidence_url = uploaded.get("url")
                cloud_public_id = uploaded.get("public_id")
        except Exception:
            logger.warning("Evidence upload failed (continuing)", exc_info=True)

        # --- 4) Respuesta final ---
        answer = _render_execute_answer(result, evidence_url=evidence_url)

        # --- 5) Guardar assistant con meta rica para UI ---
        assistant_meta: Dict[str, Any] = {
            "mode": "execute",
            "base_url": base_url,
            "duration_ms": duration_ms,
            "runner_status": result.get("status") or ("ok" if result.get("ok", True) else "error"),
            "evidence_id": evidence_id,
            # ✅ frontend debe leer este:
            "evidence_url": evidence_url,
            "cloudinary_public_id": cloud_public_id,
            # auditoría
            "steps": steps,
        }

        store.add_message(thread_id, "assistant", answer, meta=assistant_meta)

        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "runner": result,
            "evidence_url": evidence_url,
            "duration_ms": duration_ms,
            **_confidence("execute", prompt, base_url),
        }

    # ============================================================
    # ADVISE / DOC MODE
    # ============================================================
    client = _client()
    resp = client.chat.completions.create(
        model=settings.OPENAI_MODEL,
        messages=messages,
        temperature=settings.DOC_TEMPERATURE if mode == "doc" else settings.ADV_TEMPERATURE,
        max_tokens=settings.DOC_MAX_TOKENS if mode == "doc" else settings.ADV_MAX_TOKENS,
    )

    answer = (resp.choices[0].message.content or "").strip() or "¿Puedes darme un poco más de contexto?"

    store.add_message(thread_id, "assistant", answer, meta={"mode": mode})

    return {
        "mode": mode,
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        **_confidence(mode, prompt, None),
    }