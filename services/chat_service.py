# services/chat_service.py
from __future__ import annotations

import logging
import re
import time
from typing import Any, Dict, List, Optional, Tuple

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
- Señala riesgos CRÍTICOS en login, checkout, pagos, promociones, stock y performance.
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


def _persona(prompt: str, mode: str) -> str:
    p = H.low(prompt)
    if "modo automation" in p or "modo automatizacion" in p:
        return "automation"
    if "modo lead" in p:
        return "lead"
    return "automation" if mode == "execute" else "lead"


def _system_prompt(persona: str) -> str:
    return SYSTEM_PROMPT_AUTOMATION if persona == "automation" else SYSTEM_PROMPT_LEAD


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
# MEMORY (NO execute)
# ============================================================
def _is_memory_query(prompt: str) -> bool:
    p = H.low(prompt)
    keys = [
        "recuérdame", "recuerdame", "última prueba", "ultima prueba",
        "qué validamos", "que validamos", "resultado", "evidence", "evidencia",
        "qué pasó", "que paso", "resumen de la prueba", "summary de la prueba",
    ]
    return any(k in p for k in keys)


def _find_last_execute(history_msgs: List[Dict[str, Any]]) -> Optional[Tuple[int, Dict[str, Any]]]:
    for i in range(len(history_msgs) - 1, -1, -1):
        m = history_msgs[i] or {}
        if (m.get("role") or "").strip() != "assistant":
            continue
        meta = m.get("meta") or m.get("meta_json") or {}
        if (meta.get("mode") or "").strip() == "execute":
            return i, m
    return None


def _summarize_last_execute(history_msgs: List[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    hit = _find_last_execute(history_msgs)
    if not hit:
        return None

    idx, exec_msg = hit
    meta = exec_msg.get("meta") or exec_msg.get("meta_json") or {}

    user_prompt = ""
    for j in range(idx - 1, -1, -1):
        mj = history_msgs[j] or {}
        if (mj.get("role") or "").strip() == "user":
            user_prompt = (mj.get("content") or "").strip()
            if user_prompt:
                break

    base_url = (meta.get("base_url") or "").strip() or None
    status = (meta.get("runner_status") or "").strip() or "ok"
    duration_ms = meta.get("duration_ms")
    evidence_url = (meta.get("evidence_url") or "").strip() or None

    pieces: List[str] = []
    if user_prompt:
        pieces.append(f"**Última prueba:** {user_prompt}")
    if base_url:
        pieces.append(f"**URL:** {base_url}")
    pieces.append(f"**Resultado:** {status.upper()}")
    if isinstance(duration_ms, int):
        pieces.append(f"**Duración:** {duration_ms} ms")
    if evidence_url:
        pieces.append(f"**Evidence:** {evidence_url}")

    answer = "\n".join(pieces).strip()
    return {
        "answer": answer or "La última prueba fue ejecutada, pero no pude armar el resumen.",
        "base_url": base_url,
        "runner_status": status,
        "evidence_url": evidence_url,
        "duration_ms": duration_ms,
    }


# ============================================================
# EXECUTION: deterministic parser
# ============================================================
def _looks_like_saucedemo(url: str) -> bool:
    return "saucedemo.com" in (url or "").lower()


def _strip_quotes(s: str) -> str:
    s = (s or "").strip()
    if (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
        return s[1:-1].strip()
    return s


def _parse_steps_from_prompt(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    p = prompt.strip()
    low = p.lower()

    steps: List[Dict[str, Any]] = []
    steps.append({"action": "goto", "url": base_url})
    steps.append({"action": "wait_ms", "ms": 250})

    # Visibilidad por lista
    if "visibles" in low or "visible" in low:
        selectors = re.findall(r'(["\']?)(#[-\w]+|\.[-\w]+|\[[^\]]+\])\1', p)
        seen: List[str] = []
        for _, sel in selectors:
            if sel and sel not in seen:
                seen.append(sel)
        if _looks_like_saucedemo(base_url) and not seen:
            seen = ["#user-name", "#password", "#login-button"]
        for sel in seen:
            steps.append({"action": "assert_visible", "selector": sel})
        return steps if len(steps) > 2 else None

    # Fill
    fill_patterns = [
        r'(?:llena|fill)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])\s+(?:con|with)\s+(".*?"|\'.*?\')',
    ]
    for pat in fill_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            val = _strip_quotes(m.group(2))
            steps.append({"action": "fill", "selector": sel, "text": val})

    # Click
    click_patterns = [
        r'(?:haz\s+click\s+en|click)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])',
    ]
    for pat in click_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            steps.append({"action": "click", "selector": sel})

    # Assert text
    text_patterns = [
        r'(?:valida|validar|verify|assert)\s+.*?(?:texto|text).*?(".*?"|\'.*?\')',
        r'(?:assert_text_contains)\s+(".*?"|\'.*?\')',
    ]
    found_text = None
    for pat in text_patterns:
        m = re.search(pat, p, flags=re.IGNORECASE)
        if m:
            found_text = _strip_quotes(m.group(1))
            break

    if found_text:
        steps.append({"action": "assert_text_contains", "text": found_text})

    useful = [s for s in steps if s["action"] not in ("goto", "wait_ms")]
    if not useful:
        return None
    return steps


def _render_execute_answer(result: Dict[str, Any], evidence_url: Optional[str] = None) -> str:
    ok = bool(result.get("ok", True))
    status = result.get("status") or ("ok" if ok else "error")
    msg = (result.get("message") or result.get("detail") or "").strip()

    if evidence_url:
        return f"✅ Ejecutado ({status}).{(' ' + msg) if msg else ''}\nEvidence: {evidence_url}"

    runner_url = result.get("screenshot_url") or result.get("evidence_url")
    if runner_url:
        return f"✅ Ejecutado ({status}).{(' ' + msg) if msg else ''}\nEvidence: {runner_url}"

    return f"✅ Ejecutado ({status}).{(' ' + msg) if msg else ''}"


# ============================================================
# Helpers: evidence data-url
# ============================================================
def _make_png_data_url(b64_or_data_url: Optional[str]) -> Optional[str]:
    """
    Convierte:
      - "iVBORw0..." -> "data:image/png;base64,iVBORw0..."
      - "data:image/png;base64,iVBORw0..." -> igual
    """
    if not b64_or_data_url:
        return None
    s = str(b64_or_data_url).strip()
    if not s:
        return None
    if s.startswith("data:image"):
        return s
    return f"data:image/png;base64,{s}"


# ============================================================
# MAIN
# ============================================================
def handle_chat_run(req: Any) -> Dict[str, Any]:
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Prompt vacío")

    # Session
    session_id, session = H.get_session(getattr(req, "session_id", None))

    # Thread
    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    store.add_message(thread_id, "user", prompt, meta={"source": "chat"})

    # History
    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    # MEMORY
    if _is_memory_query(prompt):
        mem = _summarize_last_execute(history_msgs)
        if not mem:
            answer = "Aún no veo una ejecución previa en este chat. Pídeme que ejecute una prueba y luego te resumo el resultado."
            store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "memory": True})
            return {
                "mode": "advise",
                "persona": _persona(prompt, "advise"),
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("advise", prompt, None),
            }

        answer = mem["answer"]
        store.add_message(
            thread_id,
            "assistant",
            answer,
            meta={
                "mode": "advise",
                "memory": True,
                "base_url": mem.get("base_url"),
                "runner_status": mem.get("runner_status"),
                "evidence_url": mem.get("evidence_url"),
                "duration_ms": mem.get("duration_ms"),
            },
        )
        return {
            "mode": "advise",
            "persona": _persona(prompt, "advise"),
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "evidence_url": mem.get("evidence_url"),
            "duration_ms": mem.get("duration_ms"),
            **_confidence("advise", prompt, mem.get("base_url")),
        }

    # Intent
    if H.wants_doc(prompt):
        mode = "doc"
    elif H.wants_execute(prompt, session):
        mode = "execute"
    else:
        mode = "advise"

    persona = _persona(prompt, mode)

    # LLM messages
    messages: List[Dict[str, str]] = [{"role": "system", "content": _system_prompt(persona)}]
    for m in history_msgs:
        role = (m.get("role") or "assistant").strip()
        content = (m.get("content") or "").strip()
        if content:
            messages.append({"role": role, "content": content})

   # ============================================================
# EXECUTE
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

    # 0) Parser determinístico
    steps = _parse_steps_from_prompt(prompt, base_url)

    # 1) Fallback a LLM para steps
    if not steps:
        client = _client()
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages
            + [
                {
                    "role": "user",
                    "content": (
                        f"URL base: {base_url}\n"
                        f"Genera pasos Playwright para:\n{prompt}\n"
                        "Devuelve SOLO JSON con {\"steps\": [...]}."
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
        store.add_message(
            thread_id, "assistant", answer, meta={"mode": "execute", "base_url": base_url}
        )
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            **_confidence("execute", prompt, base_url),
        }

    # Ensure goto + remember last_url
    H.ensure_goto(steps, base_url)
    H.update_last_url(session, steps, fallback=base_url)

    # Runner
    started = time.time()
    result = execute_test(
        steps=steps,
        base_url=base_url,
        headless=bool(getattr(req, "headless", True)),
    )
    duration_ms = int((time.time() - started) * 1000)

    # ✅ SIEMPRE: prepara data-url (para render inline)
    screenshot_b64 = (
        result.get("screenshot_b64")
        or result.get("screenshotBase64")
        or result.get("screenshotB64")
        or result.get("screenshot_base64")
    )
    screenshot_data_url = _make_png_data_url(screenshot_b64) if screenshot_b64 else None
    if screenshot_data_url:
        result["screenshot_data_url"] = screenshot_data_url

    # Evidence upload -> Cloudinary (opcional)
    evidence_url: Optional[str] = None
    cloud_public_id: Optional[str] = None
    evidence_id = (result.get("evidence_id") or f"EV-{int(time.time())}").strip()

    try:
        # Validación de b64 para subir
        if screenshot_b64 and len(str(screenshot_b64)) > 500 and getattr(settings, "HAS_CLOUDINARY", False):
            b64_str = str(screenshot_b64).strip()
            if not b64_str.startswith("data:image"):
                b64_str = f"data:image/png;base64,{b64_str}"

            uploaded = upload_screenshot_b64(
                b64_str,
                evidence_id=evidence_id,
                folder="vanya/evidence",
            )

            evidence_url = uploaded.get("secure_url") or uploaded.get("image_url") or uploaded.get("url")
            cloud_public_id = uploaded.get("public_id")
            logger.info(f"Evidence uploaded successfully: {evidence_url}")
        else:
            logger.warning(
                f"Screenshot b64 invalid/too small OR no cloudinary. (len={len(str(screenshot_b64)) if screenshot_b64 else 0})"
            )
    except Exception as e:
        logger.warning(f"Evidence upload failed: {str(e)}", exc_info=True)

    # ✅ Generar PDF report (MVP)
    report_url: Optional[str] = None
    try:
        from services.report_service import generate_pdf_report

        rep = generate_pdf_report(
            prompt=prompt,
            base_url=base_url,
            runner=result,
            steps=steps,
            evidence_id=evidence_id,
            meta={
                "thread_id": thread_id,
                "session_id": session_id,
                "headless": bool(getattr(req, "headless", True)),
            },
        )
        report_url = rep.get("report_url")
        # También guardamos en result para que el frontend lo encuentre fácil
        if report_url:
            result["report_url"] = report_url
    except Exception as e:
        logger.warning(f"Report generation failed: {str(e)}", exc_info=True)

    # Render answer
    if hasattr(H, "render_execute_answer"):
        answer = H.render_execute_answer(result, evidence_url=evidence_url)
    else:
        answer = _render_execute_answer(result, evidence_url=evidence_url)

    # Meta que se guarda en el historial (para que el frontend lo pinte SIEMPRE)
    assistant_meta: Dict[str, Any] = {
        "mode": "execute",
        "base_url": base_url,
        "duration_ms": duration_ms,
        "runner_status": result.get("status") or ("ok" if result.get("ok", True) else "error"),
        "evidence_id": evidence_id,
        "evidence_url": evidence_url,
        "cloudinary_public_id": cloud_public_id,
        "report_url": report_url,  # ✅ nuevo

        # compat frontend:
        "runner": {
            "status": result.get("status"),
            "error": result.get("error"),
            "evidence_id": evidence_id,
            "evidence_url": evidence_url,
            "screenshot_url": evidence_url,               # si hay Cloudinary, úsalo
            "screenshot_data_url": screenshot_data_url,   # ✅ si NO hay Cloudinary, la UI debe pintar esto
            "duration_ms": result.get("duration_ms"),
            "report_url": report_url,                     # ✅ nuevo
        },

        "steps": steps,
    }

    store.add_message(thread_id, "assistant", answer, meta=assistant_meta)

    # Response al frontend
    return {
        "mode": "execute",
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        "runner": result,  # ✅ aquí ya viene screenshot_data_url (+ report_url si se generó)
        "evidence_url": evidence_url,
        "report_url": report_url,  # ✅ nuevo
        "duration_ms": duration_ms,
        **_confidence("execute", prompt, base_url),
    }

    # ============================================================
    # ADVISE / DOC
    # ============================================================
    client = _client()
    resp = client.chat.completions.create(
        model=settings.OPENAI_MODEL,
        messages=messages,
        temperature=settings.DOC_TEMPERATURE if mode == "doc" else settings.ADV_TEMPERATURE,
        max_tokens=settings.DOC_MAX_TOKENS if mode == "doc" else settings.ADV_MAX_TOKENS,
    )
    answer = (resp.choices[0].message.content or "").strip() or "¿Puedes darme un poco más de contexto?"
    store.add_message(thread_id, "assistant", answer, meta={"mode": mode, "persona": persona})

    return {
        "mode": mode,
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        **_confidence(mode, prompt, H.pick_base_url(req, session, prompt) if mode != "doc" else None),
    }