# services/chat_service.py
from __future__ import annotations

import logging
import time
from typing import Any, Dict, List, Optional, Tuple

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core import chat_helpers as H
from core.prompts import SYSTEM_PROMPT, SYSTEM_PROMPT_EXECUTE, SYSTEM_PROMPT_DOC  # ✅ IMPORT CORRECTO

from services import store
from runner import execute_test

# Evidence (opcional)
try:
    from services.evidence_service import upload_screenshot_b64
except Exception:
    upload_screenshot_b64 = None  # type: ignore


logger = logging.getLogger("vanya.chat_service")


# ============================================================
# OpenAI client
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


def _confidence(mode: str, prompt: str, base_url: Optional[str]) -> Dict[str, Any]:
    score = 0.55
    if mode == "execute":
        score = 0.65 + (0.15 if base_url else -0.15)
    elif mode == "doc":
        score = 0.60
    else:
        score = 0.55

    if len((prompt or "").strip()) < 20:
        score -= 0.10

    score = max(0.10, min(0.95, score))
    label = "high" if score >= 0.80 else "medium" if score >= 0.55 else "low"
    return {"confidence_0_1": round(score, 2), "confidence_label": label}


def _system_prompt_for_mode(mode: str) -> str:
    if mode == "execute":
        return SYSTEM_PROMPT_EXECUTE
    if mode == "doc":
        return SYSTEM_PROMPT_DOC
    return SYSTEM_PROMPT


# ============================================================
# MEMORY
# ============================================================
def _is_memory_query(prompt: str) -> bool:
    p = H.low(prompt)
    keys = [
        "recuérdame", "recuerdame",
        "última prueba", "ultima prueba",
        "qué validamos", "que validamos",
        "resultado", "evidence", "evidencia",
        "qué pasó", "que paso",
        "resumen", "summary",
        "la última ejecución", "la ultima ejecucion",
        "última ejecución", "ultima ejecucion",
    ]
    return any(k in p for k in keys)


def _find_last_execute_message(messages: List[Dict[str, Any]]) -> Optional[Tuple[int, Dict[str, Any]]]:
    for i in range(len(messages) - 1, -1, -1):
        m = messages[i] or {}
        if (m.get("role") or "").strip() != "assistant":
            continue
        meta = m.get("meta") or m.get("meta_json") or {}
        if (meta.get("mode") or "").strip() == "execute":
            return i, m
    return None


def _summarize_last_execute(messages: List[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    hit = _find_last_execute_message(messages)
    if not hit:
        return None

    idx, exec_msg = hit
    meta = exec_msg.get("meta") or exec_msg.get("meta_json") or {}

    user_prompt = ""
    for j in range(idx - 1, -1, -1):
        mj = messages[j] or {}
        if (mj.get("role") or "").strip() == "user":
            user_prompt = (mj.get("content") or "").strip()
            if user_prompt:
                break

    base_url = (meta.get("base_url") or "").strip() or None
    status = (meta.get("runner_status") or meta.get("status") or "").strip() or "ok"
    duration_ms = meta.get("duration_ms")
    evidence_url = (meta.get("evidence_url") or meta.get("screenshot_url") or "").strip() or None

    if not evidence_url:
        txt = (exec_msg.get("content") or "").strip()
        if "Evidence:" in txt:
            tail = txt.split("Evidence:", 1)[-1].strip()
            if tail.startswith("http"):
                evidence_url = tail.split()[0].strip()

    lines: List[str] = []
    if user_prompt:
        lines.append(f"**Última prueba:** {user_prompt}")
    if base_url:
        lines.append(f"**URL:** {base_url}")
    lines.append(f"**Resultado:** {status.upper()}")
    if isinstance(duration_ms, int):
        lines.append(f"**Duración:** {duration_ms} ms")
    if evidence_url:
        lines.append(f"**Evidence:** {evidence_url}")

    answer = "\n".join(lines).strip() or "Encontré una ejecución previa, pero no pude armar el resumen."
    return {
        "answer": answer,
        "base_url": base_url,
        "runner_status": status,
        "duration_ms": duration_ms,
        "evidence_url": evidence_url,
    }


# ============================================================
# SAUCEDEMO fallback
# ============================================================
def _looks_like_saucedemo(url: str) -> bool:
    return "saucedemo.com" in (url or "").lower()


def _needs_visibility_checks(prompt: str) -> bool:
    p = H.low(prompt)
    return any(k in p for k in ["visible", "visibilidad", "esté visible", "este visible"])


def _fallback_steps_saucedemo(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    if not _looks_like_saucedemo(base_url) or not _needs_visibility_checks(prompt):
        return None

    p = H.low(prompt)
    want_user = ("username" in p) or ("usuario" in p)
    want_pass = ("password" in p) or ("contraseña" in p) or ("contrasena" in p)
    want_login = ("login" in p) or ("botón" in p) or ("boton" in p) or ("iniciar sesión" in p) or ("iniciar sesion" in p)

    if not (want_user or want_pass or want_login):
        return None

    steps: List[Dict[str, Any]] = [{"action": "goto", "url": base_url}, {"action": "wait_ms", "ms": 300}]

    if want_user:
        steps.append({"action": "assert_visible", "selector": "#user-name"})
    if want_pass:
        steps.append({"action": "assert_visible", "selector": "#password"})
    if want_login:
        steps.append({"action": "assert_visible", "selector": "#login-button"})

    return steps


# ============================================================
# MAIN
# ============================================================
def handle_chat_run(req: Any) -> Dict[str, Any]:
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Prompt vacío")

    session_id, session = H.get_session(getattr(req, "session_id", None))

    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    store.add_message(thread_id, "user", prompt, meta={"source": "ui"})

    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("No pude cargar historial (continuo igual).", exc_info=True)
        history_msgs = []

    # MEMORY
    if _is_memory_query(prompt):
        mem = _summarize_last_execute(history_msgs)
        if not mem:
            answer = "Aún no veo una ejecución previa en este chat. Pídeme que ejecute una prueba y luego te resumo el resultado."
            store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "memory": True})
            return {"mode": "advise", "session_id": session_id, "thread_id": thread_id, "answer": answer, **_confidence("advise", prompt, None)}

        answer = mem["answer"]
        store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "memory": True, **mem})
        return {
            "mode": "advise",
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

    messages: List[Dict[str, str]] = [{"role": "system", "content": _system_prompt_for_mode(mode)}]
    for m in history_msgs:
        role = (m.get("role") or "").strip()
        content = (m.get("content") or "").strip()
        if role in ("user", "assistant") and content:
            messages.append({"role": role, "content": content})

    # EXECUTE
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
            return {"mode": "execute", "session_id": session_id, "thread_id": thread_id, "answer": answer, **_confidence("execute", prompt, None)}

        steps = _fallback_steps_saucedemo(prompt, base_url)

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
                            f"Instrucción: {prompt}\n\n"
                            "Devuelve SOLO los pasos. Puede ser un tool-call estilo run_qa_test o JSON con {steps:[...]}. "
                            "Acciones: goto, fill, click, press, assert_visible, assert_text_contains, wait_ms."
                        ),
                    }
                ],
                temperature=settings.EXEC_TEMPERATURE,
                max_tokens=settings.EXEC_MAX_TOKENS,
            )
            raw = (resp.choices[0].message.content or "").strip()
            steps = H.extract_steps_from_text(raw)

        if not steps and _looks_like_saucedemo(base_url) and _needs_visibility_checks(prompt):
            steps = [
                {"action": "goto", "url": base_url},
                {"action": "wait_ms", "ms": 300},
                {"action": "assert_visible", "selector": "#user-name"},
                {"action": "assert_visible", "selector": "#password"},
                {"action": "assert_visible", "selector": "#login-button"},
            ]

        if not steps:
            answer = "No pude generar pasos ejecutables. Dime el botón/campo exacto y el texto esperado."
            store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "base_url": base_url})
            return {"mode": "execute", "session_id": session_id, "thread_id": thread_id, "answer": answer, **_confidence("execute", prompt, base_url)}

        # ✅ defensivo: si steps vino como [], no rompemos ensure_goto
        H.ensure_goto(steps, base_url)
        H.update_last_url(session, steps, fallback=base_url)

        started = time.time()
        result = execute_test(
            steps=steps,
            base_url=base_url,
            headless=bool(getattr(req, "headless", True)),
        )
        duration_ms = int((time.time() - started) * 1000)

        evidence_url: Optional[str] = None
        cloud_public_id: Optional[str] = None
        evidence_id = (result.get("evidence_id") or f"EV-{thread_id[:6]}{int(time.time())}").strip()

        try:
            screenshot_b64 = result.get("screenshot_b64") or result.get("screenshotBase64")
            if screenshot_b64 and upload_screenshot_b64 and getattr(settings, "HAS_CLOUDINARY", False):
                up = upload_screenshot_b64(screenshot_b64, evidence_id=evidence_id, folder="vanya/evidence")
                evidence_url = up.get("url")
                cloud_public_id = up.get("public_id")
        except Exception:
            logger.warning("Evidence upload failed (continuing).", exc_info=True)

        if hasattr(H, "render_execute_answer"):
            answer = H.render_execute_answer(result, evidence_url=evidence_url)  # type: ignore
        else:
            status = result.get("status") or ("passed" if result.get("ok", True) else "failed")
            answer = f"✅ Ejecutado ({status})."
            if evidence_url:
                answer += f"\nEvidence: {evidence_url}"

        runner_status = result.get("status") or ("ok" if result.get("ok", True) else "error")

        assistant_meta: Dict[str, Any] = {
            "mode": "execute",
            "base_url": base_url,
            "duration_ms": duration_ms,
            "runner_status": runner_status,
            "evidence_id": evidence_id,
            "evidence_url": evidence_url,
            "cloudinary_public_id": cloud_public_id,
            "steps": steps,
        }

        store.add_message(thread_id, "assistant", answer, meta=assistant_meta)

        return {
            "mode": "execute",
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "runner": result,
            "evidence_url": evidence_url,
            "duration_ms": duration_ms,
            **_confidence("execute", prompt, base_url),
        }

    # ADVISE / DOC
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
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        **_confidence(mode, prompt, None),
    }