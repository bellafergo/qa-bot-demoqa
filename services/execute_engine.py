# services/execute_engine.py
from __future__ import annotations

import json
import logging
import re
import time
import uuid
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeout
from typing import Any, Dict, List, Optional, Tuple

from core.settings import settings
from core import chat_helpers as H
from services import store
from runner import execute_test

from core.redaction import redact_secrets, redact_steps
from core.step_compiler import compile_steps_from_prompt, ensure_has_assert
from core.step_validator import validate_steps
from core.step_normalizer import normalize_steps_to_target as _normalize_steps_to_target

from services.evidence_pipeline import process_evidence
from services.chat_run_test_name import fallback_test_name_from_prompt, resolve_chat_run_test_name
from services.step_llm_generator import generate_steps_llm

logger = logging.getLogger("vanya.execute_engine")


# ============================================================
# Conversational helpers (UX execute mode)
# ============================================================
def _is_probably_spanish(text: str) -> bool:
    s = (text or "").lower()
    # Heurística: basta con detectar términos comunes en español
    return any(k in s for k in ("quiero", "valida", "validar", "historia", "login", "credencial", "credenciales", "mensaje", "error", "acceso", "no permite", "agregar", "carrito", "buscar"))


def _infer_flow_summary(prompt: str) -> str:
    p = (prompt or "").lower()
    neg_login = (
        ("login" in p or "iniciar sesión" in p or "iniciar sesion" in p or "sign in" in p or "sesión" in p)
        and any(k in p for k in (
            "invalid", "incorrect", "wrong", "failing",
            "credencial", "credenciales",
            "no permite", "no acceso", "no autoriza", "acceso deneg", "error",
        ))
    )
    if neg_login:
        return "validar un login fallido con credenciales inválidas (y confirmar que se muestra un mensaje de error y no se otorga acceso)"

    if any(k in p for k in ("buscar", "search", "encuentra", "búsqueda")):
        return "buscar un producto y validar que los resultados sean los esperados"

    if any(k in p for k in ("agregar al carrito", "add to cart", "carrito", "cart")):
        return "agregar un producto al carrito y validar el cambio"

    return "ejecutar el flujo que describiste y validar el resultado"


def _infer_missing_near_base_url(prompt: str) -> dict:
    """
    When base_url is missing, we only ask for what blocks execution:
    - Always: base_url
    - Optionally: expected exact text (only if user seems to care about exact message)
    - Credentials are never requested as secrets; we only mention placeholders when relevant.
    """
    p = (prompt or "").lower()
    wants_exact_text = any(k in p for k in ("texto exacto", "mensaje exacto", "expected text", "exacto", "preciso", "que diga", "que muestre"))
    mentions_credentials = any(k in p for k in ("credencial", "credenciales", "invalid", "incorrect", "wrong", "contrasena", "password"))
    return {
        "wants_exact_text": wants_exact_text,
        "mentions_credentials": mentions_credentials,
    }


def _has_flow_intent(prompt: str) -> bool:
    """
    Detecta si el prompt ya trae pistas de un flujo (login/busqueda/carrito)
    o un objetivo verificable (error/mensaje/acceso).
    """
    p = (prompt or "").lower()
    return any(k in p for k in (
        "login", "iniciar sesión", "iniciar sesion", "sign in",
        "credencial", "credenciales", "invalid", "incorrect", "wrong",
        "buscar", "search", "producto", "productos", "resultados",
        "carrito", "add to cart", "cart", "checkout", "comprar", "pago",
        "mensaje", "error", "acceso", "no permite", "no acceso", "deneg",
        "validar", "validación", "validacion", "confirmar", "quiere", "quiero", "objetivo",
    ))


def _build_guided_missing_base_url_answer(prompt: str, *, persona: str) -> str:
    """
    Conversational alternative to a rigid checklist.
    We always request base_url, but we do NOT request "what to validate" or "credentials"
    when the prompt already contains enough intent for a guided assumption.
    """
    is_es = _is_probably_spanish(prompt)
    summary = _infer_flow_summary(prompt)
    missing = _infer_missing_near_base_url(prompt)

    if is_es:
        opt = ""
        if missing["wants_exact_text"]:
            opt = " Si tienes el texto exacto que esperas del error, pégalo aquí; si no, validaré que el mensaje de error sea visible."
        else:
            opt = " Si quieres precisión máxima, también puedes pegar el texto exacto del error esperado; si no, validaré que el mensaje de error sea visible."

        cred_opt = ""
        if missing["mentions_credentials"] or "login" in (prompt or "").lower():
            cred_opt = " Para esta prueba negativa usaré credenciales inválidas de ejemplo (placeholders), sin inventar secretos reales."

        flow_hint = ""
        if not _has_flow_intent(prompt):
            flow_hint = (
                " Para que la ejecución sea útil, dime qué resultado quieres validar "
                "(por ejemplo: que aparezca un mensaje, que un botón cambie, o que el acceso no se otorgue)."
            )

        return (
            f"Entendí que {summary}.\n\n"
            f"Para prepararlo, solo necesito la URL de tu sistema (o indica \"same\" si ya tienes un `base_url` en tu sesión)."
            f"{cred_opt}{opt}{flow_hint}"
        )

    # Fallback EN (if prompt is mostly English)
    opt = ""
    if missing["wants_exact_text"]:
        opt = " If you have the exact expected error text, paste it - otherwise I will validate that an error message is visible."
    else:
        opt = " For best results, paste the exact expected error text (optional). Otherwise I will validate the error is visible."
    cred_opt = ""
    if missing["mentions_credentials"] or "login" in (prompt or "").lower():
        cred_opt = " For the negative test, I'll use invalid example credentials (placeholders), without using real secrets."

    return (
        f"I understood you want {summary}.\n\n"
        f"To prepare the run, I only need the system URL (or tell me \"same\" if a base_url is already available in your session)."
        f"{cred_opt}{opt}"
    )


# ============================================================
# Confidence helper (UI-friendly, para EXECUTE)
# ============================================================
def _confidence(mode: str, prompt: str, base_url: Optional[str]) -> Dict[str, Any]:
    score = 0.55
    if mode == "execute":
        score = 0.65 + (0.15 if base_url else -0.15)
    elif mode == "doc":
        score = 0.60

    if len(prompt or "") < 25:
        score -= 0.10

    score = max(0.10, min(0.95, score))
    label = "high" if score >= 0.80 else "medium" if score >= 0.55 else "low"
    return {"confidence_0_1": round(score, 2), "confidence_label": label}


# ============================================================
# Runner meta normalizer (contract estable para UI)
# ============================================================
def _normalize_runner_meta(meta: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    meta = meta or {}

    runner = meta.get("runner") or {}
    if not isinstance(runner, dict):
        runner = {}

    evidence_url = (
        runner.get("evidence_url")
        or runner.get("screenshot_url")
        or runner.get("screenshotUrl")
        or meta.get("evidence_url")
        or meta.get("screenshot_url")
        or meta.get("evidenceUrl")
        or meta.get("screenshotUrl")
    )

    report_url = (
        runner.get("report_url")
        or runner.get("report_pdf_url")
        or runner.get("reportUrl")
        or meta.get("report_url")
        or meta.get("report_pdf_url")
        or meta.get("reportUrl")
    )

    status = (
        runner.get("status")
        or meta.get("runner_status")
        or meta.get("runnerStatus")
        or meta.get("status")
        or "unknown"
    )
    if isinstance(status, str):
        status = status.strip() or "unknown"

    duration_ms = (
        runner.get("duration_ms")
        or runner.get("durationMs")
        or meta.get("duration_ms")
        or meta.get("durationMs")
    )

    raw_runner = runner.get("raw") if isinstance(runner.get("raw"), dict) else runner

    meta["runner"] = {
        "status": status,
        "evidence_url": evidence_url,
        "report_url": report_url,
        "duration_ms": duration_ms,
        "raw": raw_runner,
        "pdf_error": meta.get("pdf_error"),
    }

    meta["evidence_url"] = evidence_url
    meta["report_url"] = report_url
    meta["runner_status"] = status
    meta["duration_ms"] = duration_ms

    return meta


# ============================================================
# Helpers EXECUTE (step compilation delegated to core.step_compiler)
# ============================================================
def _render_execute_answer(
    status: str,
    msg: str,
    evidence_url: Optional[str],
    report_url: Optional[str],
    evidence_id: str,
    pdf_error: Optional[str] = None,
) -> str:
    st = (status or "unknown").strip()
    prefix = "✅ Executed" if st.lower() in ("passed", "ok", "success") else "⚠️ Executed"
    parts = [f"{prefix} ({st})." + (f" {msg}" if msg else "")]
    if evidence_id:
        parts.insert(1, f"· evid: {evidence_id}")
    if evidence_url:
        parts.append(f"📸 Evidence: {evidence_url}")
    if report_url:
        parts.append(f"📄 Report: {report_url}")
    elif pdf_error:
        parts.append(f"📄 Report: not available ({pdf_error})")
    return "\n".join(parts).strip()


# ============================================================
# EXECUTE mode handler (exportado)
# ============================================================
def handle_execute_mode(
    req: Any,
    session: Dict[str, Any],
    prompt: str,
    thread_id: str,
    persona: str,
    messages: List[Dict[str, str]],
    correlation_id: Optional[str] = None,
    client_id: Optional[str] = None,
    workspace_id: Optional[str] = None,
) -> Dict[str, Any]:
    t0 = time.time()
    session_id = session.get("id")

    try:
        base_url = H.pick_base_url(req, session, prompt)
    except Exception:
        logger.exception("pick_base_url raised unexpectedly - treating as no base_url")
        answer = _build_guided_missing_base_url_answer(prompt, persona=persona)
        store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona})
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "correlation_id": correlation_id,
            "steps_count": 0,
            **_confidence("execute", prompt, None),
        }

    # ---------------------------------------------------------
    # Base URL es obligatorio para el engine genérico
    # ---------------------------------------------------------
    if not base_url:
        answer = _build_guided_missing_base_url_answer(prompt, persona=persona)
        store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona})
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "correlation_id": correlation_id,
            "steps_count": 0,
            **_confidence("execute", prompt, None),
        }

    # 1) Compilación determinística (login resolver + parser)
    steps = compile_steps_from_prompt(prompt, base_url)
    if steps:
        logger.info("Step compiler produced deterministic steps")

    # 2) LLM fallback cuando la compilación no produce steps
    if not steps:
        steps = generate_steps_llm(prompt, base_url, messages)
        if steps:
            logger.info("Step LLM generator produced fallback steps")

    if not steps:
        # Conversational guidance instead of a rigid "tell me selector" checklist.
        answer = (
            "Entendí tu intención, pero no pude identificar con suficiente precisión "
            "el elemento o criterio exacto para validarlo.\n\n"
            "Para continuar, dime solo una de estas dos cosas (la que te sea más fácil):\n"
            "1) qué elemento validar (selector o el texto visible del elemento), o\n"
            "2) el texto exacto esperado del mensaje de error (si lo tienes).\n\n"
            "Si no tienes el texto exacto, también puedo validar un criterio genérico como "
            "\"el mensaje de error se ve\" o \"el acceso es denegado\", según tu descripción."
        )
        store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona, "base_url": base_url})
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "correlation_id": correlation_id,
            "steps_count": 0,
            **_confidence("execute", prompt, base_url),
        }

    steps = ensure_has_assert(steps, base_url)
    steps = _normalize_steps_to_target(steps)

    validation = validate_steps(steps)
    if not validation.valid:
        first = validation.errors[0] if validation.errors else None
        if first:
            # Use structured fields when available (error_type/hint) to guide the user.
            hint = getattr(first, "hint", None) or None
            error_type = getattr(first, "error_type", None) or "invalid_step"
            answer = (
                "Pude preparar la ejecución, pero un paso no cumple el formato esperado.\n\n"
                f"Paso #{first.step_index + 1}: {first.action or 'unknown'} - {first.message}\n"
                f"Tipo de error: {error_type}\n"
                + (f"Pista: {hint}\n" if hint else "")
                + "Si me confirmas el selector/texto correcto para ese paso, lo preparo y reintento."
            )
        else:
            answer = "Pude inferir la ejecución, pero la validación de pasos falló. Intenta de nuevo con más detalle."
        store.add_message(
            thread_id, "assistant", answer,
            meta={"mode": "execute", "persona": persona, "base_url": base_url, "validation_errors": [e.model_dump() for e in validation.errors]},
        )
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "validation_errors": [e.model_dump() for e in validation.errors],
            "correlation_id": correlation_id,
            "steps_count": len(steps),
            **_confidence("execute", prompt, base_url),
        }

    # Log the final step list so any drop is visible in the run report
    logger.info(
        "[ENGINE] executing %d steps%s: %s",
        len(steps),
        f" [rid={correlation_id}]" if correlation_id else "",
        [{"i": i, "action": s.get("action"), "sel": s.get("selector")} for i, s in enumerate(steps)],
    )

    # 3) Ejecutar runner (nombre semántico en paralelo: solo depende del prompt)
    runner_any: Any = {}
    runner: Dict[str, Any] = {}
    chat_test_name: str = fallback_test_name_from_prompt(prompt)
    executor: Optional[ThreadPoolExecutor] = None
    name_future = None

    try:
        executor = ThreadPoolExecutor(max_workers=1)
        name_future = executor.submit(resolve_chat_run_test_name, prompt)
        runner_any = execute_test(
            base_url=base_url,
            steps=steps,
            headless=bool(getattr(req, "headless", True)),
            timeout_s=settings.RUNNER_TIMEOUT_S,
            correlation_id=correlation_id,
        )
        runner = runner_any if isinstance(runner_any, dict) else {}
    except Exception as e:
        if executor is not None:
            executor.shutdown(wait=False)
        logger.exception("Runner execution failed")
        answer = (
            "No pude ejecutar la prueba en este momento.\n\n"
            "Para reintentar con éxito, confirma solo esto:\n"
            "1) que la URL es accesible desde el entorno, y\n"
            "2) que el flujo/elemento que querías validar existe como en tu descripción.\n\n"
            "Si tienes el texto exacto esperado del mensaje de error, pégalo y lo haré más preciso."
        )
        store.add_message(
            thread_id,
            "assistant",
            answer,
            meta={
                "mode": "execute",
                "persona": persona,
                "error": redact_secrets(f"{type(e).__name__}: {e}"),
                "base_url": base_url,
                "steps": redact_steps(steps),
            },
        )
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "error": f"{type(e).__name__}: {e}",
            "correlation_id": correlation_id,
            "steps_count": len(steps) if isinstance(steps, list) else 0,
            **_confidence("execute", prompt, base_url),
        }

    # Solo aceptar LLM si ya terminó (0s) o tras espera mínima; sin bloqueos largos
    _NAME_RESULT_MAX_WAIT_S = 0.35
    try:
        fut = name_future  # type: ignore[union-attr]
        if fut.done():
            chat_test_name = fut.result()
        else:
            chat_test_name = fut.result(timeout=_NAME_RESULT_MAX_WAIT_S)
    except FuturesTimeout:
        logger.debug("chat_run test_name not ready within %ss; using fallback", _NAME_RESULT_MAX_WAIT_S)
        chat_test_name = fallback_test_name_from_prompt(prompt)
    except Exception:
        chat_test_name = fallback_test_name_from_prompt(prompt)
    finally:
        if executor is not None:
            executor.shutdown(wait=False)

    if not (chat_test_name or "").strip():
        chat_test_name = fallback_test_name_from_prompt(prompt)

    ok = bool(runner.get("ok", True))
    status = str(runner.get("status") or ("passed" if ok else "failed")).strip().lower()
    if status not in ("passed", "failed", "unknown", "error"):
        status = "passed" if ok else "failed"
    # Preserve technical runner errors as "error" so UI/clients can
    # differentiate between assertion failures and runtime issues.

    # Support for expected-fail steps: if runner returned "failed" but at least
    # one step has expected="fail", the test actually passed.
    has_expected_fail = any(s.get("expected") == "fail" for s in steps)
    if status == "failed" and has_expected_fail:
        status = "passed"
        runner["message"] = "La prueba pasó porque el fallo era el resultado esperado."

    msg = str(runner.get("message") or runner.get("detail") or runner.get("reason") or "").strip()

    duration_ms = runner.get("duration_ms")
    try:
        duration_ms = int(duration_ms) if duration_ms is not None else int((time.time() - t0) * 1000)
    except Exception:
        duration_ms = int((time.time() - t0) * 1000)

    status_label = (
        "PASSED"
        if status == "passed"
        else "FAILED"
        if status == "failed"
        else "ERROR"
        if status == "error"
        else "UNKNOWN"
    )

    # 4) Evidence (pipeline centraliza PDF, upload, fallbacks)
    evidence_meta: Dict[str, Any] = {
        "thread_id": thread_id,
        "session_id": session_id,
        "headless": getattr(req, "headless", True),
    }
    if correlation_id:
        evidence_meta["correlation_id"] = correlation_id
    if client_id:
        evidence_meta["client_id"] = client_id
    if workspace_id:
        evidence_meta["workspace_id"] = workspace_id
    evidence_result = process_evidence(
        runner=runner,
        prompt=prompt,
        base_url=base_url,
        steps=steps,
        evidence_id=None,
        status=status,
        duration_ms=duration_ms,
        meta=evidence_meta,
    )
    evidence_id = evidence_result["evidence_id"]
    evidence_url = evidence_result["evidence_url"]
    report_url = evidence_result["report_url"]
    pdf_error = evidence_result["pdf_error"]
    screenshot_data_url = evidence_result["screenshot_data_url"]

    # 6) save_run
    try:
        from services.run_store import save_run

        run_payload: Dict[str, Any] = {
            **(runner if isinstance(runner, dict) else {}),
            "evidence_id": evidence_id,
            "base_url": base_url,
            "prompt": redact_secrets(prompt),
            "steps": redact_steps(steps),
            "evidence_url": evidence_url,
            "report_url": report_url,
            "pdf_error": pdf_error,
            "duration_ms": duration_ms,
            "thread_id": thread_id,
            "session_id": session_id,
            "mode": "execute",
            "status": status,
            "status_label": status_label,
        }
        run_payload.setdefault("meta", {})
        run_payload["meta"] = dict(run_payload.get("meta") or {})
        run_payload["meta"]["test_name"] = chat_test_name
        if correlation_id or client_id or workspace_id:
            if correlation_id:
                run_payload["meta"]["correlation_id"] = correlation_id
            if client_id:
                run_payload["meta"]["client_id"] = client_id
            if workspace_id:
                run_payload["meta"]["workspace_id"] = workspace_id
        save_run(run_payload)
    except Exception:
        logger.exception("save_run failed (continuing)")

    # 7) respuesta
    answer = _render_execute_answer(
        status=status,
        msg=msg,
        evidence_url=evidence_url,
        report_url=report_url,
        evidence_id=evidence_id,
        pdf_error=pdf_error,
    )

    meta_runner = dict(runner) if isinstance(runner, dict) else {}
    meta_runner.setdefault("status", status)
    meta_runner.setdefault("duration_ms", duration_ms)
    if screenshot_data_url:
        meta_runner["screenshot_data_url"] = screenshot_data_url

    meta = {
        "mode": "execute",
        "persona": persona,
        "base_url": base_url,
        "steps": redact_steps(steps),
        "runner": {
            "status": status,
            "evidence_url": evidence_url,
            "report_url": report_url,
            "duration_ms": duration_ms,
            "raw": meta_runner,
            "pdf_error": pdf_error,
        },
        "evidence_url": evidence_url,
        "report_url": report_url,
        "pdf_error": pdf_error,
        "duration_ms": duration_ms,
        "status_label": status_label,
        "evidence_id": evidence_id,
    }

    meta = _normalize_runner_meta(meta)
    store.add_message(thread_id, "assistant", answer, meta=meta)

    return {
        "mode": "execute",
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        "runner": meta["runner"],
        "status_label": status_label,
        "evidence_id": evidence_id,
        "correlation_id": correlation_id,
        "evidence_url": meta["runner"].get("evidence_url"),
        "report_url": meta["runner"].get("report_url"),
        "duration_ms": meta["runner"].get("duration_ms"),
        "steps_count": meta["runner"].get("steps_count") or len(steps),
        **_confidence("execute", prompt, base_url),
    }
