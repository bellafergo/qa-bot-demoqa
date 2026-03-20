# services/execute_engine.py
from __future__ import annotations

import json
import logging
import re
import time
import uuid
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
from services.step_llm_generator import generate_steps_llm

logger = logging.getLogger("vanya.execute_engine")


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
        logger.exception("pick_base_url raised unexpectedly — treating as no base_url")
        answer = (
            "To execute I need:\n"
            '- URL (or say "same")\n'
            "- What to validate (button / field / expected text)\n"
            "- Credentials (if applicable)"
        )
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
        answer = (
            "To execute I need:\n"
            "- URL (or say “same”)\n"
            "- What to validate (button / field / expected text)\n"
            "- Credentials (if applicable)"
        )
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
        answer = "I couldn't generate executable steps. Tell me the exact element (selector) or expected text."
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
        err_msgs = [f"Step {e.step_index}: {e.message}" for e in validation.errors]
        answer = (
            "I couldn't execute: some steps are invalid.\n"
            + "\n".join(f"· {m}" for m in err_msgs[:5])
        )
        if len(validation.errors) > 5:
            answer += f"\n... and {len(validation.errors) - 5} more."
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

    # 3) Ejecutar runner
    runner_any: Any = {}
    runner: Dict[str, Any] = {}

    try:
        runner_any = execute_test(
            base_url=base_url,
            steps=steps,
            headless=bool(getattr(req, "headless", True)),
            timeout_s=settings.RUNNER_TIMEOUT_S,
            correlation_id=correlation_id,
        )
        runner = runner_any if isinstance(runner_any, dict) else {}
    except Exception as e:
        logger.exception("Runner execution failed")
        answer = (
            "I couldn't run the test.\n"
            "Check:\n"
            "- URL accessible\n"
            "- Credentials (if any)\n"
            "- What exactly to validate (text/button/element)\n"
            "Then retry."
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
        if correlation_id or client_id or workspace_id:
            run_payload.setdefault("meta", {})
            run_payload["meta"] = dict(run_payload.get("meta") or {})
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
