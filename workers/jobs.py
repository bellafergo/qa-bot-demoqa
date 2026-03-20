# workers/jobs.py
from __future__ import annotations

import logging
import random
import time
import traceback
from typing import Any, Dict, Optional

from services.run_store import save_run
from services.failure_intelligence import classify_failure as _classify_failure

from runner import execute_test

logger = logging.getLogger("vanya.worker")

# ============================================================
# Secret redaction — applied to all error messages before
# they are stored in DB or emitted to logs.
# ============================================================
from core.redaction import redact_secrets as _redact


# ============================================================
# Transient-error retry with exponential backoff
# ============================================================
_MAX_RETRIES = 3
_BACKOFF_BASE = 2.0
_TRANSIENT_KEYWORDS = ("timeout", "connection", "network", "temporary", "reset by peer", "broken pipe")


def _is_transient(exc: Exception) -> bool:
    """Return True for errors that are likely temporary and safe to retry."""
    if isinstance(exc, (TimeoutError, ConnectionError, OSError)):
        return True
    msg = str(exc).lower()
    return any(kw in msg for kw in _TRANSIENT_KEYWORDS)


def _run_with_retry(payload: Dict[str, Any]) -> Dict[str, Any]:
    """Call execute_test(**payload) with up to _MAX_RETRIES attempts."""
    for attempt in range(1, _MAX_RETRIES + 1):
        try:
            return execute_test(**payload)
        except Exception as e:
            if attempt < _MAX_RETRIES and _is_transient(e):
                wait = _BACKOFF_BASE ** attempt + random.uniform(0, 0.5)
                logger.warning(
                    "transient error on attempt %d/%d, retrying in %.1fs — %s: %s",
                    attempt,
                    _MAX_RETRIES,
                    wait,
                    type(e).__name__,
                    _redact(str(e)),
                )
                time.sleep(wait)
            else:
                raise  # non-transient or last attempt → propagate to outer handler


# ============================================================
# Helpers
# ============================================================
def _merge_meta(run: Dict[str, Any], meta: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    if meta and isinstance(meta, dict):
        run_meta = run.get("meta")
        if not isinstance(run_meta, dict):
            run_meta = {}
        merged = dict(run_meta)
        merged.update(meta)
        run["meta"] = merged
    return run


# ============================================================
# Job handlers
# ============================================================
def run_execute_steps_job(evidence_id: str, payload: Dict[str, Any], meta: Dict[str, Any]) -> Dict[str, Any]:
    t0 = time.time()
    save_run({
        "evidence_id": evidence_id,
        "status": "running",
        "started_at": int(t0 * 1000),
        "kind": "steps",
        "meta": meta or {},
    })

    try:
        run = _run_with_retry(payload)

        if not isinstance(run, dict):
            run = {"status": "failed", "error_message": "Runner returned non-dict"}

        run["evidence_id"] = evidence_id  # fuerza consistencia
        run = _merge_meta(run, meta)

        t1 = time.time()
        run.setdefault("duration_ms", int((t1 - t0) * 1000))
        run.setdefault("finished_at", int(t1 * 1000))

        # Normaliza status si viene raro
        status = (run.get("status") or "").lower()
        if status not in ("passed", "failed"):
            run["status"] = "failed" if run.get("ok") is False else (run.get("status") or "failed")

        # Enrich failed runs with structured failure analysis
        if run.get("status") == "failed":
            run["failure_analysis"] = _classify_failure(run)

        save_run(run)
        return run

    except Exception as e:
        t1 = time.time()
        raw_err = f"{type(e).__name__}: {e}\n{traceback.format_exc()}"
        err = _redact(raw_err)
        run = {
            "evidence_id": evidence_id,
            "status": "failed",
            "started_at": int(t0 * 1000),
            "finished_at": int(t1 * 1000),
            "duration_ms": int((t1 - t0) * 1000),
            "error_message": err[:8000],
            "kind": "steps",
            "meta": meta or {},
        }
        run["failure_analysis"] = _classify_failure(run)
        save_run(run)
        logger.exception("steps job failed evidence_id=%s", evidence_id)
        return run


def run_suite_job(evidence_id: str, payload: Dict[str, Any], meta: Dict[str, Any]) -> Dict[str, Any]:
    """
    Worker job: execute a full TestSuite and persist the SuiteRunResult.
    Payload keys: "suite" (TestSuite dict), "env" (ExecEnv dict, optional).
    The result is saved under evidence_id so /runs/{evidence_id} works.
    """
    t0 = time.time()
    save_run({
        "evidence_id": evidence_id,
        "status": "running",
        "started_at": int(t0 * 1000),
        "kind": "suite",
        "meta": meta or {},
    })

    try:
        from core.suite_models import TestSuite
        from core.exec_env import ExecEnv
        from services.test_orchestrator import run_suite

        suite = TestSuite(**payload["suite"])
        env   = ExecEnv(**payload["env"]) if payload.get("env") else None

        suite_result = run_suite(suite, env=env)

        run: Dict[str, Any] = suite_result.model_dump()
        run["evidence_id"] = evidence_id
        run["kind"]        = "suite"
        run = _merge_meta(run, meta)

        t1 = time.time()
        run.setdefault("finished_at", int(t1 * 1000))

        save_run(run)
        return run

    except Exception as e:
        t1 = time.time()
        raw_err = f"{type(e).__name__}: {e}\n{traceback.format_exc()}"
        err = _redact(raw_err)
        run = {
            "evidence_id": evidence_id,
            "status": "error",
            "started_at": int(t0 * 1000),
            "finished_at": int(t1 * 1000),
            "duration_ms": int((t1 - t0) * 1000),
            "error_message": err[:8000],
            "kind": "suite",
            "meta": meta or {},
        }
        save_run(run)
        logger.exception("suite job failed evidence_id=%s", evidence_id)
        return run
