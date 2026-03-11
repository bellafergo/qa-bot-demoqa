# workers/jobs.py
from __future__ import annotations

import logging
import time
import traceback
from typing import Any, Dict, Optional

from services.run_store import save_run

from runner import execute_test

logger = logging.getLogger("vanya.worker")


def _merge_meta(run: Dict[str, Any], meta: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    if meta and isinstance(meta, dict):
        run_meta = run.get("meta")
        if not isinstance(run_meta, dict):
            run_meta = {}
        merged = dict(run_meta)
        merged.update(meta)
        run["meta"] = merged
    return run


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
        # Ejecuta runner normal
        run = execute_test(**payload)

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

        save_run(run)
        return run

    except Exception as e:
        t1 = time.time()
        err = f"{type(e).__name__}: {e}\n{traceback.format_exc()}"
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
        save_run(run)
        logger.exception("steps job failed evidence_id=%s", evidence_id)
        return run


