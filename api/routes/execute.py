# api/routes/execute.py
from __future__ import annotations

import logging
import time
import uuid
from typing import Any, Dict, List, Optional

from fastapi import APIRouter
from pydantic import BaseModel

from services.run_store import save_run, get_run
from services.queue import get_queue
from workers.jobs import run_execute_steps_job

logger = logging.getLogger("vanya")
router = APIRouter()


class ExecuteStepsRequest(BaseModel):
    steps: List[Dict[str, Any]]
    base_url: Optional[str] = None
    headless: bool = True
    viewport: Optional[Dict[str, int]] = None
    timeout_s: Optional[int] = None
    expected: Optional[str] = None
    meta: Optional[Dict[str, Any]] = None  # tags/pr/etc


def _new_evidence_id() -> str:
    return str(uuid.uuid4())


def _enqueue(kind: str, fn, payload: Dict[str, Any], meta: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    evidence_id = _new_evidence_id()

    # Run inicial (para que la UI pueda hacer polling inmediato)
    run = {
        "evidence_id": evidence_id,
        "status": "queued",
        "created_at": int(time.time() * 1000),
        "kind": kind,
        "meta": meta or {},
        "request": payload,  # opcional, útil para debugging
    }

    try:
        save_run(run)
    except Exception as e:
        logger.warning("save_run(queued) failed (best-effort): %s: %s", type(e).__name__, e)

    # Encolar el job
    q = get_queue("vanya")
    # job_id = evidence_id para que sea trazable
    q.enqueue(fn, evidence_id, payload, meta or {}, job_id=evidence_id)

    return {
        "ok": True,
        "evidence_id": evidence_id,
        "status": "queued",
        "poll_url": f"/runs/{evidence_id}",
    }


@router.post("/execute_steps")
def execute_steps(req: ExecuteStepsRequest) -> Dict[str, Any]:
    payload = {
        "steps": req.steps,
        "base_url": req.base_url,
        "headless": req.headless,
        "viewport": req.viewport,
        "timeout_s": req.timeout_s,
        "expected": req.expected,
    }
    return _enqueue("steps", run_execute_steps_job, payload, req.meta)
