# api/routes/execute.py
from __future__ import annotations

import logging
import re
import time
import uuid
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, Request
from pydantic import BaseModel, field_validator

from services.run_store import save_run, get_run
from services.queue import get_queue
from workers.jobs import run_execute_steps_job, run_suite_job

logger = logging.getLogger("vanya")
router = APIRouter()

# ── Request guardrails ────────────────────────────────────────────────────────
_MAX_STEPS = 200          # max steps per execute request
_MAX_URL_LEN = 2048       # RFC-compliant practical cap
_MAX_TIMEOUT_S = 300      # 5 min hard ceiling
_URL_RE = re.compile(r"^https?://", re.IGNORECASE)


class ExecuteStepsRequest(BaseModel):
    steps: List[Dict[str, Any]]
    base_url: Optional[str] = None
    headless: bool = True
    viewport: Optional[Dict[str, int]] = None
    timeout_s: Optional[int] = None
    expected: Optional[str] = None
    meta: Optional[Dict[str, Any]] = None  # tags/pr/etc

    @field_validator("steps")
    @classmethod
    def validate_steps(cls, v: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        if len(v) > _MAX_STEPS:
            raise ValueError(f"steps array exceeds maximum length of {_MAX_STEPS}")
        return v

    @field_validator("base_url")
    @classmethod
    def validate_base_url(cls, v: Optional[str]) -> Optional[str]:
        if v is None:
            return v
        if len(v) > _MAX_URL_LEN:
            raise ValueError(f"base_url exceeds maximum length of {_MAX_URL_LEN} characters")
        if not _URL_RE.match(v):
            raise ValueError("base_url must start with http:// or https://")
        return v

    @field_validator("timeout_s")
    @classmethod
    def validate_timeout(cls, v: Optional[int]) -> Optional[int]:
        if v is not None and v > _MAX_TIMEOUT_S:
            raise ValueError(f"timeout_s exceeds maximum of {_MAX_TIMEOUT_S} seconds")
        return v


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
def execute_steps(req: ExecuteStepsRequest, request: Request) -> Dict[str, Any]:
    payload = {
        "steps": req.steps,
        "base_url": req.base_url,
        "headless": req.headless,
        "viewport": req.viewport,
        "timeout_s": req.timeout_s,
        "expected": req.expected,
    }
    meta = dict(req.meta or {})
    correlation_id = getattr(request.state, "request_id", None)
    if correlation_id:
        meta["correlation_id"] = correlation_id
    return _enqueue("steps", run_execute_steps_job, payload, meta)


class ExecuteSuiteRequest(BaseModel):
    """
    POST /execute_suite — enqueue a full TestSuite for async execution.
    'suite' must be a serialized TestSuite dict.
    'env' is an optional ExecEnv dict (base_url, headless, timeout_s, …).
    """
    suite: Dict[str, Any]
    env:   Optional[Dict[str, Any]] = None
    meta:  Optional[Dict[str, Any]] = None


@router.post("/execute_suite")
def execute_suite(req: ExecuteSuiteRequest, request: Request) -> Dict[str, Any]:
    payload = {"suite": req.suite, "env": req.env}
    meta = dict(req.meta or {})
    correlation_id = getattr(request.state, "request_id", None)
    if correlation_id:
        meta["correlation_id"] = correlation_id
    return _enqueue("suite", run_suite_job, payload, meta)
