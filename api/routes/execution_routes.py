# api/routes/execution_routes.py
"""
Execution Scheduler REST API
==============================

GET  /execution/health         — liveness check
GET  /execution/status         — live scheduler / worker-pool stats
POST /execution/run-batch      — create and schedule a batch job
POST /execution/retry-failed   — retry failed tests from a previous job
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.execution_scheduler_models import (
    BatchExecutionRequest,
    BatchExecutionResponse,
    ExecutionStatusResponse,
    RetryFailedRequest,
    RetryFailedResponse,
)

logger = logging.getLogger("vanya.execution_routes")

router = APIRouter(prefix="/execution", tags=["execution"])


# ── Health ─────────────────────────────────────────────────────────────────────

@router.get("/health")
def health():
    return {"status": "ok", "service": "execution_scheduler"}


# ── Status ────────────────────────────────────────────────────────────────────

@router.get("/status", response_model=ExecutionStatusResponse)
def status():
    """Return a live snapshot of the worker pool and queue state."""
    from services.catalog_orchestrator import get_execution_status
    try:
        stats = get_execution_status()
    except Exception as exc:
        logger.exception("execution/status failed")
        raise HTTPException(status_code=500, detail=f"Status error: {exc}")
    return ExecutionStatusResponse(**stats)


# ── Batch execution ───────────────────────────────────────────────────────────

@router.post("/run-batch", status_code=202, response_model=BatchExecutionResponse)
def run_batch(req: BatchExecutionRequest):
    """
    Schedule a batch of test cases for parallel execution.

    - Tests are sorted by priority/type before dispatch.
    - Execution is bounded by EXECUTION_MAX_UI_WORKERS / EXECUTION_MAX_API_WORKERS.
    - Returns immediately with the created job_id; poll /orchestrator/jobs/{job_id}
      for progress.
    """
    if not req.test_case_ids:
        raise HTTPException(status_code=422, detail="test_case_ids must not be empty")

    from services.catalog_orchestrator import orchestrator_service
    try:
        job = orchestrator_service.enqueue_suite(
            test_case_ids=req.test_case_ids,
            environment=req.environment,
        )
    except Exception as exc:
        logger.exception("execution/run-batch failed")
        raise HTTPException(status_code=500, detail=f"Batch execution error: {exc}")

    return BatchExecutionResponse(
        ok          = True,
        job_id      = job.job_id,
        status      = job.status,
        total_count = job.total_count,
    )


# ── Retry failed ──────────────────────────────────────────────────────────────

@router.post("/retry-failed", status_code=202, response_model=RetryFailedResponse)
def retry_failed(req: RetryFailedRequest):
    """
    Create and enqueue a new job that re-runs only the failed/errored
    tests from a previous job.

    Returns 404 if the original job is not found or has no failed tests.
    """
    from services.catalog_orchestrator import retry_failed_tests
    try:
        new_job = retry_failed_tests(req.job_id)
    except Exception as exc:
        logger.exception("execution/retry-failed failed")
        raise HTTPException(status_code=500, detail=f"Retry error: {exc}")

    if new_job is None:
        raise HTTPException(
            status_code=404,
            detail=f"Job '{req.job_id}' not found or has no failed tests to retry",
        )

    return RetryFailedResponse(
        ok          = True,
        job_id      = new_job.job_id,
        status      = new_job.status,
        total_count = new_job.total_count,
    )
