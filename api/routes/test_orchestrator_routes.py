# api/routes/test_orchestrator_routes.py
"""
Test Orchestrator REST API
==========================

POST /orchestrator/jobs/single        — enqueue one test case
POST /orchestrator/jobs/suite         — enqueue multiple test cases (ids or filters)
GET  /orchestrator/jobs               — list recent jobs
GET  /orchestrator/jobs/{job_id}      — get full job status
"""
from __future__ import annotations

import logging
from typing import List, Literal, Optional

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field

from models.orchestrator_job import OrchestratorJob
from services.catalog_orchestrator import orchestrator_service

logger = logging.getLogger("vanya.orchestrator_routes")

router = APIRouter(prefix="/orchestrator", tags=["orchestrator"])


# ── Request bodies ────────────────────────────────────────────────────────────

class SingleJobRequest(BaseModel):
    test_case_id: str
    environment:  str = "default"


class SuiteJobRequest(BaseModel):
    environment:   str = "default"
    limit:         int = Field(default=50, ge=1, le=200)

    # Explicit ID list — takes priority over filters when provided
    test_case_ids: Optional[List[str]] = None

    # Catalog filters — used only when test_case_ids is None
    module:   Optional[str] = None
    type:     Optional[Literal["smoke", "regression", "functional", "negative", "e2e"]] = None
    priority: Optional[Literal["low", "medium", "high", "critical"]] = None
    tags:     Optional[List[str]] = None


# ── Endpoints ─────────────────────────────────────────────────────────────────

@router.post("/jobs/single", response_model=OrchestratorJob, status_code=202)
def enqueue_single(body: SingleJobRequest):
    """
    Enqueue a single test case for async execution.

    Returns the OrchestratorJob immediately with status=queued.
    Poll GET /orchestrator/jobs/{job_id} to track progress.
    """
    try:
        job = orchestrator_service.enqueue_single(
            body.test_case_id,
            environment=body.environment,
        )
        return job
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception("enqueue_single failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/jobs/suite", response_model=OrchestratorJob, status_code=202)
def enqueue_suite(body: SuiteJobRequest):
    """
    Enqueue a suite of test cases for async execution.

    Priority of test-case selection:
      1. `test_case_ids` (explicit list) if provided
      2. Catalog filters: `module`, `type`, `priority`, `tags`

    Returns the OrchestratorJob immediately with status=queued.
    If no active test cases match, returns status=failed immediately.
    """
    try:
        job = orchestrator_service.enqueue_suite(
            test_case_ids=body.test_case_ids,
            module=body.module,
            type_=body.type,
            priority=body.priority,
            tags=body.tags,
            environment=body.environment,
            limit=body.limit,
        )
        return job
    except Exception as e:
        logger.exception("enqueue_suite failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/jobs", response_model=List[OrchestratorJob])
def list_jobs(
    limit: int = Query(100, ge=1, le=500),
    project_id: Optional[str] = Query(None, description="Jobs that include tests in this catalog project"),
):
    """Return recent orchestrator jobs, most recent first."""
    return orchestrator_service.list_jobs(limit=limit, project_id=project_id)


@router.get("/jobs/{job_id}", response_model=OrchestratorJob)
def get_job(job_id: str):
    """Return the full status and results of a single orchestrator job."""
    job = orchestrator_service.get_job(job_id)
    if job is None:
        raise HTTPException(status_code=404, detail=f"Job '{job_id}' not found")
    # Enrich with retry_job_ids (jobs spawned from this one via retry-failed)
    from services.db.orchestrator_job_repository import orch_job_repo
    retry_ids = orch_job_repo.list_job_ids_by_parent_job(job_id)
    if retry_ids:
        job.retry_job_ids = retry_ids
    return job
