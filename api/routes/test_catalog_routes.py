# api/routes/test_catalog_routes.py
"""
Test Catalog REST API
=====================

GET  /tests                    — list test cases (with optional filters)
POST /tests                    — create a new test case
GET  /tests/{test_case_id}     — get one test case
DELETE /tests/{test_case_id}   — remove a test case

POST /tests/{test_case_id}/run — execute a single test case
POST /tests/run-suite          — execute multiple test cases (filtered or by ID list)

GET  /test-runs                — list recent execution records
GET  /test-runs/{run_id}       — get one execution record
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Literal, Optional

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field

from models.test_case import TestCase, TestCaseCreate, TestCaseSummary
from models.test_run import TestRun, SuiteRunResult
from services.test_catalog_service import catalog_service

logger = logging.getLogger("vanya.test_catalog_routes")
router = APIRouter(prefix="/tests", tags=["test-catalog"])
runs_router = APIRouter(prefix="/test-runs", tags=["test-catalog"])


# ── Request bodies ────────────────────────────────────────────────────────────

class RunTestCaseRequest(BaseModel):
    environment: str = "default"
    base_url: Optional[str] = None
    headless: bool = True
    timeout_s: Optional[int] = None


class RunSuiteRequest(BaseModel):
    environment: str = "default"
    base_url: Optional[str] = None
    headless: bool = True
    timeout_s: Optional[int] = None
    # Filter fields — all optional; if test_case_ids provided, filters are ignored
    test_case_ids: Optional[List[str]] = None
    module: Optional[str] = None
    type: Optional[Literal["smoke", "regression", "functional", "negative", "e2e"]] = None
    priority: Optional[Literal["low", "medium", "high", "critical"]] = None
    tags: Optional[List[str]] = None
    limit: int = Field(default=50, ge=1, le=200)


# ── List / create ─────────────────────────────────────────────────────────────

@router.get("", response_model=List[TestCaseSummary])
def list_test_cases(
    module:   Optional[str] = Query(None),
    type:     Optional[str] = Query(None, alias="type"),
    priority: Optional[str] = Query(None),
    status:   Optional[str] = Query("active"),
    tags:     Optional[List[str]] = Query(None),
    limit:    int = Query(200, ge=1, le=1000),
):
    """
    Return all test cases matching the optional filters.

    By default only `active` cases are returned.
    Pass `status=` to include `inactive` or leave empty to see all.
    """
    cases = catalog_service.list_test_cases(
        module=module,
        type_=type,
        priority=priority,
        status=status or None,   # empty string → no filter
        tags=tags,
        limit=limit,
    )
    return [
        TestCaseSummary(
            id=c.id,
            test_case_id=c.test_case_id,
            name=c.name,
            module=c.module,
            type=c.type,
            priority=c.priority,
            status=c.status,
            test_type=getattr(c, "test_type", "ui") or "ui",
            version=c.version,
            tags=c.tags,
            steps_count=len(c.steps),
            created_at=c.created_at,
            updated_at=c.updated_at,
        )
        for c in cases
    ]


@router.post("", response_model=TestCase, status_code=201)
def create_test_case(payload: TestCaseCreate):
    """
    Create a new test case in the catalog.

    `test_case_id` must be unique (e.g. `TC-LOGIN-042`).
    """
    try:
        return catalog_service.create_test_case(payload)
    except ValueError as e:
        raise HTTPException(status_code=409, detail=str(e))


# ── Single test case ──────────────────────────────────────────────────────────

@router.get("/{test_case_id}", response_model=TestCase)
def get_test_case(test_case_id: str):
    """Return the full test case including all steps and assertions."""
    tc = catalog_service.get_test_case(test_case_id)
    if tc is None:
        raise HTTPException(status_code=404, detail=f"Test case '{test_case_id}' not found")
    return tc


@router.delete("/{test_case_id}", status_code=204)
def delete_test_case(test_case_id: str):
    """Permanently remove a test case from the catalog."""
    ok = catalog_service.delete_test_case(test_case_id)
    if not ok:
        raise HTTPException(status_code=404, detail=f"Test case '{test_case_id}' not found")


# ── Execution endpoints ───────────────────────────────────────────────────────

@router.post("/run-suite", response_model=SuiteRunResult)
def run_suite(body: RunSuiteRequest):
    """
    Execute multiple test cases and return an aggregated SuiteRunResult.

    Priority of selection:
      1. If `test_case_ids` is provided, only those cases run (order preserved).
      2. Otherwise, `module` / `type` / `priority` / `tags` filters are applied.

    Only `active` test cases are executed.
    Execution is sequential; results are returned once all cases finish.
    """
    try:
        return catalog_service.run_suite(
            environment=body.environment,
            base_url=body.base_url,
            headless=body.headless,
            timeout_s=body.timeout_s,
            module=body.module,
            type_=body.type,
            priority=body.priority,
            tags=body.tags,
            test_case_ids=body.test_case_ids,
            limit=body.limit,
        )
    except Exception as e:
        logger.exception("run_suite failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{test_case_id}/run", response_model=TestRun)
def run_test_case(test_case_id: str, body: RunTestCaseRequest = RunTestCaseRequest()):
    """
    Execute a single test case by its `test_case_id`.

    Returns a `TestRun` with status, duration, logs, and executed step details.
    """
    try:
        return catalog_service.run_test_case(
            test_case_id,
            environment=body.environment,
            base_url=body.base_url,
            headless=body.headless,
            timeout_s=body.timeout_s,
        )
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception("run_test_case failed for %s", test_case_id)
        raise HTTPException(status_code=500, detail=str(e))


# ── Run history ───────────────────────────────────────────────────────────────

@runs_router.get("", response_model=List[TestRun])
def list_test_runs(
    test_case_id: Optional[str] = Query(None),
    limit: int = Query(100, ge=1, le=500),
):
    """Return recent test execution records, most recent first."""
    return catalog_service.list_runs(test_case_id=test_case_id, limit=limit)


@runs_router.get("/{run_id}", response_model=TestRun)
def get_test_run(run_id: str):
    """Return a single execution record by run_id."""
    run = catalog_service.get_run(run_id)
    if run is None:
        raise HTTPException(status_code=404, detail=f"Run '{run_id}' not found")
    return run
