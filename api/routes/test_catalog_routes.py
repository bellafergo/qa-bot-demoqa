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

from fastapi import APIRouter, HTTPException, Query, Request
from pydantic import BaseModel, Field

from models.test_case import TestCase, TestCaseCreate, TestCaseSummary
from services.test_auto_fixer import is_unstable_selector
from models.test_run import TestRun, SuiteRunResult
from models.run_contract import CanonicalRun, CanonicalSuiteResult
from models.test_version_models import (
    TestVersionSummary, TestVersionDetail,
    RollbackRequest, RollbackResponse, VersionDiff,
)
from services.run_mapper import run_from_catalog_testrun, suite_from_catalog_suite_result
from services.run_history_service import run_history_service  # official run history source
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
    module:    Optional[str] = Query(None),
    type:      Optional[str] = Query(None, alias="type"),
    priority:  Optional[str] = Query(None),
    status:    Optional[str] = Query("active"),
    test_type: Optional[str] = Query(None),
    search:    Optional[str] = Query(None),
    tags:      Optional[List[str]] = Query(None),
    project_id: Optional[str] = Query(None, description="When set, return only tests in this project"),
    limit:     int = Query(200, ge=1, le=1000),
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
        test_type=test_type or None,
        search=search or None,
        tags=tags,
        project_id=project_id,
        limit=limit,
    )
    def _case_has_fragile_selectors(tc: TestCase) -> bool:
        for s in tc.steps:
            for fld in (s.selector, s.target):
                if isinstance(fld, str) and is_unstable_selector(fld):
                    return True
        for a in tc.assertions or []:
            for fld in (a.selector, a.target):
                if isinstance(fld, str) and is_unstable_selector(fld):
                    return True
        return False

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
            project_id=getattr(c, "project_id", None) or "default",
            version=c.version,
            tags=c.tags,
            steps_count=len(c.steps),
            has_fragile_selectors=_case_has_fragile_selectors(c),
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


class CreateTestFromRunRequest(BaseModel):
    run_id:     str = Field(..., min_length=1)
    name:       str = Field(..., min_length=1)
    project_id: str = Field(..., min_length=1)


@router.post("/from-run", response_model=TestCase, status_code=201)
def create_test_from_run(body: CreateTestFromRunRequest):
    """
    Create a catalog test from a completed execution run (history or in-memory run_store).

    `run_id` may be a SQLite test_runs.run_id or an evidence_id from planner/chat execute.
    """
    try:
        return catalog_service.create_test_from_run(
            run_id=body.run_id.strip(),
            name=body.name.strip(),
            project_id=body.project_id.strip(),
        )
    except ValueError as e:
        msg = str(e)
        if msg == "NAME_REQUIRED":
            raise HTTPException(status_code=400, detail="name is required")
        if msg == "RUN_ID_REQUIRED":
            raise HTTPException(status_code=400, detail="run_id is required")
        if msg == "RUN_NOT_FOUND":
            raise HTTPException(status_code=404, detail="Run not found")
        if msg == "RUN_HAS_NO_STEPS":
            raise HTTPException(status_code=400, detail="Run has no steps to save")
        if msg == "NO_STEPS_NORMALIZED":
            raise HTTPException(
                status_code=400,
                detail="Could not normalize run steps into catalog format",
            )
        if msg.startswith("DUPLICATE_SOURCE_RUN:"):
            existing_id = msg.split(":", 1)[1]
            raise HTTPException(
                status_code=409,
                detail=(
                    f"A catalog test already exists for this run "
                    f"(test_case_id={existing_id})"
                ),
            )
        raise HTTPException(status_code=400, detail=msg)


# ── Auto-fix preview (stateless) ─────────────────────────────────────────────

class AutoFixPreviewRequest(BaseModel):
    steps:      List[Dict[str, Any]] = Field(default_factory=list)
    assertions: List[Dict[str, Any]] = Field(default_factory=list)
    # ui → web auto-fix; desktop / api use runner-specific rules
    test_type:  Optional[str] = Field(default=None, description="ui | desktop | api")


@router.post("/auto-fix-preview")
def auto_fix_preview(body: AutoFixPreviewRequest):
    """
    Apply deterministic auto-fix rules to steps and assertions.

    Stateless — nothing is persisted.  Returns fixed steps, fixed assertions,
    and a human-readable list of changes made (type: "fix" | "warning").
    """
    from services.test_auto_fixer import auto_fix_test
    return auto_fix_test(
        body.steps,
        body.assertions,
        test_type=body.test_type,
    )


class CatalogAiEditPreviewRequest(BaseModel):
    """Body for POST /tests/ai-edit-preview (LLM proposal; does not persist)."""

    test_case_id: str
    name: str = ""
    module: str = ""
    priority: str = ""
    steps: List[Dict[str, Any]] = Field(default_factory=list)
    assertions: List[Dict[str, Any]] = Field(default_factory=list)
    instruction: str = ""


class CatalogAiEditPreviewResponse(BaseModel):
    steps: List[Dict[str, Any]]
    assertions: List[Dict[str, Any]]
    change_summary: List[str] = Field(default_factory=list)


@router.post("/ai-edit-preview", response_model=CatalogAiEditPreviewResponse)
def ai_edit_preview(body: CatalogAiEditPreviewRequest):
    """
    Propose updated steps/assertions from a natural-language instruction (OpenAI).
    Stateless preview only — caller must save via PUT /tests/{id} if desired.
    """
    from services.catalog_ai_edit import run_catalog_ai_edit

    try:
        return run_catalog_ai_edit(
            test_case_id=body.test_case_id,
            name=body.name,
            module=body.module,
            priority=body.priority,
            steps=body.steps,
            assertions=body.assertions,
            instruction=body.instruction,
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from e


# ── Single test case ──────────────────────────────────────────────────────────

@router.get("/{test_case_id}", response_model=TestCase)
def get_test_case(test_case_id: str):
    """Return the full test case including all steps and assertions."""
    tc = catalog_service.get_test_case(test_case_id)
    if tc is None:
        raise HTTPException(status_code=404, detail=f"Test case '{test_case_id}' not found")
    return tc


@router.put("/{test_case_id}", response_model=TestCase)
def update_test_case(test_case_id: str, payload: dict):
    """
    Update mutable fields of an existing test case (name, module, steps, etc.).
    Creates a new version snapshot automatically.
    """
    tc = catalog_service.get_test_case(test_case_id)
    if tc is None:
        raise HTTPException(status_code=404, detail=f"Test case '{test_case_id}' not found")
    source      = payload.pop("_source",      "manual")
    change_note = payload.pop("_change_note", "")
    updated = catalog_service.update_test_case(test_case_id, payload, source=source, change_note=change_note)
    if updated is None:
        raise HTTPException(status_code=404, detail=f"Test case '{test_case_id}' not found")
    return updated


# ── Version history ───────────────────────────────────────────────────────────

@router.get("/{test_case_id}/versions", response_model=List[TestVersionSummary])
def list_versions(test_case_id: str):
    """
    Return the complete version history for a test case, newest first.

    Each entry is a snapshot created at create/edit/rollback time.
    History is append-only — no version is ever deleted.
    """
    tc = catalog_service.get_test_case(test_case_id)
    if tc is None:
        raise HTTPException(status_code=404, detail=f"Test case '{test_case_id}' not found")
    return catalog_service.list_versions(test_case_id)


@router.get("/{test_case_id}/versions/{version}", response_model=TestVersionDetail)
def get_version(test_case_id: str, version: int):
    """Return the full snapshot for a specific version number."""
    v = catalog_service.get_version(test_case_id, version)
    if v is None:
        raise HTTPException(
            status_code=404,
            detail=f"Version {version} not found for '{test_case_id}'",
        )
    return v


@router.get("/{test_case_id}/versions/{from_version}/diff/{to_version}", response_model=VersionDiff)
def diff_versions(test_case_id: str, from_version: int, to_version: int):
    """
    Compare two version snapshots of a test case.

    Returns added/removed steps, assertions, and changed scalar fields.
    Both versions must exist. Order: from_version → to_version.
    """
    try:
        return catalog_service.diff_versions(test_case_id, from_version, to_version)
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))


@router.post("/{test_case_id}/rollback", response_model=RollbackResponse)
def rollback_test_case(test_case_id: str, body: RollbackRequest):
    """
    Restore a test case to the content of a previous version.

    Rollback CREATES a new version — it never deletes or overwrites history.

    Example: TC-100 at v3. Rollback to v1 → creates v4 with v1's content.
    Current version becomes v4.
    """
    try:
        return catalog_service.rollback_test_case(
            test_case_id,
            body.version,
            reason=body.reason,
        )
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception("rollback failed for %s", test_case_id)
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/{test_case_id}", status_code=204)
def delete_test_case(test_case_id: str):
    """Permanently remove a test case from the catalog."""
    ok = catalog_service.delete_test_case(test_case_id)
    if not ok:
        raise HTTPException(status_code=404, detail=f"Test case '{test_case_id}' not found")


# ── Execution endpoints ───────────────────────────────────────────────────────

@router.post("/run-suite", response_model=CanonicalSuiteResult)
def run_suite(body: RunSuiteRequest, request: Request):
    """
    Execute multiple test cases and return an aggregated CanonicalSuiteResult.

    Priority of selection:
      1. If `test_case_ids` is provided, only those cases run (order preserved).
      2. Otherwise, `module` / `type` / `priority` / `tags` filters are applied.

    Only `active` test cases are executed.
    Execution is sequential; results are returned once all cases finish.
    """
    try:
        correlation_id = getattr(request.state, "request_id", None)
        client_id = getattr(request.state, "client_id", None)
        workspace_id = getattr(request.state, "workspace_id", None)
        sr = catalog_service.run_suite(
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
            correlation_id=correlation_id,
            client_id=client_id,
            workspace_id=workspace_id,
        )
        return suite_from_catalog_suite_result(sr)
    except Exception as e:
        logger.exception("run_suite failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/{test_case_id}/run", response_model=CanonicalRun)
def run_test_case(test_case_id: str, request: Request, body: RunTestCaseRequest = RunTestCaseRequest()):
    """
    Execute a single test case by its `test_case_id`.

    Returns a CanonicalRun with status, duration, steps, and artifacts.
    """
    try:
        correlation_id = getattr(getattr(request, "state", None), "request_id", None)
        client_id = getattr(getattr(request, "state", None), "client_id", None)
        workspace_id = getattr(getattr(request, "state", None), "workspace_id", None)
        tr = catalog_service.run_test_case(
            test_case_id,
            environment=body.environment,
            base_url=body.base_url,
            headless=body.headless,
            timeout_s=body.timeout_s,
            correlation_id=correlation_id,
            client_id=client_id,
            workspace_id=workspace_id,
        )
        return run_from_catalog_testrun(tr)
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception("run_test_case failed for %s", test_case_id)
        raise HTTPException(status_code=500, detail=str(e))


# ── Run history ───────────────────────────────────────────────────────────────

@runs_router.get("", response_model=List[CanonicalRun])
def list_test_runs(
    test_case_id: Optional[str] = Query(None),
    project_id: Optional[str] = Query(None, description="Filter runs to tests in this catalog project"),
    limit: int = Query(100, ge=1, le=500),
):
    """
    Return recent test execution records as CanonicalRun, most recent first.

    Source: run_history_service → Supabase ``qa_runs`` when configured, else SQLite.
    Async chat/execute runs are accessible via GET /runs/{evidence_id}.
    """
    return run_history_service.list_runs(
        test_case_id=test_case_id,
        project_id=project_id,
        limit=limit,
    )


@runs_router.get("/{run_id}", response_model=CanonicalRun)
def get_test_run(run_id: str):
    """
    Return a single execution record as CanonicalRun.

    Source: run_history_service → ``qa_runs`` or SQLite (same as list).
    """
    run = run_history_service.get_run(run_id)
    if run is None:
        raise HTTPException(status_code=404, detail=f"Run '{run_id}' not found")
    return run
