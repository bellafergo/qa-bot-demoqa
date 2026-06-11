# api/routes/qmetry_integration_routes.py
"""
Read-only QMetry discovery routes (QMETRY-01A).

Route semantics (do not conflate with generic connector routes):
  GET /integrations/qmetry/status       — QMetry discovery aggregate (counts, last_sync, connected)
  GET /integrations/qmetry/projects     — project listing
  GET /integrations/qmetry/test-cases   — test case discovery
  GET /integrations/qmetry/test-cycles  — test cycle discovery
  GET /integrations/qmetry/test-suites  — test suite discovery
  GET /integrations/qmetry/test-runs    — test run discovery
  GET /integrations/qmetry/coverage     — deterministic coverage intelligence (QMETRY-01B)
  GET /integrations/qmetry/recommendations — test recommendation correlation (QMETRY-01C)

Test result discovery (execution-level pass/fail details) remains deferred beyond QMETRY-01B.

Generic connector framework status (health, enabled, config_summary):
  GET  /integrations/qmetry             — ConnectorStatus via integrations_routes
  POST /integrations/qmetry/health-check

Do not conflate GET /integrations/qmetry with GET /integrations/qmetry/status —
same split as Jira (framework status vs discovery aggregate).
"""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, HTTPException, Query

from models.qmetry_coverage_models import CoverageIntelligenceReport
from models.qmetry_recommendation_models import QMetryRecommendationReport
from models.qmetry_models import (
    QMetryConnectionStatus,
    QMetryProjectsResponse,
    QMetryTestCasesResponse,
    QMetryTestCyclesResponse,
    QMetryTestRunsResponse,
    QMetryTestSuitesResponse,
)
from services.qmetry_coverage_intelligence_service import build_coverage_intelligence_report
from services.qmetry_test_recommendation_service import build_qmetry_recommendation_report
from services.qmetry_integration_service import (
    list_projects,
    list_test_cases,
    list_test_cycles,
    list_test_runs,
    list_test_suites,
    validate_qmetry_connection,
)

logger = logging.getLogger("vanya.qmetry_integration_routes")

router = APIRouter(prefix="/integrations/qmetry", tags=["qmetry-integration"])


@router.get("/status", response_model=QMetryConnectionStatus)
def get_qmetry_status():
    """QMetry discovery aggregate — connectivity plus project/test/run counts."""
    return validate_qmetry_connection()


@router.get("/projects", response_model=QMetryProjectsResponse)
def get_qmetry_projects():
    """List QMetry projects (read-only)."""
    return list_projects()


@router.get("/test-cases", response_model=QMetryTestCasesResponse)
def get_qmetry_test_cases(
    project_key: Optional[str] = Query(None, description="Filter by QMetry project key"),
    max_results: int = Query(50, ge=1, le=100, description="Maximum test cases to return"),
):
    """List QMetry test cases (read-only)."""
    return list_test_cases(project_key=project_key, max_results=max_results)


@router.get("/test-cycles", response_model=QMetryTestCyclesResponse)
def get_qmetry_test_cycles(
    project_key: Optional[str] = Query(None, description="Filter by QMetry project key"),
    max_results: int = Query(50, ge=1, le=100, description="Maximum test cycles to return"),
):
    """List QMetry test cycles (read-only)."""
    return list_test_cycles(project_key=project_key, max_results=max_results)


@router.get("/test-suites", response_model=QMetryTestSuitesResponse)
def get_qmetry_test_suites(
    project_key: Optional[str] = Query(None, description="Filter by QMetry project key"),
    max_results: int = Query(50, ge=1, le=100, description="Maximum test suites to return"),
):
    """List QMetry test suites (read-only)."""
    return list_test_suites(project_key=project_key, max_results=max_results)


@router.get("/test-runs", response_model=QMetryTestRunsResponse)
def get_qmetry_test_runs(
    project_key: Optional[str] = Query(None, description="Filter by QMetry project key"),
    max_results: int = Query(50, ge=1, le=100, description="Maximum test runs to return"),
):
    """List QMetry test runs (read-only)."""
    return list_test_runs(project_key=project_key, max_results=max_results)


@router.get("/coverage", response_model=CoverageIntelligenceReport)
def get_qmetry_coverage(
    project_id: Optional[str] = Query(None, description="Optional project id for intelligence correlation"),
):
    """Deterministic QMetry coverage intelligence (read-only, no execution)."""
    pid = (project_id or "").strip().lower() or None
    if pid:
        from services.db.project_repository import project_repo

        if project_repo.get_project(pid) is None:
            raise HTTPException(status_code=404, detail=f"project not found: {pid}")
    try:
        return build_coverage_intelligence_report(project_id=pid)
    except Exception as exc:
        logger.exception("GET /integrations/qmetry/coverage failed")
        raise HTTPException(status_code=500, detail=f"Coverage intelligence failed: {exc}") from exc


@router.get("/recommendations", response_model=QMetryRecommendationReport)
def get_qmetry_recommendations(
    project_id: Optional[str] = Query(None, description="Optional project id for intelligence correlation"),
):
    """Deterministic QMetry test recommendations (read-only, no execution)."""
    pid = (project_id or "").strip().lower() or None
    if pid:
        from services.db.project_repository import project_repo

        if project_repo.get_project(pid) is None:
            raise HTTPException(status_code=404, detail=f"project not found: {pid}")
    try:
        return build_qmetry_recommendation_report(project_id=pid)
    except Exception as exc:
        logger.exception("GET /integrations/qmetry/recommendations failed")
        raise HTTPException(status_code=500, detail=f"Test recommendations failed: {exc}") from exc
