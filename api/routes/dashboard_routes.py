# api/routes/dashboard_routes.py
"""
QA Dashboard REST API
=====================

GET /dashboard/summary              — platform-wide summary metrics
GET /dashboard/recent-runs          — most recent TestRun records
GET /dashboard/recent-jobs          — most recent OrchestratorJob records
GET /dashboard/by-module            — run metrics grouped by test-case module
GET /dashboard/run-status-breakdown — run counts by status
GET /dashboard/job-status-breakdown — job counts by status
"""
from __future__ import annotations

import logging
from typing import List

from fastapi import APIRouter, HTTPException, Query

from models.dashboard_models import (
    DashboardSummary,
    DashboardModuleMetrics,
    RunStatusBreakdown,
    JobStatusBreakdown,
)
from models.orchestrator_job import OrchestratorJob
from models.run_contract import CanonicalRun
from services.dashboard_service import dashboard_service
from services.run_history_service import run_history_service

logger = logging.getLogger("vanya.dashboard_routes")

router = APIRouter(prefix="/dashboard", tags=["dashboard"])


@router.get("/summary", response_model=DashboardSummary)
def get_summary():
    """
    Return platform-wide QA metrics in a single compact response.

    Counts test cases, runs, and orchestrator jobs from the SQLite DB.
    pass_rate is a percentage (0–100), rounded to 2 decimal places.
    """
    try:
        return dashboard_service.get_summary()
    except Exception as e:
        logger.exception("dashboard: get_summary failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/recent-runs", response_model=List[CanonicalRun])
def get_recent_runs(limit: int = Query(20, ge=1, le=500)):
    """Return the most recent test run records as CanonicalRun, newest first.

    Source: run_history_service → SQLite (official persistent store).
    Includes both catalog runs and bridged chat/execute runs.
    """
    try:
        return run_history_service.list_runs(limit=limit)
    except Exception as e:
        logger.exception("dashboard: get_recent_runs failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/recent-jobs", response_model=List[OrchestratorJob])
def get_recent_jobs(limit: int = Query(20, ge=1, le=500)):
    """Return the most recent orchestrator job records, newest first."""
    try:
        return dashboard_service.get_recent_jobs(limit=limit)
    except Exception as e:
        logger.exception("dashboard: get_recent_jobs failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/by-module", response_model=List[DashboardModuleMetrics])
def get_by_module():
    """
    Return run metrics aggregated by test-case module.

    Modules with test cases but zero runs will have run_count=0 and pass_rate=0.0.
    Results are sorted alphabetically by module name.
    """
    try:
        return dashboard_service.get_by_module()
    except Exception as e:
        logger.exception("dashboard: get_by_module failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/run-status-breakdown", response_model=RunStatusBreakdown)
def get_run_status_breakdown():
    """Return total run counts grouped by outcome status (pass / fail / error)."""
    try:
        return dashboard_service.get_run_status_breakdown()
    except Exception as e:
        logger.exception("dashboard: get_run_status_breakdown failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/job-status-breakdown", response_model=JobStatusBreakdown)
def get_job_status_breakdown():
    """Return total orchestrator job counts grouped by status."""
    try:
        return dashboard_service.get_job_status_breakdown()
    except Exception as e:
        logger.exception("dashboard: get_job_status_breakdown failed")
        raise HTTPException(status_code=500, detail=str(e))
