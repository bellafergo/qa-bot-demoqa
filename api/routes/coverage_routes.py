# api/routes/coverage_routes.py
"""
Coverage Intelligence REST API
================================

GET /coverage/summary          — coverage metrics for all modules
GET /coverage/module/{module}  — coverage metrics for one module
GET /coverage/health           — liveness check
"""
from __future__ import annotations

import logging
from fastapi import APIRouter

from models.coverage_models import CoverageResult, CoverageSummaryResponse
from services.coverage_service import coverage_service

logger = logging.getLogger("vanya.coverage_routes")

router = APIRouter(prefix="/coverage", tags=["coverage"])


@router.get("/health")
def health():
    return {"status": "ok", "service": "coverage"}


@router.get("/summary", response_model=CoverageSummaryResponse)
def summary():
    """
    Return aggregate coverage metrics for the active test catalog.

    Includes overall coverage %, per-module breakdown, and gap recommendations.
    """
    return coverage_service.get_summary_aggregate()


@router.get("/module/{module}", response_model=CoverageResult)
def module_coverage(module: str):
    """
    Return coverage metrics for a specific module.

    The module name is matched case-insensitively and may be a partial
    match (e.g. "auth" matches "auth-service").
    """
    return coverage_service.get_module_coverage(module)
