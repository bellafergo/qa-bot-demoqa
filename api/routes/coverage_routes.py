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
from typing import List

from fastapi import APIRouter

from models.coverage_models import CoverageResult
from services.coverage_service import coverage_service

logger = logging.getLogger("vanya.coverage_routes")

router = APIRouter(prefix="/coverage", tags=["coverage"])


@router.get("/health")
def health():
    return {"status": "ok", "service": "coverage"}


@router.get("/summary", response_model=List[CoverageResult])
def summary():
    """
    Return coverage metrics for every module in the active test catalog.

    Each entry includes: total tests, executed tests, pass/fail counts,
    coverage score (0–1), missing test types, and recommendations.
    """
    return coverage_service.get_summary()


@router.get("/module/{module}", response_model=CoverageResult)
def module_coverage(module: str):
    """
    Return coverage metrics for a specific module.

    The module name is matched case-insensitively and may be a partial
    match (e.g. "auth" matches "auth-service").
    """
    return coverage_service.get_module_coverage(module)
