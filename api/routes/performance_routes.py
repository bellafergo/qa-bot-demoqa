# api/routes/performance_routes.py
"""Lightweight API endpoint timing report (in-process)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, Query

from models.performance_models import SlowEndpointsReport
from services.endpoint_timing_service import build_slow_endpoints_report

logger = logging.getLogger("vanya.performance_routes")

router = APIRouter(prefix="/platform", tags=["platform-performance"])


@router.get("/performance/slow-endpoints", response_model=SlowEndpointsReport)
def get_slow_endpoints(limit: int = Query(10, ge=1, le=50)):
    """Return top slow endpoints ranked by average duration."""
    return build_slow_endpoints_report(limit=limit)
