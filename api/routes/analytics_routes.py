# api/routes/analytics_routes.py
"""
Run Analytics API
=================

GET /analytics/runs/dashboard
    Returns aggregated run metrics in a single payload:
      - summary       (totals, pass rate, avg duration, 7d/30d windows)
      - trend         (daily breakdown for last 7 calendar days)
      - top_failures  (tests ranked by failure count, up to 10)

Stateless — reads from the official run history (SQLite) via
services.run_analytics_service.  Nothing is persisted here.
"""
from __future__ import annotations

import logging

from typing import Optional

from fastapi import APIRouter, Query

from models.analytics_models import RunsDashboard
from services.run_analytics_service import get_runs_dashboard

logger = logging.getLogger("vanya.analytics_routes")
router = APIRouter(prefix="/analytics", tags=["analytics"])


@router.get("/runs/dashboard", response_model=RunsDashboard)
def runs_analytics_dashboard(project_id: Optional[str] = Query(None)):
    """
    Return aggregated run analytics ready for dashboard display.

    Computes in real-time from the last 500 persisted runs.
    No caching — suitable for dashboard polling.
    """
    return get_runs_dashboard(project_id=project_id)
