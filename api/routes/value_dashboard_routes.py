# api/routes/value_dashboard_routes.py
"""ROI-01A — Value Dashboard (read-only aggregation)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.value_dashboard_models import ValueDashboard
from services.value_dashboard_service import build_value_dashboard

logger = logging.getLogger("vanya.value_dashboard_routes")

router = APIRouter(prefix="/projects", tags=["value-dashboard"])


@router.get("/{project_id}/value-dashboard", response_model=ValueDashboard)
def get_project_value_dashboard(project_id: str):
    """Return aggregated operational value metrics for a project."""
    pid = (project_id or "").strip().lower()
    if not pid:
        raise HTTPException(status_code=400, detail="project_id is required")

    from services.db.project_repository import project_repo

    if project_repo.get_project(pid) is None:
        raise HTTPException(status_code=404, detail=f"project not found: {pid}")

    try:
        return build_value_dashboard(project_id=pid)
    except Exception as exc:
        logger.exception("GET /projects/%s/value-dashboard failed", pid)
        raise HTTPException(status_code=500, detail=f"Value dashboard failed: {exc}") from exc
