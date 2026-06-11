# api/routes/outcome_tracking_routes.py
"""ROI-02A — Outcome Tracking (read-only aggregation)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.outcome_tracking_models import OutcomeTrackingReport
from services.outcome_tracking_service import build_outcome_tracking_report

logger = logging.getLogger("vanya.outcome_tracking_routes")

router = APIRouter(prefix="/projects", tags=["outcome-tracking"])


@router.get("/{project_id}/outcomes", response_model=OutcomeTrackingReport)
def get_project_outcomes(project_id: str):
    """Return measurable platform outcomes for a project."""
    pid = (project_id or "").strip().lower()
    if not pid:
        raise HTTPException(status_code=400, detail="project_id is required")

    from services.db.project_repository import project_repo

    if project_repo.get_project(pid) is None:
        raise HTTPException(status_code=404, detail=f"project not found: {pid}")

    try:
        return build_outcome_tracking_report(project_id=pid)
    except Exception as exc:
        logger.exception("GET /projects/%s/outcomes failed", pid)
        raise HTTPException(status_code=500, detail=f"Outcome tracking failed: {exc}") from exc
