# api/routes/executive_impact_routes.py
"""ROI-01B — Executive Impact Metrics (read-only historical comparison)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.executive_impact_models import ExecutiveImpactReport
from services.executive_impact_service import build_executive_impact_report

logger = logging.getLogger("vanya.executive_impact_routes")

router = APIRouter(prefix="/projects", tags=["executive-impact"])


@router.get("/{project_id}/executive-impact", response_model=ExecutiveImpactReport)
def get_project_executive_impact(project_id: str):
    """Return executive impact trends comparing current vs previous intelligence."""
    pid = (project_id or "").strip().lower()
    if not pid:
        raise HTTPException(status_code=400, detail="project_id is required")

    from services.db.project_repository import project_repo

    if project_repo.get_project(pid) is None:
        raise HTTPException(status_code=404, detail=f"project not found: {pid}")

    try:
        return build_executive_impact_report(project_id=pid)
    except Exception as exc:
        logger.exception("GET /projects/%s/executive-impact failed", pid)
        raise HTTPException(status_code=500, detail=f"Executive impact failed: {exc}") from exc
