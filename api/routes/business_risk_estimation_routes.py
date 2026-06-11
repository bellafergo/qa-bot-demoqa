# api/routes/business_risk_estimation_routes.py
"""ROI-01C — Executive Business Risk Estimation (read-only narrative)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.business_risk_models import BusinessRiskReport
from services.business_risk_estimation_service import build_business_risk_report

logger = logging.getLogger("vanya.business_risk_estimation_routes")

router = APIRouter(prefix="/projects", tags=["business-risk-estimation"])


@router.get("/{project_id}/business-risk", response_model=BusinessRiskReport)
def get_project_business_risk(project_id: str):
    """Return executive business risk narrative for a project."""
    pid = (project_id or "").strip().lower()
    if not pid:
        raise HTTPException(status_code=400, detail="project_id is required")

    from services.db.project_repository import project_repo

    if project_repo.get_project(pid) is None:
        raise HTTPException(status_code=404, detail=f"project not found: {pid}")

    try:
        return build_business_risk_report(project_id=pid)
    except Exception as exc:
        logger.exception("GET /projects/%s/business-risk failed", pid)
        raise HTTPException(status_code=500, detail=f"Business risk estimation failed: {exc}") from exc
