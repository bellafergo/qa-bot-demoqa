# api/routes/release_readiness_routes.py
"""REL-01A — Release Readiness intelligence compositor (read-only)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.release_readiness_models import ReleaseReadinessView
from services.release_readiness_service import build_release_readiness_view

logger = logging.getLogger("vanya.release_readiness_routes")

router = APIRouter(prefix="/projects", tags=["release-readiness"])


@router.get("/{project_id}/release-readiness", response_model=ReleaseReadinessView)
def get_project_release_readiness(project_id: str):
    """Return composed release readiness intelligence for a project."""
    pid = (project_id or "").strip().lower()
    if not pid:
        raise HTTPException(status_code=400, detail="project_id is required")

    from services.db.project_repository import project_repo

    if project_repo.get_project(pid) is None:
        raise HTTPException(status_code=404, detail=f"project not found: {pid}")

    try:
        return build_release_readiness_view(project_id=pid)
    except Exception as exc:
        logger.exception("GET /projects/%s/release-readiness failed", pid)
        raise HTTPException(status_code=500, detail=f"Release readiness failed: {exc}") from exc
