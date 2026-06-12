# api/routes/platform_observability_routes.py
"""OBS-02A — Platform self observability routes (read-only)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.platform_observability_models import PlatformObservabilityReport
from services.platform_observability_service import build_platform_observability_report

logger = logging.getLogger("vanya.platform_observability_routes")

router = APIRouter(prefix="/platform", tags=["platform-observability"])


@router.get("/observability", response_model=PlatformObservabilityReport)
def get_platform_observability():
    """Return read-only Vanya platform health derived from existing platform data."""
    try:
        return build_platform_observability_report()
    except Exception as exc:
        logger.exception("GET /platform/observability failed")
        raise HTTPException(status_code=500, detail=f"Platform observability failed: {exc}") from exc
