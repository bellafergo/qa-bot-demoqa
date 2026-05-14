# api/routes/browser_inspector_routes.py
"""
POST /inspect-url — deterministic browser inspection (Phase 1, no LLM).

See ``docs/BROWSER_INSPECTION_PHASE1.md`` for deploy and Playwright browser install.
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from core.target_url_validation import TargetURLNotAllowed
from models.app_map_models import InspectUrlMapRequest
from models.browser_inspection_models import InspectUrlRequest
from services.app_map_builder import build_app_map

logger = logging.getLogger("vanya.browser_inspector_routes")

router = APIRouter(prefix="/inspect-url", tags=["browser-inspection"])


@router.post("")
def inspect_url_endpoint(body: InspectUrlRequest):
    """
    Inspect a single URL with Playwright (viewport screenshot + structured metadata).

    Does not click, submit forms, or follow links beyond the initial navigation.
    """
    try:
        from services.browser_inspector_service import inspect_url_collect
        from services.browser_inspection_persistence import (
            merge_persist_fields_into_inspection,
            persist_light_browser_inspection,
        )

        result, inv, extras = inspect_url_collect(body)
        rid, ok, pwarn = persist_light_browser_inspection(
            result,
            inv=inv,
            extras=extras,
            project_id=body.project_id,
            app_map=None,
        )
        result = merge_persist_fields_into_inspection(
            result,
            persisted_run_id=rid if ok else None,
            persisted=ok,
            persistence_warning=pwarn,
        )
    except TargetURLNotAllowed as e:
        raise HTTPException(status_code=400, detail=str(e) or "Target URL not allowed") from None
    except Exception as exc:
        logger.exception("inspect-url failed")
        raise HTTPException(status_code=500, detail=f"Inspection failed: {exc}") from exc
    return result.model_dump()


@router.post("/map")
def inspect_url_map_endpoint(body: InspectUrlMapRequest):
    """
    Run inspection then deterministic semantic app map (single page, no crawl).

    ``execution_mode`` is ``cloud`` only; other modes are rejected by validation until
    local/hybrid agent nodes exist.
    """
    try:
        result = build_app_map(body)
    except TargetURLNotAllowed as e:
        raise HTTPException(status_code=400, detail=str(e) or "Target URL not allowed") from None
    except Exception as exc:
        logger.exception("inspect-url/map failed")
        raise HTTPException(status_code=500, detail=f"App map failed: {exc}") from exc
    return result.model_dump()
