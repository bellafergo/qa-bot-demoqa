# api/routes/browser_inspection_watch_routes.py
"""
Browser inspection watches — scheduled re-inspection (Phase 3C–3E).

Mounted at ``/browser-inspections/watch`` so paths do not collide with
``GET /browser-inspections/{inspection_id}``.

More specific ``/{watch_id}/…`` routes are registered before ``/{watch_id}``.
"""
from __future__ import annotations

import logging
from typing import Optional, Union

from fastapi import APIRouter, HTTPException, Query

from models.browser_inspection_watch_metrics_models import WatchEventItem, WatchEventsPageResponse

from models.browser_inspection_watch_models import (
    BrowserInspectionWatchCreate,
    BrowserInspectionWatchPatch,
    WatchBaselineSetRequest,
)
from services.browser_inspection_watch_service import (
    create_watch,
    execute_watch_tick,
    get_watch,
    get_watch_metrics,
    list_watch_events,
    list_watches,
    patch_watch,
    set_watch_baseline,
)

logger = logging.getLogger("vanya.browser_inspection_watch_routes")

router = APIRouter(prefix="/browser-inspections/watch", tags=["browser-inspection-watch"])


@router.post("")
def post_watch(body: BrowserInspectionWatchCreate):
    try:
        return create_watch(body).model_dump()
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("POST /browser-inspections/watch failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.get("")
def get_watch_list(
    project_id: Optional[str] = Query(default=None, max_length=256),
    limit: int = Query(default=50, ge=1, le=200),
):
    try:
        return [w.model_dump() for w in list_watches(project_id=project_id, limit=limit)]
    except Exception as exc:
        logger.exception("GET /browser-inspections/watch failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.post("/{watch_id}/baseline")
def post_watch_baseline(watch_id: str, body: WatchBaselineSetRequest):
    try:
        return set_watch_baseline(watch_id, body).model_dump()
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("POST baseline failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.get("/{watch_id}/metrics")
def get_metrics(watch_id: str):
    try:
        return get_watch_metrics(watch_id).model_dump()
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("GET watch metrics failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.get("/{watch_id}/events")
def get_events(
    watch_id: str,
    limit: int = Query(default=50, ge=1, le=200),
    cursor: Optional[str] = Query(default=None, max_length=4096),
    paged: bool = Query(default=False, description="If true, return {items, next_cursor} (Phase 3F)."),
):
    try:
        out: Union[list[WatchEventItem], WatchEventsPageResponse] = list_watch_events(
            watch_id,
            limit=limit,
            cursor=cursor,
            paged=paged,
        )
        if isinstance(out, WatchEventsPageResponse):
            return out.model_dump()
        return [e.model_dump() for e in out]
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("GET watch events failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.post("/{watch_id}/run-now")
def post_run_now(watch_id: str, force: bool = Query(default=False)):
    try:
        return execute_watch_tick(watch_id, force=force).model_dump()
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("run-now watch failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.get("/{watch_id}")
def get_one_watch(watch_id: str):
    try:
        return get_watch(watch_id).model_dump()
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("GET watch failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.patch("/{watch_id}")
def patch_one_watch(watch_id: str, body: BrowserInspectionWatchPatch):
    try:
        return patch_watch(watch_id, body).model_dump()
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("PATCH watch failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc
