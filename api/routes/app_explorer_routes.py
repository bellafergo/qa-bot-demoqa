# api/routes/app_explorer_routes.py
"""
Application Explorer REST API
================================

GET  /app-explorer/health   — liveness check
POST /app-explorer/explore  — explore a single page and return element inventory
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, HttpUrl

logger = logging.getLogger("vanya.app_explorer_routes")

router = APIRouter(prefix="/app-explorer", tags=["app-explorer"])


class ExploreRequest(BaseModel):
    url: str


# ── Health ─────────────────────────────────────────────────────────────────────

@router.get("/health")
def health():
    return {"status": "ok", "service": "app_explorer"}


# ── Explore ────────────────────────────────────────────────────────────────────

@router.post("/explore")
def explore(req: ExploreRequest):
    """
    Navigate to *url* with a headless browser and return a structured inventory
    of inputs, buttons, links, and forms found on the page.
    """
    from services.application_explorer import explore_page
    try:
        result = explore_page(req.url)
    except Exception as exc:
        logger.exception("app-explorer/explore failed for url=%s", req.url)
        raise HTTPException(status_code=500, detail=f"Exploration error: {exc}")
    return result
