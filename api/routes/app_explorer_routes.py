# api/routes/app_explorer_routes.py
"""
Application Explorer REST API
================================

GET  /app-explorer/health        — liveness check
POST /app-explorer/explore       — explore a single page and return element inventory
POST /app-explorer/explore-app   — multi-page BFS exploration (same domain)
"""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, HttpUrl

logger = logging.getLogger("vanya.app_explorer_routes")

router = APIRouter(prefix="/app-explorer", tags=["app-explorer"])


class ExploreRequest(BaseModel):
    url: str


class ExploreAppRequest(BaseModel):
    url: str
    max_pages: int = 5
    # When set, uses project login_profile + variables before multi-page crawl (if configured).
    project_id: Optional[str] = None


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


@router.post("/explore-app")
def explore_app_endpoint(req: ExploreAppRequest):
    """
    BFS-explore up to *max_pages* pages of the same domain starting from *url*.
    Returns aggregated inventory for all visited pages.

    If *project_id* is provided and the project defines a complete login_profile,
    runs automatic login in the same browser session before crawling (private apps).
    """
    from services.authenticated_exploration import explore_app_with_optional_project_auth
    from services.multi_page_explorer import explore_app

    try:
        pid = (req.project_id or "").strip() or None
        if pid:
            result = explore_app_with_optional_project_auth(
                req.url,
                req.max_pages,
                project_id=pid,
            )
        else:
            result = explore_app(req.url, max_pages=req.max_pages)
            result.setdefault("used_project_login", False)
    except Exception as exc:
        logger.exception("app-explorer/explore-app failed for url=%s", req.url)
        raise HTTPException(status_code=500, detail=f"Multi-page exploration error: {exc}")
    return result
