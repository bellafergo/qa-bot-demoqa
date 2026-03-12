# api/routes/exploration_routes.py
"""
Autonomous Exploration REST API
=================================

GET  /exploration/health              — liveness check
POST /exploration/explore             — explore and return discovered pages/flows
POST /exploration/coverage-expansion  — explore + compare against catalog + suggest drafts
POST /exploration/explore-and-suggest — explore + suggest drafts (no approval)
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.exploration_models import (
    CoverageExpansionResult,
    ExplorationRequest,
    ExplorationResult,
)
from services.exploration_service import exploration_service
from services.coverage_expansion_service import coverage_expansion_service

logger = logging.getLogger("vanya.exploration_routes")

router = APIRouter(prefix="/exploration", tags=["exploration"])


# ── Health ────────────────────────────────────────────────────────────────────

@router.get("/health")
def health():
    return {"status": "ok", "service": "exploration"}


# ── Explore ───────────────────────────────────────────────────────────────────

@router.post("/explore", response_model=ExplorationResult)
def explore(req: ExplorationRequest):
    """
    Launch a bounded exploration of a web application.

    Returns discovered pages, classified page types, discovered navigation flows,
    and inferred module assignments.

    Exploration is bounded by max_pages and max_click_depth.
    Unsafe actions (delete, pay, logout, etc.) are automatically skipped.
    """
    try:
        result = exploration_service.explore(req)
    except Exception as exc:
        logger.exception("exploration: explore endpoint failed")
        raise HTTPException(status_code=500, detail=f"Exploration error: {exc}")
    return result


# ── Coverage expansion ────────────────────────────────────────────────────────

@router.post("/coverage-expansion", response_model=CoverageExpansionResult)
def coverage_expansion(req: ExplorationRequest):
    """
    Explore the application, then compare discovered flows against the Test Catalog.

    Returns:
      - covered_flows:    flows likely already represented in the catalog
      - uncovered_flows:  newly discovered flows with no matching tests
      - coverage_ratio:   covered / total flows (0.0 – 1.0)
      - suggested_drafts: draft UI tests for uncovered flows (if generate_draft_tests=True)

    Drafts are NOT automatically saved. Call POST /test-generation/approve to persist.
    """
    try:
        result = coverage_expansion_service.expand(req)
    except Exception as exc:
        logger.exception("exploration: coverage-expansion endpoint failed")
        raise HTTPException(status_code=500, detail=f"Coverage expansion error: {exc}")
    return result


# ── Explore and suggest (convenience route) ───────────────────────────────────

@router.post("/explore-and-suggest")
def explore_and_suggest(req: ExplorationRequest):
    """
    Explore the application and generate draft test suggestions for uncovered flows.

    Identical to /coverage-expansion but always enables generate_draft_tests.
    Drafts are NOT automatically saved — explicit approval required.
    """
    # Force draft generation regardless of request setting
    enriched = req.model_copy(update={"generate_draft_tests": True})
    try:
        result = coverage_expansion_service.expand(enriched)
    except Exception as exc:
        logger.exception("exploration: explore-and-suggest endpoint failed")
        raise HTTPException(status_code=500, detail=f"Explore-and-suggest error: {exc}")

    return {
        "total_flows":      result.discovered_flows.__len__(),
        "covered_flows":    len(result.covered_flows),
        "uncovered_flows":  len(result.uncovered_flows),
        "coverage_ratio":   result.coverage_ratio,
        "suggested_drafts": result.suggested_drafts,
        "notes":            result.notes,
    }
