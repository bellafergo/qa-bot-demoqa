# api/routes/coverage_routes.py
"""
Coverage Intelligence REST API
================================

GET  /coverage/summary              — coverage metrics for all modules
GET  /coverage/module/{module}      — coverage metrics for one module
POST /coverage/generate-tests       — draft test suggestions from coverage gaps
GET  /coverage/health               — liveness check
"""
from __future__ import annotations

import logging
from fastapi import APIRouter

from typing import Any, Dict, List

from pydantic import BaseModel

from models.coverage_models import (
    CoverageResult,
    CoverageSummaryResponse,
    CoverageTestGenerationRequest,
    CoverageTestGenerationResponse,
)


class SaveCoverageDraftsRequest(BaseModel):
    module:      str
    suggestions: List[Dict[str, Any]]
from services.coverage_service import coverage_service

logger = logging.getLogger("vanya.coverage_routes")

router = APIRouter(prefix="/coverage", tags=["coverage"])


@router.get("/health")
def health():
    return {"status": "ok", "service": "coverage"}


@router.get("/summary", response_model=CoverageSummaryResponse)
def summary():
    """
    Return aggregate coverage metrics for the active test catalog.

    Includes overall coverage %, per-module breakdown, and gap recommendations.
    """
    return coverage_service.get_summary_aggregate()


@router.post("/generate-tests", response_model=CoverageTestGenerationResponse)
def generate_tests(body: CoverageTestGenerationRequest):
    """
    Generate draft test suggestions based on coverage gaps for a module.

    Reuses existing domain templates — no LLM required.
    Catalog is not modified; suggestions are returned as drafts only.
    """
    return coverage_service.generate_tests_for_gaps(body.module)


@router.post("/save-drafts")
def save_drafts(body: SaveCoverageDraftsRequest):
    """
    Persist coverage gap suggestions directly into the active test catalog.

    There is no intermediate draft storage — tests are saved as active catalog
    entries immediately. Accepts the DraftTestSuggestion shape returned by
    /coverage/generate-tests and preserves the real module name.
    """
    from services.draft_approval_service import persist_approved_drafts

    catalog_tests = []
    for s in body.suggestions:
        name  = (s.get("name") or "").strip()
        steps = s.get("suggested_steps") or []
        if not name or not steps:
            continue
        confidence = s.get("confidence") or "medium"
        test_type  = "smoke" if confidence == "low" else "regression"
        catalog_tests.append({
            "test_id":  name,
            "name":     name,
            "status":   "active",
            "priority": confidence,
            "type":     test_type,
            "module":   body.module,
            "source":   "coverage_gap",
            "reason":   s.get("rationale") or "",
            "steps":    list(steps),
            "meta": {
                "generated_from": "coverage_gap_analysis",
                "draft": True,
            },
        })

    return persist_approved_drafts(catalog_tests)


@router.get("/module/{module}", response_model=CoverageResult)
def module_coverage(module: str):
    """
    Return coverage metrics for a specific module.

    The module name is matched case-insensitively and may be a partial
    match (e.g. "auth" matches "auth-service").
    """
    return coverage_service.get_module_coverage(module)
