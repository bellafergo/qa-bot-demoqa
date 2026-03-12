# api/routes/pr_analysis_routes.py
"""
PR Impact Analysis REST API
============================

POST /pr-analysis/analyze              — full impact analysis
POST /pr-analysis/analyze-and-enqueue  — analyze + auto-enqueue if matches found
POST /pr-analysis/generate-draft-tests — impact analysis + draft test suggestions only
GET  /pr-analysis/health               — liveness check
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.pr_analysis_models import PRAnalysisRequest, PRAnalysisResult
from services.pr_analysis_service import pr_analysis_service

logger = logging.getLogger("vanya.pr_analysis_routes")

router = APIRouter(prefix="/pr-analysis", tags=["pr-analysis"])


@router.get("/health")
def health():
    """Confirm the PR analysis service is reachable."""
    return {"status": "ok", "service": "pr-analysis"}


@router.post("/analyze", response_model=PRAnalysisResult)
def analyze(req: PRAnalysisRequest):
    """
    Analyze a pull request for QA impact.

    Returns inferred modules, risk level, and matched test cases.
    Set `generate_draft_tests=true` to also receive draft test proposals.
    Set `auto_enqueue=true` to automatically enqueue matched tests.
    """
    try:
        return pr_analysis_service.analyze(req)
    except Exception as e:
        logger.exception("pr_analysis: analyze failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/analyze-and-enqueue", response_model=PRAnalysisResult)
def analyze_and_enqueue(req: PRAnalysisRequest):
    """
    Analyze impact and enqueue matched tests as an orchestrator suite job.

    Equivalent to POST /analyze with auto_enqueue=true, provided for
    explicit API semantics. If no tests match, no job is created.
    """
    try:
        forced = req.model_copy(update={"auto_enqueue": True})
        return pr_analysis_service.analyze(forced)
    except Exception as e:
        logger.exception("pr_analysis: analyze-and-enqueue failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/generate-draft-tests", response_model=PRAnalysisResult)
def generate_draft_tests(req: PRAnalysisRequest):
    """
    Analyze impact and return draft test case proposals.

    Equivalent to POST /analyze with generate_draft_tests=true.
    Draft suggestions are NOT inserted into the active test catalog.
    """
    try:
        forced = req.model_copy(update={"generate_draft_tests": True})
        return pr_analysis_service.analyze(forced)
    except Exception as e:
        logger.exception("pr_analysis: generate-draft-tests failed")
        raise HTTPException(status_code=500, detail=str(e))
