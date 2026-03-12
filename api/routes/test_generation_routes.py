# api/routes/test_generation_routes.py
"""
Test Generation REST API
========================

POST /test-generation/generate              — generate drafts from prompt / requirement
POST /test-generation/approve              — approve and save drafts into the Test Catalog
POST /test-generation/generate-from-pr     — generate from PR/module signals
GET  /test-generation/health               — liveness check
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.test_generation_models import (
    ApproveDraftRequest,
    ApproveDraftResponse,
    TestGenerationRequest,
    TestGenerationResponse,
)
from services.test_generation_service import generation_service

logger = logging.getLogger("vanya.test_generation_routes")

router = APIRouter(prefix="/test-generation", tags=["test-generation"])


@router.get("/health")
def health():
    """Confirm the Test Generation service is reachable."""
    return {"status": "ok", "service": "test-generation"}


@router.post("/generate", response_model=TestGenerationResponse)
def generate(req: TestGenerationRequest):
    """
    Generate draft test cases from a prompt, requirement text, or title.

    Supply module/changed_modules to target specific domains.
    Drafts are NOT saved to the catalog — call /approve to persist them.
    """
    try:
        return generation_service.generate(req)
    except Exception as e:
        logger.exception("test_generation: generate failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/approve", response_model=ApproveDraftResponse)
def approve(req: ApproveDraftRequest):
    """
    Persist selected draft tests into the active Test Catalog.

    Each approved draft gets a unique TC-GEN-XXXX identifier.
    Set activate=false to create the tests in inactive status.
    Existing catalog tests are never overwritten.
    """
    try:
        return generation_service.approve(req)
    except Exception as e:
        logger.exception("test_generation: approve failed")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/generate-from-pr", response_model=TestGenerationResponse)
def generate_from_pr(req: TestGenerationRequest):
    """
    Generate draft tests from PR / module signals.

    Equivalent to POST /generate with source='pr-analysis'.
    Provide changed_modules and/or risk_level along with a title
    to get domain-targeted test suggestions.
    """
    try:
        forced = req.model_copy(update={"source": "pr-analysis"})
        return generation_service.generate(forced)
    except Exception as e:
        logger.exception("test_generation: generate-from-pr failed")
        raise HTTPException(status_code=500, detail=str(e))
