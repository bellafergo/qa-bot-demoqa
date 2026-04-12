# api/routes/api_testing_routes.py
"""
OpenAPI / Swagger-based API Testing REST API
==============================================

GET  /api-testing/health              — liveness check
POST /api-testing/parse-spec          — parse spec → list[EndpointInfo]
POST /api-testing/generate-tests      — generate draft tests from spec/endpoints
POST /api-testing/approve             — approve drafts into catalog
POST /api-testing/run                 — execute API tests (catalog or drafts)
"""
from __future__ import annotations

import logging
import time
import uuid
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, HTTPException

from models.api_testing_models import (
    APITestExecutionRequest,
    APITestGenerationRequest,
    APITestGenerationResponse,
    ApproveAPITestsRequest,
    ApproveAPITestsResponse,
    DraftAPITest,
    EndpointInfo,
    OpenAPISpecRequest,
)
from models.test_run import TestRun
from services.openapi_parser_service import openapi_parser_service
from services.api_test_generation_service import api_test_generation_service

logger = logging.getLogger("vanya.api_testing_routes")

router = APIRouter(prefix="/api-testing", tags=["api-testing"])


# ── Health ────────────────────────────────────────────────────────────────────

@router.get("/health")
def health():
    return {"status": "ok", "service": "api-testing"}


# ── Parse spec ────────────────────────────────────────────────────────────────

@router.post("/parse-spec", response_model=List[EndpointInfo])
def parse_spec(req: OpenAPISpecRequest):
    """
    Parse an OpenAPI 3.x or Swagger 2.0 spec and return discovered endpoints.

    Provide either spec_text (raw JSON/YAML string) or spec_url.
    """
    try:
        endpoints = openapi_parser_service.parse(req)
    except ValueError as exc:
        raise HTTPException(status_code=422, detail=str(exc))
    except Exception as exc:
        logger.exception("api_testing: parse-spec failed")
        raise HTTPException(status_code=500, detail=f"Parser error: {exc}")
    return endpoints


# ── Generate tests ────────────────────────────────────────────────────────────

@router.post("/generate-tests", response_model=APITestGenerationResponse)
def generate_tests(req: APITestGenerationRequest):
    """
    Generate draft API test cases from:
    - An explicit list of EndpointInfo objects, OR
    - A raw OpenAPI spec (spec_text / spec_url)

    Returns DraftAPITest objects — NOT yet saved to the catalog.
    Call /api-testing/approve to persist them.
    """
    try:
        result = api_test_generation_service.generate(req)
    except Exception as exc:
        logger.exception("api_testing: generate-tests failed")
        raise HTTPException(status_code=500, detail=f"Generation error: {exc}")
    return result


# ── Approve drafts ────────────────────────────────────────────────────────────

@router.post("/approve", response_model=ApproveAPITestsResponse)
def approve_drafts(req: ApproveAPITestsRequest):
    """
    Approve draft API tests and save them to the Test Catalog.

    Each approved draft is saved with test_type="api" and tagged
    with "api-generated".  Set activate=false to save as inactive.
    """
    try:
        result = api_test_generation_service.approve(req)
    except Exception as exc:
        logger.exception("api_testing: approve failed")
        raise HTTPException(status_code=500, detail=f"Approval error: {exc}")
    return result


# ── Run API tests ─────────────────────────────────────────────────────────────

@router.post("/run")
def run_api_tests(req: APITestExecutionRequest):
    """
    Execute API tests in one of two modes:

    1. **test_case_ids** — run approved catalog tests via the orchestrator.
       Returns an OrchestratorJob (async execution).

    2. **drafts** — run draft tests directly via the API runner (synchronous).
       Returns a list of TestRun-compatible result dicts.
    """
    if req.test_case_ids:
        # Mode 1: catalog tests → orchestrator
        try:
            from services.catalog_orchestrator import orchestrator_service
            job = orchestrator_service.enqueue_suite(
                test_case_ids=req.test_case_ids,
                environment=req.environment or "default",
                project_id=req.project_id,
            )
            return {
                "mode":                "catalog",
                "orchestrator_job_id": job.job_id,
                "status":              job.status,
                "test_case_count":     len(req.test_case_ids),
            }
        except Exception as exc:
            logger.exception("api_testing: run catalog tests failed")
            raise HTTPException(status_code=500, detail=f"Orchestrator error: {exc}")

    # Mode 2: run drafts directly
    from services.api_runner import run_api_test
    from services.db.project_repository import project_repo
    from services.project_execution_context import api_runner_credential_interpolation

    results: List[Dict[str, Any]] = []
    draft_base = ""
    draft_initial: Dict[str, Any] = {}
    if req.project_id and str(req.project_id).strip():
        try:
            dp = project_repo.get_project(str(req.project_id).strip().lower())
        except Exception:
            dp = None
            logger.debug(
                "api_testing: draft run project lookup failed project_id=%s",
                req.project_id,
                exc_info=True,
            )
        else:
            if dp is not None:
                draft_initial = api_runner_credential_interpolation(dp)
                dbu = getattr(dp, "base_url", None)
                if dbu and str(dbu).strip():
                    draft_base = str(dbu).strip().rstrip("/")

    for draft in (req.drafts or []):
        base_url = draft_base
        try:
            result = run_api_test(
                steps               = draft.steps,
                base_url            = base_url,
                assertions          = draft.assertions,
                auth_config         = req.auth_config,
                timeout_s           = 30,
                initial_variables   = draft_initial,
            )
            results.append({
                "draft_id":    draft.draft_id,
                "name":        draft.name,
                "endpoint":    draft.endpoint,
                "method":      draft.method,
                "status":      result.get("status"),
                "ok":          result.get("ok"),
                "duration_ms": result.get("duration_ms"),
                "logs":        result.get("logs", []),
            })
        except Exception as exc:
            results.append({
                "draft_id": draft.draft_id,
                "name":     draft.name,
                "status":   "error",
                "ok":       False,
                "error":    str(exc),
            })

    return {
        "mode":    "draft",
        "results": results,
        "total":   len(results),
        "passed":  sum(1 for r in results if r.get("ok")),
        "failed":  sum(1 for r in results if not r.get("ok")),
    }
