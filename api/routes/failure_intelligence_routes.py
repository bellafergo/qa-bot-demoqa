# api/routes/failure_intelligence_routes.py
"""
Failure Intelligence REST API
================================

GET  /failure-intelligence/health       — liveness check
GET  /failure-intelligence/summary      — platform-wide failure intelligence aggregate
GET  /failure-intelligence/clusters     — failure clusters grouped by RCA + module
GET  /failure-intelligence/flaky-tests  — tests with high pass/fail flip rate
GET  /failure-intelligence/regressions  — tests with repeated recent failures
"""
from __future__ import annotations

import logging
from typing import List, Optional

from fastapi import APIRouter, HTTPException, Query

from models.failure_intelligence_models import (
    FailureCluster,
    FailureIntelligenceSummary,
    FlakyTestSignal,
    RegressionPattern,
)

logger = logging.getLogger("vanya.failure_intelligence_routes")

router = APIRouter(prefix="/failure-intelligence", tags=["failure-intelligence"])


# ── Health ─────────────────────────────────────────────────────────────────────

@router.get("/health")
def health():
    return {"status": "ok", "service": "failure_intelligence"}


# ── Summary ────────────────────────────────────────────────────────────────────

@router.get("/summary", response_model=FailureIntelligenceSummary)
def get_summary():
    """Return platform-wide failure intelligence: clusters, flaky count, regressions."""
    from services.failure_intelligence_service import failure_intelligence_service
    try:
        return failure_intelligence_service.get_summary()
    except Exception as exc:
        logger.exception("failure-intelligence/summary failed")
        raise HTTPException(status_code=500, detail=f"Summary error: {exc}")


# ── Clusters ───────────────────────────────────────────────────────────────────

@router.get("/clusters", response_model=List[FailureCluster])
def get_clusters(
    module:               Optional[str] = Query(None, description="Filter by module name"),
    root_cause_category:  Optional[str] = Query(None, description="Filter by RCA category"),
    limit:                int           = Query(200, ge=1, le=1000, description="Max runs to analyse"),
):
    """
    Return failure clusters derived from recent failed runs.

    Each cluster groups runs that share the same RCA category, impacted layer,
    and module — ordered by total_failures descending.
    """
    from services.failure_intelligence_service import failure_intelligence_service
    try:
        return failure_intelligence_service.get_clusters(
            limit=limit,
            module=module,
            root_cause_category=root_cause_category,
        )
    except Exception as exc:
        logger.exception("failure-intelligence/clusters failed")
        raise HTTPException(status_code=500, detail=f"Cluster error: {exc}")


# ── Flaky tests ────────────────────────────────────────────────────────────────

@router.get("/flaky-tests", response_model=List[FlakyTestSignal])
def get_flaky_tests():
    """
    Analyse recent run history per test case to detect flaky behaviour.

    Results are ordered: suspected_flaky first, then by flaky_score descending.
    """
    from services.failure_intelligence_service import failure_intelligence_service
    try:
        return failure_intelligence_service.get_flaky_tests()
    except Exception as exc:
        logger.exception("failure-intelligence/flaky-tests failed")
        raise HTTPException(status_code=500, detail=f"Flaky detection error: {exc}")


# ── Regressions ────────────────────────────────────────────────────────────────

@router.get("/regressions", response_model=List[RegressionPattern])
def get_regressions():
    """
    Return test cases with repeated recent failures ordered by failure count.
    """
    from services.failure_intelligence_service import failure_intelligence_service
    try:
        return failure_intelligence_service.get_regressions()
    except Exception as exc:
        logger.exception("failure-intelligence/regressions failed")
        raise HTTPException(status_code=500, detail=f"Regression detection error: {exc}")
