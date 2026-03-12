# api/routes/rca_routes.py
"""
Root Cause Analysis REST API
==============================

POST /rca/analyze              — analyze a test run by run_id
GET  /rca/analyze/{run_id}     — same as POST, for quick browser/curl access
GET  /rca/health               — liveness check
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.rca_models import RCAAnalysisRequest, RCAAnalysisResult
from services.rca_service import rca_service

logger = logging.getLogger("vanya.rca_routes")

router = APIRouter(prefix="/rca", tags=["rca"])


@router.get("/health")
def health():
    """Confirm the RCA service is reachable."""
    return {"status": "ok", "service": "rca"}


@router.post("/analyze", response_model=RCAAnalysisResult)
def analyze(req: RCAAnalysisRequest):
    """
    Analyze a failed test run and return the probable root cause.

    The run must exist in the run history. Pass the ``run_id`` from any
    previous test execution.
    """
    try:
        return rca_service.analyze_run_id(req.run_id)
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception("rca: analyze failed for run_id=%s", req.run_id)
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/analyze/{run_id}", response_model=RCAAnalysisResult)
def analyze_get(run_id: str):
    """
    Analyze a test run by run_id via GET.

    Convenience endpoint — identical behavior to POST /rca/analyze.
    """
    try:
        return rca_service.analyze_run_id(run_id)
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception("rca: analyze failed for run_id=%s", run_id)
        raise HTTPException(status_code=500, detail=str(e))
