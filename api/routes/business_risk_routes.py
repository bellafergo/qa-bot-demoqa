# api/routes/business_risk_routes.py
"""
Business Risk Scoring REST API
================================

POST /business-risk/analyze    — score business risk for a test run
GET  /business-risk/health     — liveness check
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.business_risk_models import BusinessRiskRequest, BusinessRiskResult
from services.business_risk_service import business_risk_service

logger = logging.getLogger("vanya.business_risk_routes")

router = APIRouter(prefix="/business-risk", tags=["business-risk"])


@router.get("/health")
def health():
    return {"status": "ok", "service": "business-risk"}


@router.post("/analyze", response_model=BusinessRiskResult)
def analyze(req: BusinessRiskRequest):
    """
    Translate a test run failure into a business risk assessment.

    Returns the affected business flow, risk level, impact summary,
    and a prioritized fix recommendation.
    """
    try:
        return business_risk_service.analyze_run_id(req.run_id)
    except ValueError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        logger.exception("business_risk: analyze failed for run_id=%s", req.run_id)
        raise HTTPException(status_code=500, detail=str(e))
