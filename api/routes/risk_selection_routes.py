# api/routes/risk_selection_routes.py
"""
Risk-Based Test Selection REST API
=====================================

POST /risk-selection/select-tests     — score and rank tests by risk signals
POST /risk-selection/select-and-run   — select tests then immediately enqueue
"""
from __future__ import annotations

import logging

from fastapi import APIRouter

from models.risk_selection_models import (
    RiskSelectionRequest,
    RiskSelectionResult,
    SelectAndRunResult,
)
from services.risk_selection_service import risk_selection_service

logger = logging.getLogger("vanya.risk_selection_routes")

router = APIRouter(prefix="/risk-selection", tags=["risk-selection"])


@router.post("/select-tests", response_model=RiskSelectionResult)
def select_tests(req: RiskSelectionRequest):
    """
    Score and rank all active tests based on:
    - Which modules changed (changed_modules)
    - Test priority (critical > high > medium > low)
    - Recent failure history
    - Business-critical module bonus

    Returns up to max_tests ranked tests with selection scores and reasons.
    """
    return risk_selection_service.select(req)


@router.post("/select-and-run", response_model=SelectAndRunResult)
def select_and_run(req: RiskSelectionRequest):
    """
    Select tests using risk signals and immediately enqueue a suite job
    via the orchestrator.

    Returns selection results plus the orchestrator job ID (if enqueued).
    """
    return risk_selection_service.select_and_run(req)
