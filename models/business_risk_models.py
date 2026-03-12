# models/business_risk_models.py
"""
Pydantic models for the Business Risk Scoring layer.
"""
from __future__ import annotations

from typing import Literal, Optional

from pydantic import BaseModel


RiskLevel  = Literal["critical", "high", "medium", "low"]
BRConfidence = Literal["low", "medium", "high"]


class BusinessRiskRequest(BaseModel):
    """Request to score business risk for a specific test run."""
    run_id: str


class BusinessRiskResult(BaseModel):
    """Business risk assessment derived from a failed test run."""
    run_id:                   str
    risk_level:               RiskLevel    = "low"
    affected_business_flow:   str          = "unknown"
    impact_summary:           str          = ""
    priority_recommendation:  str          = ""
    confidence:               BRConfidence = "low"

    # Optional enrichment fields
    rca_category:             Optional[str] = None   # forwarded from RCA for context
    test_module:              Optional[str] = None   # module that triggered the score
