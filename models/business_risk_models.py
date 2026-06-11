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


# ── ROI-01C Executive Business Risk Estimation ────────────────────────────────

ExecutiveSeverity = Literal["LOW", "MEDIUM", "HIGH", "CRITICAL"]
ExecutiveConfidence = Literal["LOW", "MEDIUM", "HIGH"]


class BusinessRiskSignal(BaseModel):
    """Deterministic evidence signal mapped to a business capability."""
    signal_id: str
    title: str
    severity: ExecutiveSeverity = "LOW"
    confidence: ExecutiveConfidence = "LOW"
    impacted_capability: str
    evidence_count: int = 1


class BusinessRiskAssessment(BaseModel):
    """Business-oriented risk narrative for a single capability."""
    risk_id: str
    capability: str
    severity: ExecutiveSeverity = "LOW"
    confidence: ExecutiveConfidence = "LOW"
    summary: str = ""
    evidence: list[str] = []


class BusinessRiskReport(BaseModel):
    """Executive-facing business risk overview from existing intelligence."""
    generated_at: str
    overall_business_risk: ExecutiveSeverity = "LOW"
    business_risks: list[BusinessRiskAssessment] = []
    top_capabilities_at_risk: list[str] = []
    executive_summary: str = ""
    signals: list[BusinessRiskSignal] = []
    has_intelligence: bool = False
