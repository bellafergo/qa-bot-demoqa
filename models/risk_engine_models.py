# models/risk_engine_models.py
"""Risk Engine v1 — explainable heuristic risk assessment contracts."""
from __future__ import annotations

from typing import List, Literal, Optional

from pydantic import BaseModel, Field

RiskLevel = Literal["LOW", "MEDIUM", "HIGH", "CRITICAL"]


class RiskFactor(BaseModel):
    """Single explainable contributor to a risk score."""
    key: str
    label: str
    score: float = Field(ge=0.0)
    max_score: float = Field(ge=0.0)
    detail: str = ""


class ModuleRisk(BaseModel):
    module: str
    module_risk_score: float = Field(ge=0.0, le=100.0)
    module_risk_level: RiskLevel = "LOW"
    factors: List[RiskFactor] = Field(default_factory=list)
    regression_count: int = 0
    flaky_count: int = 0
    incident_count: int = 0
    pass_rate: Optional[float] = None
    test_count: int = 0


class RecommendedTest(BaseModel):
    test_case_id: str
    name: str = ""
    module: str = ""
    reason: str = ""
    selection_score: float = 0.0


class RiskAssessment(BaseModel):
    """Full project risk snapshot — computed on demand, no separate persistence."""
    project_id: str
    risk_score: float = Field(ge=0.0, le=100.0)
    risk_level: RiskLevel = "LOW"
    factors: List[RiskFactor] = Field(default_factory=list)
    explanation: List[str] = Field(default_factory=list)
    module_risks: List[ModuleRisk] = Field(default_factory=list)
    recommended_tests: List[RecommendedTest] = Field(default_factory=list)
    pass_rate: Optional[float] = None
    regression_count: int = 0
    flaky_count: int = 0
    recent_incident_count: int = 0
    engine_version: str = "v1"
