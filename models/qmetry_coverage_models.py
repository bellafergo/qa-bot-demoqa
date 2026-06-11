# models/qmetry_coverage_models.py
"""Read-only QMetry coverage intelligence models (QMETRY-01B)."""
from __future__ import annotations

from datetime import datetime
from typing import List, Literal, Optional

from pydantic import BaseModel, Field

CoverageStatus = Literal["STRONG", "MODERATE", "WEAK", "NONE"]
GapSeverity = Literal["CRITICAL", "MEDIUM"]


class CoverageMatch(BaseModel):
    test_case_id: str
    test_case_name: str
    matched_module: str
    matched_capability: str
    match_reason: str


class CoverageGap(BaseModel):
    capability: str
    module: Optional[str] = None
    severity: GapSeverity = "MEDIUM"
    reason: str


class CoverageAssessment(BaseModel):
    capability: str
    total_tests: int = 0
    matched_tests: int = 0
    coverage_status: CoverageStatus = "NONE"


class CoverageIntelligenceReport(BaseModel):
    generated_at: datetime
    connected: bool = False
    total_test_cases: int = 0
    total_matches: int = 0
    coverage_matches: List[CoverageMatch] = Field(default_factory=list)
    coverage_assessments: List[CoverageAssessment] = Field(default_factory=list)
    coverage_gaps: List[CoverageGap] = Field(default_factory=list)
    executive_summary: str = ""
    data_gaps: List[str] = Field(default_factory=list)
