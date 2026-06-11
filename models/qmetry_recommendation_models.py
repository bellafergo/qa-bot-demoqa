# models/qmetry_recommendation_models.py
"""Read-only QMetry test recommendation models (QMETRY-01C)."""
from __future__ import annotations

from datetime import datetime
from typing import List, Literal

from pydantic import BaseModel, Field

RecommendationPriority = Literal["CRITICAL", "HIGH", "MEDIUM"]


class RecommendedTestCase(BaseModel):
    test_case_id: str
    test_case_name: str
    capability: str
    recommendation_reason: str
    priority: RecommendationPriority = "MEDIUM"


class RecommendationGroup(BaseModel):
    capability: str
    recommended_tests: List[RecommendedTestCase] = Field(default_factory=list)


class QMetryRecommendationReport(BaseModel):
    generated_at: datetime
    connected: bool = False
    total_recommendations: int = 0
    recommendation_groups: List[RecommendationGroup] = Field(default_factory=list)
    executive_summary: str = ""
    data_gaps: List[str] = Field(default_factory=list)
