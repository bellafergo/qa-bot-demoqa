# models/executive_impact_models.py
"""
ROI-01B — Executive Impact Metrics (read-only historical comparison).

Deterministic current-vs-previous trends from stored intelligence only.
No financial estimates, predictions, or AI.
"""
from __future__ import annotations

from typing import List, Literal, Union

from pydantic import BaseModel, Field

ImpactDirection = Literal["IMPROVING", "STABLE", "DEGRADING", "UNKNOWN"]


class ImpactMetric(BaseModel):
    metric_id: str
    title: str
    current_value: Union[int, str]
    previous_value: Union[int, str]
    delta: int = 0
    direction: ImpactDirection = "UNKNOWN"


class ExecutiveImpactReport(BaseModel):
    generated_at: str

    # Quality
    quality_health_trend: ImpactMetric
    degraded_environment_trend: ImpactMetric
    impacted_journey_trend: ImpactMetric

    # Risk
    blocked_release_trend: ImpactMetric
    critical_risk_trend: ImpactMetric
    jira_blocker_trend: ImpactMetric

    # Operations
    recommendation_trend: ImpactMetric
    approval_trend: ImpactMetric
    validation_trend: ImpactMetric

    # Executive summary
    top_improvements: List[ImpactMetric] = Field(default_factory=list)
    top_concerns: List[ImpactMetric] = Field(default_factory=list)

    has_sufficient_history: bool = False
