# models/value_dashboard_models.py
"""
ROI-01A — Value Dashboard (read-only aggregation).

Surfaces deterministic operational value metrics from existing intelligence only.
No financial estimates, predictions, or new scoring.
"""
from __future__ import annotations

from typing import List, Union

from pydantic import BaseModel, Field


class ValueMetric(BaseModel):
    metric_id: str
    title: str
    value: Union[int, str]
    description: str = ""


class ValueDashboard(BaseModel):
    generated_at: str

    # Activity
    incidents_investigated: int = Field(default=0, ge=0)
    executive_reports_generated: int = Field(default=0, ge=0)
    release_readiness_reports: int = Field(default=0, ge=0)
    scheduled_reports_generated: int = Field(default=0, ge=0)

    # Risk visibility
    blocked_releases: int = Field(default=0, ge=0)
    critical_risks_identified: int = Field(default=0, ge=0)
    jira_blockers_detected: int = Field(default=0, ge=0)
    degradation_events_detected: int = Field(default=0, ge=0)

    # Quality visibility
    quality_health_score: int = Field(default=0, ge=0, le=100)
    quality_trend: str = "UNKNOWN"
    degraded_environments: int = Field(default=0, ge=0)
    impacted_journeys: int = Field(default=0, ge=0)

    # Operational impact
    recommendations_generated: int = Field(default=0, ge=0)
    approvals_requested: int = Field(default=0, ge=0)
    validations_planned: int = Field(default=0, ge=0)
    correlated_jira_issues: int = Field(default=0, ge=0)

    # Summary highlights
    top_value_metrics: List[ValueMetric] = Field(default_factory=list)
