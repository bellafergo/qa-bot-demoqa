# models/outcome_tracking_models.py
"""
ROI-02A — Outcome Tracking (read-only aggregation).

Surfaces measurable platform outcomes from existing intelligence only.
No AI, scoring engines, or workflow automation.
"""
from __future__ import annotations

from typing import List, Union

from pydantic import BaseModel, Field


class OutcomeMetric(BaseModel):
    metric_name: str
    value: Union[int, str]


class OutcomeTrackingReport(BaseModel):
    generated_at: str

    blockers_identified: int = Field(default=0, ge=0)
    releases_blocked: int = Field(default=0, ge=0)
    recommendations_generated: int = Field(default=0, ge=0)
    incidents_investigated: int = Field(default=0, ge=0)
    executive_reports_sent: int = Field(default=0, ge=0)

    executive_summary: str = ""
    outcome_metrics: List[OutcomeMetric] = Field(default_factory=list)
