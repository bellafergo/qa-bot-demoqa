# models/insights_models.py
"""
Contracts for QA Intelligence / deep insights (correlations, timeline, recommendations).

These augment Failure Intelligence without replacing existing cluster/flaky/regression models.
"""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel, Field

from models.rca_models import RCAEvidenceSignal


class DeployContext(BaseModel):
    """Reserved for future CI/deploy correlation — never fabricated."""

    deploy_id: Optional[str] = None
    commit_sha: Optional[str] = None
    release_tag: Optional[str] = None


class TimelineBucket(BaseModel):
    date: str
    total_runs: int
    failed_runs: int
    passed_runs: int
    pass_rate: Optional[float] = None


class FailureTimelineInsight(BaseModel):
    buckets: List[TimelineBucket] = Field(default_factory=list)
    first_failure_day: Optional[str] = None
    highest_failure_day: Optional[str] = None
    pass_rate_first_half: Optional[float] = None
    pass_rate_second_half: Optional[float] = None
    pass_rate_delta_window: Optional[float] = None
    cluster_recurrence_note: str = "insufficient_evidence"
    deploy_context: DeployContext = Field(default_factory=DeployContext)
    runs_analyzed: int = 0
    note: str = ""


class RootCauseCorrelation(BaseModel):
    """Correlated view aligned with enterprise RCA reporting (derived from clusters)."""

    root_cause_id: str
    title: str
    suspected_cause: str
    confidence: str
    affected_tests: List[str] = Field(default_factory=list)
    affected_modules: List[str] = Field(default_factory=list)
    signals_used: List[RCAEvidenceSignal] = Field(default_factory=list)
    recommended_action: str = ""


class ActionableRecommendation(BaseModel):
    """Deterministic guidance only — no automatic fixes."""

    type: str
    title: str
    description: str
    confidence: str
    safe_to_apply_auto_fix: bool = False
    suggested_next_step: str
    related_test_case_ids: List[str] = Field(default_factory=list)
    source_cluster_id: Optional[str] = None


class DeepInsightsResponse(BaseModel):
    root_causes: List[RootCauseCorrelation] = Field(default_factory=list)
    timeline: FailureTimelineInsight = Field(default_factory=FailureTimelineInsight)
    recommendations: List[ActionableRecommendation] = Field(default_factory=list)
