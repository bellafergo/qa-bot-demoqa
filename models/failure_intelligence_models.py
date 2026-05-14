# models/failure_intelligence_models.py
"""
Pydantic response models for the Failure Intelligence API.
"""
from __future__ import annotations

from typing import Dict, List, Optional

from pydantic import BaseModel, Field

from models.rca_models import RCAEvidenceSignal


class BlastRadius(BaseModel):
    """Impact estimate for a failure cluster — suites omitted when not present in run meta."""

    affected_modules: List[str] = Field(default_factory=list)
    affected_tests_count: int = 0
    affected_suites_count: Optional[int] = None
    estimated_severity: str = "low"
    impact_scope: str = "unknown"


class FailureCluster(BaseModel):
    """A group of failed runs sharing the same RCA category, layer, and module."""
    cluster_id:                  str
    root_cause_category:         str
    impacted_layer:              str
    module:                      str
    representative_test_case_id: str
    run_ids:                     List[str]
    total_failures:              int
    common_signals:              List[str]
    probable_cause:              str
    confidence:                  str
    summary:                     str
    # ── QA Intelligence extensions (additive; older clients may ignore) ────────
    affected_test_case_ids:     List[str] = Field(default_factory=list)
    recommended_action:          str = ""
    signals_used:                List[RCAEvidenceSignal] = Field(default_factory=list)
    blast_radius:                Optional[BlastRadius] = None


class FlakyTestSignal(BaseModel):
    """Flakiness metrics for a single test case derived from its recent run history."""
    test_case_id:    str
    total_runs:      int
    pass_count:      int
    fail_count:      int
    error_count:     int
    flip_rate:       float   # fraction of adjacent pairs with a pass↔fail transition
    flaky_score:     float   # 0.0–1.0 composite score
    suspected_flaky: bool
    notes:           str


class FailureIntelligenceSummary(BaseModel):
    """Platform-wide failure intelligence aggregate."""
    total_failed_runs:           int
    total_clusters:              int
    flaky_tests_count:           int
    recurrent_regressions_count: int
    top_failure_categories:      Dict[str, int]   # category → total failures
    notes:                       str


class RegressionPattern(BaseModel):
    """A test case that has failed repeatedly in its most recent runs."""
    pattern_id:        str
    test_case_id:      str
    module:            str
    repeated_failures: int
    latest_root_cause: str
    affected_runs:     List[str]
    summary:           str
