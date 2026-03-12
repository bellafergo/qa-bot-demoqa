# models/rca_models.py
"""
Pydantic models for the Root Cause Analysis (RCA) layer.
"""
from __future__ import annotations

from typing import Any, List, Literal, Optional

from pydantic import BaseModel, Field


# ── Types ──────────────────────────────────────────────────────────────────────

RootCauseCategory = Literal[
    "selector_issue",
    "timeout_issue",
    "assertion_issue",
    "api_failure",
    "auth_issue",
    "data_issue",
    "navigation_issue",
    "environment_issue",
    "unknown",
]

ImpactedLayer = Literal[
    "ui",
    "api",
    "data",
    "environment",
    "auth",
    "unknown",
]

RCAConfidence = Literal["low", "medium", "high"]


# ── Input ─────────────────────────────────────────────────────────────────────

class RCAAnalysisRequest(BaseModel):
    """Request to analyze a specific test run."""
    run_id: str


# ── Evidence atom ─────────────────────────────────────────────────────────────

class RCAEvidenceSignal(BaseModel):
    """A single extracted signal that contributed to the RCA classification."""
    signal_type:  str            # timeout | selector_not_found | http_500 | assertion_failure | …
    value:        str            # the exact matched text or error message
    source:       str            # "log" | "step_error" | "meta"
    confidence:   RCAConfidence  # how strongly this signal points to its category


# ── Result ────────────────────────────────────────────────────────────────────

class RCAAnalysisResult(BaseModel):
    """Full root cause analysis result for a test run."""
    run_id:               str
    root_cause_category:  RootCauseCategory   = "unknown"
    probable_cause:       str                 = ""
    confidence:           RCAConfidence       = "low"
    impacted_layer:       ImpactedLayer       = "unknown"
    evidence_signals:     List[RCAEvidenceSignal] = Field(default_factory=list)
    recommendation:       str                 = ""
    summary:              str                 = ""
