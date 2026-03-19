# models/pr_analysis_models.py
"""
Pydantic models for the PR Impact Analysis layer.
"""
from __future__ import annotations

import uuid
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field


def _new_id() -> str:
    return str(uuid.uuid4())[:8]


# ── Input ─────────────────────────────────────────────────────────────────────

class PRAnalysisRequest(BaseModel):
    """Represents a pull request or diff to be analyzed for QA impact."""

    pr_id:              Optional[str]       = None
    title:              Optional[str]       = None
    description:        Optional[str]       = None
    changed_files:      List[str]           = Field(default_factory=list)
    diff_text:          Optional[str]       = None

    # Caller can explicitly name affected modules (merged with inferred ones)
    changed_modules:    List[str]           = Field(default_factory=list)

    author:             Optional[str]       = None
    branch:             Optional[str]       = None

    # Behaviour flags
    auto_enqueue:           bool = False
    generate_draft_tests:   bool = False
    environment:            str  = "default"


# ── Output atoms ──────────────────────────────────────────────────────────────

class DraftTestSuggestion(BaseModel):
    """A candidate test case proposed by the analyzer — not yet in the catalog."""

    draft_id:             str  = Field(default_factory=_new_id)
    name:                 str
    module:               str
    rationale:            str
    suggested_steps:      List[Dict[str, Any]] = Field(default_factory=list)
    suggested_assertions: List[Dict[str, Any]] = Field(default_factory=list)
    source_signal:        str  = ""
    confidence:           Literal["low", "medium", "high"] = "medium"


# ── Primary output ────────────────────────────────────────────────────────────

class PRAnalysisResult(BaseModel):
    """Full QA impact analysis result for a pull request."""

    pr_id:                  Optional[str]  = None

    # What was inferred
    inferred_modules:       List[str]      = Field(default_factory=list)
    inferred_risk_level:    Literal["low", "medium", "high"] = "low"
    risk_reasons:           List[str]      = Field(default_factory=list)

    # Matched catalog tests
    matched_test_case_ids:  List[str]      = Field(default_factory=list)
    matched_tests_count:    int            = 0

    # Draft suggestions (populated when generate_draft_tests=True)
    suggested_new_tests:    List[DraftTestSuggestion] = Field(default_factory=list)

    # Orchestrator job (populated when auto_enqueue=True and matches exist)
    orchestrator_job_id:    Optional[str]  = None

    # Per-test match explanations: { test_case_id: "reason string" }
    test_match_reasons:     Dict[str, str] = Field(default_factory=dict)

    # Human-readable summary
    summary:                str            = ""
    confidence:             Literal["low", "medium", "high"] = "medium"
