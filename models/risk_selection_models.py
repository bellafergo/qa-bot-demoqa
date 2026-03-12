# models/risk_selection_models.py
"""
Pydantic models for the Risk-Based Test Selection layer.
"""
from __future__ import annotations

from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field


class RiskSelectionRequest(BaseModel):
    """Request to select tests based on changed modules and risk signals."""
    changed_modules: List[str] = Field(default_factory=list)
    max_tests:       int       = 100
    priority:        Optional[Literal["critical", "high", "medium", "low"]] = None  # optional filter


class SelectedTest(BaseModel):
    """A single test case selected by the risk engine."""
    test_case_id:     str
    name:             str
    module:           str
    type:             str
    priority:         str
    selection_score:  int   = 0
    selection_reason: str   = ""


class RiskSelectionResult(BaseModel):
    """Result of a risk-based test selection run."""
    selected_tests:  List[SelectedTest] = Field(default_factory=list)
    total_selected:  int                = 0
    reasoning:       str                = ""


class SelectAndRunResult(BaseModel):
    """Result of selecting tests and immediately enqueuing them for execution."""
    selection:            RiskSelectionResult
    orchestrator_job_id:  Optional[str] = None
    enqueued:             bool          = False
