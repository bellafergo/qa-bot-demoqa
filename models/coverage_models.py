# models/coverage_models.py
"""
Pydantic models for the Coverage Intelligence layer.
"""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel, Field


class CoverageRequest(BaseModel):
    """Optional filter for coverage analysis."""
    module: Optional[str] = None


class CoverageResult(BaseModel):
    """Coverage metrics and gap analysis for one module."""
    module:             str
    total_tests:        int   = 0
    executed_tests:     int   = 0   # tests with at least one run in history
    passed_tests:       int   = 0   # tests whose last run passed
    failed_tests:       int   = 0   # tests whose last run failed / errored
    never_run_tests:    int   = 0   # tests with zero run history
    coverage_score:     float = 0.0 # executed_tests / total_tests  (0.0 – 1.0)
    missing_test_types: List[str] = Field(default_factory=list)  # e.g. ["negative", "smoke"]
    recommendations:    List[str] = Field(default_factory=list)
