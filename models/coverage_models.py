# models/coverage_models.py
"""
Pydantic models for the Coverage Intelligence layer.
"""
from __future__ import annotations

from typing import Any, List, Optional

from pydantic import BaseModel, Field


class CoverageRequest(BaseModel):
    """Optional filter for coverage analysis."""
    module: Optional[str] = None


class CoverageModuleSummary(BaseModel):
    """Per-module coverage entry in the aggregate summary response."""
    module:          str
    coverage_pct:    int            = 0
    test_case_count: int            = 0
    flows_covered:   int            = 0   # executed tests
    gaps:            List[str]      = Field(default_factory=list)
    notes:           Optional[str]  = None


class CoverageSummaryResponse(BaseModel):
    """Aggregate coverage response consumed by CoveragePage."""
    overall_coverage_pct:        int                        = 0
    total_test_cases:            int                        = 0
    total_flows_discovered:      int                        = 0
    uncovered_flows_count:       int                        = 0
    modules:                     List[CoverageModuleSummary] = Field(default_factory=list)
    discovered_pages:            List[str]                  = Field(default_factory=list)
    uncovered_flow_suggestions:  List                       = Field(default_factory=list)


class CoverageTestGenerationRequest(BaseModel):
    """Input for POST /coverage/generate-tests."""
    module: str


class CoverageTestGenerationResponse(BaseModel):
    """Draft test suggestions generated from coverage gaps for a module."""
    module:         str
    gap_summary:    List[str]      = Field(default_factory=list)   # from recommendations
    suggested_tests: List[Any]     = Field(default_factory=list)   # List[DraftTestSuggestion]


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
