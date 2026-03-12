# models/dashboard_models.py
"""
Pydantic response models for the QA Dashboard API.
"""
from __future__ import annotations

from typing import Dict, List, Optional
from pydantic import BaseModel


class DashboardSummary(BaseModel):
    """Compact platform-wide operational summary."""

    # Test cases
    total_test_cases:    int = 0
    active_test_cases:   int = 0
    inactive_test_cases: int = 0
    total_ui_tests:      int = 0
    total_api_tests:     int = 0

    # Runs
    total_runs: int   = 0
    pass_runs:  int   = 0
    fail_runs:  int   = 0
    error_runs: int   = 0
    pass_rate:  float = 0.0          # percentage, rounded to 2 dp

    # Jobs
    total_jobs:     int = 0
    queued_jobs:    int = 0
    running_jobs:   int = 0
    completed_jobs: int = 0
    partial_jobs:   int = 0
    failed_jobs:    int = 0

    # Recency
    last_run_at: Optional[str] = None   # ISO-8601 string
    last_job_at: Optional[str] = None   # ISO-8601 string


class DashboardModuleMetrics(BaseModel):
    """Run metrics aggregated by test-case module."""

    module:           str
    test_case_count:  int   = 0
    run_count:        int   = 0
    pass_count:       int   = 0
    fail_count:       int   = 0
    error_count:      int   = 0
    pass_rate:        float = 0.0   # 0.0 when run_count == 0


class RunStatusBreakdown(BaseModel):
    """Total run counts by outcome status."""
    pass_count:  int = 0
    fail_count:  int = 0
    error_count: int = 0


class JobStatusBreakdown(BaseModel):
    """Total orchestrator job counts by status."""
    queued:    int = 0
    running:   int = 0
    completed: int = 0
    partial:   int = 0
    failed:    int = 0
