# models/analytics_models.py
"""Pydantic contracts for the run-analytics endpoints."""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel


class RunAnalyticsSummary(BaseModel):
    total_runs:        int
    passed_runs:       int
    failed_runs:       int
    pass_rate:         float        # 0–100, 1 decimal
    avg_duration_ms:   int
    runs_last_7_days:  int
    runs_last_30_days: int


class DailyTrendPoint(BaseModel):
    date:      str            # YYYY-MM-DD
    total:     int
    passed:    int
    failed:    int
    pass_rate: Optional[float]  # None when total == 0


class TopFailingTest(BaseModel):
    test_case_id: str
    test_name:    str
    total_runs:   int
    failed_runs:  int
    pass_rate:    float       # 0–100, 1 decimal


class RunsDashboard(BaseModel):
    summary:      RunAnalyticsSummary
    trend:        List[DailyTrendPoint]
    top_failures: List[TopFailingTest]
