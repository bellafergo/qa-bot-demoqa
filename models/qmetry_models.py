# models/qmetry_models.py
"""
Read-only QMetry discovery models (QMETRY-01A).

Test result discovery (execution-level results) is intentionally deferred to
QMETRY-01B Coverage Intelligence — no QMetryTestResult model in QMETRY-01A.
"""
from __future__ import annotations

from datetime import datetime
from typing import List, Optional

from pydantic import BaseModel, Field


class QMetryConnectionStatus(BaseModel):
    connected: bool = False
    base_url: Optional[str] = None
    project_count: int = 0
    test_case_count: int = 0
    run_count: int = 0
    last_sync: Optional[datetime] = None


class QMetryProject(BaseModel):
    project_id: str
    project_name: str


class QMetryTestCase(BaseModel):
    test_case_id: str
    name: str
    priority: Optional[str] = None
    status: Optional[str] = None


class QMetryTestCycle(BaseModel):
    cycle_id: str
    cycle_name: str
    status: Optional[str] = None


class QMetryTestSuite(BaseModel):
    suite_id: str
    suite_name: str


class QMetryTestRun(BaseModel):
    run_id: str
    run_name: str
    status: Optional[str] = None
    execution_date: Optional[str] = None


class QMetryProjectsResponse(BaseModel):
    projects: List[QMetryProject] = Field(default_factory=list)
    total: int = 0


class QMetryTestCasesResponse(BaseModel):
    test_cases: List[QMetryTestCase] = Field(default_factory=list)
    total: int = 0


class QMetryTestCyclesResponse(BaseModel):
    test_cycles: List[QMetryTestCycle] = Field(default_factory=list)
    total: int = 0


class QMetryTestSuitesResponse(BaseModel):
    test_suites: List[QMetryTestSuite] = Field(default_factory=list)
    total: int = 0


class QMetryTestRunsResponse(BaseModel):
    test_runs: List[QMetryTestRun] = Field(default_factory=list)
    total: int = 0
