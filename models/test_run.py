# models/test_run.py
"""
Pydantic model for test execution records (TestRun).

Each time a TestCase is executed a TestRun is created and persisted.
"""
from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


def _new_uuid() -> str:
    return str(uuid.uuid4())


class TestRun(BaseModel):
    """Execution record for a single TestCase run."""
    run_id:       str      = Field(default_factory=_new_uuid)
    test_case_id: str                        # e.g. TC-DEMO-001
    test_name:    Optional[str] = None       # denormalized for convenience
    executed_at:  datetime = Field(default_factory=_now_utc)
    environment:  str = "default"            # staging | prod | local | …
    status:       Literal["pass", "fail", "error", "running"]
    duration_ms:  Optional[int] = None
    evidence_url: Optional[str] = None
    report_url:   Optional[str] = None
    evidence_id:  Optional[str] = None       # links to run_store / Cloudinary
    logs:         List[str] = Field(default_factory=list)
    steps_result: List[Dict[str, Any]] = Field(default_factory=list)  # executed step detail
    meta:         Dict[str, Any] = Field(default_factory=dict)


class SuiteRunResult(BaseModel):
    """Aggregate result of POST /tests/run-suite."""
    suite_run_id:   str      = Field(default_factory=_new_uuid)
    started_at:     datetime = Field(default_factory=_now_utc)
    finished_at:    Optional[datetime] = None
    environment:    str = "default"
    total:          int = 0
    passed:         int = 0
    failed:         int = 0
    errors:         int = 0
    duration_ms:    int = 0
    runs:           List[TestRun] = Field(default_factory=list)
    filter_applied: Dict[str, Any] = Field(default_factory=dict)
