# core/suite_models.py
"""
Compact models for test suite execution.
Backward compatible — does not modify any existing step/run structures.
"""
from __future__ import annotations

from enum import Enum
from typing import Any, Dict, List, Optional
from uuid import uuid4

from pydantic import BaseModel, Field


# ── Status enums ───────────────────────────────────────────────────────────────

class TestStatus(str, Enum):
    passed  = "passed"
    failed  = "failed"
    flaky   = "flaky"    # failed on first attempt, passed on retry
    skipped = "skipped"  # cancelled due to stop policy
    error   = "error"    # runner-level crash (not a test assertion failure)


class SuiteStatus(str, Enum):
    passed  = "passed"   # all tests passed (including flaky)
    failed  = "failed"   # all tests failed or errored
    partial = "partial"  # some passed, some failed
    error   = "error"    # orchestrator-level failure


# ── Input models ──────────────────────────────────────────────────────────────

class TestCase(BaseModel):
    """A single test: a name, a list of steps, and execution config."""
    id:         str                     = Field(default_factory=lambda: f"tc-{uuid4().hex[:8]}")
    name:       str
    steps:      List[Dict[str, Any]]
    base_url:   Optional[str]           = None
    expected:   str                     = "pass"   # "pass" | "fail"
    tags:       List[str]               = Field(default_factory=list)
    timeout_s:  Optional[int]           = None
    headless:   bool                    = True


class TestSuite(BaseModel):
    """An ordered collection of TestCases with shared execution policy."""
    id:                         str             = Field(default_factory=lambda: f"suite-{uuid4().hex[:8]}")
    name:                       str
    cases:                      List[TestCase]
    max_concurrency:            int             = 2      # browser instances in parallel
    stop_on_critical_failure:   bool            = False
    critical_failure_threshold: float           = 0.8   # stop when >80% of cases fail
    tags:                       List[str]       = Field(default_factory=list)


# ── Internal execution models ─────────────────────────────────────────────────

class ExecutionJob(BaseModel):
    """A runnable unit — one attempt of one TestCase."""
    id:        str               = Field(default_factory=lambda: f"job-{uuid4().hex[:8]}")
    suite_id:  str
    case_id:   str
    case_name: str
    steps:     List[Dict[str, Any]]
    base_url:  Optional[str]     = None
    expected:  str               = "pass"
    tags:      List[str]         = Field(default_factory=list)
    timeout_s: Optional[int]     = None
    headless:  bool              = True
    attempt:   int               = 1


# ── Output models ─────────────────────────────────────────────────────────────

class JobResult(BaseModel):
    """Result of one ExecutionJob (one attempt of one TestCase)."""
    job_id:         str
    case_id:        str
    case_name:      str
    status:         TestStatus
    attempt:        int
    duration_ms:    int
    evidence_id:    Optional[str]           = None
    evidence_url:   Optional[str]           = None
    report_url:     Optional[str]           = None
    failure_reason: Optional[str]           = None
    failure_type:   Optional[str]           = None   # transient | terminal | unknown
    steps:          List[Dict[str, Any]]    = Field(default_factory=list)
    logs:           List[str]               = Field(default_factory=list)


class BatchRun(BaseModel):
    """Metadata for a suite execution in progress."""
    id:         str             = Field(default_factory=lambda: f"batch-{uuid4().hex[:8]}")
    suite_id:   str
    suite_name: str
    jobs:       List[ExecutionJob]
    env_name:   Optional[str]   = None
    created_at: int             = 0         # epoch ms, set by orchestrator


class SuiteRunResult(BaseModel):
    """Aggregated result of a completed suite run."""
    suite_id:           str
    suite_name:         str
    batch_id:           str
    env_name:           Optional[str]       = None
    status:             SuiteStatus
    total:              int
    passed:             int
    failed:             int
    flaky:              int
    skipped:            int
    pass_rate:          float               # (passed + flaky) / total
    avg_duration_ms:    float
    top_failure_reasons: List[str]          = Field(default_factory=list)
    failed_cases:       List[str]           = Field(default_factory=list)
    job_results:        List[JobResult]     = Field(default_factory=list)
    started_at:         int                 = 0
    finished_at:        int                 = 0
    duration_ms:        int                 = 0
