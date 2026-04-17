# models/run_contract.py
"""
Canonical Run Contract
======================
Single authoritative structure for all test run records across Vanya.

Every execution path (catalog, orchestrator, chat, API) normalises through
services/run_mapper.py to produce a CanonicalRun before leaving the API layer.

Fields
------
run_id        — Canonical execution id (UUID or stable string); always present for API navigation
job_id        — set when the run was part of an orchestrator job
test_id       — test_case_id of the executed test case (was test_case_id)
test_name     — human-readable test name (denormalized for convenience)
suite_id      — suite_run_id when the run belongs to a suite
suite_name    — human-readable suite name (optional)
source        — which code path produced this run
status        — canonical status (see values below)
started_at    — ISO-8601 string, or null if unknown
finished_at   — ISO-8601 string, or null
duration_ms   — wall-clock milliseconds; always an int (0 when unavailable)
summary       — one-line description, e.g. "5/6 steps passed"
error_summary — short description of failure cause, or null
rca_summary   — RCA narrative if previously analyzed, or null
steps         — executed step details (list of dicts, same shape as steps_result)
artifacts     — screenshot + external asset links (see RunArtifacts)
meta          — execution context metadata (see RunMeta)

Status values
-------------
  queued   — job accepted, execution has not started
  running  — currently executing
  passed   — completed, all assertions satisfied
  failed   — completed with one or more failures or runner errors
  canceled — aborted before completion

Backward compatibility notes
-----------------------------
• Legacy runs (run_store / chat path) may omit the `artifacts` / `meta` objects.
  Consumers should use the helpers in run_mapper.py rather than reading raw dicts.
• `evidence_id` is optional metadata (e.g. qa_runs PK / EV-… label). URLs and lookups use `run_id` only.
• `test_id` corresponds to `test_case_id` in the original TestRun model.
• `steps` corresponds to `steps_result` in the original TestRun model.
"""
from __future__ import annotations

from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

RunStatus = Literal[
    "queued",
    "planning",
    "compiled",
    "running",
    "passed",
    "failed",
    "error",
    "canceled",
]
RunSource = Literal["chat", "catalog", "orchestrator", "api"]


class RunArtifacts(BaseModel):
    """All external evidence produced by a test run."""

    screenshot_b64: Optional[str] = None   # base-64 encoded PNG, or null
    evidence_url:   Optional[str] = None   # link to Cloudinary screenshot / HTML report
    report_url:     Optional[str] = None   # link to PDF / detailed report
    log_url:        Optional[str] = None   # link to raw log file (reserved)


class RunMeta(BaseModel):
    """Execution-context metadata — not assertion results."""

    trigger_source: Optional[str] = None   # "catalog", "chat", "orchestrator", …
    triggered_by:   Optional[str] = None   # user / CI system identifier
    environment:    Optional[str] = None   # "default", "staging", "prod"
    browser:        Optional[str] = None   # "chromium", "firefox", "webkit"
    base_url:       Optional[str] = None   # root URL under test
    correlation_id: Optional[str] = None  # x-request-id / trace id
    client_id:      Optional[str] = None  # optional multi-client partition id
    workspace_id:   Optional[str] = None  # optional multi-workspace partition id

    # ── CI / operability: flaky quarantine & retry metadata ─────────────
    flaky_signal: Optional[str] = None  # e.g. "suspected_flaky"
    flaky_score: Optional[float] = None
    flip_rate: Optional[float] = None
    retry_count: Optional[int] = None  # number of auto retries applied
    retry_policy_applied: Optional[bool] = None
    quarantine_recommended: Optional[bool] = None
    final_outcome_reason: Optional[str] = None


class CanonicalRun(BaseModel):
    """
    Unified run record.
    All API endpoints that return individual run objects MUST use this shape.
    """

    run_id:        str
    job_id:        Optional[str] = None
    test_id:       Optional[str] = None    # corresponds to test_case_id in TestRun
    test_name:     Optional[str] = None
    suite_id:      Optional[str] = None
    suite_name:    Optional[str] = None
    source:        RunSource = "catalog"
    status:        RunStatus = "failed"
    started_at:    Optional[str] = None
    finished_at:   Optional[str] = None
    duration_ms:   int = 0
    steps_count:   int = 0
    correlation_id: Optional[str] = None
    summary:       Optional[str] = None
    error_summary: Optional[str] = None
    rca_summary:   Optional[str] = None
    evidence_id:   Optional[str] = Field(
        default=None,
        description="Secondary id (e.g. qa_runs.evidence_id, EV-…). Not the navigation key; use run_id.",
    )
    steps:         List[Dict[str, Any]] = Field(default_factory=list)
    artifacts:     RunArtifacts = Field(default_factory=RunArtifacts)
    meta:          RunMeta      = Field(default_factory=RunMeta)

    # Convenience aliases (some frontends read these fields directly)
    evidence_url:  Optional[str] = None
    report_url:    Optional[str] = None


class CanonicalSuiteResult(BaseModel):
    """
    Aggregate result of POST /tests/run-suite — canonical edition.

    Drop-in replacement for SuiteRunResult with runs typed as CanonicalRun.
    Top-level counters (total/passed/failed/errors) are preserved so existing
    consumers (DraftsPage, etc.) continue to work without changes.
    """

    suite_run_id:   str
    started_at:     Optional[str] = None
    finished_at:    Optional[str] = None
    environment:    str = "default"
    total:          int = 0
    passed:         int = 0
    failed:         int = 0
    errors:         int = 0
    duration_ms:    int = 0
    runs:           List[CanonicalRun] = Field(default_factory=list)
    filter_applied: Dict[str, Any]     = Field(default_factory=dict)
