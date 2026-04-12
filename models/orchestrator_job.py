# models/orchestrator_job.py
"""
OrchestratorJob model.

Represents a batch request to execute one or more test cases through
the TestOrchestratorService.  Jobs move through:

  queued → running → completed | partial | failed
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


JobStatus = Literal["queued", "running", "completed", "partial", "failed"]
JobType   = Literal["single", "suite", "batch", "shard"]   # batch/shard added in Block 16


class OrchestratorJob(BaseModel):
    """A queued or completed execution job."""

    job_id:           str      = Field(default_factory=_new_uuid)
    job_type:         JobType  = "single"

    # Status
    status:           JobStatus = "queued"
    error_message:    Optional[str] = None

    # Scope
    test_case_ids:    List[str]        = Field(default_factory=list)
    environment:      str              = "default"
    # Catalog / tenant partition — drives per-project queue + concurrency (v1)
    project_id:       str              = Field(default="default", min_length=1)

    # Counters — updated as tests complete
    total_count:      int = 0
    completed_count:  int = 0
    passed_count:     int = 0
    failed_count:     int = 0
    error_count:      int = 0

    # Run IDs produced by this job (appended as each test finishes)
    run_ids:          List[str]        = Field(default_factory=list)

    # Lightweight per-test summaries embedded in the job response
    results:          List[Dict[str, Any]] = Field(default_factory=list)

    # Execution-scheduler extensions (added in parallel-execution block)
    retry_count:      int = 0
    skipped_count:    int = 0
    scheduling_notes: Optional[str] = None

    # Trigger context — JSON-encoded dict set by caller, never overwritten by scheduler
    # Keys: source, selection_type, pr_title, pr_branch, inferred_modules,
    #       selected_modules, selected_test_ids
    context_json:     Optional[str] = None

    # Retry correlation — when this job was created via retry-failed
    parent_job_id:    Optional[str] = None
    # Populated at read time: job_ids of retries spawned from this job (not persisted)
    retry_job_ids:    Optional[List[str]] = None

    # Timestamps
    created_at:       datetime = Field(default_factory=_now_utc)
    started_at:       Optional[datetime] = None
    finished_at:      Optional[datetime] = None

    @property
    def duration_ms(self) -> Optional[int]:
        if self.started_at and self.finished_at:
            delta = self.finished_at - self.started_at
            return int(delta.total_seconds() * 1000)
        return None
