# models/execution_scheduler_models.py
"""
Request / response models for the execution scheduler and worker-pool routes.
"""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel, Field


class ExecutionStatusResponse(BaseModel):
    """Live snapshot of the execution scheduler state."""
    active_jobs:          int = 0   # jobs currently running
    queued_jobs:          int = 0   # jobs waiting to be dispatched
    active_workers:       int = 0   # tests currently holding a semaphore slot
    queue_depth:          int = 0   # queued_jobs + queued_tasks
    max_workers:          int = 0
    max_ui_workers:       int = 0
    max_api_workers:      int = 0
    running_ui_workers:   int = 0
    running_api_workers:  int = 0
    queued_tasks:         int = 0   # tests submitted to executor but not yet running
    running_tasks:        int = 0   # tests currently executing (= active_workers)
    completed_tasks:      int = 0   # total tests completed since process start
    retried_tasks:        int = 0   # total retries since process start


class BatchExecutionRequest(BaseModel):
    """Input for POST /execution/run-batch."""
    test_case_ids:   List[str]
    environment:     str            = "default"
    scheduling_mode: Optional[str]  = None   # "priority" (default) | "fifo" (no-op v1)


class BatchExecutionResponse(BaseModel):
    """Response from POST /execution/run-batch."""
    ok:          bool
    job_id:      str
    status:      str
    total_count: int


class RetryFailedRequest(BaseModel):
    """Input for POST /execution/retry-failed."""
    job_id: str


class RetryFailedResponse(BaseModel):
    """Response from POST /execution/retry-failed."""
    ok:          bool
    job_id:      str
    status:      str
    total_count: int
