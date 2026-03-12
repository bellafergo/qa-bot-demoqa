# services/catalog_orchestrator.py
"""
Catalog Orchestrator Service
=============================

Adds an async job-queue layer on top of TestCatalogService so that
test executions happen asynchronously with bounded concurrency.

Architecture
------------

  POST /orchestrator/jobs/single|suite
      ↓
  CatalogOrchestratorService.enqueue_single / enqueue_suite
      ↓  creates OrchestratorJob, pushes to _QUEUE
  _worker_loop  (daemon thread — one global loop)
      ↓  pulls job from _QUEUE, acquires _SEM (semaphore = MAX_CONCURRENCY)
  _run_job  (per-job thread, bounded by semaphore)
      ↓  calls catalog_service.run_test_case() per test_case_id
      ↓  updates job counters + status atomically under _job_lock

Upgrade path
------------
- Swap _QUEUE with a Redis list / Celery task
- Swap _JOB_STORE with a DB table
- Replace threading.Semaphore with Celery / Ray workers

Thread safety
-------------
All mutations to OrchestratorJob go through _job_lock.
Module-level state is safe to share across uvicorn worker threads
when running with a single worker (the default for development).
"""
from __future__ import annotations

import logging
import os
import queue
import threading
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from models.orchestrator_job import OrchestratorJob, JobStatus
from services.test_catalog_service import catalog_service
from services.db.orchestrator_job_repository import orch_job_repo

logger = logging.getLogger("vanya.catalog_orchestrator")

# ── Configuration ─────────────────────────────────────────────────────────────

MAX_CONCURRENCY: int = int(os.getenv("ORCHESTRATOR_MAX_CONCURRENCY", "2"))
MAX_JOBS_KEPT:   int = 500   # in-memory GC threshold

# ── Module-level state ────────────────────────────────────────────────────────

_job_lock:  threading.RLock    = threading.RLock()
_JOB_STORE: Dict[str, OrchestratorJob] = {}
_JOB_ORDER: List[str]          = []   # insertion order, oldest first

_QUEUE:     queue.Queue         = queue.Queue()
_SEM:       threading.Semaphore = threading.Semaphore(MAX_CONCURRENCY)

_worker_started: bool = False
_worker_lock:    threading.Lock = threading.Lock()


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


# ── Job store helpers ─────────────────────────────────────────────────────────

def _save_job(job: OrchestratorJob) -> None:
    """Persist job to in-memory store AND DB."""
    with _job_lock:
        _JOB_STORE[job.job_id] = job
        if job.job_id not in _JOB_ORDER:
            _JOB_ORDER.append(job.job_id)
        # GC: prune oldest entries from memory (DB retains full history)
        while len(_JOB_ORDER) > MAX_JOBS_KEPT:
            oldest = _JOB_ORDER.pop(0)
            _JOB_STORE.pop(oldest, None)
    try:
        orch_job_repo.create_job(job)
    except Exception:
        logger.exception("orchestrator: DB create_job failed for %s", job.job_id)


def _persist_job(job: OrchestratorJob) -> None:
    """Sync in-memory job state to DB (called after status changes)."""
    try:
        orch_job_repo.update_job(job)
    except Exception:
        logger.exception("orchestrator: DB update_job failed for %s", job.job_id)


def _get_job(job_id: str) -> Optional[OrchestratorJob]:
    """Return from in-memory cache if present, else load from DB."""
    with _job_lock:
        if job_id in _JOB_STORE:
            return _JOB_STORE[job_id]
    # Not in memory — try DB (for historical jobs)
    try:
        return orch_job_repo.get_job(job_id)
    except Exception:
        logger.exception("orchestrator: DB get_job failed for %s", job_id)
        return None


def _list_jobs(limit: int = 100) -> List[OrchestratorJob]:
    """
    Return jobs from DB (full persistent history).
    For active jobs not yet flushed to DB, in-memory state is fresher,
    so merge: DB results are overridden by in-memory version where present.
    """
    try:
        db_jobs = orch_job_repo.list_jobs(limit=limit)
    except Exception:
        logger.exception("orchestrator: DB list_jobs failed, falling back to memory")
        with _job_lock:
            return [_JOB_STORE[jid] for jid in reversed(_JOB_ORDER) if jid in _JOB_STORE][:limit]

    # Override DB snapshots with live in-memory state for active jobs
    with _job_lock:
        mem = dict(_JOB_STORE)

    result = [mem.get(j.job_id, j) for j in db_jobs]
    return result


# ── Job execution ─────────────────────────────────────────────────────────────

def _update_job(job: OrchestratorJob, **kwargs: Any) -> None:
    """Apply arbitrary field updates to a job under the lock."""
    with _job_lock:
        for k, v in kwargs.items():
            setattr(job, k, v)


def _run_job(job: OrchestratorJob) -> None:
    """
    Execute all test_case_ids in a job sequentially, updating counters
    after each run.  Runs in its own daemon thread.
    """
    _update_job(job, status="running", started_at=_now_utc())
    _persist_job(job)   # DB: status → running
    logger.info("orchestrator: job %s started — %d tests", job.job_id, job.total_count)

    for tc_id in job.test_case_ids:
        try:
            run = catalog_service.run_test_case(tc_id, environment=job.environment)
            summary: Dict[str, Any] = {
                "test_case_id": tc_id,
                "run_id":       run.run_id,
                "status":       run.status,
                "duration_ms":  run.duration_ms,
            }
            with _job_lock:
                job.run_ids.append(run.run_id)
                job.results.append(summary)
                job.completed_count += 1
                if run.status == "pass":
                    job.passed_count += 1
                elif run.status == "error":
                    job.error_count += 1
                else:
                    job.failed_count += 1

        except Exception as e:
            logger.exception(
                "orchestrator: error running %s in job %s", tc_id, job.job_id
            )
            summary = {
                "test_case_id": tc_id,
                "run_id":       None,
                "status":       "error",
                "duration_ms":  None,
                "error":        str(e),
            }
            with _job_lock:
                job.results.append(summary)
                job.completed_count += 1
                job.error_count += 1

    # Determine final job status
    with _job_lock:
        p = job.passed_count
        f = job.failed_count
        e = job.error_count

        if f == 0 and e == 0:
            final_status: JobStatus = "completed"
        elif p == 0:
            final_status = "failed"
        else:
            final_status = "partial"

        job.status      = final_status
        job.finished_at = _now_utc()

    _persist_job(job)   # DB: final status + counters
    logger.info(
        "orchestrator: job %s → %s  (pass=%d fail=%d error=%d)",
        job.job_id, final_status, p, f, e,
    )


# ── Background worker loop ────────────────────────────────────────────────────

def _worker_loop() -> None:
    """
    Daemon loop: pulls jobs from _QUEUE and dispatches each to its own
    thread, bounded by _SEM so at most MAX_CONCURRENCY run simultaneously.
    """
    logger.info("orchestrator: worker loop started (max_concurrency=%d)", MAX_CONCURRENCY)
    while True:
        try:
            job: OrchestratorJob = _QUEUE.get(timeout=1)
        except queue.Empty:
            continue

        _SEM.acquire()

        def _task(j: OrchestratorJob = job) -> None:
            try:
                _run_job(j)
            finally:
                _SEM.release()

        t = threading.Thread(
            target=_task,
            daemon=True,
            name=f"orch-{job.job_id[:8]}",
        )
        t.start()


def ensure_worker_started() -> None:
    """Start the background worker loop exactly once (idempotent)."""
    global _worker_started
    with _worker_lock:
        if _worker_started:
            return
        t = threading.Thread(target=_worker_loop, daemon=True, name="orch-worker-loop")
        t.start()
        _worker_started = True
    logger.info("orchestrator: background worker started")


# ── Public service facade ─────────────────────────────────────────────────────

class CatalogOrchestratorService:
    """
    Thin stateless facade over the module-level queue and job store.
    All state lives at module level for singleton safety across imports.
    """

    def enqueue_single(
        self,
        test_case_id: str,
        *,
        environment: str = "default",
    ) -> OrchestratorJob:
        """
        Create and enqueue a single-test job.

        Returns the job immediately (status=queued).
        Raises ValueError if test_case_id is not in the catalog.
        """
        tc = catalog_service.get_test_case(test_case_id)
        if tc is None:
            raise ValueError(f"Test case '{test_case_id}' not found in catalog")

        job = OrchestratorJob(
            job_type="single",
            test_case_ids=[test_case_id],
            total_count=1,
            environment=environment,
        )
        _save_job(job)
        _QUEUE.put(job)
        logger.info(
            "orchestrator: enqueued single job %s — tc=%s env=%s",
            job.job_id, test_case_id, environment,
        )
        return job

    def enqueue_suite(
        self,
        *,
        test_case_ids: Optional[List[str]] = None,
        module: Optional[str] = None,
        type_: Optional[str] = None,
        priority: Optional[str] = None,
        tags: Optional[List[str]] = None,
        environment: str = "default",
        limit: int = 50,
    ) -> OrchestratorJob:
        """
        Create and enqueue a suite job.

        Priority:
          1. test_case_ids — explicit list; inactive/missing IDs are skipped.
          2. Catalog filters (module, type_, priority, tags) — applied otherwise.

        Returns the job immediately (status=queued or failed if empty).
        """
        if test_case_ids is not None:
            resolved: List[str] = []
            missing:  List[str] = []
            for tc_id in test_case_ids:
                tc = catalog_service.get_test_case(tc_id)
                if tc and tc.status == "active":
                    resolved.append(tc_id)
                else:
                    missing.append(tc_id)
            if missing:
                logger.warning(
                    "orchestrator: suite — unknown/inactive ids skipped: %s", missing
                )
        else:
            cases    = catalog_service.list_test_cases(
                module=module, type_=type_, priority=priority,
                tags=tags, status="active", limit=limit,
            )
            resolved = [c.test_case_id for c in cases]

        job = OrchestratorJob(
            job_type="suite",
            test_case_ids=resolved,
            total_count=len(resolved),
            environment=environment,
        )

        if not resolved:
            # Nothing to run — settle immediately
            job.status        = "failed"
            job.finished_at   = _now_utc()
            job.error_message = "No active test cases matched the request"
            _save_job(job)
            logger.warning(
                "orchestrator: suite job %s has 0 active tests — marked failed",
                job.job_id,
            )
            return job

        _save_job(job)
        _QUEUE.put(job)
        logger.info(
            "orchestrator: enqueued suite job %s — %d tests env=%s",
            job.job_id, len(resolved), environment,
        )
        return job

    def get_job(self, job_id: str) -> Optional[OrchestratorJob]:
        return _get_job(job_id)

    def list_jobs(self, limit: int = 100) -> List[OrchestratorJob]:
        return _list_jobs(limit=limit)


# Module-level singleton — routes import this
orchestrator_service = CatalogOrchestratorService()


# ── Test isolation helpers ────────────────────────────────────────────────────

def _reset_for_testing() -> None:
    """Wipe all in-memory and DB job state. For use in tests only."""
    with _job_lock:
        _JOB_STORE.clear()
        _JOB_ORDER.clear()
    while not _QUEUE.empty():
        try:
            _QUEUE.get_nowait()
        except Exception:
            break
    try:
        orch_job_repo.clear_all()
    except Exception:
        pass
