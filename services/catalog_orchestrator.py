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
      ↓  pulls job from _QUEUE, acquires _SEM (job-level semaphore)
  _run_job  (per-job thread — dispatches tests to ThreadPoolExecutor)
      ↓  _run_single_test() per test_case_id (parallel, bounded per-type)
      ↓  updates job counters + status atomically under _job_lock

Parallel execution (execution-scheduler block)
----------------------------------------------
- _run_job now runs tests in parallel via ThreadPoolExecutor
- _UI_SEM / _API_SEM enforce per-type concurrency limits
- _schedule_tests() sorts tests by priority+type before dispatch
- _run_single_test() retries on error status or runner exception

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
import time
import uuid
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional, Tuple

from models.orchestrator_job import OrchestratorJob, JobStatus
from services.test_catalog_service import catalog_service
from services.db.orchestrator_job_repository import orch_job_repo

logger = logging.getLogger("vanya.catalog_orchestrator")

# ── Configuration ─────────────────────────────────────────────────────────────

MAX_CONCURRENCY:         int = int(os.getenv("ORCHESTRATOR_MAX_CONCURRENCY", "2"))
MAX_JOBS_KEPT:           int = 500

# Parallel execution settings (execution-scheduler block)
EXECUTION_MAX_WORKERS:   int = int(os.getenv("EXECUTION_MAX_WORKERS",    "4"))
EXECUTION_MAX_UI_WORKERS: int = int(os.getenv("EXECUTION_MAX_UI_WORKERS", "2"))
EXECUTION_MAX_API_WORKERS: int = int(os.getenv("EXECUTION_MAX_API_WORKERS", "4"))
EXECUTION_RETRY_LIMIT:   int = int(os.getenv("EXECUTION_RETRY_LIMIT",    "1"))

# ── Module-level state ────────────────────────────────────────────────────────

_job_lock:  threading.RLock    = threading.RLock()
_JOB_STORE: Dict[str, OrchestratorJob] = {}
_JOB_ORDER: List[str]          = []   # insertion order, oldest first

_QUEUE:     queue.Queue         = queue.Queue()
_SEM:       threading.Semaphore = threading.Semaphore(MAX_CONCURRENCY)

_worker_started: bool = False
_worker_lock:    threading.Lock = threading.Lock()

# Per-type concurrency semaphores
_UI_SEM:  threading.Semaphore = threading.Semaphore(EXECUTION_MAX_UI_WORKERS)
_API_SEM: threading.Semaphore = threading.Semaphore(EXECUTION_MAX_API_WORKERS)

# Observability counters
_STATS: Dict[str, int] = {
    "active_workers":  0,   # tests currently holding a semaphore slot
    "running_ui":      0,   # UI tests currently executing
    "running_api":     0,   # API tests currently executing
    "queued_tasks":    0,   # tasks submitted to executor but not yet running
    "completed_tasks": 0,   # total tests completed since process start
    "retried_tasks":   0,   # total retry attempts since process start
}
_STATS_LOCK: threading.Lock = threading.Lock()


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


# ── Scheduling ────────────────────────────────────────────────────────────────

_PRIORITY_ORDER: Dict[str, int] = {
    "critical": 0,
    "high":     1,
    "medium":   2,
    "low":      3,
}

_TYPE_ORDER: Dict[str, int] = {
    "smoke":      0,
    "regression": 1,
    "e2e":        2,
    "api":        3,
    "ui":         4,
    "negative":   5,
}


def _schedule_tests(test_case_ids: List[str]) -> Tuple[List[str], str]:
    """
    Sort test_case_ids by priority (critical first) then type (smoke first).

    Returns (sorted_ids, scheduling_notes_str).
    Uses a stable sort so tests with equal keys preserve insertion order.
    """
    from services.db.catalog_repository import catalog_repo

    def _sort_key(tc_id: str) -> Tuple[int, int, str]:
        try:
            tc = catalog_repo.get_test_case(tc_id)
            if tc is None:
                return (99, 99, tc_id)
            p = _PRIORITY_ORDER.get((tc.priority or "low").lower(), 3)
            t = _TYPE_ORDER.get((tc.type or "smoke").lower(), 4)
            return (p, t, tc_id)
        except Exception:
            return (99, 99, tc_id)

    sorted_ids = sorted(test_case_ids, key=_sort_key)

    # Build human-readable notes for the first 3 tests
    tops: List[str] = []
    for tc_id in sorted_ids[:3]:
        try:
            from services.db.catalog_repository import catalog_repo as _cr
            tc = _cr.get_test_case(tc_id)
            if tc:
                tops.append(f"{tc.priority}/{tc.type_}")
        except Exception:
            pass

    suffix = "…" if len(sorted_ids) > 3 else ""
    notes = (
        f"Scheduled {len(sorted_ids)} tests: [{', '.join(tops)}{suffix}]"
        if tops
        else f"Scheduled {len(sorted_ids)} tests"
    )
    return sorted_ids, notes


# ── Test-type lookup ──────────────────────────────────────────────────────────

def _get_test_type(tc_id: str) -> str:
    """Return 'ui' or 'api' for a test case. Defaults to 'ui' on any error."""
    try:
        from services.db.catalog_repository import catalog_repo
        tc = catalog_repo.get_test_case(tc_id)
        if tc:
            return getattr(tc, "test_type", "ui") or "ui"
    except Exception:
        pass
    return "ui"


# ── Single-test executor with retry ──────────────────────────────────────────

def _run_single_test(
    job: OrchestratorJob,
    tc_id: str,
    retry_limit: int,
) -> Dict[str, Any]:
    """
    Execute one test case with retry policy.

    Retry conditions:
      - run.status == "error" (runner crash / timeout) → retry up to retry_limit
      - run.status == "fail"  (assertion failure)      → do NOT retry
      - run.status == "pass"                           → done

    Per-type semaphores (UI_SEM / API_SEM) ensure bounded concurrency
    across all jobs running simultaneously.
    """
    test_type = _get_test_type(tc_id)
    sem = _UI_SEM if test_type == "ui" else _API_SEM

    # Task is now actively processing (was "queued" in the executor)
    with _STATS_LOCK:
        _STATS["queued_tasks"] = max(0, _STATS["queued_tasks"] - 1)

    was_retried = False
    last_result: Dict[str, Any] = {
        "test_case_id": tc_id,
        "status":       "error",
        "run_id":       None,
        "duration_ms":  None,
        "attempt":      1,
        "test_type":    test_type,
    }

    for attempt in range(retry_limit + 1):
        if attempt > 0:
            was_retried = True
            time.sleep(0.3)   # brief pause before retry

        # Acquire per-type semaphore — blocks until a slot is free
        sem.acquire()

        # Track this attempt as active
        with _STATS_LOCK:
            _STATS["active_workers"] += 1
            if test_type == "ui":
                _STATS["running_ui"] += 1
            else:
                _STATS["running_api"] += 1

        try:
            run = catalog_service.run_test_case(tc_id, environment=job.environment)
            last_result = {
                "test_case_id": tc_id,
                "run_id":       run.run_id,
                "status":       run.status,
                "duration_ms":  run.duration_ms,
                "attempt":      attempt + 1,
                "test_type":    test_type,
            }
        except Exception as exc:
            last_result = {
                "test_case_id": tc_id,
                "run_id":       None,
                "status":       "error",
                "duration_ms":  None,
                "error":        f"{type(exc).__name__}: {exc}",
                "attempt":      attempt + 1,
                "test_type":    test_type,
            }
        finally:
            sem.release()
            with _STATS_LOCK:
                _STATS["active_workers"] = max(0, _STATS["active_workers"] - 1)
                if test_type == "ui":
                    _STATS["running_ui"] = max(0, _STATS["running_ui"] - 1)
                else:
                    _STATS["running_api"] = max(0, _STATS["running_api"] - 1)

        # Retry decision: only on error, not on assertion failures
        if last_result["status"] == "error" and attempt < retry_limit:
            logger.info(
                "orchestrator: retrying %s (attempt %d→%d) — status=error",
                tc_id, attempt + 1, attempt + 2,
            )
            continue

        break   # success ("pass"), assertion failure ("fail"), or retries exhausted

    # Final accounting (once per test, after all retry attempts)
    with _STATS_LOCK:
        _STATS["completed_tasks"] += 1
        if was_retried:
            _STATS["retried_tasks"] += 1

    return last_result


# ── Job execution ─────────────────────────────────────────────────────────────

def _update_job(job: OrchestratorJob, **kwargs: Any) -> None:
    """Apply arbitrary field updates to a job under the lock."""
    with _job_lock:
        for k, v in kwargs.items():
            setattr(job, k, v)


def _run_job(job: OrchestratorJob) -> None:
    """
    Execute all test_case_ids in a job in parallel, bounded by per-type
    semaphores.  Tests are scheduled (sorted) before dispatch.
    Runs in its own daemon thread (spawned by _worker_loop).
    """
    _update_job(job, status="running", started_at=_now_utc())
    _persist_job(job)
    logger.info("orchestrator: job %s started — %d tests", job.job_id, job.total_count)

    # ── Smart scheduling: sort by priority/type ───────────────────────────────
    sorted_ids, notes = _schedule_tests(job.test_case_ids)
    _update_job(job, scheduling_notes=notes)
    logger.debug("orchestrator: job %s — %s", job.job_id, notes)

    if not sorted_ids:
        with _job_lock:
            job.status     = "failed"
            job.finished_at = _now_utc()
            job.error_message = "No test_case_ids to execute"
        _persist_job(job)
        return

    # Track how many tests are "queued" in the executor
    with _STATS_LOCK:
        _STATS["queued_tasks"] += len(sorted_ids)

    # ── Parallel dispatch via ThreadPoolExecutor ───────────────────────────────
    n_workers = min(EXECUTION_MAX_WORKERS, len(sorted_ids))

    with ThreadPoolExecutor(
        max_workers=n_workers,
        thread_name_prefix=f"orch-exec-{job.job_id[:8]}",
    ) as pool:
        futures = {
            pool.submit(_run_single_test, job, tc_id, EXECUTION_RETRY_LIMIT): tc_id
            for tc_id in sorted_ids
        }

        for future in as_completed(futures):
            tc_id = futures[future]
            try:
                summary = future.result()
            except Exception as exc:
                summary = {
                    "test_case_id": tc_id,
                    "run_id":       None,
                    "status":       "error",
                    "duration_ms":  None,
                    "error":        f"Executor error: {type(exc).__name__}: {exc}",
                    "attempt":      1,
                    "test_type":    "unknown",
                }

            with _job_lock:
                job.results.append(summary)
                job.completed_count += 1
                if summary.get("run_id"):
                    job.run_ids.append(summary["run_id"])

                s = summary.get("status", "error")
                if s == "pass":
                    job.passed_count += 1
                elif s == "error":
                    job.error_count += 1
                else:
                    job.failed_count += 1

                # Track total retries in the job record
                extra_attempts = summary.get("attempt", 1) - 1
                if extra_attempts > 0:
                    job.retry_count = job.retry_count + extra_attempts

    # ── Finalize job status ───────────────────────────────────────────────────
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

    _persist_job(job)
    logger.info(
        "orchestrator: job %s → %s  (pass=%d fail=%d error=%d retry=%d)",
        job.job_id, final_status, p, f, e, job.retry_count,
    )


# ── Background worker loop ────────────────────────────────────────────────────

def _worker_loop() -> None:
    """
    Daemon loop: pulls jobs from _QUEUE and dispatches each to its own
    thread, bounded by _SEM so at most MAX_CONCURRENCY jobs run simultaneously.
    """
    logger.info(
        "orchestrator: worker loop started (max_concurrency=%d, max_workers=%d)",
        MAX_CONCURRENCY, EXECUTION_MAX_WORKERS,
    )
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


# ── Observability ─────────────────────────────────────────────────────────────

def get_execution_status() -> Dict[str, Any]:
    """
    Return a snapshot of live execution stats for the /execution/status route.
    All values are point-in-time approximations (no locking across both stores).
    """
    with _STATS_LOCK:
        stats = dict(_STATS)

    with _job_lock:
        active_jobs = sum(1 for j in _JOB_STORE.values() if j.status == "running")
        queued_jobs = sum(1 for j in _JOB_STORE.values() if j.status == "queued")

    return {
        "active_jobs":          active_jobs,
        "queued_jobs":          queued_jobs,
        "active_workers":       stats["active_workers"],
        "queue_depth":          queued_jobs + stats["queued_tasks"],
        "max_workers":          EXECUTION_MAX_WORKERS,
        "max_ui_workers":       EXECUTION_MAX_UI_WORKERS,
        "max_api_workers":      EXECUTION_MAX_API_WORKERS,
        "running_ui_workers":   stats["running_ui"],
        "running_api_workers":  stats["running_api"],
        "queued_tasks":         stats["queued_tasks"],
        "running_tasks":        stats["active_workers"],
        "completed_tasks":      stats["completed_tasks"],
        "retried_tasks":        stats["retried_tasks"],
    }


def retry_failed_tests(job_id: str) -> Optional[OrchestratorJob]:
    """
    Create and enqueue a new suite job that re-runs only the failed/errored
    tests from a previous job.

    Returns the new OrchestratorJob (status=queued) or None if:
    - the original job is not found
    - the original job has no failed/errored results
    """
    original = _get_job(job_id)
    if original is None:
        return None

    # Collect failed/error test_case_ids (preserve order, deduplicate)
    seen: set = set()
    failed_ids: List[str] = []
    for result in (original.results or []):
        tc_id  = result.get("test_case_id")
        status = result.get("status", "")
        if tc_id and status in ("fail", "error") and tc_id not in seen:
            seen.add(tc_id)
            failed_ids.append(tc_id)

    if not failed_ids:
        return None

    new_job = OrchestratorJob(
        job_type         = "suite",
        test_case_ids    = failed_ids,
        total_count      = len(failed_ids),
        environment      = original.environment,
        scheduling_notes = (
            f"Retry of {len(failed_ids)} failed test(s) from job {job_id[:8]}"
        ),
    )
    _save_job(new_job)
    _QUEUE.put(new_job)
    logger.info(
        "orchestrator: retry job %s — %d failed tests from job %s",
        new_job.job_id, len(failed_ids), job_id,
    )
    return new_job


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
    with _STATS_LOCK:
        for k in list(_STATS):
            _STATS[k] = 0
    try:
        orch_job_repo.clear_all()
    except Exception:
        pass
