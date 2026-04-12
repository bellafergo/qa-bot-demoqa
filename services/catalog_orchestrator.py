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
      ↓  creates OrchestratorJob, pushes to per-project pending deques
  _worker_loop  (daemon thread — one global loop)
      ↓  fair round-robin pop, per-project concurrency cap, then _SEM (global cap)
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
- Swap pending deques / _QUEUE with a Redis list / Celery task
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
import threading
import time
import uuid
from collections import defaultdict, deque
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional, Tuple

from models.orchestrator_job import OrchestratorJob, JobStatus
from services.test_catalog_service import catalog_service
from services.failure_intelligence_service import failure_intelligence_service
from services.db.orchestrator_job_repository import orch_job_repo

logger = logging.getLogger("vanya.catalog_orchestrator")

# ── Configuration ─────────────────────────────────────────────────────────────

MAX_CONCURRENCY:         int = int(os.getenv("ORCHESTRATOR_MAX_CONCURRENCY", "2"))
MAX_CONCURRENT_JOBS_PER_PROJECT: int = max(
    1, int(os.getenv("ORCHESTRATOR_MAX_CONCURRENT_JOBS_PER_PROJECT", "2"))
)
MAX_JOBS_KEPT:           int = 500
SHARD_SIZE:              int = int(os.getenv("ORCHESTRATOR_SHARD_SIZE", "20"))  # Block 16

# Parallel execution settings (execution-scheduler block)
EXECUTION_MAX_WORKERS:   int = int(os.getenv("EXECUTION_MAX_WORKERS",    "4"))
EXECUTION_MAX_UI_WORKERS: int = int(os.getenv("EXECUTION_MAX_UI_WORKERS", "2"))
EXECUTION_MAX_API_WORKERS: int = int(os.getenv("EXECUTION_MAX_API_WORKERS", "4"))
EXECUTION_RETRY_LIMIT:   int = int(os.getenv("EXECUTION_RETRY_LIMIT",    "1"))

# ── Flaky quarantine + retry policy (execution-scheduler block) ────────────
# Defaults are conservative: disabled unless explicitly enabled.
FLAKY_AUTO_RETRY_ENABLED: bool = str(os.getenv("FLAKY_AUTO_RETRY_ENABLED", "false")).lower() == "true"
FLAKY_AUTO_RETRY_MAX: int = max(0, int(os.getenv("FLAKY_AUTO_RETRY_MAX", "1")))
FLAKY_QUARANTINE_RECOMMENDATION_ENABLED: bool = (
    str(os.getenv("FLAKY_QUARANTINE_RECOMMENDATION_ENABLED", "true")).lower() == "true"
)

# ── Module-level state ────────────────────────────────────────────────────────

_job_lock:  threading.RLock    = threading.RLock()
_JOB_STORE: Dict[str, OrchestratorJob] = {}
_JOB_ORDER: List[str]          = []   # insertion order, oldest first

_SEM:       threading.Semaphore = threading.Semaphore(MAX_CONCURRENCY)

# Per-project pending queues + fair dispatch (round-robin across project keys)
_SCHED_LOCK: threading.Lock = threading.Lock()
_PENDING_BY_PROJECT: Dict[str, deque] = defaultdict(deque)
_RR_LAST_PROJECT: Optional[str] = None
# Reserved slots: incremented when a job is popped for dispatch, decremented when the job thread ends
_RUNNING_BY_PROJECT: Dict[str, int] = defaultdict(int)

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


def _list_jobs(limit: int = 100, project_id: Optional[str] = None) -> List[OrchestratorJob]:
    """
    Return jobs from DB (full persistent history).
    For active jobs not yet flushed to DB, in-memory state is fresher,
    so merge: DB results are overridden by in-memory version where present.
    """
    try:
        db_jobs = orch_job_repo.list_jobs(limit=limit, project_id=project_id)
    except Exception:
        logger.exception("orchestrator: DB list_jobs failed, falling back to memory")
        with _job_lock:
            return [_JOB_STORE[jid] for jid in reversed(_JOB_ORDER) if jid in _JOB_STORE][:limit]

    # Override DB snapshots with live in-memory state for active jobs
    with _job_lock:
        mem = dict(_JOB_STORE)

    result = [mem.get(j.job_id, j) for j in db_jobs]
    return result


def _normalize_project_id(project_id: Optional[str]) -> str:
    s = (project_id or "").strip()
    return s if s else "default"


def _enqueue_pending(job: OrchestratorJob) -> None:
    """Partition pending work by project_id (logical queue per tenant)."""
    pid = _normalize_project_id(job.project_id)
    job.project_id = pid
    with _SCHED_LOCK:
        _PENDING_BY_PROJECT[pid].append(job)
    logger.debug(
        "orchestrator: pending queued job=%s project_id=%s pending_depth=%d",
        job.job_id[:8],
        pid,
        len(_PENDING_BY_PROJECT[pid]),
    )


def _fair_pop_next_job() -> Optional[OrchestratorJob]:
    """
    Deterministic fair dispatch: round-robin across project keys that have
    pending jobs. Respects MAX_CONCURRENT_JOBS_PER_PROJECT using reserved slots
    (incremented here, decremented when the job thread finishes).
    """
    global _RR_LAST_PROJECT
    with _SCHED_LOCK:
        keys = sorted(k for k, dq in _PENDING_BY_PROJECT.items() if dq)
        if not keys:
            return None
        start = 0
        if _RR_LAST_PROJECT in keys:
            start = (keys.index(_RR_LAST_PROJECT) + 1) % len(keys)
        n = len(keys)
        for i in range(n):
            pid = keys[(start + i) % n]
            if _RUNNING_BY_PROJECT.get(pid, 0) >= MAX_CONCURRENT_JOBS_PER_PROJECT:
                continue
            dq = _PENDING_BY_PROJECT[pid]
            if not dq:
                continue
            job = dq.popleft()
            jpid = _normalize_project_id(job.project_id)
            _RUNNING_BY_PROJECT[jpid] = _RUNNING_BY_PROJECT.get(jpid, 0) + 1
            _RR_LAST_PROJECT = jpid
            return job
        return None


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

    Within the same priority+type bucket, original insertion order is preserved
    (fair FIFO ordering — tests submitted earlier run first).

    Returns (sorted_ids, scheduling_notes_str).
    """
    from services.db.catalog_repository import catalog_repo

    def _sort_key(idx_and_id: Tuple[int, str]) -> Tuple[int, int, int]:
        idx, tc_id = idx_and_id
        try:
            tc = catalog_repo.get_test_case(tc_id)
            if tc is None:
                return (99, 99, idx)
            p = _PRIORITY_ORDER.get((tc.priority or "low").lower(), 3)
            t = _TYPE_ORDER.get((tc.type or "smoke").lower(), 4)
            return (p, t, idx)   # idx = FIFO tiebreaker within same priority+type
        except Exception:
            return (99, 99, idx)

    sorted_indexed = sorted(enumerate(test_case_ids), key=_sort_key)
    sorted_ids = [tc_id for _, tc_id in sorted_indexed]

    # Build human-readable notes for the first 3 tests
    tops: List[str] = []
    for tc_id in sorted_ids[:3]:
        try:
            from services.db.catalog_repository import catalog_repo as _cr
            tc = _cr.get_test_case(tc_id)
            if tc:
                tops.append(f"{tc.priority}/{tc.type}")
        except Exception:
            pass

    suffix = "…" if len(sorted_ids) > 3 else ""
    notes = (
        f"Scheduled {len(sorted_ids)} tests: [{', '.join(tops)}{suffix}]"
        if tops
        else f"Scheduled {len(sorted_ids)} tests"
    )
    return sorted_ids, notes


def _derive_project_id_from_tests(test_case_ids: List[str]) -> str:
    """Majority project_id among catalog rows (v1 tenant label for queue partitioning)."""
    counts: Dict[str, int] = defaultdict(int)
    for tc_id in test_case_ids:
        tc = catalog_service.get_test_case(tc_id)
        if tc:
            counts[_normalize_project_id(getattr(tc, "project_id", None))] += 1
    if not counts:
        return "default"
    return max(counts.items(), key=lambda kv: kv[1])[0]


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

    # ── Optional flakiness auto-retry policy ───────────────────────────────
    flaky_signal: Optional[str] = None
    flaky_score: Optional[float] = None
    flip_rate: Optional[float] = None
    suspected_flaky: bool = False

    if FLAKY_AUTO_RETRY_ENABLED and FLAKY_AUTO_RETRY_MAX > 0:
        try:
            s = failure_intelligence_service.get_flaky_test_signal(tc_id)
            suspected_flaky = bool(getattr(s, "suspected_flaky", False))
            if suspected_flaky:
                flaky_signal = "suspected_flaky"
                flaky_score = getattr(s, "flaky_score", None)
                flip_rate = getattr(s, "flip_rate", None)
        except Exception:
            # Non-fatal: if flaky detection fails, fall back to error-only retry.
            logger.debug("orchestrator: flaky detection failed for %s", tc_id, exc_info=True)

    flaky_retry_limit = FLAKY_AUTO_RETRY_MAX if suspected_flaky else 0
    max_attempts = 1 + max(0, retry_limit) + flaky_retry_limit

    # Separate counters for auditability and to avoid mixing semantics.
    error_retries_used = 0
    flaky_retries_used = 0

    initial_status: Optional[str] = None
    last_run_obj: Any = None

    was_retried = False
    last_result: Dict[str, Any] = {
        "test_case_id": tc_id,
        "status":       "error",
        "run_id":       None,
        "duration_ms":  None,
        "attempt":      1,
        "test_type":    test_type,
        "flaky_signal": flaky_signal,
        "retry_policy_applied": False,
        "retry_count": 0,
        "quarantine_recommended": False,
        "final_outcome_reason": None,
    }

    for attempt in range(max_attempts):
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
            # Propagate trigger context into each run's meta for RunsPage visibility
            _extra_meta: Optional[Dict[str, Any]] = None
            if getattr(job, "context_json", None):
                try:
                    import json as _json
                    _extra_meta = {"trigger_context": _json.loads(job.context_json)}
                except Exception:
                    pass

            # Flaky metadata is attached to every attempt so it can be audited
            # in run history / evidence pages.
            if suspected_flaky and flaky_signal:
                _extra_meta = dict(_extra_meta or {})
                _extra_meta.update(
                    {
                        "flaky_signal": flaky_signal,
                        "flaky_score": flaky_score,
                        "flip_rate": flip_rate,
                        "retry_policy_applied": True,  # may still result in 0 retries
                    }
                )

            # Catálogo UI: TestCatalogService._execute → prepare_web_steps_for_execution
            # + validate_steps + execute_test (misma tubería que GET/POST /tests síncronos).
            run = catalog_service.run_test_case(
                tc_id,
                environment=job.environment,
                extra_meta=_extra_meta,
                correlation_id=str(job.job_id),
            )
            last_run_obj = run
            if initial_status is None:
                initial_status = run.status
            last_result = {
                "test_case_id": tc_id,
                "run_id":       run.run_id,
                "status":       run.status,
                "duration_ms":  run.duration_ms,
                "attempt":      attempt + 1,
                "test_type":    test_type,
                "flaky_signal": flaky_signal,
                "retry_policy_applied": False,  # finalized after loop
                "retry_count": 0,
                "quarantine_recommended": False,
                "final_outcome_reason": None,
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
                "flaky_signal": flaky_signal,
                "retry_policy_applied": False,
                "retry_count": 0,
                "quarantine_recommended": False,
                "final_outcome_reason": None,
            }
        finally:
            sem.release()
            with _STATS_LOCK:
                _STATS["active_workers"] = max(0, _STATS["active_workers"] - 1)
                if test_type == "ui":
                    _STATS["running_ui"] = max(0, _STATS["running_ui"] - 1)
                else:
                    _STATS["running_api"] = max(0, _STATS["running_api"] - 1)

        final_status = last_result.get("status")
        if final_status == "pass":
            break

        # Retry decision:
        # - Always allow error retries (runner crash / timeout) up to retry_limit
        # - If suspected flaky: allow retries on both "fail" and "error" up to
        #   FLAKY_AUTO_RETRY_MAX, but never exceed overall max_attempts.
        if final_status == "error":
            if error_retries_used < retry_limit:
                error_retries_used += 1
                logger.info(
                    "orchestrator: retrying %s (attempt %d→%d) — status=error (error-retry %d/%d)",
                    tc_id, attempt + 1, attempt + 2, error_retries_used, retry_limit,
                )
                continue
            if suspected_flaky and flaky_retries_used < flaky_retry_limit:
                flaky_retries_used += 1
                logger.info(
                    "orchestrator: retrying %s (attempt %d→%d) — status=error (flaky-retry %d/%d)",
                    tc_id, attempt + 1, attempt + 2, flaky_retries_used, flaky_retry_limit,
                )
                continue

        if final_status == "fail" and suspected_flaky and flaky_retries_used < flaky_retry_limit:
            flaky_retries_used += 1
            logger.info(
                "orchestrator: retrying %s (attempt %d→%d) — status=fail (flaky-retry %d/%d)",
                tc_id, attempt + 1, attempt + 2, flaky_retries_used, flaky_retry_limit,
            )
            continue

        # No further retry: stop
        break

    # ── Final flaky policy outcome (after retries) ─────────────────────────
    final_status = last_result.get("status")
    retry_count_total = int(error_retries_used + flaky_retries_used)
    retry_policy_applied = bool(suspected_flaky and retry_count_total > 0)

    quarantine_recommended = bool(
        FLAKY_QUARANTINE_RECOMMENDATION_ENABLED
        and suspected_flaky
        and final_status in ("fail", "error")
        and retry_count_total > 0
    )

    if final_status == "pass":
        if retry_count_total > 0 and suspected_flaky:
            final_outcome_reason = "pass_after_flaky_retries"
        else:
            final_outcome_reason = "passed"
    else:
        if quarantine_recommended:
            final_outcome_reason = "quarantine_candidate_after_flaky_retries"
        else:
            final_outcome_reason = "failed"

    last_result.update(
        {
            "flaky_signal": flaky_signal,
            "retry_policy_applied": retry_policy_applied,
            "retry_count": retry_count_total,
            "quarantine_recommended": quarantine_recommended,
            "final_outcome_reason": final_outcome_reason,
        }
    )

    # Persist metadata updates in the final attempt run record.
    try:
        if (
            last_run_obj is not None
            and callable(getattr(last_run_obj, "model_dump", None))
            and isinstance(getattr(last_run_obj, "meta", None), dict)
        ):
            last_run_obj.meta.update(
                {
                    "flaky_signal": flaky_signal,
                    "flaky_score": flaky_score,
                    "flip_rate": flip_rate,
                    "retry_count": retry_count_total,
                    "retry_policy_applied": retry_policy_applied,
                    "quarantine_recommended": quarantine_recommended,
                    "final_outcome_reason": final_outcome_reason,
                }
            )
            # Upsert meta into SQLite
            from services.db.test_run_repository import test_run_repo
            test_run_repo.create_run(last_run_obj)
    except Exception:
        logger.debug("orchestrator: failed to upsert flaky metadata for %s", tc_id, exc_info=True)

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
    Daemon loop: fair-schedules jobs across projects, then dispatches each to its
    own thread. Global cap: _SEM (MAX_CONCURRENCY). Per-project cap:
    MAX_CONCURRENT_JOBS_PER_PROJECT (reserved until the job thread exits).
    """
    logger.info(
        "orchestrator: worker loop started (max_concurrency=%d, max_per_project=%d, max_workers=%d)",
        MAX_CONCURRENCY, MAX_CONCURRENT_JOBS_PER_PROJECT, EXECUTION_MAX_WORKERS,
    )
    while True:
        job = _fair_pop_next_job()
        if job is None:
            time.sleep(0.05)
            continue

        _SEM.acquire()

        def _task(j: OrchestratorJob = job) -> None:
            pid = _normalize_project_id(j.project_id)
            try:
                _run_job(j)
            finally:
                _SEM.release()
                with _SCHED_LOCK:
                    c = _RUNNING_BY_PROJECT.get(pid, 0) - 1
                    if c <= 0:
                        _RUNNING_BY_PROJECT.pop(pid, None)
                    else:
                        _RUNNING_BY_PROJECT[pid] = c

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

    with _SCHED_LOCK:
        orchestrator_pending_by_project = {
            k: len(v) for k, v in _PENDING_BY_PROJECT.items() if v
        }
        orchestrator_reserved_by_project = {
            k: c for k, c in _RUNNING_BY_PROJECT.items() if c
        }

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
        "orchestrator_pending_by_project": orchestrator_pending_by_project,
        "orchestrator_reserved_by_project": orchestrator_reserved_by_project,
        "max_concurrent_jobs_per_project": MAX_CONCURRENT_JOBS_PER_PROJECT,
    }


def get_project_queue_stats(project_id: str) -> Dict[str, Any]:
    """Live pending + reserved slots for one project (optional helper for APIs)."""
    pid = _normalize_project_id(project_id)
    with _SCHED_LOCK:
        pending_depth = len(_PENDING_BY_PROJECT.get(pid, ()))
        reserved = _RUNNING_BY_PROJECT.get(pid, 0)
    with _job_lock:
        running_jobs = sum(
            1
            for j in _JOB_STORE.values()
            if j.status == "running" and _normalize_project_id(getattr(j, "project_id", None)) == pid
        )
    return {
        "project_id": pid,
        "pending_queue_depth": pending_depth,
        "reserved_dispatch_slots": reserved,
        "running_jobs_in_memory": running_jobs,
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
        project_id       = _normalize_project_id(getattr(original, "project_id", None)),
        parent_job_id    = job_id,
        scheduling_notes = (
            f"Retry of {len(failed_ids)} failed test(s) from job {job_id[:8]}"
        ),
    )
    _save_job(new_job)
    _enqueue_pending(new_job)
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
        project_id: Optional[str] = None,
    ) -> OrchestratorJob:
        """
        Create and enqueue a single-test job.

        Returns the job immediately (status=queued).
        Raises ValueError if test_case_id is not in the catalog.
        """
        tc = catalog_service.get_test_case(test_case_id)
        if tc is None:
            raise ValueError(f"Test case '{test_case_id}' not found in catalog")

        pid = _normalize_project_id(project_id if project_id is not None else getattr(tc, "project_id", None))
        job = OrchestratorJob(
            job_type="single",
            test_case_ids=[test_case_id],
            total_count=1,
            environment=environment,
            project_id=pid,
        )
        _save_job(job)
        _enqueue_pending(job)
        logger.info(
            "orchestrator: enqueued single job %s — tc=%s env=%s project_id=%s",
            job.job_id, test_case_id, environment, job.project_id,
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
        context_json: Optional[str] = None,
        project_id: Optional[str] = None,
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

        derived_pid = (
            _normalize_project_id(project_id)
            if (project_id is not None and str(project_id).strip())
            else _derive_project_id_from_tests(resolved)
        )
        job = OrchestratorJob(
            job_type="suite",
            test_case_ids=resolved,
            total_count=len(resolved),
            environment=environment,
            context_json=context_json,
            project_id=derived_pid,
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
        _enqueue_pending(job)
        logger.info(
            "orchestrator: enqueued suite job %s — %d tests env=%s project_id=%s",
            job.job_id, len(resolved), environment, job.project_id,
        )
        return job

    def enqueue_batch(
        self,
        test_case_ids: List[str],
        *,
        environment: str = "default",
        project_id: Optional[str] = None,
    ) -> OrchestratorJob:
        """
        Enqueue a list of test IDs as a single 'batch' job.

        Unlike enqueue_suite, this does NO catalog filter resolution — callers
        are responsible for supplying valid, active test_case_ids.  Useful for
        direct programmatic dispatch (e.g. from PR analysis, coverage, CI hooks).

        Returns the job immediately (status=queued).
        """
        derived_pid = (
            _normalize_project_id(project_id)
            if (project_id is not None and str(project_id).strip())
            else _derive_project_id_from_tests(list(test_case_ids))
        )
        if not test_case_ids:
            job = OrchestratorJob(
                job_type="batch",
                test_case_ids=[],
                total_count=0,
                environment=environment,
                project_id=derived_pid,
            )
            job.status        = "failed"
            job.finished_at   = _now_utc()
            job.error_message = "enqueue_batch: empty test_case_ids list"
            _save_job(job)
            return job

        job = OrchestratorJob(
            job_type="batch",
            test_case_ids=list(test_case_ids),
            total_count=len(test_case_ids),
            environment=environment,
            project_id=derived_pid,
        )
        _save_job(job)
        _enqueue_pending(job)
        logger.info(
            "orchestrator: enqueued batch job %s — %d tests env=%s project_id=%s",
            job.job_id, len(test_case_ids), environment, job.project_id,
        )
        return job

    def enqueue_with_sharding(
        self,
        test_case_ids: List[str],
        *,
        shard_size: Optional[int] = None,
        environment: str = "default",
        project_id: Optional[str] = None,
    ) -> List[OrchestratorJob]:
        """
        Split a large list of test IDs into shard sub-jobs and enqueue them all.

        Shards run concurrently (each is an independent job).  Useful for suites
        larger than SHARD_SIZE to avoid blocking the worker loop on a single job.

        Returns the list of created OrchestratorJob objects (all status=queued).
        """
        size = shard_size or SHARD_SIZE
        if size < 1:
            size = SHARD_SIZE

        shards = [
            test_case_ids[i: i + size]
            for i in range(0, len(test_case_ids), size)
        ]

        derived_pid = (
            _normalize_project_id(project_id)
            if (project_id is not None and str(project_id).strip())
            else _derive_project_id_from_tests(list(test_case_ids))
        )
        jobs: List[OrchestratorJob] = []
        for idx, shard in enumerate(shards):
            job = OrchestratorJob(
                job_type="shard",
                test_case_ids=shard,
                total_count=len(shard),
                environment=environment,
                project_id=derived_pid,
                scheduling_notes=(
                    f"Shard {idx + 1}/{len(shards)} of {len(test_case_ids)} total tests"
                ),
            )
            _save_job(job)
            _enqueue_pending(job)
            jobs.append(job)

        logger.info(
            "orchestrator: enqueued %d shard jobs — %d total tests env=%s",
            len(jobs), len(test_case_ids), environment,
        )
        return jobs

    def get_job(self, job_id: str) -> Optional[OrchestratorJob]:
        return _get_job(job_id)

    def list_jobs(self, limit: int = 100, project_id: Optional[str] = None) -> List[OrchestratorJob]:
        return _list_jobs(limit=limit, project_id=project_id)


# Module-level singleton — routes import this
orchestrator_service = CatalogOrchestratorService()


# ── Test isolation helpers ────────────────────────────────────────────────────

def _reset_for_testing() -> None:
    """Wipe all in-memory and DB job state. For use in tests only."""
    with _job_lock:
        _JOB_STORE.clear()
        _JOB_ORDER.clear()
    with _SCHED_LOCK:
        for dq in _PENDING_BY_PROJECT.values():
            dq.clear()
        _PENDING_BY_PROJECT.clear()
        _RUNNING_BY_PROJECT.clear()
        global _RR_LAST_PROJECT
        _RR_LAST_PROJECT = None
    with _STATS_LOCK:
        for k in list(_STATS):
            _STATS[k] = 0
    try:
        orch_job_repo.clear_all()
    except Exception:
        pass
