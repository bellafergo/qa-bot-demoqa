# services/test_orchestrator.py
"""
Test Suite Orchestrator.

Responsibilities:
  - Split a TestSuite into ExecutionJobs
  - Run jobs concurrently (ThreadPoolExecutor, max_concurrency)
  - Apply RetryPolicy per job
  - Track stop-on-critical-failure threshold
  - Aggregate results into SuiteRunResult

The generic execute_test() path is used unchanged per job.
Evidence/report/store behavior is unaffected.
"""
from __future__ import annotations

import logging
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed, Future
from typing import Any, Callable, Dict, List, Optional

from runners.generic_steps import execute_test

from core.exec_env import ExecEnv, DEFAULT_ENV
from core.suite_models import (
    BatchRun,
    ExecutionJob,
    JobResult,
    SuiteRunResult,
    TestStatus,
    TestSuite,
)
from services.retry_policy import RetryPolicy, classify_runner_failure
from services.suite_aggregator import aggregate_results

logger = logging.getLogger("vanya.orchestrator")


# ── Job building ──────────────────────────────────────────────────────────────

def _build_jobs(suite: TestSuite, env: ExecEnv) -> List[ExecutionJob]:
    """Create one ExecutionJob per TestCase, merging env defaults."""
    jobs = []
    for case in suite.cases:
        jobs.append(ExecutionJob(
            suite_id=suite.id,
            case_id=case.id,
            case_name=case.name,
            steps=case.steps,
            base_url=case.base_url or env.base_url,
            expected=case.expected,
            tags=list(set(case.tags + suite.tags)),
            timeout_s=case.timeout_s or env.timeout_s,
            headless=case.headless if not env.headless else env.headless,
            attempt=1,
        ))
    return jobs


# ── Single-job execution with retry ──────────────────────────────────────────

def _raw_to_job_result(
    job: ExecutionJob,
    raw: Dict[str, Any],
    attempt: int,
    status_override: Optional[TestStatus] = None,
) -> JobResult:
    """Convert a raw execute_test() result dict → JobResult."""
    raw_status = str(raw.get("status") or "failed").lower()

    if status_override is not None:
        status = status_override
    elif raw_status == "passed":
        status = TestStatus.passed
    elif raw_status in ("failed", "fail"):
        status = TestStatus.failed
    else:
        status = TestStatus.error

    failure_type = classify_runner_failure(raw) if status != TestStatus.passed else None

    return JobResult(
        job_id=job.id,
        case_id=job.case_id,
        case_name=job.case_name,
        status=status,
        attempt=attempt,
        duration_ms=int(raw.get("duration_ms") or 0),
        evidence_id=str(raw.get("evidence_id") or "").strip() or None,
        evidence_url=str(raw.get("evidence_url") or "").strip() or None,
        report_url=str(raw.get("report_url") or "").strip() or None,
        failure_reason=(str(raw.get("reason") or "")).strip() or None,
        failure_type=failure_type,
        steps=raw.get("steps") or [],
        logs=raw.get("logs") or [],
    )


def _skipped_result(job: ExecutionJob) -> JobResult:
    return JobResult(
        job_id=job.id,
        case_id=job.case_id,
        case_name=job.case_name,
        status=TestStatus.skipped,
        attempt=0,
        duration_ms=0,
        failure_reason="Skipped: suite stopped due to critical failure threshold",
        failure_type="terminal",
    )


def _execute_with_retry(
    job: ExecutionJob,
    env: ExecEnv,
    policy: RetryPolicy,
) -> JobResult:
    """
    Execute a job, retrying according to the policy.
    Returns the final JobResult (passed, failed, flaky, or error).
    """
    last_raw: Dict[str, Any] = {}
    last_result: Optional[JobResult] = None

    for attempt in range(1, policy.max_retries + 2):  # +2: first attempt is 1
        logger.debug(
            "[ORCH] case=%r attempt=%d/%d",
            job.case_name, attempt, policy.max_retries + 1,
        )

        try:
            raw = execute_test(
                steps=job.steps,
                base_url=job.base_url,
                headless=env.headless,
                viewport=env.viewport,
                timeout_s=job.timeout_s or env.timeout_s or None,
                expected=job.expected,
                correlation_id=str(job.id),
            )
        except Exception as e:
            raw = {
                "ok": False,
                "status": "error",
                "reason": f"{type(e).__name__}: {e}",
                "steps": [],
                "logs": [f"[ORCH] runner exception: {type(e).__name__}: {e}"],
                "duration_ms": 0,
            }

        last_raw = raw

        if raw.get("ok") or raw.get("status") == "passed":
            # Passed — if this is a retry, mark as flaky
            status = TestStatus.flaky if attempt > 1 else TestStatus.passed
            last_result = _raw_to_job_result(job, raw, attempt, status_override=status)
            logger.info(
                "[ORCH] case=%r %s (attempt %d)",
                job.case_name, status.value, attempt,
            )
            return last_result

        # Failed — decide whether to retry
        if not policy.should_retry(raw, attempt):
            break

        logger.info(
            "[ORCH] case=%r attempt %d failed (transient), retrying…",
            job.case_name, attempt,
        )
        time.sleep(0.5 * attempt)  # brief back-off: 0.5s, 1.0s, …

    # All attempts exhausted
    last_result = _raw_to_job_result(job, last_raw, attempt=attempt)
    logger.info("[ORCH] case=%r FAILED after %d attempt(s)", job.case_name, attempt)
    return last_result


# ── Public API ────────────────────────────────────────────────────────────────

def run_suite(
    suite: TestSuite,
    env: Optional[ExecEnv] = None,
    policy: Optional[RetryPolicy] = None,
    on_job_complete: Optional[Callable[[JobResult], None]] = None,
) -> SuiteRunResult:
    """
    Execute a TestSuite and return an aggregated SuiteRunResult.

    Args:
        suite:            The TestSuite to run.
        env:              Execution environment (base_url, headless, etc.).
                          Defaults to ExecEnv() with no overrides.
        policy:           Retry policy. Defaults to RetryPolicy.default().
        on_job_complete:  Optional callback invoked after each job finishes
                          (useful for streaming progress to the UI).

    Returns:
        SuiteRunResult with per-job results and aggregated statistics.
    """
    env    = env    or DEFAULT_ENV
    policy = policy or RetryPolicy.default()

    started_at = int(time.time() * 1000)
    jobs       = _build_jobs(suite, env)

    batch = BatchRun(
        suite_id=suite.id,
        suite_name=suite.name,
        jobs=jobs,
        env_name=env.name,
        created_at=started_at,
    )

    logger.info(
        "[ORCH] Starting suite=%r  cases=%d  concurrency=%d  env=%r",
        suite.name, len(jobs), suite.max_concurrency, env.name,
    )

    completed_results: List[JobResult] = []
    stop_event = threading.Event()
    lock = threading.Lock()

    def _run_job(job: ExecutionJob) -> JobResult:
        if stop_event.is_set():
            result = _skipped_result(job)
        else:
            result = _execute_with_retry(job, env, policy)

        with lock:
            completed_results.append(result)

            # Check critical failure threshold
            if (
                suite.stop_on_critical_failure
                and not stop_event.is_set()
            ):
                n_done    = len(completed_results)
                n_failed  = sum(
                    1 for r in completed_results
                    if r.status in (TestStatus.failed, TestStatus.error)
                )
                if n_done > 0 and (n_failed / n_done) > suite.critical_failure_threshold:
                    logger.warning(
                        "[ORCH] Critical failure threshold exceeded (%.0f%% failed). "
                        "Stopping remaining jobs.",
                        100 * n_failed / n_done,
                    )
                    stop_event.set()

        if on_job_complete:
            try:
                on_job_complete(result)
            except Exception:
                pass  # callback errors must not break the run

        return result

    # Run jobs with bounded concurrency
    with ThreadPoolExecutor(max_workers=suite.max_concurrency) as executor:
        futures: List[Future] = [executor.submit(_run_job, job) for job in jobs]
        # Collect in completion order (for logging) but preserve job order in results
        for future in as_completed(futures):
            try:
                future.result()
            except Exception as e:
                logger.error("[ORCH] Unexpected future error: %s", e)

    # Restore original job order (completed_results arrives in completion order)
    job_id_to_result = {r.job_id: r for r in completed_results}
    ordered_results  = [job_id_to_result[j.id] for j in jobs if j.id in job_id_to_result]

    finished_at = int(time.time() * 1000)

    result = aggregate_results(
        job_results=ordered_results,
        suite=suite,
        batch=batch,
        started_at=started_at,
        finished_at=finished_at,
    )

    logger.info(
        "[ORCH] Suite=%r done: %s  passed=%d  failed=%d  flaky=%d  skipped=%d  %.1fs",
        suite.name, result.status.value,
        result.passed, result.failed, result.flaky, result.skipped,
        result.duration_ms / 1000,
    )

    return result
