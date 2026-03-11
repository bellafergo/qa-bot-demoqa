# services/suite_aggregator.py
"""
Aggregates a list of JobResults into a SuiteRunResult.
Pure function — no side effects, no I/O, safe to unit-test.
"""
from __future__ import annotations

import time
from collections import Counter
from typing import List, Optional

from core.suite_models import (
    BatchRun,
    JobResult,
    SuiteRunResult,
    SuiteStatus,
    TestStatus,
    TestSuite,
)


def aggregate_results(
    job_results: List[JobResult],
    suite: TestSuite,
    batch: BatchRun,
    started_at: Optional[int] = None,
    finished_at: Optional[int] = None,
) -> SuiteRunResult:
    """
    Compute a SuiteRunResult from a completed list of JobResults.

    Args:
        job_results:  list of completed JobResult objects
        suite:        the TestSuite that was run
        batch:        the BatchRun metadata
        started_at:   epoch-ms when the run started (defaults to now)
        finished_at:  epoch-ms when the run finished (defaults to now)
    """
    now_ms = int(time.time() * 1000)
    started_at  = started_at  or now_ms
    finished_at = finished_at or now_ms

    total   = len(job_results)
    passed  = sum(1 for r in job_results if r.status == TestStatus.passed)
    failed  = sum(1 for r in job_results if r.status == TestStatus.failed)
    flaky   = sum(1 for r in job_results if r.status == TestStatus.flaky)
    skipped = sum(1 for r in job_results if r.status == TestStatus.skipped)
    errors  = sum(1 for r in job_results if r.status == TestStatus.error)

    effective_failures = failed + errors

    # Pass rate: passed + flaky count as "OK"
    pass_rate = round((passed + flaky) / total, 4) if total > 0 else 0.0

    # Average duration (skipped jobs have 0 ms — excluded from avg)
    timed_results = [r for r in job_results if r.status != TestStatus.skipped]
    avg_duration_ms = (
        round(sum(r.duration_ms for r in timed_results) / len(timed_results), 1)
        if timed_results else 0.0
    )

    # Top failure reasons (up to 5 most common)
    reason_counter: Counter = Counter()
    for r in job_results:
        if r.failure_reason and r.status in (TestStatus.failed, TestStatus.error):
            # Truncate long reasons for display
            short = (r.failure_reason or "")[:120].strip()
            if short:
                reason_counter[short] += 1
    top_failure_reasons = [reason for reason, _ in reason_counter.most_common(5)]

    # Names of failed cases for quick inspection
    failed_cases = [
        r.case_name
        for r in job_results
        if r.status in (TestStatus.failed, TestStatus.error)
    ]

    # Suite-level status
    if total == 0:
        status = SuiteStatus.error
    elif effective_failures == 0 and skipped == 0:
        status = SuiteStatus.passed
    elif passed == 0 and flaky == 0:
        status = SuiteStatus.failed
    else:
        status = SuiteStatus.partial

    return SuiteRunResult(
        suite_id=suite.id,
        suite_name=suite.name,
        batch_id=batch.id,
        env_name=batch.env_name,
        status=status,
        total=total,
        passed=passed,
        failed=failed,
        flaky=flaky,
        skipped=skipped,
        pass_rate=pass_rate,
        avg_duration_ms=avg_duration_ms,
        top_failure_reasons=top_failure_reasons,
        failed_cases=failed_cases,
        job_results=job_results,
        started_at=started_at,
        finished_at=finished_at,
        duration_ms=finished_at - started_at,
    )
