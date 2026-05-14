# services/insights_timeline_service.py
"""
Failure timeline from canonical run history (Supabase qa_runs when enabled, else SQLite).

No deploy metadata is invented; optional DeployContext fields stay null until wired.
"""
from __future__ import annotations

import logging
from datetime import datetime, timedelta, timezone
from typing import List, Optional

from models.insights_models import (
    DeployContext,
    FailureTimelineInsight,
    TimelineBucket,
)
from services.run_history_service import run_history_service

logger = logging.getLogger("vanya.insights_timeline")

_PASSED = frozenset({"pass", "passed", "completed"})
_TERMINAL = frozenset({"pass", "passed", "completed", "fail", "failed", "error"})


def _is_passed(status: str) -> bool:
    return str(status or "").lower() in _PASSED


def _is_terminal(status: str) -> bool:
    return str(status or "").lower() in _TERMINAL


def _day_of(iso: Optional[str]) -> Optional[str]:
    if not iso:
        return None
    try:
        return iso[:10]
    except Exception:
        return None


def get_failure_timeline(
    *,
    project_id: Optional[str] = None,
    days: int = 14,
    run_limit: int = 500,
) -> FailureTimelineInsight:
    """
    Bucket terminal runs by calendar day (UTC date of started_at).
    """
    pid = (project_id or "").strip() or None
    days = max(7, min(int(days), 30))
    run_limit = max(1, min(int(run_limit), 500))

    try:
        runs = run_history_service.list_runs(limit=run_limit, project_id=pid)
    except Exception:
        logger.exception("insights_timeline: list_runs failed")
        runs = []

    terminal = [r for r in runs if _is_terminal(str(r.status or ""))]

    now = datetime.now(timezone.utc)
    bucket_keys: List[str] = []
    for i in range(days - 1, -1, -1):
        bucket_keys.append((now - timedelta(days=i)).date().isoformat())

    raw: dict = {d: {"total": 0, "passed": 0, "failed": 0} for d in bucket_keys}

    for r in terminal:
        d = _day_of(r.started_at)
        if not d or d not in raw:
            continue
        raw[d]["total"] += 1
        if _is_passed(str(r.status or "")):
            raw[d]["passed"] += 1
        else:
            raw[d]["failed"] += 1

    buckets: List[TimelineBucket] = []
    first_fail_day: Optional[str] = None
    max_fail = -1
    highest_fail_day: Optional[str] = None

    for d in bucket_keys:
        b = raw[d]
        t = b["total"]
        pr = round(b["passed"] / t * 100, 1) if t > 0 else None
        buckets.append(
            TimelineBucket(
                date=d,
                total_runs=t,
                failed_runs=b["failed"],
                passed_runs=b["passed"],
                pass_rate=pr,
            )
        )
        if b["failed"] > 0:
            if first_fail_day is None:
                first_fail_day = d
            if b["failed"] > max_fail:
                max_fail = b["failed"]
                highest_fail_day = d

    half = max(len(buckets) // 2, 1)
    first_half = buckets[:half]
    second_half = buckets[half:]

    def _avg_pass_rate(bs: List[TimelineBucket]) -> Optional[float]:
        vals = [x.pass_rate for x in bs if x.pass_rate is not None and x.total_runs > 0]
        if not vals:
            return None
        return round(sum(vals) / len(vals), 1)

    pr_first = _avg_pass_rate(first_half)
    pr_second = _avg_pass_rate(second_half)
    delta = (
        round(pr_second - pr_first, 1)
        if pr_first is not None and pr_second is not None
        else None
    )

    note = (
        "Deploy/commit correlation is not available in run metadata yet; "
        "timeline uses execution dates only."
    )

    return FailureTimelineInsight(
        buckets=buckets,
        first_failure_day=first_fail_day,
        highest_failure_day=highest_fail_day,
        pass_rate_first_half=pr_first,
        pass_rate_second_half=pr_second,
        pass_rate_delta_window=delta,
        cluster_recurrence_note="insufficient_evidence",
        deploy_context=DeployContext(),
        runs_analyzed=len(terminal),
        note=note,
    )
