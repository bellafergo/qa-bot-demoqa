# services/run_analytics_service.py
"""
Run Analytics Service
=====================

Computes aggregated metrics from the persistent run history:
  - Summary  (totals, pass/fail rate, avg duration, 7d / 30d windows)
  - Daily trend (last 7 days, one row per calendar day)
  - Top failing tests (ranked by failed_runs desc)

No LLM.  No external dependencies.  Pure Python aggregation over
run_history_service.list_runs(), which reads from qa_runs (Supabase) or SQLite.

Usage:
    from services.run_analytics_service import get_runs_dashboard
    data = get_runs_dashboard()
"""
from __future__ import annotations

import logging
from collections import defaultdict
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional

from services.run_history_service import run_history_service

logger = logging.getLogger("vanya.run_analytics")

# Statuses that count as "terminal" (completed execution)
_PASSED_STATUSES  = frozenset({"pass", "passed", "completed"})
_TERMINAL_STATUSES = frozenset({"pass", "passed", "completed", "fail", "failed", "error"})


def _is_passed(status: str) -> bool:
    return str(status or "").lower() in _PASSED_STATUSES


def _is_terminal(status: str) -> bool:
    return str(status or "").lower() in _TERMINAL_STATUSES


def _day_of(iso: Optional[str]) -> Optional[str]:
    """Return YYYY-MM-DD from an ISO-8601 string, or None."""
    if not iso:
        return None
    try:
        return iso[:10]   # "2024-01-15T10:30:00" → "2024-01-15"
    except Exception:
        return None


def get_runs_dashboard(project_id: Optional[str] = None) -> Dict[str, Any]:
    """
    Return a dict with three keys:
      summary       — RunAnalyticsSummary-compatible dict
      trend         — list[DailyTrendPoint-compatible dict], 7 items (last 7 days)
      top_failures  — list[TopFailingTest-compatible dict], up to 10 items
    """
    try:
        pid = (project_id or "").strip() or None
        runs = run_history_service.list_runs(limit=500, project_id=pid)
    except Exception:
        logger.exception("run_analytics: failed to fetch runs")
        runs = []

    terminal = [r for r in runs if _is_terminal(r.status)]

    # ── Summary ────────────────────────────────────────────────────────────────

    total  = len(terminal)
    passed = sum(1 for r in terminal if _is_passed(r.status))
    failed = total - passed
    pass_rate = round(passed / total * 100, 1) if total > 0 else 0.0

    durations = [
        r.duration_ms for r in terminal
        if isinstance(r.duration_ms, (int, float)) and r.duration_ms > 0
    ]
    avg_duration_ms = round(sum(durations) / len(durations)) if durations else 0

    now       = datetime.now(timezone.utc)
    cutoff_7  = (now - timedelta(days=7)).date()
    cutoff_30 = (now - timedelta(days=30)).date()

    runs_last_7  = 0
    runs_last_30 = 0
    for r in terminal:
        d = _day_of(r.started_at)
        if not d:
            continue
        try:
            rd = datetime.strptime(d, "%Y-%m-%d").date()
            if rd >= cutoff_7:
                runs_last_7  += 1
            if rd >= cutoff_30:
                runs_last_30 += 1
        except Exception:
            pass

    summary = {
        "total_runs":       total,
        "passed_runs":      passed,
        "failed_runs":      failed,
        "pass_rate":        pass_rate,
        "avg_duration_ms":  avg_duration_ms,
        "runs_last_7_days":  runs_last_7,
        "runs_last_30_days": runs_last_30,
    }

    # ── Daily trend (last 7 calendar days) ────────────────────────────────────

    # Build bucket keys for the last 7 days (oldest → newest)
    trend_buckets: Dict[str, Dict[str, Any]] = {}
    for i in range(6, -1, -1):
        day = (now - timedelta(days=i)).date().isoformat()
        trend_buckets[day] = {"date": day, "total": 0, "passed": 0, "failed": 0}

    for r in terminal:
        d = _day_of(r.started_at)
        if d and d in trend_buckets:
            trend_buckets[d]["total"] += 1
            if _is_passed(r.status):
                trend_buckets[d]["passed"] += 1
            else:
                trend_buckets[d]["failed"] += 1

    trend = []
    for bucket in trend_buckets.values():
        t = bucket["total"]
        bucket["pass_rate"] = round(bucket["passed"] / t * 100, 1) if t > 0 else None
        trend.append(bucket)

    # ── Top failing tests (by failed_runs desc) ────────────────────────────────

    by_test: Dict[str, Dict[str, Any]] = defaultdict(
        lambda: {"test_case_id": "", "test_name": "", "total_runs": 0, "failed_runs": 0}
    )

    for r in terminal:
        tid = getattr(r, "test_id", None) or getattr(r, "test_case_id", None) or ""
        if not tid:
            continue
        entry = by_test[tid]
        entry["test_case_id"] = tid
        # Keep most recent non-empty test_name
        if not entry["test_name"] and r.test_name:
            entry["test_name"] = r.test_name
        entry["total_runs"] += 1
        if not _is_passed(r.status):
            entry["failed_runs"] += 1

    top_failures = sorted(
        [v for v in by_test.values() if v["failed_runs"] > 0],
        key=lambda x: x["failed_runs"],
        reverse=True,
    )[:10]

    for tf in top_failures:
        tr = tf["total_runs"]
        tf["pass_rate"] = round((1 - tf["failed_runs"] / tr) * 100, 1) if tr > 0 else 0.0
        if not tf["test_name"]:
            tf["test_name"] = tf["test_case_id"]

    return {
        "summary":      summary,
        "trend":        trend,
        "top_failures": top_failures,
    }
