# services/test_recommendation.py
"""
Test Recommendation Engine for Vanya.

    recommend_tests(
        changed_modules: list[str],
        runs:            list[dict],
        catalog:         list[dict],
    ) -> list[dict]

Rules (MVP):
1. Tests belonging to a changed module → reason="module_changed"
2. If that module also has recent failures  → bump priority one step
3. Tests with ≥ FAILURE_THRESHOLD failures in runs → reason="historical_failure"
4. No duplicates; module_changed takes precedence when both rules apply
5. Results sorted: high → medium → low

Output per recommendation:
    {
        "test_id":  str,
        "reason":   "module_changed" | "historical_failure",
        "priority": "high" | "medium" | "low",
    }

Pure module — no I/O, no side effects.
"""
from __future__ import annotations

from collections import Counter
from typing import Any, Dict, List, Set

# ── Constants ──────────────────────────────────────────────────────────────────

_PRIORITY_ORDER: Dict[str, int] = {"high": 0, "medium": 1, "low": 2}
_BUMP:           Dict[str, str] = {"low": "medium", "medium": "high", "high": "high"}
_FAILED_STATUSES = frozenset({"failed", "fail", "error"})

FAILURE_THRESHOLD: int = 2   # min failures to qualify as "historical_failure"


# ── Public API ─────────────────────────────────────────────────────────────────

def recommend_tests(
    changed_modules: List[str],
    runs:            List[Dict[str, Any]],
    catalog:         List[Dict[str, Any]],
) -> List[Dict[str, Any]]:
    """
    Return a prioritised list of recommended tests.

    Only tests present in *catalog* are ever included in the output.
    """
    changed: Set[str] = {
        _s(m).lower() for m in (changed_modules or []) if _s(m).strip()
    }

    # ── index catalog: test_id → module ───────────────────────────────────────
    tid_to_module: Dict[str, str] = {}
    for test in (catalog or []):
        if not isinstance(test, dict):
            continue
        tid = _s(test.get("test_id"))
        mod = _s(test.get("module")).lower()
        if tid and mod and tid not in tid_to_module:
            tid_to_module[tid] = mod

    # ── analyse run history ────────────────────────────────────────────────────
    failure_counts: Counter     = Counter()
    failed_modules: Set[str]    = set()

    for run in (runs or []):
        if not isinstance(run, dict):
            continue
        if _s(run.get("status")).lower() not in _FAILED_STATUSES:
            continue
        tid = _s(run.get("test_case_id") or run.get("test_id"))
        if tid:
            failure_counts[tid] += 1
            # resolve module from run field, then fall back to catalog index
            mod = _s(run.get("module")).lower() or tid_to_module.get(tid, "")
            if mod:
                failed_modules.add(mod)

    frequent: Set[str] = {t for t, c in failure_counts.items() if c >= FAILURE_THRESHOLD}

    # ── build recommendations ──────────────────────────────────────────────────
    seen: Set[str]           = set()
    recs: List[Dict[str, Any]] = []

    for test in (catalog or []):
        if not isinstance(test, dict):
            continue
        tid = _s(test.get("test_id"))
        if not tid or tid in seen:
            continue

        mod          = _s(test.get("module")).lower()
        raw_priority = _s(test.get("priority")).lower()
        base_priority = raw_priority if raw_priority in _PRIORITY_ORDER else "medium"

        is_changed    = bool(mod and mod in changed)
        is_historical = tid in frequent

        if not is_changed and not is_historical:
            continue

        reason   = "module_changed" if is_changed else "historical_failure"
        priority = base_priority
        if is_changed and mod in failed_modules:
            priority = _BUMP.get(priority, priority)

        seen.add(tid)
        recs.append({"test_id": tid, "reason": reason, "priority": priority})

    recs.sort(key=lambda r: _PRIORITY_ORDER.get(r["priority"], 1))
    return recs


# ── Helpers ────────────────────────────────────────────────────────────────────

def _s(v: Any) -> str:
    try:
        return str(v) if v is not None else ""
    except Exception:
        return ""
