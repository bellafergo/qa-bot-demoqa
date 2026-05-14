# services/insights_metrics.py
"""
Deterministic blast-radius style metrics for failure clusters.
"""
from __future__ import annotations

from typing import List, Optional, Set

from models.failure_intelligence_models import BlastRadius


def _unique_suite_ids_from_runs(runs) -> Set[str]:
    keys: Set[str] = set()
    for run in runs or []:
        meta = getattr(run, "meta", None) or {}
        if not isinstance(meta, dict):
            continue
        sid = meta.get("suite_run_id") or meta.get("suite_id") or meta.get("job_id")
        if sid:
            keys.add(str(sid).strip())
    return keys


def compute_blast_radius(
    *,
    module: str,
    affected_test_case_ids: List[str],
    total_failures: int,
    root_cause_category: str,
    runs,
) -> BlastRadius:
    """
    Heuristic impact assessment from observable run/cluster facts only.
    """
    n_tests = len(affected_test_case_ids or [])
    mods = sorted({m for m in [module] if m and m != "unknown"}) or (
        [module] if module else []
    )

    suite_ids = _unique_suite_ids_from_runs(runs)
    suite_count: Optional[int] = len(suite_ids) if suite_ids else None

    cat = (root_cause_category or "unknown").lower()
    tf = int(total_failures or 0)

    # Severity
    if cat in ("auth_issue", "environment_issue") and tf >= 3:
        severity = "critical"
    elif cat == "api_failure" and tf >= 6:
        severity = "high"
    elif tf >= 10 or n_tests >= 8:
        severity = "high"
    elif tf >= 4 or n_tests >= 3:
        severity = "medium"
    else:
        severity = "low"

    # Impact scope (Spanish copy lives in i18n; keep canonical English tokens)
    if n_tests <= 1 and tf <= 2:
        scope = "isolated_failure"
    elif n_tests >= 2 and (module and module != "unknown"):
        scope = "localized_regression"
    elif n_tests >= 6 or tf >= 15:
        scope = "widespread_or_systemic"
    else:
        scope = "localized_regression" if n_tests >= 2 else "unknown"

    return BlastRadius(
        affected_modules=mods,
        affected_tests_count=n_tests,
        affected_suites_count=suite_count,
        estimated_severity=severity,
        impact_scope=scope,
    )
