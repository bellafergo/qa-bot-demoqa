# services/risk_engine_service.py
"""
Risk Engine v1 — explainable heuristic scoring (no AI).

Pure functions: inputs are pre-aggregated signals from existing Vanya stores.
"""
from __future__ import annotations

from datetime import datetime, timedelta, timezone
from typing import Dict, List, Optional, Tuple

from models.risk_engine_models import (
    ModuleRisk,
    RecommendedTest,
    RiskAssessment,
    RiskFactor,
    RiskLevel,
)

# ── Thresholds ────────────────────────────────────────────────────────────────

_LEVEL_LOW = 25.0
_LEVEL_MEDIUM = 50.0
_LEVEL_HIGH = 75.0

_SEVERITY_WEIGHT = {
    "critical": 6.0,
    "high": 4.0,
    "medium": 2.5,
    "low": 1.0,
    "info": 0.5,
}

_PRIORITY_SCORE = {"critical": 5, "high": 3, "medium": 1, "low": 0}

_RECENT_DAYS = 7


def risk_level_from_score(score: float) -> RiskLevel:
    if score >= _LEVEL_HIGH:
        return "CRITICAL"
    if score >= _LEVEL_MEDIUM:
        return "HIGH"
    if score >= _LEVEL_LOW:
        return "MEDIUM"
    return "LOW"


def _clip_score(value: float, cap: float) -> float:
    return round(min(cap, max(0.0, value)), 1)


def _parse_iso_age_days(iso: str) -> Optional[float]:
    if not iso:
        return None
    try:
        ts = iso.replace("Z", "+00:00")
        dt = datetime.fromisoformat(ts)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        delta = datetime.now(timezone.utc) - dt
        return max(0.0, delta.total_seconds() / 86400.0)
    except Exception:
        return None


def is_recent(iso: str, days: int = _RECENT_DAYS) -> bool:
    age = _parse_iso_age_days(iso)
    return age is not None and age <= days


def compute_module_risk(
    module: str,
    *,
    regression_count: int = 0,
    failure_count: int = 0,
    flaky_count: int = 0,
    incident_count: int = 0,
    incident_severity_sum: float = 0.0,
    pass_rate: Optional[float] = None,
    test_count: int = 0,
    recent_failure_count: int = 0,
) -> ModuleRisk:
    """Heuristic module risk from aggregated signals."""
    factors: List[RiskFactor] = []

    reg_pts = _clip_score(regression_count * 8.0, 20.0)
    if reg_pts:
        factors.append(RiskFactor(
            key="regressions",
            label="Regressions",
            score=reg_pts,
            max_score=20.0,
            detail=f"{regression_count} recurrent regression(s)",
        ))

    fail_pts = _clip_score(failure_count * 4.0, 18.0)
    if fail_pts:
        factors.append(RiskFactor(
            key="failures",
            label="Failure frequency",
            score=fail_pts,
            max_score=18.0,
            detail=f"{failure_count} failure history entries",
        ))

    flaky_pts = _clip_score(flaky_count * 5.0, 15.0)
    if flaky_pts:
        factors.append(RiskFactor(
            key="flaky",
            label="Flaky tests",
            score=flaky_pts,
            max_score=15.0,
            detail=f"{flaky_count} suspected flaky test(s)",
        ))

    inc_pts = _clip_score(incident_severity_sum, 20.0)
    if inc_pts:
        factors.append(RiskFactor(
            key="incidents",
            label="Incidents",
            score=inc_pts,
            max_score=20.0,
            detail=f"{incident_count} incident(s)",
        ))

    rate_pts = 0.0
    if pass_rate is not None and test_count > 0:
        rate_pts = _clip_score((100.0 - pass_rate) * 0.2, 20.0)
        if rate_pts:
            factors.append(RiskFactor(
                key="success_rate",
                label="Low success rate",
                score=rate_pts,
                max_score=20.0,
                detail=f"{pass_rate:.0f}% pass rate",
            ))

    coverage_pts = 0.0
    if test_count < 3 and (failure_count > 0 or regression_count > 0):
        coverage_pts = _clip_score(10.0 - test_count * 2.0, 10.0)
        factors.append(RiskFactor(
            key="coverage",
            label="Low test coverage",
            score=coverage_pts,
            max_score=10.0,
            detail=f"Only {test_count} catalog test(s) for this module",
        ))

    recency_pts = _clip_score(recent_failure_count * 3.0, 12.0)
    if recency_pts:
        factors.append(RiskFactor(
            key="recency",
            label="Recent problems",
            score=recency_pts,
            max_score=12.0,
            detail=f"{recent_failure_count} recent failure(s) (≤{_RECENT_DAYS}d)",
        ))

    total = round(min(100.0, sum(f.score for f in factors)), 1)
    return ModuleRisk(
        module=module,
        module_risk_score=total,
        module_risk_level=risk_level_from_score(total),
        factors=factors,
        regression_count=regression_count,
        flaky_count=flaky_count,
        incident_count=incident_count,
        pass_rate=pass_rate,
        test_count=test_count,
    )


def compute_project_risk(
    project_id: str,
    *,
    pass_rate: Optional[float] = None,
    run_fail_rate: float = 0.0,
    regressions: List[Dict],
    flaky_tests: List[Dict],
    failure_history: List[Dict],
    incidents: List[Dict],
    module_stats: Dict[str, Dict],
    related_tests: List[Dict],
) -> RiskAssessment:
    """
    Aggregate project-level risk from pre-fetched signals.

    ``module_stats`` keys are module names; values may include:
    test_count, pass_rate, regression_count, flaky_count, failure_count,
    incident_count, incident_severity_sum, recent_failure_count.
    """
    factors: List[RiskFactor] = []

    reg_count = len(regressions)
    reg_pts = _clip_score(reg_count * 4.0, 12.0)
    factors.append(RiskFactor(
        key="regressions",
        label="Regressions detected",
        score=reg_pts,
        max_score=12.0,
        detail=f"{reg_count} recurrent regression pattern(s)",
    ))

    fail_total = sum(int(f.get("count") or 1) for f in failure_history)
    fail_pts = _clip_score(min(12.0, len(failure_history) * 2.0 + fail_total * 0.4), 12.0)
    factors.append(RiskFactor(
        key="failure_frequency",
        label="Failure frequency",
        score=fail_pts,
        max_score=12.0,
        detail=f"{len(failure_history)} tests with failure history ({fail_total} total failures)",
    ))

    flaky_count = sum(1 for f in flaky_tests if f.get("suspected_flaky"))
    flaky_pts = _clip_score(flaky_count * 3.5, 10.0)
    factors.append(RiskFactor(
        key="flaky_tests",
        label="Flaky tests",
        score=flaky_pts,
        max_score=10.0,
        detail=f"{flaky_count} suspected flaky test(s)",
    ))

    inc_pts = 0.0
    recent_inc = 0
    for inc in incidents[:20]:
        sev = str(inc.get("severity") or "info").lower()
        inc_pts += _SEVERITY_WEIGHT.get(sev, 0.5)
        if is_recent(str(inc.get("created_at") or "")):
            recent_inc += 1
    inc_pts = _clip_score(inc_pts, 18.0)
    factors.append(RiskFactor(
        key="incidents",
        label="Incident severity",
        score=inc_pts,
        max_score=18.0,
        detail=f"{len(incidents)} incident(s), {recent_inc} in last {_RECENT_DAYS} days",
    ))

    effective_pass = pass_rate if pass_rate is not None else (100.0 - run_fail_rate * 100.0)
    rate_pts = _clip_score((100.0 - effective_pass) * 0.18, 18.0)
    factors.append(RiskFactor(
        key="success_rate",
        label="Historical success rate",
        score=rate_pts,
        max_score=18.0,
        detail=f"{effective_pass:.0f}% success rate",
    ))

    thin_modules = sum(
        1 for m, s in module_stats.items()
        if int(s.get("test_count") or 0) < 3
        and (int(s.get("failure_count") or 0) > 0 or int(s.get("regression_count") or 0) > 0)
    )
    cov_pts = _clip_score(thin_modules * 3.0, 10.0)
    if cov_pts:
        factors.append(RiskFactor(
            key="coverage_gap",
            label="Coverage gaps",
            score=cov_pts,
            max_score=10.0,
            detail=f"{thin_modules} module(s) with failures but few tests",
        ))

    recent_fail = sum(
        1 for f in failure_history
        if is_recent(str(f.get("last_failed_at") or ""))
    )
    recency_pts = _clip_score(recent_inc * 4.0 + recent_fail * 2.0, 20.0)
    factors.append(RiskFactor(
        key="recency",
        label="Recency of problems",
        score=recency_pts,
        max_score=20.0,
        detail=f"{recent_inc} recent incident(s), {recent_fail} recent failure(s)",
    ))

    risk_score = round(min(100.0, sum(f.score for f in factors)), 1)
    risk_level = risk_level_from_score(risk_score)

    from services.module_canonical import merge_module_stats_dicts, module_display_label

    merged_stats, label_map = merge_module_stats_dicts(
        module_stats,
        extra_labels=[str(r.get("module") or "") for r in related_tests if r.get("module")],
    )

    module_risks: List[ModuleRisk] = []
    for mod_key, stats in sorted(merged_stats.items(), key=lambda x: x[0].lower()):
        if not mod_key:
            continue
        display = module_display_label(mod_key, labels_by_key=label_map)
        module_risks.append(compute_module_risk(
            display,
            regression_count=int(stats.get("regression_count") or 0),
            failure_count=int(stats.get("failure_count") or 0),
            flaky_count=int(stats.get("flaky_count") or 0),
            incident_count=int(stats.get("incident_count") or 0),
            incident_severity_sum=float(stats.get("incident_severity_sum") or 0.0),
            pass_rate=stats.get("pass_rate"),
            test_count=int(stats.get("test_count") or 0),
            recent_failure_count=int(stats.get("recent_failure_count") or 0),
        ))
    module_risks.sort(key=lambda m: -m.module_risk_score)

    recommended = _build_recommended_tests(
        module_risks[:5],
        related_tests=related_tests,
        regressions=regressions,
        flaky_tests=flaky_tests,
    )

    explanation = _build_explanation(
        risk_score=risk_score,
        risk_level=risk_level,
        recent_inc=recent_inc,
        reg_count=reg_count,
        effective_pass=effective_pass,
        flaky_count=flaky_count,
        module_risks=module_risks,
    )

    return RiskAssessment(
        project_id=project_id,
        risk_score=risk_score,
        risk_level=risk_level,
        factors=factors,
        explanation=explanation,
        module_risks=module_risks,
        recommended_tests=recommended,
        pass_rate=round(effective_pass, 1) if effective_pass is not None else None,
        regression_count=reg_count,
        flaky_count=flaky_count,
        recent_incident_count=recent_inc,
    )


def _build_explanation(
    *,
    risk_score: float,
    risk_level: RiskLevel,
    recent_inc: int,
    reg_count: int,
    effective_pass: float,
    flaky_count: int,
    module_risks: List[ModuleRisk],
) -> List[str]:
    lines: List[str] = [f"Overall risk {risk_score:.0f}/100 ({risk_level})"]
    if recent_inc:
        lines.append(f"{recent_inc} recent incident(s) in the last {_RECENT_DAYS} days")
    if reg_count:
        lines.append(f"{reg_count} recurrent regression pattern(s)")
    if effective_pass < 100:
        lines.append(f"{effective_pass:.0f}% historical success rate")
    if flaky_count:
        lines.append(f"{flaky_count} suspected flaky test(s)")
    top = [m.module for m in module_risks[:3] if m.module_risk_score >= _LEVEL_LOW]
    if top:
        lines.append(f"Highest-risk modules: {', '.join(top)}")
    return lines


def _build_recommended_tests(
    top_modules: List[ModuleRisk],
    *,
    related_tests: List[Dict],
    regressions: List[Dict],
    flaky_tests: List[Dict],
    limit: int = 12,
) -> List[RecommendedTest]:
    """Recommend catalog tests for risky modules based on history and priority."""
    risky_modules = {m.module.lower() for m in top_modules if m.module}
    if not risky_modules:
        return []

    regression_ids = {str(r.get("test_case_id") or "") for r in regressions}
    flaky_ids = {str(f.get("test_case_id") or "") for f in flaky_tests if f.get("suspected_flaky")}

    scored: List[Tuple[float, RecommendedTest]] = []
    for tc in related_tests:
        tc_id = str(tc.get("test_case_id") or "")
        mod = str(tc.get("module") or "")
        if mod.lower() not in risky_modules:
            continue

        score = 0.0
        reasons: List[str] = []
        if tc_id in regression_ids:
            score += 8.0
            reasons.append("recurrent regression")
        if tc_id in flaky_ids:
            score += 6.0
            reasons.append("suspected flaky")
        status = str(tc.get("last_run_status") or "").lower()
        if status in ("fail", "error", "failed"):
            score += 5.0
            reasons.append("recent failure")
        prio = str(tc.get("priority") or "").lower()
        score += _PRIORITY_SCORE.get(prio, 0)
        if prio in ("critical", "high"):
            reasons.append(f"{prio} priority")

        if score <= 0:
            score = 2.0
            reasons.append("module at elevated risk")

        scored.append((score, RecommendedTest(
            test_case_id=tc_id,
            name=str(tc.get("name") or tc_id),
            module=mod,
            reason="; ".join(reasons) if reasons else "related to high-risk module",
            selection_score=round(score, 1),
        )))

    scored.sort(key=lambda x: (-x[0], x[1].module.lower(), x[1].test_case_id))
    seen: set = set()
    out: List[RecommendedTest] = []
    for _, rec in scored:
        if rec.test_case_id in seen:
            continue
        seen.add(rec.test_case_id)
        out.append(rec)
        if len(out) >= limit:
            break
    return out
