# services/executive_impact_service.py
"""
ROI-01B — Executive Impact Metrics (read-only historical comparison).

Compares current vs previous incident intelligence deterministically.
No AI, financial estimates, predictions, or new scoring.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import List, Optional

from models.executive_impact_models import ExecutiveImpactReport, ImpactDirection, ImpactMetric
from models.incident_models import ProjectIncidentInvestigationReport
from services.value_dashboard_service import (
    _count_critical_risks,
    _count_degraded_environments,
    _count_impacted_journeys,
    _load_incident_reports,
    _quality_snapshot,
    _release_status_blocked,
)

logger = logging.getLogger("vanya.executive_impact")


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _direction(
    current: int,
    previous: int,
    *,
    lower_is_better: bool = False,
    higher_is_better: bool = False,
) -> ImpactDirection:
    if current == previous:
        return "STABLE"
    if lower_is_better:
        return "IMPROVING" if current < previous else "DEGRADING"
    if higher_is_better:
        return "IMPROVING" if current > previous else "DEGRADING"
    return "UNKNOWN"


def _impact_metric(
    *,
    metric_id: str,
    title: str,
    current: int,
    previous: int,
    lower_is_better: bool = False,
    higher_is_better: bool = False,
    has_history: bool,
) -> ImpactMetric:
    if not has_history:
        return ImpactMetric(
            metric_id=metric_id,
            title=title,
            current_value=current,
            previous_value=previous,
            delta=0,
            direction="UNKNOWN",
        )
    return ImpactMetric(
        metric_id=metric_id,
        title=title,
        current_value=current,
        previous_value=previous,
        delta=current - previous,
        direction=_direction(
            current,
            previous,
            lower_is_better=lower_is_better,
            higher_is_better=higher_is_better,
        ),
    )


def _jira_blocker_count(report: Optional[ProjectIncidentInvestigationReport]) -> int:
    if report is None or report.jira_issue_intelligence is None:
        return 0
    intel = report.jira_issue_intelligence
    if not intel.connected:
        return 0
    return int(intel.blocker_count or 0)


def _recommendation_count(report: Optional[ProjectIncidentInvestigationReport]) -> int:
    if report is None:
        return 0
    return len(report.recommended_actions or [])


def _approval_count(report: Optional[ProjectIncidentInvestigationReport]) -> int:
    if report is None or report.approval_workflow is None:
        return 0
    return len(report.approval_workflow.requests or [])


def _validation_count(report: Optional[ProjectIncidentInvestigationReport]) -> int:
    if report is None or report.database_validation is None:
        return 0
    return len(report.database_validation.checks or [])


def _blocked_release_count(report: Optional[ProjectIncidentInvestigationReport]) -> int:
    if report is None:
        return 0
    return 1 if _release_status_blocked(report) else 0


def _quality_health_score(report: Optional[ProjectIncidentInvestigationReport]) -> int:
    score, _trend = _quality_snapshot(report)
    return score


def _build_highlights(metrics: List[ImpactMetric]) -> tuple[List[ImpactMetric], List[ImpactMetric]]:
    improvements = [
        m for m in metrics if m.direction == "IMPROVING" and m.delta != 0
    ]
    concerns = [
        m for m in metrics if m.direction == "DEGRADING" and m.delta != 0
    ]
    improvements.sort(key=lambda m: abs(m.delta), reverse=True)
    concerns.sort(key=lambda m: abs(m.delta), reverse=True)
    return improvements[:3], concerns[:3]


def _unknown_metric(metric_id: str, title: str) -> ImpactMetric:
    return ImpactMetric(
        metric_id=metric_id,
        title=title,
        current_value=0,
        previous_value=0,
        delta=0,
        direction="UNKNOWN",
    )


def build_executive_impact_report(project_id: str) -> ExecutiveImpactReport:
    """Build deterministic executive impact trends from incident history."""
    pid = (project_id or "").strip().lower()
    reports = _load_incident_reports(pid)
    has_history = len(reports) >= 2

    current = reports[0] if reports else None
    previous = reports[1] if len(reports) >= 2 else None

    if current is None:
        empty_metrics = [
            _unknown_metric("quality_health", "Quality Health"),
            _unknown_metric("degraded_environments", "Environment Stability"),
            _unknown_metric("impacted_journeys", "Journey Stability"),
            _unknown_metric("blocked_releases", "Blocked Releases"),
            _unknown_metric("critical_risks", "Critical Risks"),
            _unknown_metric("jira_blockers", "Jira Blockers"),
            _unknown_metric("recommendations", "Recommendations"),
            _unknown_metric("approvals", "Approvals"),
            _unknown_metric("validations", "Validations"),
        ]
        return ExecutiveImpactReport(
            generated_at=_utc_now_iso(),
            quality_health_trend=empty_metrics[0],
            degraded_environment_trend=empty_metrics[1],
            impacted_journey_trend=empty_metrics[2],
            blocked_release_trend=empty_metrics[3],
            critical_risk_trend=empty_metrics[4],
            jira_blocker_trend=empty_metrics[5],
            recommendation_trend=empty_metrics[6],
            approval_trend=empty_metrics[7],
            validation_trend=empty_metrics[8],
            has_sufficient_history=False,
        )

    quality_health_trend = _impact_metric(
        metric_id="quality_health",
        title="Quality Health",
        current=_quality_health_score(current),
        previous=_quality_health_score(previous),
        higher_is_better=True,
        has_history=has_history,
    )
    degraded_environment_trend = _impact_metric(
        metric_id="degraded_environments",
        title="Environment Stability",
        current=_count_degraded_environments(current),
        previous=_count_degraded_environments(previous) if previous else 0,
        lower_is_better=True,
        has_history=has_history,
    )
    impacted_journey_trend = _impact_metric(
        metric_id="impacted_journeys",
        title="Journey Stability",
        current=_count_impacted_journeys(current),
        previous=_count_impacted_journeys(previous) if previous else 0,
        lower_is_better=True,
        has_history=has_history,
    )
    blocked_release_trend = _impact_metric(
        metric_id="blocked_releases",
        title="Blocked Releases",
        current=_blocked_release_count(current),
        previous=_blocked_release_count(previous),
        lower_is_better=True,
        has_history=has_history,
    )
    critical_risk_trend = _impact_metric(
        metric_id="critical_risks",
        title="Critical Risks",
        current=_count_critical_risks(current),
        previous=_count_critical_risks(previous) if previous else 0,
        lower_is_better=True,
        has_history=has_history,
    )
    jira_blocker_trend = _impact_metric(
        metric_id="jira_blockers",
        title="Jira Blockers",
        current=_jira_blocker_count(current),
        previous=_jira_blocker_count(previous),
        lower_is_better=True,
        has_history=has_history,
    )
    recommendation_trend = _impact_metric(
        metric_id="recommendations",
        title="Recommendations",
        current=_recommendation_count(current),
        previous=_recommendation_count(previous),
        higher_is_better=True,
        has_history=has_history,
    )
    approval_trend = _impact_metric(
        metric_id="approvals",
        title="Approvals",
        current=_approval_count(current),
        previous=_approval_count(previous),
        higher_is_better=True,
        has_history=has_history,
    )
    validation_trend = _impact_metric(
        metric_id="validations",
        title="Validations",
        current=_validation_count(current),
        previous=_validation_count(previous),
        higher_is_better=True,
        has_history=has_history,
    )

    all_trends = [
        quality_health_trend,
        degraded_environment_trend,
        impacted_journey_trend,
        blocked_release_trend,
        critical_risk_trend,
        jira_blocker_trend,
        recommendation_trend,
        approval_trend,
        validation_trend,
    ]
    top_improvements, top_concerns = _build_highlights(all_trends) if has_history else ([], [])

    return ExecutiveImpactReport(
        generated_at=_utc_now_iso(),
        quality_health_trend=quality_health_trend,
        degraded_environment_trend=degraded_environment_trend,
        impacted_journey_trend=impacted_journey_trend,
        blocked_release_trend=blocked_release_trend,
        critical_risk_trend=critical_risk_trend,
        jira_blocker_trend=jira_blocker_trend,
        recommendation_trend=recommendation_trend,
        approval_trend=approval_trend,
        validation_trend=validation_trend,
        top_improvements=top_improvements,
        top_concerns=top_concerns,
        has_sufficient_history=has_history,
    )
