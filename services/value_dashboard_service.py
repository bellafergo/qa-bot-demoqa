# services/value_dashboard_service.py
"""
ROI-01A — Value Dashboard (read-only aggregation).

Deterministic counts from stored incident intelligence and report metadata only.
No AI, financial estimates, predictions, or new scoring engines.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import List, Optional

from models.incident_models import ProjectIncidentInvestigationReport
from models.value_dashboard_models import ValueDashboard, ValueMetric

logger = logging.getLogger("vanya.value_dashboard")

_DEGRADED_ENV_STATUSES = frozenset({"DEGRADED", "BROKEN", "WARNING", "CRITICAL"})
_IMPACTED_JOURNEY_STATUSES = frozenset({"BROKEN", "DEGRADED", "FAILED", "INCONSISTENT"})
_REPORT_LIMIT = 100


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _load_incident_reports(project_id: str) -> List[ProjectIncidentInvestigationReport]:
    from services.db.incident_report_repository import incident_report_repo

    pid = (project_id or "").strip().lower()
    if not pid:
        return []

    rows = incident_report_repo.list_full_reports(project_id=pid, limit=_REPORT_LIMIT)
    reports: List[ProjectIncidentInvestigationReport] = []
    for full in rows:
        if not isinstance(full, dict):
            continue
        try:
            reports.append(ProjectIncidentInvestigationReport.model_validate(full))
        except Exception as exc:
            logger.debug(
                "value_dashboard: skip invalid report project_id=%s report_id=%s: %s",
                pid,
                full.get("id"),
                exc,
            )
    return reports


def _count_critical_risks(report: ProjectIncidentInvestigationReport) -> int:
    count = 0
    if report.contract_risk_assessment is not None:
        count += sum(
            1
            for assessment in report.contract_risk_assessment.assessments or []
            if str(assessment.overall_risk_level or "").upper() == "CRITICAL"
        )
    if report.deployment_risk_assessment is not None:
        if str(report.deployment_risk_assessment.risk_level or "").lower() == "critical":
            count += 1
    if report.executive_quality_report is not None:
        if str(report.executive_quality_report.overall_risk_level or "").upper() == "CRITICAL":
            count += 1
    return count


def _count_degradation_events(report: ProjectIncidentInvestigationReport) -> int:
    degradation = report.early_degradation
    if degradation is None and report.release_readiness is not None:
        degradation = report.release_readiness.early_degradation
    if degradation is None:
        return 0
    return sum(len(assessment.signals or []) for assessment in degradation.assessments or [])


def _count_degraded_environments(report: ProjectIncidentInvestigationReport) -> int:
    multi = report.multi_environment
    if multi is None and report.release_readiness is not None:
        multi = report.release_readiness.multi_environment
    if multi is None:
        return 0
    return sum(
        1
        for env in multi.environments or []
        if str(env.status or "").upper() in _DEGRADED_ENV_STATUSES
    )


def _count_impacted_journeys(report: ProjectIncidentInvestigationReport) -> int:
    djv = report.data_journey_validation
    if djv is None and report.release_readiness is not None:
        djv = report.release_readiness.data_journey_validation
    if djv is None:
        return 0
    return sum(
        1
        for result in djv.results or []
        if str(result.status or "").upper() in _IMPACTED_JOURNEY_STATUSES
    )


def _quality_snapshot(report: Optional[ProjectIncidentInvestigationReport]) -> tuple[int, str]:
    if report is None:
        return 0, "UNKNOWN"

    if report.quality_health is not None and report.quality_health.overall_score is not None:
        score = int(report.quality_health.overall_score)
    elif report.executive_quality_report is not None:
        score = int(report.executive_quality_report.overall_quality_score or 0)
    elif report.release_readiness is not None and report.release_readiness.quality_health is not None:
        score = int(report.release_readiness.quality_health.overall_score or 0)
    else:
        score = 0

    trend = "UNKNOWN"
    if report.quality_trends is not None and report.quality_trends.overall_trend:
        trend = str(report.quality_trends.overall_trend).upper()
    elif report.quality_health is not None and report.quality_health.trend:
        trend = str(report.quality_health.trend).upper()
    elif report.executive_quality_report is not None and report.executive_quality_report.quality_trend:
        trend = str(report.executive_quality_report.quality_trend).upper()
    elif report.release_readiness is not None and report.release_readiness.quality_trends is not None:
        trend = str(report.release_readiness.quality_trends.overall_trend or "UNKNOWN").upper()

    return score, trend


def _release_status_blocked(report: ProjectIncidentInvestigationReport) -> bool:
    if report.release_readiness is not None:
        return str(report.release_readiness.overall_status or "").upper() == "BLOCKED"
    return False


def _scheduled_reports_count(project_id: str) -> int:
    try:
        from services.scheduled_report_service import build_executive_report_center

        center = build_executive_report_center(project_id)
        if center is None:
            return 0
        return sum(1 for schedule in center.schedules if schedule.enabled)
    except Exception as exc:
        logger.debug("value_dashboard: scheduled report count failed project_id=%s: %s", project_id, exc)
        return 0


def _build_top_value_metrics(
    *,
    incidents_investigated: int,
    executive_reports_generated: int,
    blocked_releases: int,
    critical_risks_identified: int,
    jira_blockers_detected: int,
    recommendations_generated: int,
    validations_planned: int,
    correlated_jira_issues: int,
) -> List[ValueMetric]:
    candidates = [
        (
            "incidents_investigated",
            "Incidents Investigated",
            incidents_investigated,
            "Incident investigations completed by Vanya.",
        ),
        (
            "executive_reports_generated",
            "Executive Reports Generated",
            executive_reports_generated,
            "Executive quality reports produced from investigations.",
        ),
        (
            "blocked_releases",
            "Blocked Releases",
            blocked_releases,
            "Release readiness assessments flagged as blocked.",
        ),
        (
            "critical_risks_identified",
            "Critical Risks Identified",
            critical_risks_identified,
            "Critical risk entries surfaced across intelligence.",
        ),
        (
            "jira_blockers_detected",
            "Jira Blockers Detected",
            jira_blockers_detected,
            "Correlated Jira blocker issues identified.",
        ),
        (
            "recommendations_generated",
            "Recommendations Generated",
            recommendations_generated,
            "Recommended actions produced for remediation.",
        ),
        (
            "validations_planned",
            "Validations Planned",
            validations_planned,
            "Database validation checks planned for execution.",
        ),
        (
            "correlated_jira_issues",
            "Correlated Jira Issues",
            correlated_jira_issues,
            "Jira issues correlated with incident intelligence.",
        ),
    ]
    ranked = sorted(
        (item for item in candidates if item[2] > 0),
        key=lambda item: item[2],
        reverse=True,
    )
    return [
        ValueMetric(
            metric_id=metric_id,
            title=title,
            value=value,
            description=description,
        )
        for metric_id, title, value, description in ranked[:6]
    ]


def build_value_dashboard(project_id: str) -> ValueDashboard:
    """Aggregate deterministic value metrics from existing project intelligence."""
    pid = (project_id or "").strip().lower()
    reports = _load_incident_reports(pid)
    latest = reports[0] if reports else None

    incidents_investigated = len(reports)
    executive_reports_generated = sum(1 for r in reports if r.executive_quality_report is not None)
    release_readiness_reports = sum(
        1 for r in reports if r.release_readiness is not None or r.executive_quality_report is not None
    )
    scheduled_reports_generated = _scheduled_reports_count(pid)

    blocked_releases = sum(1 for r in reports if _release_status_blocked(r))
    if blocked_releases == 0 and latest is not None:
        try:
            from services.release_readiness_service import build_release_readiness_view

            current = build_release_readiness_view(project_id=pid, incident_report=latest)
            if str(current.overall_status or "").upper() == "BLOCKED":
                blocked_releases = 1
        except Exception:
            pass

    critical_risks_identified = sum(_count_critical_risks(r) for r in reports)
    jira_blockers_detected = sum(
        int(r.jira_issue_intelligence.blocker_count)
        for r in reports
        if r.jira_issue_intelligence is not None and r.jira_issue_intelligence.connected
    )
    degradation_events_detected = sum(_count_degradation_events(r) for r in reports)

    quality_health_score, quality_trend = _quality_snapshot(latest)
    degraded_environments = _count_degraded_environments(latest) if latest else 0
    impacted_journeys = _count_impacted_journeys(latest) if latest else 0

    recommendations_generated = sum(len(r.recommended_actions or []) for r in reports)
    approvals_requested = sum(
        len(r.approval_workflow.requests or [])
        for r in reports
        if r.approval_workflow is not None
    )
    validations_planned = sum(
        len(r.database_validation.checks or [])
        for r in reports
        if r.database_validation is not None
    )
    correlated_jira_issues = sum(
        int(r.jira_issue_intelligence.correlated_issues)
        for r in reports
        if r.jira_issue_intelligence is not None and r.jira_issue_intelligence.connected
    )

    top_value_metrics = _build_top_value_metrics(
        incidents_investigated=incidents_investigated,
        executive_reports_generated=executive_reports_generated,
        blocked_releases=blocked_releases,
        critical_risks_identified=critical_risks_identified,
        jira_blockers_detected=jira_blockers_detected,
        recommendations_generated=recommendations_generated,
        validations_planned=validations_planned,
        correlated_jira_issues=correlated_jira_issues,
    )

    return ValueDashboard(
        generated_at=_utc_now_iso(),
        incidents_investigated=incidents_investigated,
        executive_reports_generated=executive_reports_generated,
        release_readiness_reports=release_readiness_reports,
        scheduled_reports_generated=scheduled_reports_generated,
        blocked_releases=blocked_releases,
        critical_risks_identified=critical_risks_identified,
        jira_blockers_detected=jira_blockers_detected,
        degradation_events_detected=degradation_events_detected,
        quality_health_score=quality_health_score,
        quality_trend=quality_trend,
        degraded_environments=degraded_environments,
        impacted_journeys=impacted_journeys,
        recommendations_generated=recommendations_generated,
        approvals_requested=approvals_requested,
        validations_planned=validations_planned,
        correlated_jira_issues=correlated_jira_issues,
        top_value_metrics=top_value_metrics,
    )
