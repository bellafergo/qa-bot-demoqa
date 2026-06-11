# services/outcome_tracking_service.py
"""
ROI-02A — Outcome Tracking (read-only aggregation).

Counts measurable outcomes from existing platform data only.
No AI, new risk engines, scoring systems, or workflow automation.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import List

from models.incident_models import ProjectIncidentInvestigationReport
from models.outcome_tracking_models import OutcomeMetric, OutcomeTrackingReport
from services.value_dashboard_service import (
    _load_incident_reports,
    _release_status_blocked,
)

logger = logging.getLogger("vanya.outcome_tracking")

_REPORT_LIMIT = 100
_AUDIT_LIMIT = 500


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _count_blockers_identified(reports: List[ProjectIncidentInvestigationReport]) -> int:
    return sum(
        int(r.jira_issue_intelligence.blocker_count)
        for r in reports
        if r.jira_issue_intelligence is not None and r.jira_issue_intelligence.connected
    )


def _count_releases_blocked(project_id: str, reports: List[ProjectIncidentInvestigationReport]) -> int:
    blocked = sum(1 for r in reports if _release_status_blocked(r))
    if blocked > 0:
        return blocked

    latest = reports[0] if reports else None
    if latest is None:
        return 0

    try:
        from services.release_readiness_service import build_release_readiness_view

        current = build_release_readiness_view(project_id=project_id, incident_report=latest)
        if str(current.overall_status or "").upper() == "BLOCKED":
            return 1
    except Exception as exc:
        logger.debug("outcome_tracking: release readiness fallback failed project_id=%s: %s", project_id, exc)
    return 0


def _count_incident_recommendations(reports: List[ProjectIncidentInvestigationReport]) -> int:
    return sum(len(r.recommended_actions or []) for r in reports)


def _count_qmetry_recommendations(
    project_id: str,
    reports: List[ProjectIncidentInvestigationReport],
) -> int:
    stored_counts = [
        int(r.qmetry_recommendation_report.total_recommendations)
        for r in reports
        if r.qmetry_recommendation_report is not None
        and int(r.qmetry_recommendation_report.total_recommendations or 0) > 0
    ]
    if stored_counts:
        return max(stored_counts)

    try:
        from services.qmetry_test_recommendation_service import build_qmetry_recommendation_report

        report = build_qmetry_recommendation_report(project_id=project_id)
        return int(report.total_recommendations or 0)
    except Exception as exc:
        logger.debug("outcome_tracking: qmetry recommendation count failed project_id=%s: %s", project_id, exc)
        return 0


def _count_recommendations_generated(
    project_id: str,
    reports: List[ProjectIncidentInvestigationReport],
) -> int:
    incident_actions = _count_incident_recommendations(reports)
    qmetry_recommendations = _count_qmetry_recommendations(project_id, reports)
    return incident_actions + qmetry_recommendations


def _count_executive_reports_sent(project_id: str) -> int:
    from services.db.audit_event_repository import audit_event_repo

    prefix = f"{(project_id or '').strip().lower()}:"
    if prefix == ":":
        return 0

    rows = audit_event_repo.list_events(
        event_type="REPORT_SENT",
        resource_type="REPORTS",
        limit=_AUDIT_LIMIT,
    )
    return sum(
        1
        for row in rows
        if str(row.get("result") or "").upper() == "SUCCESS"
        and str(row.get("resource_id") or "").startswith(prefix)
    )


def _build_executive_summary(
    *,
    blockers_identified: int,
    releases_blocked: int,
    recommendations_generated: int,
    incidents_investigated: int,
    executive_reports_sent: int,
) -> str:
    totals = (
        blockers_identified,
        releases_blocked,
        recommendations_generated,
        incidents_investigated,
        executive_reports_sent,
    )
    if all(value == 0 for value in totals):
        return "No measurable outcomes available yet."

    clauses: List[str] = []
    if blockers_identified > 0:
        noun = "blocker" if blockers_identified == 1 else "blockers"
        clauses.append(f"identified {blockers_identified} {noun}")
    if releases_blocked > 0:
        noun = "release" if releases_blocked == 1 else "releases"
        clauses.append(f"flagged {releases_blocked} {noun} as blocked")
    if recommendations_generated > 0:
        noun = "test recommendation" if recommendations_generated == 1 else "test recommendations"
        clauses.append(f"generated {recommendations_generated} {noun}")
    if incidents_investigated > 0:
        noun = "incident" if incidents_investigated == 1 else "incidents"
        clauses.append(f"investigated {incidents_investigated} {noun}")
    if executive_reports_sent > 0:
        noun = "executive report" if executive_reports_sent == 1 else "executive reports"
        clauses.append(f"delivered {executive_reports_sent} {noun}")

    if len(clauses) == 1:
        body = clauses[0]
    elif len(clauses) == 2:
        body = f"{clauses[0]} and {clauses[1]}"
    else:
        body = ", ".join(clauses[:-1]) + f", and {clauses[-1]}"

    return f"During the measured period, Vanya {body}."


def _build_outcome_metrics(
    *,
    blockers_identified: int,
    releases_blocked: int,
    recommendations_generated: int,
    incidents_investigated: int,
    executive_reports_sent: int,
) -> List[OutcomeMetric]:
    return [
        OutcomeMetric(metric_name="blockers_identified", value=blockers_identified),
        OutcomeMetric(metric_name="releases_blocked", value=releases_blocked),
        OutcomeMetric(metric_name="recommendations_generated", value=recommendations_generated),
        OutcomeMetric(metric_name="incidents_investigated", value=incidents_investigated),
        OutcomeMetric(metric_name="executive_reports_sent", value=executive_reports_sent),
    ]


def build_outcome_tracking_report(project_id: str) -> OutcomeTrackingReport:
    """Aggregate measurable outcomes from existing project intelligence and audit data."""
    pid = (project_id or "").strip().lower()
    reports = _load_incident_reports(pid)

    blockers_identified = _count_blockers_identified(reports)
    releases_blocked = _count_releases_blocked(pid, reports)
    recommendations_generated = _count_recommendations_generated(pid, reports)
    incidents_investigated = len(reports)
    executive_reports_sent = _count_executive_reports_sent(pid)

    executive_summary = _build_executive_summary(
        blockers_identified=blockers_identified,
        releases_blocked=releases_blocked,
        recommendations_generated=recommendations_generated,
        incidents_investigated=incidents_investigated,
        executive_reports_sent=executive_reports_sent,
    )

    return OutcomeTrackingReport(
        generated_at=_utc_now_iso(),
        blockers_identified=blockers_identified,
        releases_blocked=releases_blocked,
        recommendations_generated=recommendations_generated,
        incidents_investigated=incidents_investigated,
        executive_reports_sent=executive_reports_sent,
        executive_summary=executive_summary,
        outcome_metrics=_build_outcome_metrics(
            blockers_identified=blockers_identified,
            releases_blocked=releases_blocked,
            recommendations_generated=recommendations_generated,
            incidents_investigated=incidents_investigated,
            executive_reports_sent=executive_reports_sent,
        ),
    )
