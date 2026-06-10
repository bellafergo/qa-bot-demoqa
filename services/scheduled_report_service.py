# services/scheduled_report_service.py
"""
Enterprise ENT-02B — Scheduled Executive Reports (read-only foundation).

Defines recurring executive report templates, scheduling metadata, and deterministic
previews from stored incident intelligence. No delivery, schedulers, or external calls.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Tuple

from models.scheduled_report_models import (
    ExecutiveReportCenter,
    ExecutiveReportPreview,
    ExecutiveReportSchedule,
)

logger = logging.getLogger("vanya.scheduled_reports")

_ALLOWED_FREQUENCIES = frozenset({"DAILY", "WEEKLY", "MONTHLY"})
_ALLOWED_REPORT_TYPES = frozenset(
    {"QUALITY_BRIEF", "EXECUTIVE_SUMMARY", "RELEASE_READINESS", "INCIDENT_REVIEW"}
)

_NEXT_RUN_PREVIEW = {
    "DAILY": "Tomorrow 8:00 AM",
    "WEEKLY": "Monday 8:00 AM",
    "MONTHLY": "1st of month 8:00 AM",
}

_REPORT_TITLES = {
    "QUALITY_BRIEF": "Weekly Quality Brief",
    "EXECUTIVE_SUMMARY": "Executive Summary",
    "RELEASE_READINESS": "Release Readiness",
    "INCIDENT_REVIEW": "Incident Review",
}

_DEFAULT_SCHEDULES: Tuple[Tuple[str, str, str, str, bool, int], ...] = (
    ("quality_brief", "Weekly Quality Brief", "WEEKLY", "QUALITY_BRIEF", True, 3),
    ("executive_summary", "Executive Summary", "WEEKLY", "EXECUTIVE_SUMMARY", True, 5),
    ("release_readiness", "Release Readiness", "MONTHLY", "RELEASE_READINESS", True, 4),
    ("incident_review", "Incident Review", "MONTHLY", "INCIDENT_REVIEW", True, 2),
)

_CRITICAL_CONTRACT_LEVELS = frozenset({"CRITICAL", "HIGH"})
_BROKEN_JOURNEY_STATUSES = frozenset({"BROKEN", "FAILED", "INCONSISTENT"})


def _empty_center() -> ExecutiveReportCenter:
    return ExecutiveReportCenter(schedules=[], latest_preview=None, total_schedules=0)


def _schedule(
    slug: str,
    name: str,
    frequency: str,
    report_type: str,
    enabled: bool,
    recipients_count: int,
) -> ExecutiveReportSchedule:
    return ExecutiveReportSchedule(
        schedule_id=f"exec_schedule:{slug}",
        name=name,
        frequency=frequency,
        enabled=enabled,
        report_type=report_type,
        recipients_count=recipients_count,
        next_run_preview=_NEXT_RUN_PREVIEW.get(frequency),
    )


def _default_schedules() -> List[ExecutiveReportSchedule]:
    return [
        _schedule(slug, name, frequency, report_type, enabled, recipients)
        for slug, name, frequency, report_type, enabled, recipients in _DEFAULT_SCHEDULES
    ]


def _load_latest_incident_report(project_id: str) -> Optional[Dict[str, Any]]:
    from services.db.incident_report_repository import incident_report_repo

    rows = incident_report_repo.list_reports(project_id=project_id, limit=1)
    if not rows:
        return None
    report_id = str(rows[0].get("id") or "").strip()
    if not report_id:
        return None
    full = incident_report_repo.get(report_id)
    return full if isinstance(full, dict) else None


def _has_preview_inputs(report: Dict[str, Any]) -> bool:
    keys = (
        "executive_quality_report",
        "quality_health",
        "quality_trends",
        "deployment_risk_assessment",
        "contract_risk_assessment",
        "data_journey_validation",
        "decision_center",
        "historical_learning",
    )
    return any(report.get(key) for key in keys)


def _quality_score(report: Dict[str, Any]) -> int:
    qh = report.get("quality_health")
    if isinstance(qh, dict) and qh.get("overall_score") is not None:
        return int(qh["overall_score"])
    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict) and eqr.get("overall_quality_score") is not None:
        return int(eqr["overall_quality_score"])
    return 0


def _quality_trend(report: Dict[str, Any]) -> str:
    qt = report.get("quality_trends")
    if isinstance(qt, dict) and qt.get("overall_trend"):
        return str(qt["overall_trend"]).upper()
    qh = report.get("quality_health")
    if isinstance(qh, dict) and qh.get("trend"):
        return str(qh["trend"]).upper()
    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict) and eqr.get("quality_trend"):
        return str(eqr["quality_trend"]).upper()
    return "UNKNOWN"


def _risk_level(report: Dict[str, Any]) -> str:
    deployment = report.get("deployment_risk_assessment")
    if isinstance(deployment, dict) and deployment.get("risk_level"):
        return str(deployment["risk_level"]).upper()
    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict) and eqr.get("overall_risk_level"):
        return str(eqr["overall_risk_level"]).upper()
    dc = report.get("decision_center")
    if isinstance(dc, dict) and dc.get("top_risk_level"):
        return str(dc["top_risk_level"]).upper()
    return "LOW"


def _executive_summary(report: Dict[str, Any]) -> str:
    dc = report.get("decision_center")
    if isinstance(dc, dict):
        summary = str(dc.get("executive_summary") or "").strip()
        if summary:
            return summary
    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict):
        summary = str(eqr.get("executive_summary") or "").strip()
        if summary:
            return summary
    qh = report.get("quality_health")
    if isinstance(qh, dict):
        summary = str(qh.get("summary") or "").strip()
        if summary:
            return summary
    return "Executive quality intelligence is available for scheduled reporting preview."


def _journey_name_map(report: Dict[str, Any]) -> Dict[str, str]:
    djv = report.get("data_journey_validation")
    if not isinstance(djv, dict):
        return {}
    mapping: Dict[str, str] = {}
    for journey in djv.get("journeys") or []:
        jid = str(journey.get("journey_id") or "").strip()
        name = str(journey.get("name") or jid).strip()
        if jid:
            mapping[jid] = name
    return mapping


def _top_risks(report: Dict[str, Any]) -> List[str]:
    risks: List[str] = []

    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict):
        for item in eqr.get("top_risks") or []:
            text = str(item or "").strip()
            if text and text not in risks:
                risks.append(text)

    cra = report.get("contract_risk_assessment")
    if isinstance(cra, dict):
        for assessment in sorted(
            cra.get("assessments") or [],
            key=lambda a: str(a.get("overall_risk_level") or ""),
            reverse=True,
        ):
            level = str(assessment.get("overall_risk_level") or "LOW").upper()
            if level not in _CRITICAL_CONTRACT_LEVELS:
                continue
            contract_id = str(assessment.get("contract_id") or "contract").strip()
            label = contract_id.replace("_", " ").replace("-", " ").title()
            text = f"{label} contract risk is {level}"
            if text not in risks:
                risks.append(text)

    djv = report.get("data_journey_validation")
    journey_names = _journey_name_map(report)
    if isinstance(djv, dict):
        for result in djv.get("results") or []:
            status = str(result.get("status") or "").upper()
            if status not in _BROKEN_JOURNEY_STATUSES:
                continue
            jid = str(result.get("journey_id") or "").strip()
            name = journey_names.get(jid) or jid or "Journey"
            text = f"{name} journey is {status}"
            if text not in risks:
                risks.append(text)

    deployment = report.get("deployment_risk_assessment")
    if isinstance(deployment, dict):
        for factor in deployment.get("contributing_factors") or []:
            title = str(factor.get("title") or factor.get("description") or "").strip()
            if title and title not in risks:
                risks.append(title)

    return risks[:5]


def _top_recommendations(report: Dict[str, Any]) -> List[str]:
    recs: List[str] = []

    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict):
        for item in eqr.get("top_recommendations") or []:
            text = str(item or "").strip()
            if text and text not in recs:
                recs.append(text)

    dc = report.get("decision_center")
    if isinstance(dc, dict):
        takeaways = sorted(
            dc.get("key_takeaways") or [],
            key=lambda i: int(i.get("priority") or 99),
        )
        for insight in takeaways:
            title = str(insight.get("title") or "").strip()
            if title and title not in recs:
                recs.append(title)

    return recs[:5]


def _incident_count(report: Dict[str, Any]) -> int:
    hl = report.get("historical_learning")
    if isinstance(hl, dict):
        similar = hl.get("similar_incidents") or []
        if similar:
            return len(similar)
    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict) and eqr.get("open_incident_count") is not None:
        return int(eqr["open_incident_count"])
    return 0


def _critical_contract_count(report: Dict[str, Any]) -> int:
    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict) and eqr.get("critical_contract_count") is not None:
        return int(eqr["critical_contract_count"])
    cra = report.get("contract_risk_assessment")
    if not isinstance(cra, dict):
        return 0
    return sum(
        1
        for a in cra.get("assessments") or []
        if str(a.get("overall_risk_level") or "").upper() in _CRITICAL_CONTRACT_LEVELS
    )


def _broken_journey_count(report: Dict[str, Any]) -> int:
    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict) and eqr.get("broken_journey_count") is not None:
        return int(eqr["broken_journey_count"])
    djv = report.get("data_journey_validation")
    if not isinstance(djv, dict):
        return 0
    return sum(
        1
        for r in djv.get("results") or []
        if str(r.get("status") or "").upper() in _BROKEN_JOURNEY_STATUSES
    )


def build_executive_report_preview(
    project_id: str,
    report: Dict[str, Any],
    *,
    report_type: str = "QUALITY_BRIEF",
) -> ExecutiveReportPreview:
    pid = (project_id or "").strip().lower()
    rtype = str(report_type or "QUALITY_BRIEF").upper()
    if rtype not in _ALLOWED_REPORT_TYPES:
        rtype = "QUALITY_BRIEF"

    generated_at = str(report.get("created_at") or "2026-01-01T08:00:00+00:00").strip()

    if rtype == "RELEASE_READINESS":
        from models.incident_models import ProjectIncidentInvestigationReport
        from services.release_readiness_service import (
            build_release_readiness_executive_preview,
            build_release_readiness_view,
        )

        incident = ProjectIncidentInvestigationReport.model_validate(report)
        view = build_release_readiness_view(project_id=pid, incident_report=incident)
        return build_release_readiness_executive_preview(pid, view, generated_at=generated_at)

    return ExecutiveReportPreview(
        preview_id=f"exec_preview:{pid}:{rtype.lower()}",
        title=_REPORT_TITLES.get(rtype, _REPORT_TITLES["QUALITY_BRIEF"]),
        generated_at=generated_at,
        quality_score=_quality_score(report),
        quality_trend=_quality_trend(report),
        risk_level=_risk_level(report),
        executive_summary=_executive_summary(report),
        top_risks=_top_risks(report),
        top_recommendations=_top_recommendations(report),
        incident_count=_incident_count(report),
        critical_contract_count=_critical_contract_count(report),
        broken_journey_count=_broken_journey_count(report),
    )


def build_executive_report_center(project_id: str) -> Optional[ExecutiveReportCenter]:
    pid = (project_id or "").strip().lower()
    if not pid:
        return None

    from services.db.project_repository import project_repo

    project = project_repo.get_project(pid)
    if project is None:
        return _empty_center()

    schedules = _default_schedules()
    latest_report = _load_latest_incident_report(pid)
    latest_preview = None
    if latest_report and _has_preview_inputs(latest_report):
        latest_preview = build_executive_report_preview(pid, latest_report, report_type="QUALITY_BRIEF")

    return ExecutiveReportCenter(
        schedules=schedules,
        latest_preview=latest_preview,
        total_schedules=len(schedules),
    )
