# services/release_readiness_service.py
"""
REL-01A — Release Readiness intelligence compositor (read-only).

Assembles existing incident intelligence slices, project SCM connection status,
and integration dispatcher readiness. No external API calls, rescoring, or mutation.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from models.incident_models import ProjectIncidentInvestigationReport
from models.release_readiness_models import ReleaseReadinessView
from models.scheduled_report_models import ExecutiveReportPreview
from services.integration_dispatcher import integration_dispatcher
from services.jira_issue_intelligence_service import jira_executive_risk_lines

logger = logging.getLogger("vanya.release_readiness")

_CRITICAL_CONTRACT_LEVELS = frozenset({"CRITICAL", "HIGH"})
_BROKEN_JOURNEY_STATUSES = frozenset({"BROKEN", "FAILED", "INCONSISTENT"})
_DECISION_STATUS_MAP = {
    "GREEN": "GO",
    "YELLOW": "CAUTION",
    "ORANGE": "CAUTION",
    "RED": "BLOCKED",
}


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _load_latest_incident_report(project_id: str) -> Optional[ProjectIncidentInvestigationReport]:
    from services.db.incident_report_repository import incident_report_repo

    rows = incident_report_repo.list_reports(project_id=project_id, limit=1)
    if not rows:
        return None
    report_id = str(rows[0].get("id") or "").strip()
    if not report_id:
        return None
    full = incident_report_repo.get(report_id)
    if not isinstance(full, dict):
        return None
    try:
        return ProjectIncidentInvestigationReport.model_validate(full)
    except Exception as exc:
        logger.debug("release_readiness: invalid stored report project_id=%s: %s", project_id, exc)
        return None


def _github_status(project_id: str):
    try:
        from services.project_github_settings_service import get_project_github_status

        return get_project_github_status(project_id, validate=False)
    except LookupError:
        return None
    except Exception as exc:
        logger.debug("release_readiness: github status unavailable project_id=%s: %s", project_id, exc)
        return None


def _azure_devops_status(project_id: str):
    try:
        from services.project_azure_devops_settings_service import get_project_azure_devops_status

        return get_project_azure_devops_status(project_id, validate=False)
    except LookupError:
        return None
    except Exception as exc:
        logger.debug("release_readiness: azure status unavailable project_id=%s: %s", project_id, exc)
        return None


def _derive_overall_status(view: ReleaseReadinessView) -> str:
    if view.decision_center is not None:
        mapped = _DECISION_STATUS_MAP.get(str(view.decision_center.overall_status or "").upper())
        if mapped:
            return mapped

    if view.deployment_risk_assessment is not None:
        level = str(view.deployment_risk_assessment.risk_level or "low").lower()
        if level in ("high", "critical"):
            return "BLOCKED"
        if level == "medium":
            return "CAUTION"
        if level == "low":
            return "GO"

    if view.quality_health is not None:
        status = str(view.quality_health.overall_status or "").upper()
        if status in ("GOOD", "HEALTHY", "GO"):
            return "GO"
        if status in ("WARNING", "DEGRADED", "CAUTION", "YELLOW"):
            return "CAUTION"
        if status in ("CRITICAL", "BAD", "BLOCKED", "RED"):
            return "BLOCKED"

    return "UNKNOWN"


def _derive_summary(view: ReleaseReadinessView) -> str:
    base = ""
    if view.decision_center is not None:
        text = str(view.decision_center.executive_summary or "").strip()
        if text:
            base = text
    if not base and view.executive_quality_report is not None:
        text = str(view.executive_quality_report.executive_summary or "").strip()
        if text:
            base = text
    if not base and view.quality_health is not None:
        text = str(view.quality_health.summary or "").strip()
        if text:
            base = text
    if not base:
        base = "Release readiness intelligence is not yet available for this project."

    intel = view.jira_issue_intelligence
    if intel is not None and intel.blocker_count > 0:
        note = "Open Jira blockers detected."
        if note not in base:
            return f"{base} {note}".strip() if base else note
    return base


def _journey_name_map(view: ReleaseReadinessView) -> Dict[str, str]:
    djv = view.data_journey_validation
    if djv is None:
        return {}
    mapping: Dict[str, str] = {}
    for journey in djv.journeys:
        jid = str(journey.journey_id or "").strip()
        name = str(journey.name or jid).strip()
        if jid:
            mapping[jid] = name
    return mapping


def top_risks_from_view(view: ReleaseReadinessView, *, limit: int = 5) -> List[str]:
    risks: List[str] = []

    if view.executive_quality_report is not None:
        for item in view.executive_quality_report.top_risks or []:
            text = str(item or "").strip()
            if text and text not in risks:
                risks.append(text)

    if view.contract_risk_assessment is not None:
        for assessment in sorted(
            view.contract_risk_assessment.assessments or [],
            key=lambda a: str(a.overall_risk_level or ""),
            reverse=True,
        ):
            level = str(assessment.overall_risk_level or "LOW").upper()
            if level not in _CRITICAL_CONTRACT_LEVELS:
                continue
            contract_id = str(assessment.contract_id or "contract").strip()
            label = contract_id.replace("_", " ").replace("-", " ").title()
            text = f"{label} contract risk is {level}"
            if text not in risks:
                risks.append(text)

    journey_names = _journey_name_map(view)
    if view.data_journey_validation is not None:
        for result in view.data_journey_validation.results or []:
            status = str(result.status or "").upper()
            if status not in _BROKEN_JOURNEY_STATUSES:
                continue
            jid = str(result.journey_id or "").strip()
            name = journey_names.get(jid) or jid or "Journey"
            text = f"{name} journey is {status}"
            if text not in risks:
                risks.append(text)

    if view.deployment_risk_assessment is not None:
        for factor in view.deployment_risk_assessment.contributing_factors or []:
            title = str(factor.title or factor.description or "").strip()
            if title and title not in risks:
                risks.append(title)

    if view.multi_environment is not None:
        for promo in view.multi_environment.promotion_readiness or []:
            if str(promo.readiness_status or "").upper() == "BLOCKED":
                for blocker in promo.blockers or []:
                    text = str(blocker or "").strip()
                    if text and text not in risks:
                        risks.append(text)

    jira_lines = jira_executive_risk_lines(view.jira_issue_intelligence)
    if jira_lines:
        merged = list(jira_lines)
        for line in risks:
            if line not in merged:
                merged.append(line)
        risks = merged

    return risks[:limit]


def top_recommendations_from_view(view: ReleaseReadinessView, *, limit: int = 5) -> List[str]:
    recs: List[str] = []

    if view.executive_quality_report is not None:
        for item in view.executive_quality_report.top_recommendations or []:
            text = str(item or "").strip()
            if text and text not in recs:
                recs.append(text)

    if view.decision_center is not None:
        takeaways = sorted(
            view.decision_center.key_takeaways or [],
            key=lambda i: int(i.priority or 99),
        )
        for insight in takeaways:
            title = str(insight.title or "").strip()
            if title and title not in recs:
                recs.append(title)

    if view.multi_environment is not None:
        for promo in view.multi_environment.promotion_readiness or []:
            for rec in promo.recommended_validations or []:
                text = str(rec or "").strip()
                if text and text not in recs:
                    recs.append(text)

    return recs[:limit]


def _quality_score(view: ReleaseReadinessView) -> int:
    if view.quality_health is not None and view.quality_health.overall_score is not None:
        return int(view.quality_health.overall_score)
    if view.executive_quality_report is not None and view.executive_quality_report.overall_quality_score is not None:
        return int(view.executive_quality_report.overall_quality_score)
    return 0


def _quality_trend(view: ReleaseReadinessView) -> str:
    if view.quality_trends is not None and view.quality_trends.overall_trend:
        return str(view.quality_trends.overall_trend).upper()
    if view.quality_health is not None and view.quality_health.trend:
        return str(view.quality_health.trend).upper()
    if view.executive_quality_report is not None and view.executive_quality_report.quality_trend:
        return str(view.executive_quality_report.quality_trend).upper()
    return "UNKNOWN"


def _risk_level(view: ReleaseReadinessView) -> str:
    if view.deployment_risk_assessment is not None and view.deployment_risk_assessment.risk_level:
        return str(view.deployment_risk_assessment.risk_level).upper()
    if view.executive_quality_report is not None and view.executive_quality_report.overall_risk_level:
        return str(view.executive_quality_report.overall_risk_level).upper()
    if view.decision_center is not None and view.decision_center.top_risk_level:
        return str(view.decision_center.top_risk_level).upper()
    return str(view.overall_status or "UNKNOWN").upper()


def _incident_count(view: ReleaseReadinessView) -> int:
    if view.executive_quality_report is not None and view.executive_quality_report.open_incident_count is not None:
        return int(view.executive_quality_report.open_incident_count)
    return 0


def _critical_contract_count(view: ReleaseReadinessView) -> int:
    if view.executive_quality_report is not None and view.executive_quality_report.critical_contract_count is not None:
        return int(view.executive_quality_report.critical_contract_count)
    if view.contract_risk_assessment is None:
        return 0
    return sum(
        1
        for a in view.contract_risk_assessment.assessments or []
        if str(a.overall_risk_level or "").upper() in _CRITICAL_CONTRACT_LEVELS
    )


def _broken_journey_count(view: ReleaseReadinessView) -> int:
    if view.executive_quality_report is not None and view.executive_quality_report.broken_journey_count is not None:
        return int(view.executive_quality_report.broken_journey_count)
    if view.data_journey_validation is None:
        return 0
    return sum(
        1
        for r in view.data_journey_validation.results or []
        if str(r.status or "").upper() in _BROKEN_JOURNEY_STATUSES
    )


def _build_data_gaps(
    *,
    incident_report: Optional[ProjectIncidentInvestigationReport],
    github_connected: bool,
    azure_connected: bool,
) -> List[str]:
    gaps: List[str] = []
    if incident_report is None:
        gaps.append(
            "No incident investigation report — run Incident Investigator for full release intelligence."
        )
    else:
        slice_checks = (
        ("deployment_risk_assessment", incident_report.deployment_risk_assessment, "Deployment risk assessment unavailable."),
        ("contract_risk_assessment", incident_report.contract_risk_assessment, "Contract risk assessment unavailable."),
        ("data_journey_validation", incident_report.data_journey_validation, "Data journey validation unavailable."),
        ("enterprise_dependency_map", incident_report.enterprise_dependency_map, "Enterprise dependency map unavailable."),
        ("multi_environment", incident_report.multi_environment, "Multi-environment intelligence unavailable."),
        ("quality_health", incident_report.quality_health, "Quality health report unavailable."),
        ("quality_trends", incident_report.quality_trends, "Quality trend report unavailable."),
        ("early_degradation", incident_report.early_degradation, "Early degradation report unavailable."),
        ("executive_quality_report", incident_report.executive_quality_report, "Executive quality report unavailable."),
        ("decision_center", incident_report.decision_center, "Decision center summary unavailable."),
    )
        for _key, value, message in slice_checks:
            if value is None:
                gaps.append(message)

        if incident_report.multi_environment is not None and not incident_report.multi_environment.promotion_readiness:
            gaps.append("multi_environment.promotion_readiness empty — configure environments.")

        jira_intel = incident_report.jira_issue_intelligence
        if jira_intel is not None and not jira_intel.connected:
            gaps.append("Jira not connected — external blockers may be missing.")

    if not github_connected and not azure_connected:
        gaps.append("Repository not connected — GitHub and Azure DevOps both disconnected.")

    return gaps


def _scm_connected(github, azure_devops) -> tuple[bool, bool]:
    gh_ok = bool(
        github is not None
        and github.connected
        and github.enabled
        and str(github.full_name or "").strip()
    )
    az_ok = bool(
        azure_devops is not None
        and azure_devops.connected
        and azure_devops.enabled
        and str(azure_devops.repository_id or "").strip()
    )
    return gh_ok, az_ok


def build_release_readiness_view(
    *,
    project_id: str,
    incident_report: Optional[ProjectIncidentInvestigationReport] = None,
) -> ReleaseReadinessView:
    """Compose release readiness from stored intelligence and connection metadata only."""
    pid = (project_id or "").strip().lower()
    source = incident_report
    if source is None and pid:
        source = _load_latest_incident_report(pid)

    github = _github_status(pid) if pid else None
    azure_devops = _azure_devops_status(pid) if pid else None
    gh_connected, az_connected = _scm_connected(github, azure_devops)

    view = ReleaseReadinessView(
        project_id=pid,
        generated_at=_utc_now_iso(),
        source_incident_report_id=str(source.id or "").strip() or None if source else None,
        deployment_risk_assessment=source.deployment_risk_assessment if source else None,
        contract_risk_assessment=source.contract_risk_assessment if source else None,
        data_journey_validation=source.data_journey_validation if source else None,
        enterprise_dependency_map=source.enterprise_dependency_map if source else None,
        multi_environment=source.multi_environment if source else None,
        quality_health=source.quality_health if source else None,
        quality_trends=source.quality_trends if source else None,
        early_degradation=source.early_degradation if source else None,
        executive_quality_report=source.executive_quality_report if source else None,
        decision_center=source.decision_center if source else None,
        jira_issue_intelligence=source.jira_issue_intelligence if source else None,
        github=github,
        azure_devops=azure_devops,
        integration_readiness=integration_dispatcher.readiness(),
        data_gaps=_build_data_gaps(
            incident_report=source,
            github_connected=gh_connected,
            azure_connected=az_connected,
        ),
    )
    view.overall_status = _derive_overall_status(view)
    view.summary = _derive_summary(view)
    from services.audit_event_service import safe_record_event

    safe_record_event(
        event_type="RELEASE_READINESS_VIEWED",
        resource_type="RELEASES",
        resource_id=pid or "unknown",
        action="view",
        result="SUCCESS",
        metadata={"overall_status": view.overall_status},
    )
    return view


def build_release_readiness_executive_preview(
    project_id: str,
    view: ReleaseReadinessView,
    *,
    generated_at: Optional[str] = None,
) -> ExecutiveReportPreview:
    """Map compositor view to ENT-02B executive preview shape for RELEASE_READINESS."""
    pid = (project_id or "").strip().lower()
    ts = str(generated_at or view.generated_at or _utc_now_iso()).strip()
    intel = view.jira_issue_intelligence
    return ExecutiveReportPreview(
        preview_id=f"exec_preview:{pid}:release_readiness",
        title="Release Readiness",
        generated_at=ts,
        quality_score=_quality_score(view),
        quality_trend=_quality_trend(view),
        risk_level=_risk_level(view),
        executive_summary=view.summary,
        top_risks=top_risks_from_view(view),
        top_recommendations=top_recommendations_from_view(view),
        incident_count=_incident_count(view),
        critical_contract_count=_critical_contract_count(view),
        broken_journey_count=_broken_journey_count(view),
        jira_blocker_count=int(intel.blocker_count) if intel else 0,
        jira_blocker_keys=[
            str(b.issue_key).strip()
            for b in (intel.top_blockers or [])
            if str(b.issue_key or "").strip()
        ][:5] if intel else [],
    )
