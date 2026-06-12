# services/business_risk_estimation_service.py
"""
ROI-01C — Executive Business Risk Estimation (read-only narrative).

Translates existing technical intelligence into business capability risk statements.
No financial estimates, predictions, AI, or new scoring engines.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Dict, List, Optional

from models.business_risk_models import (
    BusinessRiskAssessment,
    BusinessRiskReport,
    BusinessRiskSignal,
    ExecutiveConfidence,
    ExecutiveSeverity,
)
from models.incident_models import ProjectIncidentInvestigationReport
from services.capability_mapping_service import map_text_to_capability
from services.value_dashboard_service import _load_incident_reports, _release_status_blocked

logger = logging.getLogger("vanya.business_risk_estimation")

_BROKEN_JOURNEY_STATUSES = frozenset({"BROKEN", "DEGRADED", "FAILED", "INCONSISTENT"})
_CRITICAL_CONTRACT_LEVELS = frozenset({"CRITICAL", "HIGH"})
_DEGRADED_ENV_STATUSES = frozenset({"DEGRADED", "BROKEN", "WARNING", "CRITICAL"})
_SEVERITY_RANK = {"LOW": 0, "MEDIUM": 1, "HIGH": 2, "CRITICAL": 3}


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _confidence_from_evidence(count: int) -> ExecutiveConfidence:
    if count >= 4:
        return "HIGH"
    if count >= 2:
        return "MEDIUM"
    return "LOW"


def _max_severity(*levels: ExecutiveSeverity) -> ExecutiveSeverity:
    if not levels:
        return "LOW"
    return max(levels, key=lambda s: _SEVERITY_RANK.get(s, 0))


def _has_intelligence(report: Optional[ProjectIncidentInvestigationReport]) -> bool:
    if report is None:
        return False
    checks = (
        report.deployment_risk_assessment,
        report.contract_risk_assessment,
        report.data_journey_validation,
        report.enterprise_dependency_map,
        report.multi_environment,
        report.release_readiness,
        report.quality_health,
        report.quality_trends,
        report.early_degradation,
        report.executive_quality_report,
        report.jira_issue_intelligence,
        report.servicenow_intelligence,
        report.decision_center,
    )
    return any(item is not None for item in checks)


def _release_readiness_view(report: ProjectIncidentInvestigationReport):
    if report.release_readiness is not None:
        return report.release_readiness
    return None


def _collect_evidence_by_capability(
    report: ProjectIncidentInvestigationReport,
) -> Dict[str, Dict[str, int]]:
    """Return capability -> evidence_type -> count."""
    buckets: Dict[str, Dict[str, int]] = {}

    def bump(capability: str, evidence_type: str, amount: int = 1) -> None:
        cap = capability or "Platform Operations"
        buckets.setdefault(cap, {})
        buckets[cap][evidence_type] = buckets[cap].get(evidence_type, 0) + amount

    # Jira blockers
    intel = report.jira_issue_intelligence
    if intel is not None and intel.connected and intel.blocker_count > 0:
        for blocker in intel.top_blockers or []:
            cap = map_text_to_capability(blocker.related_module or "") or map_text_to_capability(blocker.summary or "")
            bump(cap or "Platform Operations", "jira_blockers")

    # ServiceNow operational correlations
    sn_intel = report.servicenow_intelligence
    if sn_intel is not None and sn_intel.connected:
        for group in (
            sn_intel.incident_correlations,
            sn_intel.change_correlations,
            sn_intel.service_correlations,
            sn_intel.cmdb_correlations,
        ):
            for corr in group or []:
                if corr.capability:
                    bump(corr.capability, "servicenow_operational")

    # Broken journeys
    djv = report.data_journey_validation
    if djv is not None:
        journey_map = {j.journey_id: j for j in djv.journeys or []}
        for result in djv.results or []:
            if str(result.status or "").upper() not in _BROKEN_JOURNEY_STATUSES:
                continue
            journey = journey_map.get(result.journey_id)
            name = journey.name if journey else result.journey_id
            area = journey.business_area if journey else ""
            cap = map_text_to_capability(area or name or "")
            bump(cap or "Customer Purchase Flow", "broken_journeys")

    # Critical contracts
    cra = report.contract_risk_assessment
    if cra is not None:
        for assessment in cra.assessments or []:
            level = str(assessment.overall_risk_level or "").upper()
            if level not in _CRITICAL_CONTRACT_LEVELS:
                continue
            cap = None
            for module in assessment.affected_modules or []:
                cap = map_text_to_capability(module)
                if cap:
                    break
            if not cap:
                cap = map_text_to_capability(assessment.contract_id or "")
            bump(cap or "Platform Operations", "critical_contracts")

    # Environment issues
    multi = report.multi_environment
    if multi is not None:
        for env in multi.environments or []:
            if str(env.status or "").upper() not in _DEGRADED_ENV_STATUSES:
                continue
            cap = map_text_to_capability(env.name or env.environment_id or "")
            bump(cap or "Platform Operations", "environment_issues")

    # Release blocked (platform-wide)
    if _release_status_blocked(report):
        bump("Platform Operations", "release_blocked")

    # Deployment risk
    dep = report.deployment_risk_assessment
    if dep is not None and str(dep.risk_level or "").lower() in ("high", "critical"):
        bump("Platform Operations", "deployment_risk")

    # Critical dependencies
    edm = report.enterprise_dependency_map
    if edm is not None:
        for node in edm.nodes or []:
            if str(node.risk_level or "").upper() not in ("HIGH", "CRITICAL"):
                continue
            cap = map_text_to_capability(node.name or "")
            bump(cap or "Platform Operations", "critical_dependencies")

    # Decision center impacted area
    dc = report.decision_center
    if dc is not None:
        for insight in dc.key_takeaways or []:
            cap = map_text_to_capability(insight.title or "")
            if cap:
                bump(cap, "decision_insights")

    return buckets


_EVIDENCE_LABELS = {
    "jira_blockers": "open Jira blockers",
    "broken_journeys": "broken data journeys",
    "critical_contracts": "critical contract changes",
    "environment_issues": "degraded environments",
    "release_blocked": "blocked release readiness",
    "deployment_risk": "elevated deployment risk",
    "critical_dependencies": "critical dependency map signals",
    "decision_insights": "decision center concerns",
    "servicenow_operational": "ServiceNow operational correlations",
}


def _severity_for_capability(
    evidence: Dict[str, int],
    *,
    has_jira_blockers: bool,
    global_critical_contracts: bool = False,
) -> ExecutiveSeverity:
    broken = evidence.get("broken_journeys", 0) > 0
    critical_contract = evidence.get("critical_contracts", 0) > 0
    release_blocked = evidence.get("release_blocked", 0) > 0
    env_issues = evidence.get("environment_issues", 0) > 0
    jira = evidence.get("jira_blockers", 0) > 0
    deployment = evidence.get("deployment_risk", 0) > 0
    dependencies = evidence.get("critical_dependencies", 0) > 0

    if broken and (critical_contract or global_critical_contracts):
        return "CRITICAL"
    if release_blocked and (jira or has_jira_blockers):
        return "HIGH"
    if critical_contract or dependencies:
        return "HIGH"
    if env_issues and not jira and not has_jira_blockers:
        return "MEDIUM"
    if broken or jira or deployment:
        return "MEDIUM"
    if env_issues or evidence.get("decision_insights", 0) > 0:
        return "LOW"
    return "LOW"


def _evidence_labels(evidence: Dict[str, int]) -> List[str]:
    labels: List[str] = []
    for key, count in sorted(evidence.items(), key=lambda item: item[1], reverse=True):
        if count <= 0:
            continue
        base = _EVIDENCE_LABELS.get(key, key.replace("_", " "))
        labels.append(base if count == 1 else f"{count} {base}")
    return labels


def _build_signals(
    capability: str,
    evidence: Dict[str, int],
    severity: ExecutiveSeverity,
) -> List[BusinessRiskSignal]:
    signals: List[BusinessRiskSignal] = []
    total = sum(evidence.values())
    confidence = _confidence_from_evidence(total)
    for idx, (key, count) in enumerate(sorted(evidence.items(), key=lambda item: item[1], reverse=True)):
        if count <= 0:
            continue
        title = _EVIDENCE_LABELS.get(key, key.replace("_", " ")).title()
        signals.append(
            BusinessRiskSignal(
                signal_id=f"signal:{capability.lower().replace(' ', '_')}:{key}:{idx}",
                title=title,
                severity=severity,
                confidence=confidence,
                impacted_capability=capability,
                evidence_count=count,
            )
        )
    return signals


def _build_assessment(capability: str, evidence: Dict[str, int], severity: ExecutiveSeverity) -> BusinessRiskAssessment:
    labels = _evidence_labels(evidence)
    total = sum(evidence.values())
    confidence = _confidence_from_evidence(total)
    if labels:
        summary = (
            f"{capability} is at {severity.lower()} business risk supported by "
            f"{', '.join(labels)}."
        )
    else:
        summary = f"{capability} has limited business risk signals in current intelligence."
    return BusinessRiskAssessment(
        risk_id=f"risk:{capability.lower().replace(' ', '_')}",
        capability=capability,
        severity=severity,
        confidence=confidence,
        summary=summary,
        evidence=labels,
    )


def _executive_summary(risks: List[BusinessRiskAssessment]) -> str:
    if not risks:
        return "No business capability risks identified from current intelligence."
    ranked = sorted(risks, key=lambda r: _SEVERITY_RANK.get(r.severity, 0), reverse=True)
    top = ranked[0]
    if len(top.evidence) >= 2:
        evidence_text = ", ".join(top.evidence[:3])
        return (
            f"{top.capability} is at elevated risk due to {evidence_text}."
        )
    if top.evidence:
        return f"{top.capability} is at elevated risk due to {top.evidence[0]}."
    return f"{top.capability} is at {top.severity.lower()} business risk based on current intelligence."


def build_business_risk_report(project_id: str) -> BusinessRiskReport:
    """Build executive business risk narrative from stored project intelligence."""
    pid = (project_id or "").strip().lower()
    reports = _load_incident_reports(pid)
    latest = reports[0] if reports else None

    if not _has_intelligence(latest):
        return BusinessRiskReport(
            generated_at=_utc_now_iso(),
            overall_business_risk="LOW",
            executive_summary="Insufficient intelligence available to assess business risk.",
            has_intelligence=False,
        )

    assert latest is not None
    buckets = _collect_evidence_by_capability(latest)
    global_jira = bool(
        latest.jira_issue_intelligence is not None
        and latest.jira_issue_intelligence.connected
        and latest.jira_issue_intelligence.blocker_count > 0
    )
    global_critical_contracts = any(
        evidence.get("critical_contracts", 0) > 0 for evidence in buckets.values()
    )

    assessments: List[BusinessRiskAssessment] = []
    all_signals: List[BusinessRiskSignal] = []

    for capability, evidence in sorted(buckets.items()):
        if not evidence:
            continue
        enriched = dict(evidence)
        if (
            global_critical_contracts
            and enriched.get("broken_journeys", 0) > 0
            and enriched.get("critical_contracts", 0) == 0
        ):
            enriched["critical_contracts"] = 1
        severity = _severity_for_capability(
            enriched,
            has_jira_blockers=global_jira,
            global_critical_contracts=global_critical_contracts,
        )
        assessments.append(_build_assessment(capability, enriched, severity))
        all_signals.extend(_build_signals(capability, enriched, severity))

    assessments.sort(key=lambda a: _SEVERITY_RANK.get(a.severity, 0), reverse=True)
    overall = _max_severity(*(a.severity for a in assessments)) if assessments else "LOW"
    top_capabilities = [a.capability for a in assessments[:4]]

    return BusinessRiskReport(
        generated_at=_utc_now_iso(),
        overall_business_risk=overall,
        business_risks=assessments,
        top_capabilities_at_risk=top_capabilities,
        executive_summary=_executive_summary(assessments),
        signals=all_signals,
        has_intelligence=True,
    )
