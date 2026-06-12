# services/servicenow_intelligence_service.py
"""
SNOW-01B — Read-only ServiceNow operational intelligence (deterministic correlation).

Correlates discovered ServiceNow entities with existing Vanya intelligence slices.
No new scoring engines, LLMs, or ServiceNow writes.
"""
from __future__ import annotations

import logging
from typing import Dict, List, Optional, Set, Tuple

from models.business_risk_models import BusinessRiskReport
from models.incident_models import ProjectIncidentInvestigationReport
from models.jira_issue_intelligence_models import JiraIssueIntelligenceReport
from models.qmetry_coverage_models import CoverageIntelligenceReport
from models.release_readiness_models import ReleaseReadinessView
from models.servicenow_intelligence_models import (
    ServiceNowConfidence,
    ServiceNowCorrelation,
    ServiceNowIntelligenceReport,
)
from models.servicenow_models import ServiceNowChange, ServiceNowCI, ServiceNowIncident, ServiceNowService
from services.capability_mapping_service import map_text_to_capability, match_module_keyword
from services.servicenow_integration_service import (
    list_changes,
    list_cmdb_items,
    list_incidents,
    list_services,
    validate_servicenow_connection,
)

logger = logging.getLogger("vanya.servicenow_intelligence")

_BLOCKED_RELEASE_STATUSES = frozenset({"BLOCKED", "NO_GO", "CAUTION"})
_BUSINESS_RISK_SEVERITIES = frozenset({"HIGH", "CRITICAL"})
_CHANGE_KEYWORDS = ("payments", "checkout", "authentication", "inventory")
_INCIDENT_KEYWORDS = ("payments", "checkout", "authentication", "order", "orders")


def _empty_report(*, connected: bool = False, gap: Optional[str] = None) -> ServiceNowIntelligenceReport:
    gaps = [gap] if gap else []
    return ServiceNowIntelligenceReport(connected=connected, data_gaps=gaps)


def _confidence_from_signal_count(count: int) -> ServiceNowConfidence:
    if count >= 3:
        return "HIGH"
    if count >= 2:
        return "MEDIUM"
    return "LOW"


def _capability_from_text(text: str) -> Optional[str]:
    cap = map_text_to_capability(text)
    if cap:
        return cap
    hit = match_module_keyword(text)
    return hit[1] if hit else None


def _text_has_keywords(text: str, keywords: Tuple[str, ...]) -> bool:
    hay = (text or "").lower()
    return any(kw in hay for kw in keywords)


def _business_risk_capabilities(report: Optional[BusinessRiskReport]) -> Set[str]:
    if report is None:
        return set()
    caps: Set[str] = set()
    for risk in report.business_risks or []:
        if str(risk.severity or "").upper() in _BUSINESS_RISK_SEVERITIES:
            caps.add(risk.capability)
    for cap in report.top_capabilities_at_risk or []:
        if cap:
            caps.add(cap)
    return caps


def _jira_pressure_capabilities(intel: Optional[JiraIssueIntelligenceReport]) -> Set[str]:
    if intel is None or not intel.connected:
        return set()
    caps: Set[str] = set()
    for blocker in intel.top_blockers or []:
        for text in (blocker.related_module, blocker.summary, blocker.issue_key):
            cap = map_text_to_capability(text or "")
            if cap:
                caps.add(cap)
    return caps


def _release_pressure_capabilities(release_readiness: Optional[ReleaseReadinessView]) -> Set[str]:
    if release_readiness is None:
        return set()
    status = str(release_readiness.overall_status or "").upper()
    if status not in _BLOCKED_RELEASE_STATUSES:
        return set()
    try:
        from services.release_readiness_service import top_risks_from_view

        caps: Set[str] = set()
        for risk_text in top_risks_from_view(release_readiness):
            cap = map_text_to_capability(risk_text)
            if cap:
                caps.add(cap)
        return caps
    except Exception as exc:
        logger.debug("servicenow intelligence: release top risks failed: %s", exc)
        return set()


def _dependency_capabilities(incident_report: Optional[ProjectIncidentInvestigationReport]) -> Set[str]:
    if incident_report is None or incident_report.enterprise_dependency_map is None:
        return set()
    caps: Set[str] = set()
    for node in incident_report.enterprise_dependency_map.nodes or []:
        if str(node.risk_level or "").upper() not in {"HIGH", "CRITICAL"}:
            continue
        cap = map_text_to_capability(node.name or "")
        if cap:
            caps.add(cap)
    return caps


def _impacted_module_capabilities(incident_report: Optional[ProjectIncidentInvestigationReport]) -> Set[str]:
    if incident_report is None:
        return set()
    caps: Set[str] = set()
    for mod in incident_report.impacted_modules or []:
        hit = match_module_keyword(mod or "")
        if hit:
            caps.add(hit[1])
        cap = map_text_to_capability(mod or "")
        if cap:
            caps.add(cap)
    for item in incident_report.impacted_modules_ranked or []:
        hit = match_module_keyword(item.module or "")
        if hit:
            caps.add(hit[1])
        cap = map_text_to_capability(item.module or "")
        if cap:
            caps.add(cap)
    return caps


def _coverage_gap_capabilities(coverage: Optional[CoverageIntelligenceReport]) -> Set[str]:
    if coverage is None or not coverage.connected:
        return set()
    caps: Set[str] = set()
    for gap in coverage.gaps or []:
        if str(gap.severity or "").upper() != "CRITICAL":
            continue
        if gap.capability:
            caps.add(gap.capability)
    return caps


def _multi_environment_capabilities(incident_report: Optional[ProjectIncidentInvestigationReport]) -> Set[str]:
    if incident_report is None or incident_report.multi_environment is None:
        return set()
    caps: Set[str] = set()
    for env in incident_report.multi_environment.environments or []:
        if str(env.status or "").upper() not in {"DEGRADED", "BROKEN", "WARNING", "CRITICAL"}:
            continue
        cap = map_text_to_capability(env.name or env.environment_id or "")
        if cap:
            caps.add(cap)
    return caps


def _build_signal_index(
    *,
    incident_report: Optional[ProjectIncidentInvestigationReport],
    release_readiness: Optional[ReleaseReadinessView],
    business_risk_report: Optional[BusinessRiskReport],
    jira_intel: Optional[JiraIssueIntelligenceReport],
    coverage_intelligence: Optional[CoverageIntelligenceReport],
) -> Dict[str, Set[str]]:
    """Map capability -> active intelligence signal names."""
    index: Dict[str, Set[str]] = {}

    def add(capability: str, signal: str) -> None:
        if not capability:
            return
        index.setdefault(capability, set()).add(signal)

    for cap in _business_risk_capabilities(business_risk_report):
        add(cap, "business_risk")
    for cap in _release_pressure_capabilities(release_readiness):
        add(cap, "release_readiness")
    for cap in _jira_pressure_capabilities(jira_intel):
        add(cap, "jira_blockers")
    for cap in _dependency_capabilities(incident_report):
        add(cap, "dependency_map")
    for cap in _impacted_module_capabilities(incident_report):
        add(cap, "impacted_modules")
    for cap in _coverage_gap_capabilities(coverage_intelligence):
        add(cap, "coverage_gap")
    for cap in _multi_environment_capabilities(incident_report):
        add(cap, "multi_environment")

    return index


def _signals_for_capability(capability: str, signal_index: Dict[str, Set[str]]) -> List[str]:
    return sorted(signal_index.get(capability, set()))


def _build_reason(capability: str, signals: List[str]) -> str:
    if not signals:
        return f"Capability match: {capability}"
    joined = ", ".join(s.replace("_", " ") for s in signals)
    return f"{capability} correlates with {joined}"


def _correlate_entity(
    *,
    entity_type: str,
    entity_id: str,
    text: str,
    capability: str,
    signal_index: Dict[str, Set[str]],
    require_keyword: bool = False,
    keywords: Tuple[str, ...] = (),
) -> Optional[ServiceNowCorrelation]:
    if require_keyword and keywords and not _text_has_keywords(text, keywords):
        return None
    if not capability:
        return None
    signals = _signals_for_capability(capability, signal_index)
    if not signals:
        return None
    return ServiceNowCorrelation(
        entity_type=entity_type,
        entity_id=entity_id,
        capability=capability,
        correlation_reason=_build_reason(capability, signals),
        confidence=_confidence_from_signal_count(len(signals)),
    )


def _correlate_cmdb_item(
    item: ServiceNowCI,
    *,
    incident_report: Optional[ProjectIncidentInvestigationReport],
    signal_index: Dict[str, Set[str]],
) -> Optional[ServiceNowCorrelation]:
    text = f"{item.name} {item.class_name}".strip()
    capability = _capability_from_text(text)
    signals: Set[str] = set()
    if capability:
        signals.update(_signals_for_capability(capability, signal_index))

    name_l = (item.name or "").lower()
    if incident_report and incident_report.enterprise_dependency_map is not None:
        for node in incident_report.enterprise_dependency_map.nodes or []:
            node_name = (node.name or "").lower()
            if node_name and (node_name in name_l or name_l in node_name):
                cap = map_text_to_capability(node.name or "") or capability
                if cap:
                    signals.update(_signals_for_capability(cap, signal_index))
                    signals.add("dependency_map")
                    if not capability:
                        capability = cap

    for mod in _impacted_module_capabilities(incident_report):
        mod_l = mod.lower()
        if mod_l and mod_l in name_l:
            signals.update(_signals_for_capability(mod, signal_index))
            signals.add("impacted_modules")
            if not capability:
                capability = mod

    if not capability or not signals:
        return None

    signal_list = sorted(signals)
    return ServiceNowCorrelation(
        entity_type="cmdb",
        entity_id=item.name,
        capability=capability,
        correlation_reason=_build_reason(capability, signal_list),
        confidence=_confidence_from_signal_count(len(signal_list)),
    )


def _sort_correlations(items: List[ServiceNowCorrelation]) -> List[ServiceNowCorrelation]:
    rank = {"HIGH": 3, "MEDIUM": 2, "LOW": 1}
    return sorted(items, key=lambda c: (-rank.get(c.confidence, 0), c.entity_id))


def _build_executive_summary(
    *,
    incident_count: int,
    change_count: int,
    service_count: int,
    cmdb_count: int,
    top_risks: List[str],
) -> str:
    total = incident_count + change_count + service_count + cmdb_count
    if total <= 0:
        return "ServiceNow is connected but no operational correlations were detected against current intelligence."
    parts = [f"{total} ServiceNow operational correlation(s) detected"]
    if incident_count:
        parts.append(f"{incident_count} incident(s)")
    if change_count:
        parts.append(f"{change_count} change(s)")
    if service_count:
        parts.append(f"{service_count} service(s)")
    if cmdb_count:
        parts.append(f"{cmdb_count} CMDB item(s)")
    summary = "; ".join(parts) + "."
    if top_risks:
        summary += f" Top risk: {top_risks[0]}."
    return summary


def _top_operational_risk_lines(
    correlations: List[ServiceNowCorrelation],
    *,
    limit: int = 5,
) -> List[str]:
    lines: List[str] = []
    for item in _sort_correlations(correlations)[:limit]:
        label = item.entity_type.replace("_", " ").title()
        text = f"ServiceNow {label} {item.entity_id}: {item.capability} ({item.confidence} confidence)"
        if text not in lines:
            lines.append(text)
    return lines


def build_servicenow_intelligence_report(
    *,
    project_id: Optional[str] = None,
    incident_report: Optional[ProjectIncidentInvestigationReport] = None,
    release_readiness: Optional[ReleaseReadinessView] = None,
    business_risk_report: Optional[BusinessRiskReport] = None,
    jira_intel: Optional[JiraIssueIntelligenceReport] = None,
    coverage_intelligence: Optional[CoverageIntelligenceReport] = None,
    max_items: int = 50,
) -> ServiceNowIntelligenceReport:
    """Build deterministic ServiceNow intelligence from discovery + existing slices."""
    connection = validate_servicenow_connection()
    if not connection.connected:
        return _empty_report(gap="No ServiceNow connection configured.")

    if incident_report is None and project_id:
        try:
            from services.value_dashboard_service import _load_incident_reports

            reports = _load_incident_reports(project_id)
            incident_report = reports[0] if reports else None
        except Exception as exc:
            logger.debug("servicenow intelligence: incident load failed: %s", exc)

    if release_readiness is None and incident_report is not None:
        release_readiness = incident_report.release_readiness

    if jira_intel is None and incident_report is not None:
        jira_intel = incident_report.jira_issue_intelligence

    if coverage_intelligence is None and incident_report is not None:
        coverage_intelligence = incident_report.coverage_intelligence

    if business_risk_report is None and project_id:
        try:
            from services.business_risk_estimation_service import build_business_risk_report

            business_risk_report = build_business_risk_report(project_id=project_id)
        except Exception as exc:
            logger.debug("servicenow intelligence: business risk load failed: %s", exc)

    signal_index = _build_signal_index(
        incident_report=incident_report,
        release_readiness=release_readiness,
        business_risk_report=business_risk_report,
        jira_intel=jira_intel,
        coverage_intelligence=coverage_intelligence,
    )

    if not signal_index:
        return ServiceNowIntelligenceReport(
            connected=True,
            executive_summary="ServiceNow connected but no intelligence signals are available for correlation.",
            data_gaps=["No business risk, release, Jira, dependency, or coverage pressure signals."],
        )

    incidents_resp = list_incidents(limit=max_items)
    changes_resp = list_changes(limit=max_items)
    services_resp = list_services(limit=max_items)
    cmdb_resp = list_cmdb_items(limit=max_items)

    incident_correlations: List[ServiceNowCorrelation] = []
    for inc in incidents_resp.incidents:
        text = f"{inc.number} {inc.short_description}"
        cap = _capability_from_text(text)
        corr = _correlate_entity(
            entity_type="incident",
            entity_id=inc.number,
            text=text,
            capability=cap or "",
            signal_index=signal_index,
            require_keyword=True,
            keywords=_INCIDENT_KEYWORDS,
        )
        if corr:
            incident_correlations.append(corr)

    change_correlations: List[ServiceNowCorrelation] = []
    for ch in changes_resp.changes:
        text = f"{ch.number} {ch.short_description}"
        cap = _capability_from_text(text)
        corr = _correlate_entity(
            entity_type="change",
            entity_id=ch.number,
            text=text,
            capability=cap or "",
            signal_index=signal_index,
            require_keyword=True,
            keywords=_CHANGE_KEYWORDS,
        )
        if corr:
            change_correlations.append(corr)

    service_correlations: List[ServiceNowCorrelation] = []
    for svc in services_resp.services:
        cap = _capability_from_text(svc.name)
        corr = _correlate_entity(
            entity_type="service",
            entity_id=svc.name,
            text=svc.name,
            capability=cap or "",
            signal_index=signal_index,
        )
        if corr:
            service_correlations.append(corr)

    cmdb_correlations: List[ServiceNowCorrelation] = []
    for ci in cmdb_resp.items:
        corr = _correlate_cmdb_item(ci, incident_report=incident_report, signal_index=signal_index)
        if corr:
            cmdb_correlations.append(corr)

    incident_correlations = _sort_correlations(incident_correlations)
    change_correlations = _sort_correlations(change_correlations)
    service_correlations = _sort_correlations(service_correlations)
    cmdb_correlations = _sort_correlations(cmdb_correlations)

    all_correlations = (
        incident_correlations + change_correlations + service_correlations + cmdb_correlations
    )
    top_risks = _top_operational_risk_lines(all_correlations)
    executive_summary = _build_executive_summary(
        incident_count=len(incident_correlations),
        change_count=len(change_correlations),
        service_count=len(service_correlations),
        cmdb_count=len(cmdb_correlations),
        top_risks=top_risks,
    )

    gaps: List[str] = []
    if not all_correlations:
        gaps.append("No ServiceNow operational correlations detected.")

    return ServiceNowIntelligenceReport(
        connected=True,
        incident_correlations=incident_correlations,
        change_correlations=change_correlations,
        service_correlations=service_correlations,
        cmdb_correlations=cmdb_correlations,
        top_operational_risks=top_risks,
        executive_summary=executive_summary,
        data_gaps=gaps,
    )


def servicenow_executive_risk_lines(
    intel: Optional[ServiceNowIntelligenceReport],
    *,
    limit: int = 3,
) -> List[str]:
    """Convert stored ServiceNow intelligence into executive-friendly risk lines."""
    if intel is None or not intel.connected:
        return []
    return list(intel.top_operational_risks or [])[:limit]


def enrich_executive_quality_top_risks_with_servicenow(
    executive_quality_report: Optional[object],
    servicenow_intel: Optional[ServiceNowIntelligenceReport],
    *,
    risk_cap: int = 5,
) -> None:
    """Post-enrich executive quality top_risks with ServiceNow operational risks."""
    if executive_quality_report is None:
        return
    lines = servicenow_executive_risk_lines(servicenow_intel)
    if not lines:
        return
    merged = list(lines)
    for line in list(getattr(executive_quality_report, "top_risks", None) or []):
        if line not in merged:
            merged.append(line)
    executive_quality_report.top_risks = merged[:risk_cap]
