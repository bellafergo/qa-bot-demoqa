# services/qmetry_coverage_intelligence_service.py
"""
QMETRY-01B — Deterministic QMetry coverage intelligence (read-only).

Correlates existing intelligence slices with QMetry test case discovery.
No AI, embeddings, execution, uploads, or QMetry writes.

Deferred to QMETRY-01C+ (not used for gap severity pressure in QMETRY-01B):
  enterprise_dependency_map, data_journey_validation, contract_risk_assessment,
  deployment_risk_assessment — business risk and release compositors already ingest
  these; coverage reuses their outputs via BusinessRiskReport and release top risks.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Dict, List, Optional, Set, Tuple

from models.business_risk_models import BusinessRiskReport
from models.incident_models import ProjectIncidentInvestigationReport
from models.qmetry_coverage_models import (
    CoverageAssessment,
    CoverageGap,
    CoverageIntelligenceReport,
    CoverageMatch,
    CoverageStatus,
    GapSeverity,
)
from models.qmetry_models import QMetryTestCase
from services.capability_mapping_service import (
    known_capabilities,
    known_module_keywords,
    map_text_to_capability,
    match_module_keyword,
)
from services.qmetry_integration_service import list_test_cases, validate_qmetry_connection

logger = logging.getLogger("vanya.qmetry_coverage_intelligence")

_BLOCKED_RELEASE_STATUSES = frozenset({"BLOCKED", "NO_GO"})
_BUSINESS_RISK_SEVERITIES = frozenset({"HIGH", "CRITICAL"})


def _coverage_status(matched_count: int) -> CoverageStatus:
    if matched_count <= 0:
        return "NONE"
    if matched_count == 1:
        return "WEAK"
    if matched_count <= 4:
        return "MODERATE"
    return "STRONG"


def _match_test_case(test_case: QMetryTestCase) -> Optional[Tuple[str, str, str]]:
    """Return (module, capability, reason) when test case name matches a module keyword."""
    return match_module_keyword(test_case.name or test_case.test_case_id or "")


def _jira_pressure_capabilities(incident_report: Optional[ProjectIncidentInvestigationReport]) -> Set[str]:
    pressured: Set[str] = set()
    if not incident_report or not incident_report.jira_issue_intelligence:
        return pressured
    for blocker in incident_report.jira_issue_intelligence.top_blockers or []:
        for text in (blocker.related_module, blocker.summary, blocker.issue_key):
            cap = map_text_to_capability(text or "")
            if cap:
                pressured.add(cap)
    return pressured


def _release_blocked_pressure_capabilities(
    release_readiness: Optional[object],
) -> Set[str]:
    """When release is blocked, pressure only capabilities named in release top risks."""
    if release_readiness is None:
        return set()
    status = str(getattr(release_readiness, "overall_status", "") or "").upper()
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
        logger.debug("qmetry coverage: release top risks failed: %s", exc)
        return set()


def _capabilities_from_business_risk_report(report: Optional[BusinessRiskReport]) -> Set[str]:
    if report is None:
        return set()
    caps: Set[str] = set()
    for risk in report.business_risks or []:
        if str(risk.severity or "").upper() in _BUSINESS_RISK_SEVERITIES:
            caps.add(risk.capability)
    for cap in report.top_capabilities_at_risk or []:
        caps.add(cap)
    return caps


def _business_risk_capabilities(
    project_id: Optional[str],
    *,
    business_risk_report: Optional[BusinessRiskReport] = None,
) -> Set[str]:
    if business_risk_report is not None:
        return _capabilities_from_business_risk_report(business_risk_report)
    if not project_id:
        return set()
    try:
        from services.business_risk_estimation_service import build_business_risk_report

        return _capabilities_from_business_risk_report(build_business_risk_report(project_id=project_id))
    except Exception as exc:
        logger.debug("qmetry coverage: business risk lookup failed: %s", exc)
        return set()


def _collect_pressure_capabilities(
    *,
    incident_report: Optional[ProjectIncidentInvestigationReport],
    release_readiness: Optional[object],
    business_risk_capabilities: Set[str],
    coverage_match_capabilities: Set[str],
) -> Set[str]:
    """Capabilities under business/release/jira/coverage pressure for gap severity."""
    pressured: Set[str] = set(business_risk_capabilities)
    pressured.update(_jira_pressure_capabilities(incident_report))
    pressured.update(_release_blocked_pressure_capabilities(release_readiness))
    pressured.update(coverage_match_capabilities)
    return pressured


def _build_executive_summary(
    *,
    assessments: List[CoverageAssessment],
    gaps: List[CoverageGap],
    pressured: Set[str],
    jira_blocker_count: int,
) -> str:
    weak = [a for a in assessments if a.coverage_status in ("WEAK", "NONE")]
    if not weak and not gaps:
        return "All tracked business capabilities have moderate or strong QMetry test coverage."

    target = weak[0] if weak else None
    if target is None and gaps:
        target = CoverageAssessment(
            capability=gaps[0].capability,
            matched_tests=0,
            total_tests=0,
            coverage_status="NONE",
        )

    if target is None:
        return "QMetry test cases were discovered but no capability-level coverage mapping was produced."

    status_phrase = {
        "NONE": "no test coverage",
        "WEAK": "weak test coverage",
        "MODERATE": "moderate test coverage",
        "STRONG": "strong test coverage",
    }.get(target.coverage_status, "limited test coverage")

    parts = [
        f"{target.capability} has {status_phrase}",
        f"({target.matched_tests} matching test case{'s' if target.matched_tests != 1 else ''})",
    ]

    critical_gaps = [g for g in gaps if g.severity == "CRITICAL"]
    if jira_blocker_count > 0 and target.capability in pressured:
        parts.append(
            f"Open Jira blockers are associated with a capability that currently has "
            f"only {target.matched_tests} matching test case{'s' if target.matched_tests != 1 else ''}"
        )
    elif critical_gaps:
        parts.append(
            f"{len(critical_gaps)} critical coverage gap{'s' if len(critical_gaps) != 1 else ''} "
            "overlap with business or release risk signals"
        )

    return ". ".join(parts) + "."


def build_coverage_intelligence_report(
    *,
    project_id: Optional[str] = None,
    incident_report: Optional[ProjectIncidentInvestigationReport] = None,
    release_readiness: Optional[object] = None,
    business_risk_report: Optional[BusinessRiskReport] = None,
) -> CoverageIntelligenceReport:
    """
    Build deterministic coverage intelligence from QMetry discovery and existing slices.

    Reuses jira_issue_intelligence, release_readiness, and business_risk narratives only.
    Pass business_risk_report when already computed to avoid redundant recomputation.
    """
    now = datetime.now(timezone.utc)
    data_gaps: List[str] = []

    connection = validate_qmetry_connection()
    if not connection.connected:
        data_gaps.append("No QMetry connection configured.")
        return CoverageIntelligenceReport(
            generated_at=now,
            connected=False,
            executive_summary="QMetry is not connected — coverage intelligence unavailable.",
            data_gaps=data_gaps,
        )

    test_cases_resp = list_test_cases(max_results=100)
    test_cases = test_cases_resp.test_cases
    if not test_cases:
        data_gaps.append("No QMetry test cases discovered.")
        return CoverageIntelligenceReport(
            generated_at=now,
            connected=True,
            total_test_cases=0,
            executive_summary="QMetry is connected but no test cases were discovered.",
            data_gaps=data_gaps,
        )

    if incident_report is None and project_id:
        try:
            from services.value_dashboard_service import _load_incident_reports

            reports = _load_incident_reports(project_id)
            incident_report = reports[0] if reports else None
        except Exception as exc:
            logger.debug("qmetry coverage: incident load failed: %s", exc)

    if release_readiness is None and incident_report is not None:
        release_readiness = incident_report.release_readiness

    matches: List[CoverageMatch] = []
    capability_counts: Dict[str, int] = {cap: 0 for cap in known_capabilities()}
    coverage_match_capabilities: Set[str] = set()

    for tc in test_cases:
        hit = _match_test_case(tc)
        if not hit:
            continue
        module, capability, reason = hit
        capability_counts[capability] = capability_counts.get(capability, 0) + 1
        coverage_match_capabilities.add(capability)
        matches.append(
            CoverageMatch(
                test_case_id=tc.test_case_id,
                test_case_name=tc.name,
                matched_module=module,
                matched_capability=capability,
                match_reason=reason,
            )
        )

    assessments = [
        CoverageAssessment(
            capability=cap,
            total_tests=len(test_cases),
            matched_tests=capability_counts.get(cap, 0),
            coverage_status=_coverage_status(capability_counts.get(cap, 0)),
        )
        for cap in known_capabilities()
    ]

    business_pressure = _business_risk_capabilities(
        project_id,
        business_risk_report=business_risk_report,
    )
    pressured = _collect_pressure_capabilities(
        incident_report=incident_report,
        release_readiness=release_readiness,
        business_risk_capabilities=business_pressure,
        coverage_match_capabilities=coverage_match_capabilities,
    )

    gaps: List[CoverageGap] = []
    for assessment in assessments:
        if assessment.matched_tests > 0:
            continue
        severity: GapSeverity = "CRITICAL" if assessment.capability in pressured else "MEDIUM"
        reason = (
            f"No QMetry test cases match {assessment.capability}; "
            f"capability is implicated in business risk, Jira blockers, or release readiness."
            if severity == "CRITICAL"
            else f"No QMetry test cases match {assessment.capability}."
        )
        module = next(
            (rule[1] for rule in known_module_keywords() if rule[2] == assessment.capability),
            None,
        )
        gaps.append(
            CoverageGap(
                capability=assessment.capability,
                module=module,
                severity=severity,
                reason=reason,
            )
        )

    jira_blocker_count = 0
    if incident_report and incident_report.jira_issue_intelligence:
        jira_blocker_count = incident_report.jira_issue_intelligence.blocker_count or 0

    if not matches:
        data_gaps.append("No coverage matches found for known business capability keywords.")

    executive_summary = _build_executive_summary(
        assessments=assessments,
        gaps=gaps,
        pressured=pressured,
        jira_blocker_count=jira_blocker_count,
    )

    return CoverageIntelligenceReport(
        generated_at=now,
        connected=True,
        total_test_cases=len(test_cases),
        total_matches=len(matches),
        coverage_matches=matches,
        coverage_assessments=assessments,
        coverage_gaps=gaps,
        executive_summary=executive_summary,
        data_gaps=data_gaps,
    )
