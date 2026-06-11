# services/qmetry_test_recommendation_service.py
"""
QMETRY-01C — Deterministic QMetry test recommendation correlation (read-only).

Connects release risk, Jira blockers, business capabilities, and coverage intelligence
to specific QMetry test cases. No AI, execution, uploads, or QMetry writes.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Dict, List, Optional, Set, Tuple

from models.business_risk_models import BusinessRiskReport
from models.incident_models import ProjectIncidentInvestigationReport
from models.qmetry_coverage_models import CoverageIntelligenceReport
from models.qmetry_recommendation_models import (
    QMetryRecommendationReport,
    RecommendationGroup,
    RecommendationPriority,
    RecommendedTestCase,
)
from services.capability_mapping_service import map_text_to_capability
from services.qmetry_integration_service import validate_qmetry_connection

logger = logging.getLogger("vanya.qmetry_test_recommendation")

_BLOCKED_RELEASE_STATUSES = frozenset({"BLOCKED", "NO_GO"})
_PRIORITY_RANK = {"CRITICAL": 0, "HIGH": 1, "MEDIUM": 2}


def _jira_blocker_capabilities(incident_report: Optional[ProjectIncidentInvestigationReport]) -> Set[str]:
    caps: Set[str] = set()
    if not incident_report or not incident_report.jira_issue_intelligence:
        return caps
    for blocker in incident_report.jira_issue_intelligence.top_blockers or []:
        for text in (blocker.related_module, blocker.summary, blocker.issue_key):
            cap = map_text_to_capability(text or "")
            if cap:
                caps.add(cap)
    return caps


def _blocked_release_capabilities(release_readiness: Optional[object]) -> Set[str]:
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
        logger.debug("qmetry recommendations: release top risks failed: %s", exc)
        return set()


def _critical_capabilities(
    *,
    business_risk_report: Optional[BusinessRiskReport],
    incident_report: Optional[ProjectIncidentInvestigationReport],
    release_readiness: Optional[object],
) -> Set[str]:
    caps: Set[str] = set()
    if business_risk_report:
        for risk in business_risk_report.business_risks or []:
            if str(risk.severity or "").upper() == "CRITICAL":
                caps.add(risk.capability)
    caps.update(_jira_blocker_capabilities(incident_report))
    caps.update(_blocked_release_capabilities(release_readiness))
    return caps


def _high_capabilities(
    *,
    business_risk_report: Optional[BusinessRiskReport],
    coverage: CoverageIntelligenceReport,
) -> Set[str]:
    caps: Set[str] = set()
    if business_risk_report:
        for risk in business_risk_report.business_risks or []:
            if str(risk.severity or "").upper() == "HIGH":
                caps.add(risk.capability)
    for gap in coverage.coverage_gaps or []:
        caps.add(gap.capability)
    return caps


def _medium_capabilities(
    coverage: CoverageIntelligenceReport,
    *,
    critical: Set[str],
    high: Set[str],
) -> Set[str]:
    caps: Set[str] = set()
    for assessment in coverage.coverage_assessments or []:
        if assessment.matched_tests > 0:
            caps.add(assessment.capability)
    return caps - critical - high


def _capability_priority(
    capability: str,
    *,
    critical: Set[str],
    high: Set[str],
    medium: Set[str],
) -> Optional[RecommendationPriority]:
    if capability in critical:
        return "CRITICAL"
    if capability in high:
        return "HIGH"
    if capability in medium:
        return "MEDIUM"
    return None


def _recommendation_reason(
    capability: str,
    priority: RecommendationPriority,
    *,
    jira_caps: Set[str],
    coverage: CoverageIntelligenceReport,
    has_high_business_risk: bool,
) -> str:
    parts: List[str] = []
    if priority == "CRITICAL":
        if capability in jira_caps:
            parts.append("open Jira blocker overlap")
        parts.append("critical business or release risk")
    elif priority == "HIGH":
        if has_high_business_risk:
            parts.append("high business risk")
        if any(g.capability == capability for g in coverage.coverage_gaps or []):
            parts.append("coverage gap")
        if not parts:
            parts.append("elevated capability risk")
    else:
        assessment = next(
            (a for a in coverage.coverage_assessments or [] if a.capability == capability),
            None,
        )
        status = (assessment.coverage_status if assessment else "MODERATE").lower()
        parts.append(f"{status} coverage")
    return " and ".join(parts)


def _build_executive_summary(
    *,
    groups: List[RecommendationGroup],
    focus_capability: Optional[str],
    jira_caps: Set[str],
    coverage: CoverageIntelligenceReport,
) -> str:
    if not groups:
        return "No QMetry test cases are recommended for review based on current intelligence."

    total = sum(len(g.recommended_tests) for g in groups)
    cap = focus_capability or groups[0].capability
    assessment = next((a for a in coverage.coverage_assessments or [] if a.capability == cap), None)
    weak = assessment and assessment.coverage_status in ("WEAK", "NONE")

    parts = [f"{cap} is associated with"]
    detail: List[str] = []
    if cap in jira_caps:
        detail.append("open Jira blockers")
    if weak:
        detail.append("weak coverage")
    if not detail:
        detail.append("current risk and coverage signals")
    parts.append(" and ".join(detail))
    parts.append(f". {total} QMetry test case{'s' if total != 1 else ''} are recommended for review.")
    return "".join(parts)


def build_qmetry_recommendation_report(
    *,
    project_id: Optional[str] = None,
    incident_report: Optional[ProjectIncidentInvestigationReport] = None,
    release_readiness: Optional[object] = None,
    business_risk_report: Optional[BusinessRiskReport] = None,
    coverage_intelligence: Optional[CoverageIntelligenceReport] = None,
) -> QMetryRecommendationReport:
    """Build deterministic test recommendations from coverage and existing intelligence."""
    now = datetime.now(timezone.utc)
    data_gaps: List[str] = []

    connection = validate_qmetry_connection()
    if not connection.connected:
        data_gaps.append("No QMetry connection configured.")
        return QMetryRecommendationReport(
            generated_at=now,
            connected=False,
            executive_summary="QMetry is not connected — test recommendations unavailable.",
            data_gaps=data_gaps,
        )

    if incident_report is None and project_id:
        try:
            from services.value_dashboard_service import _load_incident_reports

            reports = _load_incident_reports(project_id)
            incident_report = reports[0] if reports else None
        except Exception as exc:
            logger.debug("qmetry recommendations: incident load failed: %s", exc)

    if release_readiness is None and incident_report is not None:
        release_readiness = getattr(incident_report, "release_readiness", None)

    if business_risk_report is None and project_id:
        try:
            from services.business_risk_estimation_service import build_business_risk_report

            business_risk_report = build_business_risk_report(project_id=project_id)
        except Exception as exc:
            logger.debug("qmetry recommendations: business risk failed: %s", exc)

    if coverage_intelligence is None:
        from services.qmetry_coverage_intelligence_service import build_coverage_intelligence_report

        coverage_intelligence = build_coverage_intelligence_report(
            project_id=project_id,
            incident_report=incident_report,
            release_readiness=release_readiness,
            business_risk_report=business_risk_report,
        )

    coverage = coverage_intelligence
    if not coverage.connected:
        data_gaps.append("No QMetry connection configured.")
        return QMetryRecommendationReport(
            generated_at=now,
            connected=False,
            executive_summary="QMetry is not connected — test recommendations unavailable.",
            data_gaps=data_gaps,
        )

    if coverage.total_test_cases == 0:
        data_gaps.append("No QMetry test cases discovered.")
        return QMetryRecommendationReport(
            generated_at=now,
            connected=True,
            executive_summary="QMetry is connected but no test cases were discovered.",
            data_gaps=data_gaps,
        )

    if not coverage.coverage_matches:
        data_gaps.append("No coverage intelligence available for recommendation correlation.")
        return QMetryRecommendationReport(
            generated_at=now,
            connected=True,
            executive_summary="Coverage intelligence is unavailable — no test recommendations generated.",
            data_gaps=data_gaps,
        )

    jira_caps = _jira_blocker_capabilities(incident_report)
    critical = _critical_capabilities(
        business_risk_report=business_risk_report,
        incident_report=incident_report,
        release_readiness=release_readiness,
    )
    high = _high_capabilities(business_risk_report=business_risk_report, coverage=coverage)
    medium = _medium_capabilities(coverage, critical=critical, high=high)

    high_business_caps: Set[str] = set()
    if business_risk_report:
        for risk in business_risk_report.business_risks or []:
            if str(risk.severity or "").upper() == "HIGH":
                high_business_caps.add(risk.capability)

    by_test: Dict[str, RecommendedTestCase] = {}
    for match in coverage.coverage_matches:
        priority = _capability_priority(
            match.matched_capability,
            critical=critical,
            high=high,
            medium=medium,
        )
        if priority is None:
            continue
        reason = _recommendation_reason(
            match.matched_capability,
            priority,
            jira_caps=jira_caps,
            coverage=coverage,
            has_high_business_risk=match.matched_capability in high_business_caps,
        )
        existing = by_test.get(match.test_case_id)
        candidate = RecommendedTestCase(
            test_case_id=match.test_case_id,
            test_case_name=match.test_case_name,
            capability=match.matched_capability,
            recommendation_reason=reason,
            priority=priority,
        )
        if existing is None or _PRIORITY_RANK[priority] < _PRIORITY_RANK[existing.priority]:
            by_test[match.test_case_id] = candidate

    grouped: Dict[str, List[RecommendedTestCase]] = {}
    for rec in by_test.values():
        grouped.setdefault(rec.capability, []).append(rec)

    groups: List[RecommendationGroup] = []
    for capability in sorted(grouped.keys()):
        tests = sorted(
            grouped[capability],
            key=lambda t: (_PRIORITY_RANK[t.priority], t.test_case_name),
        )
        groups.append(RecommendationGroup(capability=capability, recommended_tests=tests))

    groups.sort(
        key=lambda g: (
            min(_PRIORITY_RANK[t.priority] for t in g.recommended_tests),
            g.capability,
        )
    )

    total = len(by_test)
    if total == 0:
        data_gaps.append("No recommendations matched current capability priority rules.")

    focus = None
    if groups:
        focus = groups[0].capability

    executive_summary = _build_executive_summary(
        groups=groups,
        focus_capability=focus,
        jira_caps=jira_caps,
        coverage=coverage,
    )

    return QMetryRecommendationReport(
        generated_at=now,
        connected=True,
        total_recommendations=total,
        recommendation_groups=groups,
        executive_summary=executive_summary,
        data_gaps=data_gaps,
    )
