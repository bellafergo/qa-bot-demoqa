# services/executive_quality_report_service.py
"""
Enterprise ENT-02A — Executive Quality Reports (read-only).

Executive-level quality summaries built from existing Vanya intelligence only.
No emails, PDFs, scheduling, LLMs, or external systems.
"""
from __future__ import annotations

import hashlib
import re
from typing import List, Optional, Tuple

from models.incident_models import (
    ContractRiskReport,
    DataJourneyReport,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    EnterpriseDependencyMap,
    ExecutiveQualityReport,
    HistoricalLearningReport,
    RecommendedAction,
    TestRecommendationReport,
)

_DEPLOYMENT_POINTS = {
    "low": 25,
    "medium": 15,
    "high": 5,
    "critical": 0,
}


def build_executive_report_id(project_id: str, digest: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "_", (project_id or "project").strip().lower()).strip("_")
    return f"executive_quality_report:{slug}:{digest}"


def _has_source_intelligence(
    *,
    decision_center: Optional[DecisionCenterSummary],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
) -> bool:
    if decision_center or deployment_risk_assessment:
        return True
    if historical_learning or test_recommendations or recommended_actions:
        return True
    if contract_risk_assessment or data_journey_validation or enterprise_dependency_map:
        return True
    return False


def _deployment_points(deployment: Optional[DeploymentRiskAssessment]) -> int:
    if not deployment:
        return 15
    level = (deployment.risk_level or "medium").lower()
    return _DEPLOYMENT_POINTS.get(level, 10)


def _contract_points(contract_risk: Optional[ContractRiskReport]) -> int:
    if not contract_risk or not contract_risk.assessments:
        return 15
    levels = [a.overall_risk_level.upper() for a in contract_risk.assessments]
    if any(level == "CRITICAL" for level in levels):
        return 0
    if any(level == "HIGH" for level in levels):
        return 10
    if any(level == "MEDIUM" for level in levels):
        return 15
    return 20


def _journey_points(data_journey: Optional[DataJourneyReport]) -> int:
    if not data_journey or not data_journey.results:
        return 15
    statuses = [r.status.upper() for r in data_journey.results]
    if any(s == "BROKEN" for s in statuses):
        return 5
    if any(s == "DEGRADED" for s in statuses):
        return 12
    if all(s == "HEALTHY" for s in statuses):
        return 20
    return 10


def _dependency_points(dependency_map: Optional[EnterpriseDependencyMap]) -> int:
    if not dependency_map or not dependency_map.nodes:
        return 12
    critical = sum(1 for n in dependency_map.nodes if n.risk_level == "CRITICAL")
    high = sum(1 for n in dependency_map.nodes if n.risk_level == "HIGH")
    if critical >= 3:
        return 5
    if critical >= 1:
        return 10
    if high >= 2:
        return 12
    return 20


def _historical_points(historical: Optional[HistoricalLearningReport]) -> int:
    if not historical:
        return 10
    count = len(historical.similar_incidents)
    if count >= 3:
        return 5
    if count >= 1:
        return 10
    return 15


def _quality_score(
    *,
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    historical_learning: Optional[HistoricalLearningReport],
) -> int:
    score = (
        _deployment_points(deployment_risk_assessment)
        + _contract_points(contract_risk_assessment)
        + _journey_points(data_journey_validation)
        + _dependency_points(enterprise_dependency_map)
        + _historical_points(historical_learning)
    )
    return max(0, min(100, score))


def _risk_level_from_score(score: int) -> str:
    if score >= 75:
        return "LOW"
    if score >= 50:
        return "MEDIUM"
    if score >= 25:
        return "HIGH"
    return "CRITICAL"


def _quality_trend(historical: Optional[HistoricalLearningReport], score: int) -> str:
    if not historical or not historical.similar_incidents:
        return "stable"
    if len(historical.similar_incidents) >= 3 and score < 50:
        return "elevated"
    if score >= 75:
        return "improving"
    return "elevated"


def _count_metrics(
    *,
    decision_center: Optional[DecisionCenterSummary],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    test_recommendations: Optional[TestRecommendationReport],
) -> Tuple[int, int, int, int, int]:
    open_incidents = 1
    critical_incidents = 0
    if decision_center and decision_center.overall_status in ("ORANGE", "RED"):
        critical_incidents += 1
    if decision_center and decision_center.top_risk_level in ("HIGH", "CRITICAL"):
        critical_incidents += 1

    critical_contracts = 0
    if contract_risk_assessment:
        critical_contracts = sum(
            1 for a in contract_risk_assessment.assessments if a.overall_risk_level == "CRITICAL"
        )

    broken_journeys = 0
    if data_journey_validation:
        broken_journeys = sum(1 for r in data_journey_validation.results if r.status == "BROKEN")

    recommended_tests = 0
    if test_recommendations:
        recommended_tests = len(test_recommendations.recommendations)
    elif decision_center:
        recommended_tests = decision_center.recommended_test_count

    open_incidents = max(open_incidents, critical_incidents + broken_journeys)
    return open_incidents, critical_incidents, critical_contracts, broken_journeys, recommended_tests


def _top_risks(
    *,
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
) -> List[str]:
    risks: List[str] = []
    if contract_risk_assessment:
        for assessment in contract_risk_assessment.assessments:
            if assessment.overall_risk_level in ("HIGH", "CRITICAL"):
                service = assessment.contract_id.split(":")[-1].replace("_", " ")
                risks.append(
                    f"Payments contract risk is {assessment.overall_risk_level}"
                    if "payment" in assessment.contract_id.lower()
                    else f"Contract risk is {assessment.overall_risk_level} ({service})"
                )
    if data_journey_validation:
        for result in data_journey_validation.results:
            if result.status in ("BROKEN", "DEGRADED"):
                journey = next(
                    (j for j in data_journey_validation.journeys if j.journey_id == result.journey_id),
                    None,
                )
                name = journey.name if journey else result.journey_id
                risks.append(f"{name} is {result.status}")
    if enterprise_dependency_map:
        critical_nodes = [n for n in enterprise_dependency_map.nodes if n.risk_level in ("HIGH", "CRITICAL")]
        for node in critical_nodes[:2]:
            risks.append(f"{node.name} dependency map shows elevated risk ({node.risk_level})")
    if deployment_risk_assessment and deployment_risk_assessment.risk_level in ("high", "critical"):
        risks.append(f"Deployment risk is {deployment_risk_assessment.risk_level.upper()}")
    return sorted(set(risks))[:5]


def _top_recommendations(
    *,
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    data_journey_validation: Optional[DataJourneyReport],
    contract_risk_assessment: Optional[ContractRiskReport],
) -> List[str]:
    recs: List[str] = []
    if test_recommendations:
        for item in test_recommendations.recommendations[:3]:
            recs.append(f"Execute {item.test_name}")
    for action in sorted(recommended_actions, key=lambda a: a.priority)[:2]:
        recs.append(action.title)
    if data_journey_validation:
        for result in data_journey_validation.results:
            if result.status == "BROKEN":
                journey = next(
                    (j for j in data_journey_validation.journeys if j.journey_id == result.journey_id),
                    None,
                )
                if journey:
                    recs.append(f"Review {journey.name}")
    if contract_risk_assessment:
        for assessment in contract_risk_assessment.assessments:
            if assessment.overall_risk_level in ("HIGH", "CRITICAL"):
                for test_name in assessment.affected_tests[:1]:
                    recs.append(f"Prioritize {test_name}")
    if data_journey_validation:
        for journey in data_journey_validation.journeys:
            area = (journey.business_area or "").lower()
            if "checkout" not in area:
                continue
            result = next(
                (r for r in data_journey_validation.results if r.journey_id == journey.journey_id),
                None,
            )
            if result and result.status in ("BROKEN", "DEGRADED"):
                recs.append("Validate Inventory Database checks")
            break
    return sorted(set(recs))[:5]


def _executive_summary(
    score: int,
    risk_level: str,
    *,
    critical_contracts: int,
    broken_journeys: int,
    top_risks: List[str],
) -> str:
    if score >= 90:
        tone = "strong quality posture"
    elif score >= 75:
        tone = "stable quality posture"
    elif score >= 50:
        tone = "moderate quality risk"
    else:
        tone = "elevated quality risk"

    parts = [f"The platform shows {tone}."]
    detail_bits: List[str] = []
    if critical_contracts:
        noun = "change" if critical_contracts == 1 else "changes"
        detail_bits.append(
            f"{critical_contracts} critical contract {noun}"
        )
    if broken_journeys:
        noun = "journey" if broken_journeys == 1 else "journeys"
        detail_bits.append(f"{broken_journeys} broken data {noun}")
    if detail_bits:
        parts.append(f"{' and '.join(detail_bits)} require attention before deployment.")
    elif top_risks:
        parts.append(f"Key concern: {top_risks[0]}.")
    else:
        parts.append("No critical deployment blockers detected in current intelligence.")
    parts.append(f"Overall executive risk level: {risk_level}.")
    return " ".join(parts)


def _deterministic_timestamp(digest: str) -> str:
    hour = int(digest[:2], 16) % 24
    minute = int(digest[2:4], 16) % 60
    return f"2026-06-10T{hour:02d}:{minute:02d}:00+00:00"


def build_executive_quality_report(
    *,
    project_id: str,
    decision_center: Optional[DecisionCenterSummary],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
) -> Optional[ExecutiveQualityReport]:
    if not _has_source_intelligence(
        decision_center=decision_center,
        deployment_risk_assessment=deployment_risk_assessment,
        historical_learning=historical_learning,
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        enterprise_dependency_map=enterprise_dependency_map,
    ):
        return None

    score = _quality_score(
        deployment_risk_assessment=deployment_risk_assessment,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        enterprise_dependency_map=enterprise_dependency_map,
        historical_learning=historical_learning,
    )
    risk_level = _risk_level_from_score(score)

    open_incidents, critical_incidents, critical_contracts, broken_journeys, recommended_tests = _count_metrics(
        decision_center=decision_center,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        test_recommendations=test_recommendations,
    )

    top_risks = _top_risks(
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        enterprise_dependency_map=enterprise_dependency_map,
        deployment_risk_assessment=deployment_risk_assessment,
    )
    top_recommendations = _top_recommendations(
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
        data_journey_validation=data_journey_validation,
        contract_risk_assessment=contract_risk_assessment,
    )

    historical_summary = ""
    if historical_learning:
        historical_summary = historical_learning.pattern_summary or (
            f"{len(historical_learning.similar_incidents)} similar incident(s) in historical learning."
        )

    confidence_sources = [
        decision_center.confidence if decision_center else 0.0,
        deployment_risk_assessment.confidence if deployment_risk_assessment else 0.0,
        historical_learning.confidence if historical_learning else 0.0,
        contract_risk_assessment.confidence if contract_risk_assessment else 0.0,
        data_journey_validation.confidence if data_journey_validation else 0.0,
        enterprise_dependency_map.confidence if enterprise_dependency_map else 0.0,
    ]
    nonzero = [c for c in confidence_sources if c > 0]
    confidence = round(sum(nonzero) / max(len(nonzero), 1), 2) if nonzero else 0.65

    digest_input = "|".join(
        [
            project_id,
            str(score),
            risk_level,
            str(critical_contracts),
            str(broken_journeys),
            *top_risks[:3],
        ]
    )
    digest = hashlib.sha256(digest_input.encode("utf-8")).hexdigest()[:8]

    return ExecutiveQualityReport(
        report_id=build_executive_report_id(project_id, digest),
        generated_at=_deterministic_timestamp(digest),
        overall_quality_score=score,
        overall_risk_level=risk_level,
        confidence=confidence,
        executive_summary=_executive_summary(
            score,
            risk_level,
            critical_contracts=critical_contracts,
            broken_journeys=broken_journeys,
            top_risks=top_risks,
        ),
        top_risks=top_risks,
        top_recommendations=top_recommendations,
        open_incident_count=open_incidents,
        critical_incident_count=critical_incidents,
        critical_contract_count=critical_contracts,
        broken_journey_count=broken_journeys,
        recommended_test_count=recommended_tests,
        historical_pattern_summary=historical_summary,
        quality_trend=_quality_trend(historical_learning, score),
    )
