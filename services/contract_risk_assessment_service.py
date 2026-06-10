# services/contract_risk_assessment_service.py
"""
Integration Intelligence INT-02B — Contract Risk Assessment (read-only).

Transforms API contract changes into business-impact risk intelligence using
existing incident signals only. No live APIs, network calls, or mutations.
"""
from __future__ import annotations

import hashlib
import re
from typing import Dict, List, Optional, Set

from models.incident_models import (
    ApiContract,
    ApiContractReport,
    ContractChange,
    ContractRiskAssessment,
    ContractRiskFactor,
    ContractRiskReport,
    DataJourneyReport,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    HistoricalLearningReport,
    IncidentImpactNode,
    RecommendedTest,
    TestRecommendationReport,
)

_BASE_SEVERITY: Dict[str, int] = {
    "added_field": 10,
    "removed_field": 50,
    "rename": 60,
    "type_change": 80,
}

_MULTIPLIER_POINTS = {
    "journey_impact": 10,
    "historical_incidents": 10,
    "critical_module": 15,
    "high_deployment_risk": 15,
    "recommended_tests": 5,
}

_RISK_LEVELS = (
    (24, "LOW"),
    (49, "MEDIUM"),
    (74, "HIGH"),
    (100, "CRITICAL"),
)

_JOURNEY_LABELS = {
    "checkout": "Checkout",
    "payments": "Payments",
    "authentication": "Authentication",
}

_AREA_SUFFIXES = (" flow", " smoke", " suite", " regression", " ui", " module")


def build_business_assessment_id(contract_id: str) -> str:
    return f"business_risk_assessment:{contract_id}"


def build_factor_id(contract_id: str, suffix: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "_", (suffix or "factor").strip().lower()).strip("_")
    return f"factor:{contract_id}:{slug}"


def _normalize_area(name: str) -> str:
    s = (name or "").strip().lower()
    for suffix in _AREA_SUFFIXES:
        if s.endswith(suffix):
            s = s[: -len(suffix)].strip()
    return re.sub(r"\s+", " ", s) or ""


def _area_category(area: str) -> str:
    key = _normalize_area(area)
    if any(k in key for k in ("checkout", "cart", "order")):
        return "checkout"
    if any(k in key for k in ("payment", "payments", "billing")):
        return "payments"
    if any(k in key for k in ("auth", "authentication", "login", "session")):
        return "authentication"
    if any(k in key for k in ("inventory", "stock")):
        return "inventory"
    if any(k in key for k in ("order", "orders")):
        return "orders"
    return key or "general"


def _contract_categories(contract: ApiContract) -> Set[str]:
    blob = f"{contract.service_name} {contract.endpoint}".lower()
    categories: Set[str] = set()
    if any(k in blob for k in ("payment", "billing")):
        categories.add("payments")
    if any(k in blob for k in ("checkout", "cart", "order")):
        categories.add("checkout")
    if any(k in blob for k in ("auth", "login", "session")):
        categories.add("authentication")
    if not categories:
        categories.add("general")
    return categories


def _field_categories(field_name: str) -> Set[str]:
    blob = (field_name or "").lower()
    categories: Set[str] = set()
    if any(k in blob for k in ("amount", "total", "payment", "card", "billing")):
        categories.update({"payments", "checkout"})
    if any(k in blob for k in ("order", "cart", "checkout")):
        categories.add("checkout")
    if any(k in blob for k in ("inventory", "stock", "sku")):
        categories.add("inventory")
    if any(k in blob for k in ("auth", "token", "session", "password", "login")):
        categories.add("authentication")
    return categories


def _risk_level_from_score(score: int) -> str:
    for upper, level in _RISK_LEVELS:
        if score <= upper:
            return level
    return "CRITICAL"


def _base_score(changes: List[ContractChange]) -> Tuple[int, List[ContractRiskFactor]]:
    if not changes:
        return 0, []
    factors: List[ContractRiskFactor] = []
    max_score = 0
    for change in sorted(changes, key=lambda c: (c.change_type, c.field_name)):
        points = _BASE_SEVERITY.get(change.change_type, 10)
        max_score = max(max_score, points)
        factors.append(
            ContractRiskFactor(
                factor_id=build_factor_id("", f"change_{change.change_type}_{change.field_name}"),
                title=f"Contract change: {change.change_type.replace('_', ' ')}",
                description=change.description or f"Field '{change.field_name}' changed.",
                severity=change.severity or "LOW",
                weight=float(points),
            )
        )
    return max_score, factors


def _affected_journeys(
    contract: ApiContract,
    changes: List[ContractChange],
    data_journey_validation: Optional[DataJourneyReport],
    impact_map: List[IncidentImpactNode],
) -> List[str]:
    categories: Set[str] = set(_contract_categories(contract))
    for change in changes:
        categories.update(_field_categories(change.field_name))

    labels: Set[str] = set()
    for key in ("checkout", "payments", "authentication"):
        if key in categories and any(_field_categories(c.field_name) for c in changes):
            labels.add(_JOURNEY_LABELS[key])

    if data_journey_validation:
        for journey in data_journey_validation.journeys:
            area = _area_category(journey.business_area or journey.name)
            if area in categories:
                labels.add(_JOURNEY_LABELS.get(area, journey.name))

    for node in impact_map:
        area = _area_category(node.title)
        if area in categories:
            labels.add(_JOURNEY_LABELS.get(area, node.title))

    return sorted(labels)


def _affected_modules(
    contract: ApiContract,
    changes: List[ContractChange],
    impact_map: List[IncidentImpactNode],
) -> List[str]:
    categories = set(_contract_categories(contract))
    for change in changes:
        categories.update(_field_categories(change.field_name))

    modules: Set[str] = set()
    for node in impact_map:
        title = (node.title or "").strip()
        if not title:
            continue
        area = _area_category(title)
        if area in categories or any(cat in area for cat in categories):
            modules.add(title)

    if not modules:
        for cat in sorted(categories):
            if cat == "general":
                continue
            modules.add(cat.title())

    return sorted(modules)


def _affected_tests(
    contract: ApiContract,
    changes: List[ContractChange],
    test_recommendations: Optional[TestRecommendationReport],
) -> List[str]:
    categories = set(_contract_categories(contract))
    for change in changes:
        categories.update(_field_categories(change.field_name))

    tests: List[str] = []
    if not test_recommendations:
        return tests

    for rec in test_recommendations.recommendations:
        blob = f"{rec.test_name} {rec.reason}".lower()
        if any(cat in blob for cat in categories if cat != "general"):
            tests.append(rec.test_name)
        elif "payment" in categories and any(k in blob for k in ("payment", "checkout", "billing")):
            tests.append(rec.test_name)

    return sorted(set(tests))


def _historical_incident_count(
    contract: ApiContract,
    changes: List[ContractChange],
    historical_learning: Optional[HistoricalLearningReport],
) -> int:
    if not historical_learning:
        return 0
    categories = set(_contract_categories(contract))
    for change in changes:
        categories.update(_field_categories(change.field_name))

    count = 0
    for incident in historical_learning.similar_incidents:
        blob = f"{incident.title} {incident.summary}".lower()
        if any(cat in blob for cat in categories if cat != "general"):
            count += 1
        elif any(tok in blob for tok in ("payment", "checkout", "auth", "contract", "api")):
            count += 1
    return count


def _has_critical_module(impact_map: List[IncidentImpactNode], modules: List[str]) -> bool:
    module_set = {m.lower() for m in modules}
    for node in impact_map:
        title = (node.title or "").lower()
        if title in module_set and node.severity == "high":
            return True
    return False


def _build_summary(
    contract: ApiContract,
    changes: List[ContractChange],
    journeys: List[str],
    modules: List[str],
    risk_level: str,
    historical_count: int,
    deployment_level: str,
) -> str:
    removed = [c for c in changes if c.change_type == "removed_field"]
    if removed:
        fields = ", ".join(f"'{c.field_name}'" for c in removed)
        lead = f"The {contract.service_name} contract removed required field(s) {fields}."
    elif changes:
        lead = f"The {contract.service_name} contract has {len(changes)} business-impacting change(s)."
    else:
        lead = f"The {contract.service_name} contract was evaluated for business risk."

    parts = [lead]
    if journeys:
        parts.append(f"{' and '.join(journeys)} journey(s) may be impacted.")
    if modules:
        parts.append(f"Affected modules: {', '.join(modules)}.")
    if historical_count:
        parts.append(f"Historical incidents indicate elevated deployment risk ({historical_count} similar).")
    elif deployment_level in ("high", "critical"):
        parts.append("Deployment risk is elevated for the impacted area.")
    parts.append(f"Overall business risk level: {risk_level}.")
    return " ".join(parts)


def _assess_contract(
    contract: ApiContract,
    changes: List[ContractChange],
    *,
    impact_map: List[IncidentImpactNode],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    data_journey_validation: Optional[DataJourneyReport],
    decision_center: Optional[DecisionCenterSummary],
) -> ContractRiskAssessment:
    contract_id = contract.contract_id
    base_score, factors = _base_score(changes)
    for factor in factors:
        factor.factor_id = build_factor_id(contract_id, factor.factor_id.split(":")[-1])

    journeys = _affected_journeys(contract, changes, data_journey_validation, impact_map)
    modules = _affected_modules(contract, changes, impact_map)
    tests = _affected_tests(contract, changes, test_recommendations)
    historical_count = _historical_incident_count(contract, changes, historical_learning)
    deploy_level = (deployment_risk_assessment.risk_level or "low").lower() if deployment_risk_assessment else "low"

    score = base_score
    if journeys:
        score += _MULTIPLIER_POINTS["journey_impact"]
        factors.append(
            ContractRiskFactor(
                factor_id=build_factor_id(contract_id, "journey_impact"),
                title="Journey impact",
                description=f"Affected journeys: {', '.join(journeys)}.",
                severity="HIGH" if len(journeys) > 1 else "MEDIUM",
                weight=float(_MULTIPLIER_POINTS["journey_impact"]),
            )
        )
    if historical_count >= 1:
        score += _MULTIPLIER_POINTS["historical_incidents"]
        factors.append(
            ContractRiskFactor(
                factor_id=build_factor_id(contract_id, "historical_incidents"),
                title="Historical incidents",
                description=f"{historical_count} similar historical incident(s) correlate with this contract area.",
                severity="HIGH" if historical_count >= 3 else "MEDIUM",
                weight=float(_MULTIPLIER_POINTS["historical_incidents"]),
            )
        )
    if _has_critical_module(impact_map, modules):
        score += _MULTIPLIER_POINTS["critical_module"]
        factors.append(
            ContractRiskFactor(
                factor_id=build_factor_id(contract_id, "critical_module"),
                title="Critical module impact",
                description="A high-severity impacted module is linked to this contract change.",
                severity="HIGH",
                weight=float(_MULTIPLIER_POINTS["critical_module"]),
            )
        )
    if deploy_level in ("high", "critical"):
        score += _MULTIPLIER_POINTS["high_deployment_risk"]
        factors.append(
            ContractRiskFactor(
                factor_id=build_factor_id(contract_id, "high_deployment_risk"),
                title="High deployment risk",
                description=f"Deployment risk level is {deploy_level.upper()}.",
                severity="HIGH",
                weight=float(_MULTIPLIER_POINTS["high_deployment_risk"]),
            )
        )
    if tests:
        score += _MULTIPLIER_POINTS["recommended_tests"]
        factors.append(
            ContractRiskFactor(
                factor_id=build_factor_id(contract_id, "recommended_tests"),
                title="Recommended tests present",
                description=f"Prioritized tests: {', '.join(tests)}.",
                severity="MEDIUM",
                weight=float(_MULTIPLIER_POINTS["recommended_tests"]),
            )
        )
    if decision_center and decision_center.overall_status in ("ORANGE", "RED"):
        factors.append(
            ContractRiskFactor(
                factor_id=build_factor_id(contract_id, "decision_center_signal"),
                title="Quality decision center signal",
                description=decision_center.executive_summary or "Decision center indicates elevated quality risk.",
                severity="HIGH" if decision_center.overall_status == "RED" else "MEDIUM",
                weight=0.0,
            )
        )

    score = min(100, score)
    risk_level = _risk_level_from_score(score)
    confidence = round(min(0.95, 0.55 + len(changes) * 0.08 + len(journeys) * 0.05), 2)

    summary = _build_summary(
        contract,
        changes,
        journeys,
        modules,
        risk_level,
        historical_count,
        deploy_level,
    )

    assessment = ContractRiskAssessment(
        assessment_id=build_business_assessment_id(contract_id),
        contract_id=contract_id,
        overall_risk_level=risk_level,
        risk_score=score,
        confidence=confidence,
        summary=summary,
        affected_journeys=journeys,
        affected_modules=modules,
        affected_tests=tests,
        factors=sorted(factors, key=lambda f: f.factor_id),
    )
    return assessment


def build_contract_risk_assessment(
    *,
    api_contract_intelligence: Optional[ApiContractReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    impact_map: List[IncidentImpactNode],
    test_recommendations: Optional[TestRecommendationReport],
    data_journey_validation: Optional[DataJourneyReport],
    decision_center: Optional[DecisionCenterSummary],
) -> Optional[ContractRiskReport]:
    if not api_contract_intelligence or not api_contract_intelligence.risk_assessments:
        return None

    contracts_by_id = {c.contract_id: c for c in api_contract_intelligence.contracts}
    assessments: List[ContractRiskAssessment] = []

    for change_assessment in sorted(api_contract_intelligence.risk_assessments, key=lambda a: a.assessment_id):
        if not change_assessment.changes:
            continue
        contract_id = change_assessment.assessment_id.replace("assessment:", "", 1)
        contract = contracts_by_id.get(contract_id)
        if not contract:
            contract = next(
                (c for c in api_contract_intelligence.contracts if contract_id in c.contract_id),
                None,
            )
        if not contract:
            continue
        assessment = _assess_contract(
            contract,
            change_assessment.changes,
            impact_map=impact_map,
            deployment_risk_assessment=deployment_risk_assessment,
            historical_learning=historical_learning,
            test_recommendations=test_recommendations,
            data_journey_validation=data_journey_validation,
            decision_center=decision_center,
        )
        assessments.append(assessment)

    if not assessments:
        return None

    critical = sum(1 for a in assessments if a.overall_risk_level == "CRITICAL")
    high = sum(1 for a in assessments if a.overall_risk_level == "HIGH")
    if critical:
        summary = f"{len(assessments)} contract(s) assessed; {critical} CRITICAL business risk profile(s)."
    elif high:
        summary = f"{len(assessments)} contract(s) assessed; {high} HIGH business risk profile(s)."
    else:
        summary = f"{len(assessments)} contract(s) assessed for business-impact risk."

    digest = hashlib.sha256("|".join(a.assessment_id for a in assessments).encode("utf-8")).hexdigest()[:8]
    confidence = round(
        min(0.95, sum(a.confidence for a in assessments) / max(len(assessments), 1)),
        2,
    )

    return ContractRiskReport(
        assessments=assessments,
        summary=f"{summary} Ref: {digest}.",
        confidence=confidence,
    )
