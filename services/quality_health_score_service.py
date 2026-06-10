# services/quality_health_score_service.py
"""
Observability OBS-01A — Quality Health Score (read-only).

Executive KPI summarizing quality health by project, environment, and module
using existing Vanya intelligence only. No execution, external calls, or LLMs.
"""
from __future__ import annotations

import hashlib
import re
from typing import List, Optional

from models.incident_models import (
    ContractRiskReport,
    DatabaseValidationReport,
    DataJourneyReport,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    EnterpriseDependencyMap,
    ExecutiveQualityReport,
    HistoricalLearningReport,
    IncidentImpactNode,
    MultiEnvironmentReport,
    QualityHealthFactor,
    QualityHealthReport,
    QualityHealthScore,
    RecommendedAction,
    TestRecommendationReport,
)

_ENV_DEDUCTION = {"BROKEN": 25, "DEGRADED": 12}
_CONTRACT_DEDUCTION = {"CRITICAL": 25, "HIGH": 15, "MEDIUM": 8}
_JOURNEY_DEDUCTION = {"BROKEN": 25, "DEGRADED": 12}
_DEPLOYMENT_DEDUCTION = {"critical": 30, "high": 20, "medium": 10}
_DEPENDENCY_DEDUCTION = {"CRITICAL": 20, "HIGH": 10, "MEDIUM": 5}
_IMPACT_SEVERITY_DEDUCTION = {"high": 15, "medium": 8, "low": 3}


def build_score_id(scope_type: str, scope_name: str, environment: Optional[str] = None) -> str:
    type_slug = re.sub(r"[^a-z0-9]+", "_", scope_type.strip().lower()).strip("_")
    name_slug = re.sub(r"[^a-z0-9]+", "_", scope_name.strip().lower()).strip("_")
    if environment:
        env_slug = re.sub(r"[^a-z0-9]+", "_", environment.strip().lower()).strip("_")
        return f"quality_health:{type_slug}:{name_slug}:{env_slug}"
    return f"quality_health:{type_slug}:{name_slug}"


def build_factor_id(scope_type: str, suffix: str) -> str:
    type_slug = re.sub(r"[^a-z0-9]+", "_", scope_type.strip().lower()).strip("_")
    suffix_slug = re.sub(r"[^a-z0-9]+", "_", suffix.strip().lower()).strip("_")
    return f"factor:{type_slug}:{suffix_slug}"


def _has_source_intelligence(
    *,
    executive_quality_report: Optional[ExecutiveQualityReport],
    multi_environment: Optional[MultiEnvironmentReport],
    decision_center: Optional[DecisionCenterSummary],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    database_validation: Optional[DatabaseValidationReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    impact_map: List[IncidentImpactNode],
) -> bool:
    return any(
        [
            executive_quality_report,
            multi_environment,
            decision_center,
            deployment_risk_assessment,
            contract_risk_assessment,
            data_journey_validation,
            database_validation,
            enterprise_dependency_map,
            historical_learning,
            test_recommendations,
            recommended_actions,
            impact_map,
        ]
    )


def _clamp_score(score: int) -> int:
    return max(0, min(100, score))


def _score_from_factors(factors: List[QualityHealthFactor]) -> int:
    total = sum(f.impact for f in factors)
    return _clamp_score(100 - total)


def _status_from_score(score: int, has_data: bool) -> str:
    if not has_data:
        return "UNKNOWN"
    if score >= 90:
        return "EXCELLENT"
    if score >= 75:
        return "GOOD"
    if score >= 50:
        return "ATTENTION"
    return "HIGH_RISK"


def _module_matches(text: str, module_name: str) -> bool:
    if not text or not module_name:
        return False
    return module_name.lower() in text.lower()


def _historical_is_degrading(historical_learning: Optional[HistoricalLearningReport]) -> bool:
    if not historical_learning:
        return False
    if historical_learning.similar_incidents:
        return True
    summary = (historical_learning.pattern_summary or "").lower()
    return any(token in summary for token in ("repeat", "recurring", "pattern", "similar", "again"))


def _project_trend(
    *,
    executive_quality_report: Optional[ExecutiveQualityReport],
    historical_learning: Optional[HistoricalLearningReport],
    factors: List[QualityHealthFactor],
) -> str:
    if _historical_is_degrading(historical_learning):
        return "DEGRADING"
    if executive_quality_report:
        trend = (executive_quality_report.quality_trend or "").lower()
        if trend in ("improving", "up"):
            return "IMPROVING"
        critical = executive_quality_report.critical_contract_count + executive_quality_report.broken_journey_count
        if critical == 0 and executive_quality_report.overall_quality_score >= 75:
            return "STABLE"
        if critical > 0:
            return "DEGRADING"
        return "STABLE"
    if factors:
        return "STABLE"
    return "UNKNOWN"


def _deployment_factors(
    deployment: Optional[DeploymentRiskAssessment],
    scope_type: str = "project",
) -> List[QualityHealthFactor]:
    if not deployment:
        return []
    level = (deployment.risk_level or "low").lower()
    impact = _DEPLOYMENT_DEDUCTION.get(level)
    if not impact:
        return []
    severity = "CRITICAL" if level == "critical" else "HIGH" if level == "high" else "MEDIUM"
    return [
        QualityHealthFactor(
            factor_id=build_factor_id(scope_type, "deployment_risk"),
            title=f"Deployment risk is {level.upper()}",
            description=deployment.summary,
            impact=impact,
            severity=severity,
            related_entity_type="deployment_risk",
            related_entity_id="deployment_risk_assessment",
        )
    ]


def _contract_factors(
    contract_risk: Optional[ContractRiskReport],
    *,
    scope_type: str = "project",
    module_filter: Optional[str] = None,
) -> List[QualityHealthFactor]:
    if not contract_risk:
        return []
    factors: List[QualityHealthFactor] = []
    for assessment in contract_risk.assessments:
        if module_filter and not any(
            _module_matches(m, module_filter) for m in assessment.affected_modules
        ) and not _module_matches(assessment.contract_id, module_filter):
            continue
        level = assessment.overall_risk_level.upper()
        impact = _CONTRACT_DEDUCTION.get(level)
        if not impact:
            continue
        label = assessment.contract_id.replace("contract:", "").replace("_", " ").title()
        factors.append(
            QualityHealthFactor(
                factor_id=build_factor_id(scope_type, f"contract_{assessment.contract_id}"),
                title=f"{label} contract risk is {level}",
                description=assessment.summary,
                impact=impact,
                severity=level,
                related_entity_type="contract_risk",
                related_entity_id=assessment.contract_id,
            )
        )
    return factors


def _journey_factors(
    data_journey: Optional[DataJourneyReport],
    *,
    scope_type: str = "project",
    journey_id_filter: Optional[str] = None,
) -> List[QualityHealthFactor]:
    if not data_journey:
        return []
    factors: List[QualityHealthFactor] = []
    for result in data_journey.results:
        if journey_id_filter and result.journey_id != journey_id_filter:
            continue
        status = result.status.upper()
        impact = _JOURNEY_DEDUCTION.get(status)
        if not impact:
            continue
        journey = next((j for j in data_journey.journeys if j.journey_id == result.journey_id), None)
        name = journey.name if journey else result.journey_id
        factors.append(
            QualityHealthFactor(
                factor_id=build_factor_id(scope_type, f"journey_{result.journey_id}"),
                title=f"{name} is {status}",
                description=result.summary,
                impact=impact,
                severity="CRITICAL" if status == "BROKEN" else "HIGH",
                related_entity_type="data_journey",
                related_entity_id=result.journey_id,
            )
        )
    return factors


def _dependency_factors(
    dependency_map: Optional[EnterpriseDependencyMap],
    *,
    scope_type: str = "project",
    name_filter: Optional[str] = None,
) -> List[QualityHealthFactor]:
    if not dependency_map:
        return []
    factors: List[QualityHealthFactor] = []
    for node in dependency_map.nodes:
        if name_filter and not _module_matches(node.name, name_filter):
            continue
        level = node.risk_level.upper()
        impact = _DEPENDENCY_DEDUCTION.get(level)
        if not impact:
            continue
        factors.append(
            QualityHealthFactor(
                factor_id=build_factor_id(scope_type, f"dependency_{node.node_id}"),
                title=f"{node.name} dependency risk is {level}",
                description=node.description,
                impact=impact,
                severity=level,
                related_entity_type="dependency_map",
                related_entity_id=node.node_id,
            )
        )
    return factors


def _test_recommendation_factors(
    test_recommendations: Optional[TestRecommendationReport],
    *,
    scope_type: str = "project",
    module_filter: Optional[str] = None,
) -> List[QualityHealthFactor]:
    if not test_recommendations:
        return []
    factors: List[QualityHealthFactor] = []
    for rec in test_recommendations.recommendations:
        if module_filter and not _module_matches(rec.test_name, module_filter):
            continue
        if len(factors) >= 4:
            break
        factors.append(
            QualityHealthFactor(
                factor_id=build_factor_id(scope_type, f"test_{rec.recommendation_id}"),
                title=f"{rec.test_name} validation recommended",
                description=rec.reason,
                impact=5,
                severity="MEDIUM",
                related_entity_type="recommended_test",
                related_entity_id=rec.recommendation_id,
            )
        )
    return factors


def _historical_factors(
    historical_learning: Optional[HistoricalLearningReport],
    *,
    scope_type: str = "project",
    module_filter: Optional[str] = None,
) -> List[QualityHealthFactor]:
    if not historical_learning or not _historical_is_degrading(historical_learning):
        return []
    if module_filter:
        matched = any(
            _module_matches(inc.summary or "", module_filter) or _module_matches(inc.incident_id, module_filter)
            for inc in historical_learning.similar_incidents
        )
        if not matched and not _module_matches(historical_learning.pattern_summary, module_filter):
            return []
    return [
        QualityHealthFactor(
            factor_id=build_factor_id(scope_type, "historical_learning"),
            title="Similar incidents detected historically",
            description=historical_learning.pattern_summary or "Repeated incident patterns observed.",
            impact=10,
            severity="HIGH",
            related_entity_type="historical_learning",
            related_entity_id="historical_learning",
        )
    ]


def _environment_factors(
    multi_environment: Optional[MultiEnvironmentReport],
    environment_name: str,
) -> List[QualityHealthFactor]:
    if not multi_environment:
        return []
    profile = next(
        (e for e in multi_environment.environments if e.name == environment_name or e.environment_id == environment_name),
        None,
    )
    if not profile:
        return []
    factors: List[QualityHealthFactor] = []
    impact = _ENV_DEDUCTION.get(profile.status)
    if impact:
        factors.append(
            QualityHealthFactor(
                factor_id=build_factor_id("environment", f"status_{profile.environment_id}"),
                title=f"{profile.name} environment is {profile.status}",
                description=f"Environment status derived from {len([s for s in multi_environment.signals if s.environment_id == profile.environment_id])} signal(s).",
                impact=impact,
                severity="CRITICAL" if profile.status == "BROKEN" else "HIGH",
                related_entity_type="multi_environment",
                related_entity_id=profile.environment_id,
            )
        )
    for signal in multi_environment.signals:
        if signal.environment_id != profile.environment_id:
            continue
        if signal.severity in ("CRITICAL", "HIGH") and not any(f.related_entity_id == signal.related_entity_id for f in factors):
            factors.append(
                QualityHealthFactor(
                    factor_id=build_factor_id("environment", signal.signal_id),
                    title=signal.title,
                    description=signal.description,
                    impact=10 if signal.severity == "HIGH" else 15,
                    severity=signal.severity,
                    related_entity_type=signal.related_entity_type,
                    related_entity_id=signal.related_entity_id,
                )
            )
    return factors


def _impact_module_factors(node: IncidentImpactNode) -> List[QualityHealthFactor]:
    severity = (node.severity or "low").lower()
    impact = _IMPACT_SEVERITY_DEDUCTION.get(severity, 3)
    return [
        QualityHealthFactor(
            factor_id=build_factor_id("module", node.title),
            title=f"{node.title} impact is {severity.upper()}",
            description=node.description,
            impact=impact,
            severity="HIGH" if severity == "high" else "MEDIUM",
            related_entity_type=node.related_entity_type or "impact_map",
            related_entity_id=node.related_entity_id,
        )
    ]


def _avg_confidence(values: List[float]) -> float:
    nums = [v for v in values if v > 0]
    if not nums:
        return 0.65
    return round(sum(nums) / len(nums), 2)


def _build_project_score(
    *,
    executive_quality_report: Optional[ExecutiveQualityReport],
    multi_environment: Optional[MultiEnvironmentReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    project_trend: str,
) -> QualityHealthScore:
    factors: List[QualityHealthFactor] = []
    factors.extend(_deployment_factors(deployment_risk_assessment))
    factors.extend(_contract_factors(contract_risk_assessment))
    factors.extend(_journey_factors(data_journey_validation))
    factors.extend(_dependency_factors(enterprise_dependency_map))
    factors.extend(_test_recommendation_factors(test_recommendations))
    factors.extend(_historical_factors(historical_learning))

    if multi_environment:
        for env in multi_environment.environments:
            if env.status in _ENV_DEDUCTION and env.is_production:
                factors.append(
                    QualityHealthFactor(
                        factor_id=build_factor_id("project", f"prod_{env.environment_id}"),
                        title=f"Production environment is {env.status}",
                        description=f"{env.name} health affects release readiness.",
                        impact=_ENV_DEDUCTION[env.status],
                        severity="CRITICAL" if env.status == "BROKEN" else "HIGH",
                        related_entity_type="multi_environment",
                        related_entity_id=env.environment_id,
                    )
                )

    score = _score_from_factors(factors)
    if executive_quality_report and executive_quality_report.overall_quality_score > 0:
        score = _clamp_score(round((score + executive_quality_report.overall_quality_score) / 2))

    confidences = [
        executive_quality_report.confidence if executive_quality_report else 0,
        deployment_risk_assessment.confidence if deployment_risk_assessment else 0,
        contract_risk_assessment.confidence if contract_risk_assessment else 0,
        data_journey_validation.confidence if data_journey_validation else 0,
        multi_environment.confidence if multi_environment else 0,
    ]

    has_data = bool(factors) or executive_quality_report is not None
    return QualityHealthScore(
        score_id=build_score_id("project", "overall"),
        scope_type="project",
        scope_name="Project",
        environment=None,
        score=score,
        status=_status_from_score(score, has_data),
        confidence=_avg_confidence(confidences),
        trend=project_trend,
        contributing_factors=factors[:8],
    )


def build_quality_health_report(
    *,
    project_id: str,
    executive_quality_report: Optional[ExecutiveQualityReport] = None,
    multi_environment: Optional[MultiEnvironmentReport] = None,
    decision_center: Optional[DecisionCenterSummary] = None,
    deployment_risk_assessment: Optional[DeploymentRiskAssessment] = None,
    contract_risk_assessment: Optional[ContractRiskReport] = None,
    data_journey_validation: Optional[DataJourneyReport] = None,
    database_validation: Optional[DatabaseValidationReport] = None,
    enterprise_dependency_map: Optional[EnterpriseDependencyMap] = None,
    historical_learning: Optional[HistoricalLearningReport] = None,
    test_recommendations: Optional[TestRecommendationReport] = None,
    recommended_actions: Optional[List[RecommendedAction]] = None,
    impact_map: Optional[List[IncidentImpactNode]] = None,
) -> Optional[QualityHealthReport]:
    impact_map = impact_map or []
    recommended_actions = recommended_actions or []

    if not _has_source_intelligence(
        executive_quality_report=executive_quality_report,
        multi_environment=multi_environment,
        decision_center=decision_center,
        deployment_risk_assessment=deployment_risk_assessment,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        database_validation=database_validation,
        enterprise_dependency_map=enterprise_dependency_map,
        historical_learning=historical_learning,
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
        impact_map=impact_map,
    ):
        return None

    project_factors_preview: List[QualityHealthFactor] = []
    project_factors_preview.extend(_deployment_factors(deployment_risk_assessment))
    project_factors_preview.extend(_contract_factors(contract_risk_assessment))
    project_trend = _project_trend(
        executive_quality_report=executive_quality_report,
        historical_learning=historical_learning,
        factors=project_factors_preview,
    )

    scores: List[QualityHealthScore] = []
    project_score = _build_project_score(
        executive_quality_report=executive_quality_report,
        multi_environment=multi_environment,
        deployment_risk_assessment=deployment_risk_assessment,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        enterprise_dependency_map=enterprise_dependency_map,
        historical_learning=historical_learning,
        test_recommendations=test_recommendations,
        project_trend=project_trend,
    )
    scores.append(project_score)

    if multi_environment:
        for env in multi_environment.environments:
            factors = _environment_factors(multi_environment, env.name)
            score = _score_from_factors(factors)
            scores.append(
                QualityHealthScore(
                    score_id=build_score_id("environment", env.name, env.environment_id),
                    scope_type="environment",
                    scope_name=env.name,
                    environment=env.name,
                    score=score,
                    status=_status_from_score(score, bool(factors)),
                    confidence=multi_environment.confidence,
                    trend="DEGRADING" if env.status in ("BROKEN", "DEGRADED") else project_trend,
                    contributing_factors=factors[:6],
                )
            )

    seen_modules = set()
    for node in impact_map:
        module_name = node.title.strip()
        if not module_name or module_name.lower() in seen_modules:
            continue
        seen_modules.add(module_name.lower())
        factors: List[QualityHealthFactor] = []
        factors.extend(_impact_module_factors(node))
        factors.extend(_contract_factors(contract_risk_assessment, scope_type="module", module_filter=module_name))
        factors.extend(_test_recommendation_factors(test_recommendations, scope_type="module", module_filter=module_name))
        factors.extend(_historical_factors(historical_learning, scope_type="module", module_filter=module_name))
        factors.extend(_dependency_factors(enterprise_dependency_map, scope_type="module", name_filter=module_name))
        score = _score_from_factors(factors)
        degrading = score < 50 or any(f.severity == "CRITICAL" for f in factors)
        scores.append(
            QualityHealthScore(
                score_id=build_score_id("module", module_name),
                scope_type="module",
                scope_name=module_name,
                environment=None,
                score=score,
                status=_status_from_score(score, bool(factors)),
                confidence=_avg_confidence([node.confidence, historical_learning.confidence if historical_learning else 0]),
                trend="DEGRADING" if degrading or _historical_is_degrading(historical_learning) else project_trend,
                contributing_factors=factors[:6],
            )
        )

    if data_journey_validation:
        for journey in data_journey_validation.journeys:
            factors = _journey_factors(data_journey_validation, scope_type="journey", journey_id_filter=journey.journey_id)
            score = _score_from_factors(factors)
            result = next((r for r in data_journey_validation.results if r.journey_id == journey.journey_id), None)
            degrading = result and result.status in ("BROKEN", "DEGRADED")
            scores.append(
                QualityHealthScore(
                    score_id=build_score_id("journey", journey.journey_id),
                    scope_type="journey",
                    scope_name=journey.name,
                    environment=None,
                    score=score,
                    status=_status_from_score(score, bool(factors)),
                    confidence=result.confidence if result else data_journey_validation.confidence,
                    trend="DEGRADING" if degrading else project_trend,
                    contributing_factors=factors[:4],
                )
            )

    if contract_risk_assessment:
        for assessment in contract_risk_assessment.assessments:
            label = assessment.contract_id.replace("contract:", "").replace("_", " ").title()
            factors = _contract_factors(
                ContractRiskReport(assessments=[assessment], summary="", confidence=contract_risk_assessment.confidence),
                scope_type="contract",
            )
            score = _score_from_factors(factors)
            scores.append(
                QualityHealthScore(
                    score_id=build_score_id("contract", assessment.contract_id),
                    scope_type="contract",
                    scope_name=label,
                    environment=None,
                    score=score,
                    status=_status_from_score(score, bool(factors)),
                    confidence=assessment.confidence,
                    trend="DEGRADING" if assessment.overall_risk_level in ("CRITICAL", "HIGH") else project_trend,
                    contributing_factors=factors[:4],
                )
            )

    overall_score = project_score.score
    overall_status = project_score.status
    overall_confidence = project_score.confidence
    overall_trend = project_trend

    high_risk = [s for s in scores if s.status == "HIGH_RISK"]
    attention = [s for s in scores if s.status == "ATTENTION"]
    digest = hashlib.sha256(f"{project_id}|{overall_score}|{len(scores)}".encode("utf-8")).hexdigest()[:8]

    if high_risk:
        names = ", ".join(s.scope_name for s in high_risk[:3])
        summary = f"Quality health score {overall_score}/100 ({overall_status}). High-risk areas: {names}. Ref: {digest}."
    elif attention:
        names = ", ".join(s.scope_name for s in attention[:3])
        summary = f"Quality health score {overall_score}/100 ({overall_status}). Areas needing attention: {names}. Ref: {digest}."
    else:
        summary = f"Quality health score {overall_score}/100 ({overall_status}). Trend {overall_trend}. Ref: {digest}."

    return QualityHealthReport(
        overall_score=overall_score,
        overall_status=overall_status,
        confidence=overall_confidence,
        trend=overall_trend,
        scores=scores,
        summary=summary,
    )
