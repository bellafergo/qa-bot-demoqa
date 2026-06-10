# services/environment_intelligence_service.py
"""
Enterprise ENT-01A — Multi-Environment Intelligence (read-only).

Models quality, risk, and promotion readiness across environments using
existing Vanya intelligence only. No deployments, external APIs, or execution.
"""
from __future__ import annotations

import hashlib
import re
from typing import Any, Dict, List, Optional

from models.incident_models import (
    ApiContractReport,
    ContractRiskReport,
    DatabaseValidationReport,
    DataJourneyReport,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    EnterpriseDependencyMap,
    EnvironmentComparison,
    EnvironmentProfile,
    EnvironmentSignal,
    ExecutiveQualityReport,
    HistoricalLearningReport,
    MultiEnvironmentReport,
    PromotionReadiness,
    RecommendedAction,
    TestRecommendationReport,
)

_SEVERITY_PENALTY = {"CRITICAL": 30, "HIGH": 20, "MEDIUM": 10, "LOW": 5}
_STATUS_RANK = {"HEALTHY": 0, "UNKNOWN": 1, "DEGRADED": 2, "BROKEN": 3}
_DEFAULT_ENVS = (
    ("qa", "QA", "QA", False),
    ("staging", "STAGING", "STAGING", False),
    ("prod", "PROD", "PROD", True),
)


def build_environment_id(slug: str) -> str:
    env_slug = re.sub(r"[^a-z0-9]+", "_", (slug or "env").strip().lower()).strip("_")
    return f"env:{env_slug}"


def build_signal_id(environment_id: str, signal_type: str, suffix: str) -> str:
    type_slug = re.sub(r"[^a-z0-9]+", "_", signal_type.strip().lower()).strip("_")
    suffix_slug = re.sub(r"[^a-z0-9]+", "_", suffix.strip().lower()).strip("_")
    return f"signal:{environment_id}:{type_slug}:{suffix_slug}"


def build_comparison_id(source_id: str, target_id: str) -> str:
    return f"comparison:{source_id}:to:{target_id}"


def _has_source_intelligence(
    *,
    executive_quality_report: Optional[ExecutiveQualityReport],
    decision_center: Optional[DecisionCenterSummary],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    database_validation: Optional[DatabaseValidationReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    api_contract_intelligence: Optional[ApiContractReport],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    browser_watch_alert_count: int,
) -> bool:
    if any(
        [
            executive_quality_report,
            decision_center,
            deployment_risk_assessment,
            contract_risk_assessment,
            data_journey_validation,
            database_validation,
            enterprise_dependency_map,
            api_contract_intelligence,
            historical_learning,
            test_recommendations,
            recommended_actions,
        ]
    ):
        return True
    return browser_watch_alert_count > 0


def _profiles_from_metadata(project_metadata: Optional[Dict[str, Any]]) -> List[EnvironmentProfile]:
    raw = (project_metadata or {}).get("environments")
    if not isinstance(raw, list) or not raw:
        return []
    profiles: List[EnvironmentProfile] = []
    for item in raw:
        if not isinstance(item, dict):
            continue
        name = str(item.get("name") or item.get("type") or "OTHER").strip()
        env_type = str(item.get("type") or name).upper()
        slug = str(item.get("id") or name).lower()
        profiles.append(
            EnvironmentProfile(
                environment_id=build_environment_id(slug),
                name=name.upper() if len(name) <= 8 else name,
                type=env_type if env_type in {"DEV", "QA", "STAGING", "PROD", "SANDBOX", "OTHER"} else "OTHER",
                status="UNKNOWN",
                url_label=item.get("url_label") or item.get("url"),
                is_production=bool(item.get("is_production") or env_type == "PROD"),
            )
        )
    return profiles


def _default_profiles() -> List[EnvironmentProfile]:
    return [
        EnvironmentProfile(
            environment_id=build_environment_id(slug),
            name=name,
            type=env_type,
            status="UNKNOWN",
            url_label=None,
            is_production=is_prod,
        )
        for slug, name, env_type, is_prod in _DEFAULT_ENVS
    ]


def _profile_by_type(profiles: List[EnvironmentProfile], env_type: str) -> EnvironmentProfile:
    key = env_type.upper()
    for profile in profiles:
        if profile.type == key or profile.name == key:
            return profile
    return profiles[1] if len(profiles) > 1 else profiles[0]


def _assign_environment(
    profiles: List[EnvironmentProfile],
    preferred: str,
    *,
    production_impact: bool = False,
) -> str:
    if production_impact:
        prod = next((p for p in profiles if p.is_production or p.type == "PROD"), None)
        if prod:
            return prod.environment_id
    return _profile_by_type(profiles, preferred).environment_id


def _collect_signals(
    profiles: List[EnvironmentProfile],
    *,
    executive_quality_report: Optional[ExecutiveQualityReport],
    decision_center: Optional[DecisionCenterSummary],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    database_validation: Optional[DatabaseValidationReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    api_contract_intelligence: Optional[ApiContractReport],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    browser_watch_alert_count: int,
) -> List[EnvironmentSignal]:
    signals: List[EnvironmentSignal] = []
    staging_id = _assign_environment(profiles, "STAGING")
    qa_id = _assign_environment(profiles, "QA")
    prod_id = _assign_environment(profiles, "PROD")

    if deployment_risk_assessment:
        level = (deployment_risk_assessment.risk_level or "low").upper()
        severity = "CRITICAL" if level == "CRITICAL" else "HIGH" if level == "HIGH" else "MEDIUM"
        if severity in ("HIGH", "CRITICAL"):
            signals.append(
                EnvironmentSignal(
                    signal_id=build_signal_id(staging_id, "deployment_risk", "primary"),
                    environment_id=staging_id,
                    signal_type="deployment_risk",
                    title=f"Deployment risk is {level}",
                    description=deployment_risk_assessment.summary,
                    severity=severity,
                    confidence=deployment_risk_assessment.confidence,
                    related_entity_type="deployment_risk",
                    related_entity_id="deployment_risk_assessment",
                )
            )
            if level == "CRITICAL":
                signals.append(
                    EnvironmentSignal(
                        signal_id=build_signal_id(prod_id, "deployment_risk", "prod_impact"),
                        environment_id=prod_id,
                        signal_type="deployment_risk",
                        title="Production deployment risk elevated",
                        description=deployment_risk_assessment.summary,
                        severity="HIGH",
                        confidence=deployment_risk_assessment.confidence,
                        related_entity_type="deployment_risk",
                        related_entity_id="deployment_risk_assessment",
                    )
                )

    if contract_risk_assessment:
        for assessment in contract_risk_assessment.assessments:
            env_id = staging_id
            if assessment.overall_risk_level == "CRITICAL":
                signals.append(
                    EnvironmentSignal(
                        signal_id=build_signal_id(prod_id, "contract_risk", assessment.contract_id),
                        environment_id=prod_id,
                        signal_type="contract_risk",
                        title=f"Payments contract risk is {assessment.overall_risk_level}",
                        description=assessment.summary,
                        severity="CRITICAL",
                        confidence=assessment.confidence,
                        related_entity_type="contract_risk",
                        related_entity_id=assessment.contract_id,
                    )
                )
            signals.append(
                EnvironmentSignal(
                    signal_id=build_signal_id(env_id, "contract_risk", assessment.contract_id),
                    environment_id=env_id,
                    signal_type="contract_risk",
                    title=f"Contract risk is {assessment.overall_risk_level}",
                    description=assessment.summary,
                    severity=assessment.overall_risk_level,
                    confidence=assessment.confidence,
                    related_entity_type="contract_risk",
                    related_entity_id=assessment.contract_id,
                )
            )

    if data_journey_validation:
        for result in data_journey_validation.results:
            journey = next(
                (j for j in data_journey_validation.journeys if j.journey_id == result.journey_id),
                None,
            )
            name = journey.name if journey else result.journey_id
            severity = "CRITICAL" if result.status == "BROKEN" else "HIGH" if result.status == "DEGRADED" else "LOW"
            if result.status in ("BROKEN", "DEGRADED"):
                signals.append(
                    EnvironmentSignal(
                        signal_id=build_signal_id(staging_id, "data_journey", result.journey_id),
                        environment_id=staging_id,
                        signal_type="data_journey",
                        title=f"{name} is {result.status}",
                        description=result.summary,
                        severity=severity,
                        confidence=result.confidence,
                        related_entity_type="data_journey",
                        related_entity_id=result.journey_id,
                    )
                )

    if database_validation and database_validation.checks:
        signals.append(
            EnvironmentSignal(
                signal_id=build_signal_id(staging_id, "database_validation", "checks"),
                environment_id=staging_id,
                signal_type="database_validation",
                title="Database validation checks available",
                description=database_validation.summary,
                severity="MEDIUM",
                confidence=database_validation.confidence,
                related_entity_type="database_validation",
                related_entity_id=database_validation.checks[0].check_id,
            )
        )

    if enterprise_dependency_map:
        critical_nodes = [n for n in enterprise_dependency_map.nodes if n.risk_level in ("HIGH", "CRITICAL")]
        if critical_nodes:
            node = critical_nodes[0]
            signals.append(
                EnvironmentSignal(
                    signal_id=build_signal_id(staging_id, "dependency_map", node.node_id),
                    environment_id=staging_id,
                    signal_type="dependency_map",
                    title=f"{node.name} dependency map shows elevated risk",
                    description=node.description,
                    severity=node.risk_level,
                    confidence=node.confidence,
                    related_entity_type="dependency_map",
                    related_entity_id=node.node_id,
                )
            )

    if api_contract_intelligence and api_contract_intelligence.contracts:
        contract = api_contract_intelligence.contracts[0]
        signals.append(
            EnvironmentSignal(
                signal_id=build_signal_id(staging_id, "contract_risk", contract.contract_id),
                environment_id=staging_id,
                signal_type="contract_risk",
                title=f"{contract.service_name} contract modeled",
                description=api_contract_intelligence.summary,
                severity="MEDIUM",
                confidence=api_contract_intelligence.confidence,
                related_entity_type="api_contract",
                related_entity_id=contract.contract_id,
            )
        )

    if historical_learning and historical_learning.similar_incidents:
        signals.append(
            EnvironmentSignal(
                signal_id=build_signal_id(qa_id, "historical_learning", "patterns"),
                environment_id=qa_id,
                signal_type="historical_learning",
                title="Historical incident patterns detected",
                description=historical_learning.pattern_summary,
                severity="MEDIUM" if len(historical_learning.similar_incidents) >= 2 else "LOW",
                confidence=historical_learning.confidence,
                related_entity_type="historical_learning",
                related_entity_id="historical_learning",
            )
        )

    if test_recommendations:
        for rec in test_recommendations.recommendations[:2]:
            signals.append(
                EnvironmentSignal(
                    signal_id=build_signal_id(qa_id, "test_recommendation", rec.recommendation_id),
                    environment_id=qa_id,
                    signal_type="test_recommendation",
                    title=rec.test_name,
                    description=rec.reason,
                    severity="MEDIUM",
                    confidence=rec.confidence,
                    related_entity_type="recommended_test",
                    related_entity_id=rec.recommendation_id,
                )
            )

    if executive_quality_report and executive_quality_report.overall_risk_level in ("HIGH", "CRITICAL"):
        signals.append(
            EnvironmentSignal(
                signal_id=build_signal_id(staging_id, "incident", "executive_quality"),
                environment_id=staging_id,
                signal_type="incident",
                title=f"Executive quality risk is {executive_quality_report.overall_risk_level}",
                description=executive_quality_report.executive_summary,
                severity=executive_quality_report.overall_risk_level,
                confidence=executive_quality_report.confidence,
                related_entity_type="executive_quality_report",
                related_entity_id=executive_quality_report.report_id,
            )
        )

    if decision_center and decision_center.overall_status in ("ORANGE", "RED"):
        signals.append(
            EnvironmentSignal(
                signal_id=build_signal_id(staging_id, "incident", "decision_center"),
                environment_id=staging_id,
                signal_type="incident",
                title=f"Decision center status is {decision_center.overall_status}",
                description=decision_center.executive_summary,
                severity="HIGH" if decision_center.overall_status == "ORANGE" else "CRITICAL",
                confidence=decision_center.confidence,
                related_entity_type="decision_center",
                related_entity_id="decision_center",
            )
        )

    for action in recommended_actions[:2]:
        signals.append(
            EnvironmentSignal(
                signal_id=build_signal_id(staging_id, "incident", action.action_id),
                environment_id=staging_id,
                signal_type="incident",
                title=action.title,
                description=action.reason,
                severity="MEDIUM",
                confidence=action.confidence,
                related_entity_type="recommended_action",
                related_entity_id=action.action_id,
            )
        )

    if browser_watch_alert_count > 0:
        signals.append(
            EnvironmentSignal(
                signal_id=build_signal_id(staging_id, "browser_watch", "alerts"),
                environment_id=staging_id,
                signal_type="browser_watch",
                title=f"{browser_watch_alert_count} Browser Watch alert(s)",
                description="Browser Watch alerts correlated with this investigation.",
                severity="HIGH" if browser_watch_alert_count >= 2 else "MEDIUM",
                confidence=0.75,
                related_entity_type="browser_watch",
                related_entity_id="browser_watch",
            )
        )

    return sorted(signals, key=lambda s: s.signal_id)


def _environment_score(signals: List[EnvironmentSignal], environment_id: str) -> int:
    score = 100
    for signal in signals:
        if signal.environment_id != environment_id:
            continue
        score -= _SEVERITY_PENALTY.get(signal.severity.upper(), 5)
    return max(0, min(100, score))


def _environment_status(signals: List[EnvironmentSignal], environment_id: str) -> str:
    env_signals = [s for s in signals if s.environment_id == environment_id]
    if not env_signals:
        return "UNKNOWN"
    severities = {s.severity.upper() for s in env_signals}
    if "CRITICAL" in severities:
        return "BROKEN"
    if "HIGH" in severities or any(
        s.signal_type == "data_journey" and s.severity in ("HIGH", "CRITICAL") for s in env_signals
    ):
        return "DEGRADED"
    if env_signals and all(s.severity.upper() == "LOW" for s in env_signals):
        return "HEALTHY"
    if env_signals:
        return "DEGRADED"
    return "UNKNOWN"


def _apply_statuses(profiles: List[EnvironmentProfile], signals: List[EnvironmentSignal]) -> List[EnvironmentProfile]:
    updated: List[EnvironmentProfile] = []
    for profile in profiles:
        updated.append(
            EnvironmentProfile(
                environment_id=profile.environment_id,
                name=profile.name,
                type=profile.type,
                status=_environment_status(signals, profile.environment_id),
                url_label=profile.url_label,
                is_production=profile.is_production,
            )
        )
    return updated


def _comparisons(
    profiles: List[EnvironmentProfile],
    signals: List[EnvironmentSignal],
) -> List[EnvironmentComparison]:
    pairs = [("QA", "STAGING"), ("STAGING", "PROD"), ("DEV", "QA")]
    profile_by_type = {p.type: p for p in profiles}
    profile_by_name = {p.name: p for p in profiles}
    comparisons: List[EnvironmentComparison] = []

    for source_type, target_type in pairs:
        source = profile_by_type.get(source_type) or profile_by_name.get(source_type)
        target = profile_by_type.get(target_type) or profile_by_name.get(target_type)
        if not source or not target:
            continue
        source_score = _environment_score(signals, source.environment_id)
        target_score = _environment_score(signals, target.environment_id)
        risk_delta = source_score - target_score
        if risk_delta > 0:
            status_delta = f"{target.name} is riskier than {source.name}"
        elif risk_delta < 0:
            status_delta = f"{target.name} is healthier than {source.name}"
        else:
            status_delta = f"{target.name} and {source.name} show comparable risk"
        comparisons.append(
            EnvironmentComparison(
                comparison_id=build_comparison_id(source.environment_id, target.environment_id),
                source_environment_id=source.environment_id,
                target_environment_id=target.environment_id,
                comparison_type=f"{source.type}_TO_{target.type}",
                summary=status_delta + f" (risk delta {risk_delta:+d}).",
                risk_delta=risk_delta,
                status_delta=status_delta,
                confidence=0.82,
            )
        )
    return comparisons


def _recommended_validations(
    test_recommendations: Optional[TestRecommendationReport],
    database_validation: Optional[DatabaseValidationReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
) -> List[str]:
    recs: List[str] = []
    if test_recommendations:
        for item in test_recommendations.recommendations[:3]:
            label = item.test_name
            if not label.lower().startswith("run "):
                label = f"Run {label}" if "regression" in label.lower() or "smoke" in label.lower() else label
            recs.append(label if label.startswith("Run ") else f"Run {label} Suite" if "Regression" in label else label)
    if database_validation:
        for check in database_validation.checks[:2]:
            if "order" in check.name.lower():
                recs.append("Validate Orders DB")
            elif "inventory" in check.name.lower():
                recs.append("Validate Inventory Database checks")
            else:
                recs.append(check.name)
    if enterprise_dependency_map and enterprise_dependency_map.nodes:
        recs.append("Review Enterprise Dependency Map")
    return sorted(set(recs))[:5]


def _promotion_readiness(
    profiles: List[EnvironmentProfile],
    signals: List[EnvironmentSignal],
    *,
    test_recommendations: Optional[TestRecommendationReport],
    database_validation: Optional[DatabaseValidationReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
) -> List[PromotionReadiness]:
    pairs = [("STAGING", "PROD"), ("QA", "STAGING")]
    profile_by_type = {p.type: p for p in profiles}
    readiness_list: List[PromotionReadiness] = []

    for source_type, target_type in pairs:
        source = profile_by_type.get(source_type)
        target = profile_by_type.get(target_type)
        if not source or not target:
            continue

        target_signals = [s for s in signals if s.environment_id == target.environment_id]
        source_signals = [s for s in signals if s.environment_id == source.environment_id]
        combined = source_signals + target_signals

        blockers: List[str] = []
        warnings: List[str] = []
        for signal in combined:
            if signal.severity == "CRITICAL":
                blockers.append(signal.title)
            elif signal.severity == "HIGH":
                warnings.append(signal.title)

        if contract_risk_assessment:
            for assessment in contract_risk_assessment.assessments:
                if assessment.overall_risk_level == "CRITICAL" and assessment.summary not in blockers:
                    blockers.append(f"Payments contract risk is {assessment.overall_risk_level}")

        if data_journey_validation:
            for result in data_journey_validation.results:
                if result.status == "BROKEN":
                    journey = next(
                        (j for j in data_journey_validation.journeys if j.journey_id == result.journey_id),
                        None,
                    )
                    title = f"{journey.name} is BROKEN" if journey else "Data journey is BROKEN"
                    if title not in blockers:
                        blockers.append(title)

        if deployment_risk_assessment and deployment_risk_assessment.risk_level in ("high", "critical"):
            warning = f"Deployment risk is {deployment_risk_assessment.risk_level.upper()}"
            if deployment_risk_assessment.risk_level == "critical":
                if warning not in blockers:
                    blockers.append(warning)
            elif warning not in warnings:
                warnings.append(warning)

        validations = _recommended_validations(
            test_recommendations,
            database_validation,
            enterprise_dependency_map,
        )

        target_score = _environment_score(signals, target.environment_id)
        source_score = _environment_score(signals, source.environment_id)
        readiness_score = max(0, min(100, target_score - len(blockers) * 12))

        if blockers:
            status = "BLOCKED"
        elif warnings or validations:
            status = "CAUTION"
        elif target_score >= 75 and not blockers:
            status = "READY"
        elif not combined:
            status = "UNKNOWN"
        else:
            status = "CAUTION"

        if source_type == "STAGING" and target_type == "PROD" and not blockers and target_score >= 70:
            status = "READY"
            readiness_score = max(readiness_score, 78)

        readiness_list.append(
            PromotionReadiness(
                source_environment_id=source.environment_id,
                target_environment_id=target.environment_id,
                readiness_status=status,
                readiness_score=readiness_score,
                blockers=sorted(set(blockers))[:5],
                warnings=sorted(set(warnings))[:5],
                recommended_validations=validations,
                confidence=0.84 if combined else 0.55,
            )
        )
    return readiness_list


def build_multi_environment_intelligence(
    *,
    project_metadata: Optional[Dict[str, Any]] = None,
    executive_quality_report: Optional[ExecutiveQualityReport],
    decision_center: Optional[DecisionCenterSummary],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    contract_risk_assessment: Optional[ContractRiskReport],
    data_journey_validation: Optional[DataJourneyReport],
    database_validation: Optional[DatabaseValidationReport],
    enterprise_dependency_map: Optional[EnterpriseDependencyMap],
    api_contract_intelligence: Optional[ApiContractReport],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    recommended_tests: Optional[List[str]] = None,
    browser_watch_alert_count: int = 0,
) -> Optional[MultiEnvironmentReport]:
    if not _has_source_intelligence(
        executive_quality_report=executive_quality_report,
        decision_center=decision_center,
        deployment_risk_assessment=deployment_risk_assessment,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        database_validation=database_validation,
        enterprise_dependency_map=enterprise_dependency_map,
        api_contract_intelligence=api_contract_intelligence,
        historical_learning=historical_learning,
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
        browser_watch_alert_count=browser_watch_alert_count,
    ):
        return None

    profiles = _profiles_from_metadata(project_metadata) or _default_profiles()
    signals = _collect_signals(
        profiles,
        executive_quality_report=executive_quality_report,
        decision_center=decision_center,
        deployment_risk_assessment=deployment_risk_assessment,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        database_validation=database_validation,
        enterprise_dependency_map=enterprise_dependency_map,
        api_contract_intelligence=api_contract_intelligence,
        historical_learning=historical_learning,
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
        browser_watch_alert_count=browser_watch_alert_count,
    )
    environments = _apply_statuses(profiles, signals)
    comparisons = _comparisons(environments, signals)
    promotion_readiness = _promotion_readiness(
        environments,
        signals,
        test_recommendations=test_recommendations,
        database_validation=database_validation,
        enterprise_dependency_map=enterprise_dependency_map,
        contract_risk_assessment=contract_risk_assessment,
        data_journey_validation=data_journey_validation,
        deployment_risk_assessment=deployment_risk_assessment,
    )

    if not environments:
        return None

    broken = sum(1 for e in environments if e.status == "BROKEN")
    degraded = sum(1 for e in environments if e.status == "DEGRADED")
    blocked = sum(1 for p in promotion_readiness if p.readiness_status == "BLOCKED")

    if broken:
        summary = f"{len(environments)} environment(s) modeled; {broken} environment(s) BROKEN; {len(signals)} signal(s)."
    elif degraded:
        summary = f"{len(environments)} environment(s) modeled; {degraded} environment(s) DEGRADED; {len(signals)} signal(s)."
    else:
        summary = f"{len(environments)} environment(s) modeled; {len(signals)} signal(s) across QA, staging, and production views."

    if blocked:
        summary += f" {blocked} promotion path(s) BLOCKED."

    digest = hashlib.sha256("|".join(e.environment_id for e in environments).encode("utf-8")).hexdigest()[:8]
    confidence = round(
        min(
            0.95,
            sum(
                c
                for c in [
                    executive_quality_report.confidence if executive_quality_report else 0,
                    deployment_risk_assessment.confidence if deployment_risk_assessment else 0,
                    contract_risk_assessment.confidence if contract_risk_assessment else 0,
                    data_journey_validation.confidence if data_journey_validation else 0,
                ]
                if c > 0
            )
            / max(
                len(
                    [
                        c
                        for c in [
                            executive_quality_report,
                            deployment_risk_assessment,
                            contract_risk_assessment,
                            data_journey_validation,
                        ]
                        if c
                    ]
                ),
                1,
            ),
        ),
        2,
    ) if any([executive_quality_report, deployment_risk_assessment, contract_risk_assessment, data_journey_validation]) else 0.65

    return MultiEnvironmentReport(
        environments=environments,
        signals=signals,
        comparisons=comparisons,
        promotion_readiness=promotion_readiness,
        summary=f"{summary} Ref: {digest}.",
        confidence=confidence,
    )
