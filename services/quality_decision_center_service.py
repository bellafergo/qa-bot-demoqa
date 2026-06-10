# services/quality_decision_center_service.py
"""
Incident Investigator II-05C — Quality Decision Center (read-only).

Executive summary layer built entirely from existing investigator outputs.
No new scanners, execution paths, or external calls.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional

from models.incident_models import (
    DecisionCenterInsight,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    IncidentHypothesis,
    IncidentImpactNode,
    IncidentStorylineStep,
    InvestigationPlanItem,
    RecommendedAction,
    TestRecommendationReport,
)

_RISK_LEVEL_STATUS = {
    "low": "GREEN",
    "medium": "YELLOW",
    "high": "ORANGE",
    "critical": "RED",
}
_SEVERITY_RANK = {"high": 0, "medium": 1, "low": 2}


@dataclass
class _InsightDraft:
    title: str
    description: str
    priority: int
    confidence_impact: float
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


def _has_intelligence(
    *,
    hypotheses: List[IncidentHypothesis],
    impact_map: List[IncidentImpactNode],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    investigation_plan: List[InvestigationPlanItem],
    storyline: List[IncidentStorylineStep],
) -> bool:
    if deployment_risk_assessment:
        return True
    if impact_map or test_recommendations or recommended_actions:
        return True
    if investigation_plan or storyline:
        return True
    return any(h.confidence >= 0.45 and h.basis != "assumption" for h in hypotheses)


def _overall_status(
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    impact_map: List[IncidentImpactNode],
) -> str:
    if deployment_risk_assessment:
        return _RISK_LEVEL_STATUS.get(deployment_risk_assessment.risk_level, "YELLOW")
    high = sum(1 for n in impact_map if n.severity == "high")
    medium = sum(1 for n in impact_map if n.severity == "medium")
    if high >= 2:
        return "ORANGE"
    if high >= 1:
        return "ORANGE"
    if medium >= 2:
        return "YELLOW"
    if impact_map:
        return "YELLOW"
    return "GREEN"


def _top_impacted_area(impact_map: List[IncidentImpactNode]) -> Optional[str]:
    if not impact_map:
        return None
    ordered = sorted(
        impact_map,
        key=lambda n: (_SEVERITY_RANK.get(n.severity, 9), -n.confidence, -n.related_entity_count),
    )
    return ordered[0].title


def _top_hypothesis(hypotheses: List[IncidentHypothesis]) -> Optional[str]:
    if not hypotheses:
        return None
    top = hypotheses[0]
    statement = (top.statement or "").strip()
    if len(statement) > 96:
        return f"{statement[:93]}..."
    return statement or None


def _executive_summary(
    *,
    top_area: Optional[str],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    test_recommendations: Optional[TestRecommendationReport],
    impact_map: List[IncidentImpactNode],
    storyline: List[IncidentStorylineStep],
) -> str:
    if deployment_risk_assessment and deployment_risk_assessment.summary:
        base = deployment_risk_assessment.summary.strip()
    elif test_recommendations and test_recommendations.summary:
        base = test_recommendations.summary.strip()
    else:
        parts: List[str] = []
        if top_area:
            parts.append(f"{top_area}-related signals")
        browser_steps = [s for s in storyline if s.related_entity_type == "browser_watch"]
        if browser_steps:
            parts.append("browser alerts")
        if any(n.severity == "high" for n in impact_map):
            parts.append("repeated failure clusters")
        if parts:
            base = f"{', '.join(parts).capitalize()} indicate elevated deployment risk."
        else:
            base = "Incident intelligence suggests follow-up validation before release."

    if top_area and top_area.lower() not in base.lower():
        return f"{base} {top_area} appears to be the primary impacted area."
    return base


def _center_confidence(
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    test_recommendations: Optional[TestRecommendationReport],
    hypotheses: List[IncidentHypothesis],
) -> float:
    scores: List[float] = []
    if deployment_risk_assessment:
        scores.append(float(deployment_risk_assessment.confidence))
    if test_recommendations:
        scores.append(float(test_recommendations.recommendation_confidence))
    if hypotheses:
        scores.append(float(hypotheses[0].confidence))
    if not scores:
        return 0.5
    return round(min(1.0, sum(scores) / len(scores) + min(0.08, len(scores) * 0.02)), 2)


def _upsert_insight(pool: Dict[str, _InsightDraft], draft: _InsightDraft) -> None:
    key = draft.title.strip().lower()
    existing = pool.get(key)
    if existing is None or draft.priority < existing.priority or (
        draft.priority == existing.priority and draft.confidence_impact > existing.confidence_impact
    ):
        pool[key] = draft


def build_decision_center(
    *,
    hypotheses: List[IncidentHypothesis],
    investigation_plan: List[InvestigationPlanItem],
    storyline: List[IncidentStorylineStep],
    impact_map: List[IncidentImpactNode],
    recommended_actions: List[RecommendedAction],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    test_recommendations: Optional[TestRecommendationReport],
    limit: int = 6,
) -> Optional[DecisionCenterSummary]:
    """Build read-only executive decision center from existing investigator outputs."""
    if not _has_intelligence(
        hypotheses=hypotheses,
        impact_map=impact_map,
        deployment_risk_assessment=deployment_risk_assessment,
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
        investigation_plan=investigation_plan,
        storyline=storyline,
    ):
        return None

    top_area = _top_impacted_area(impact_map)
    top_hyp = _top_hypothesis(hypotheses)
    status = _overall_status(deployment_risk_assessment, impact_map)
    risk_score = int(deployment_risk_assessment.risk_score) if deployment_risk_assessment else 0
    risk_level = (
        deployment_risk_assessment.risk_level.upper()
        if deployment_risk_assessment
        else status
    )

    pool: Dict[str, _InsightDraft] = {}

    if top_area:
        top_node = next((n for n in impact_map if n.title == top_area), impact_map[0] if impact_map else None)
        _upsert_insight(pool, _InsightDraft(
            title=f"{top_area} is the primary impacted area",
            description=top_node.description if top_node else f"Multiple signals concentrate on {top_area}.",
            priority=1,
            confidence_impact=top_node.confidence if top_node else 0.7,
            related_entity_type=top_node.related_entity_type if top_node else None,
            related_entity_id=top_node.related_entity_id if top_node else None,
        ))

    for step in storyline:
        if step.related_entity_type == "browser_watch":
            _upsert_insight(pool, _InsightDraft(
                title="Browser alerts correlate with recent failures",
                description=step.description or step.title,
                priority=2,
                confidence_impact=float(step.confidence),
                related_entity_type=step.related_entity_type,
                related_entity_id=step.related_entity_id,
            ))
            break

    if test_recommendations and test_recommendations.recommendations:
        top_test = test_recommendations.recommendations[0]
        _upsert_insight(pool, _InsightDraft(
            title="Smoke validation is strongly recommended",
            description=f"Start with '{top_test.test_name}' to reduce deployment uncertainty.",
            priority=3,
            confidence_impact=float(top_test.confidence),
            related_entity_type=top_test.related_entity_type,
            related_entity_id=top_test.related_entity_id,
        ))

    if top_hyp:
        hid = (hypotheses[0].id or f"H{hypotheses[0].rank}" or "hypothesis").strip()
        _upsert_insight(pool, _InsightDraft(
            title="Leading hypothesis should guide validation",
            description=top_hyp,
            priority=4,
            confidence_impact=float(hypotheses[0].confidence),
            related_entity_type="hypothesis",
            related_entity_id=hid,
        ))

    if deployment_risk_assessment and deployment_risk_assessment.contributing_factors:
        factor = deployment_risk_assessment.contributing_factors[0]
        _upsert_insight(pool, _InsightDraft(
            title=f"Deployment risk driver: {factor.title}",
            description=factor.description,
            priority=2 if deployment_risk_assessment.risk_level in ("high", "critical") else 5,
            confidence_impact=float(deployment_risk_assessment.confidence),
            related_entity_type=factor.related_entity_type,
            related_entity_id=factor.related_entity_id,
        ))

    if investigation_plan:
        plan = investigation_plan[0]
        _upsert_insight(pool, _InsightDraft(
            title=f"Next investigation step: {plan.title}",
            description=plan.reason or plan.title,
            priority=min(6, max(2, int(plan.priority / 15))),
            confidence_impact=float(plan.priority) / 100.0,
            related_entity_type=plan.related_entity_type,
            related_entity_id=plan.related_entity_id,
        ))

    if recommended_actions:
        action = recommended_actions[0]
        _upsert_insight(pool, _InsightDraft(
            title=f"Top recommended action: {action.title}",
            description=action.reason or action.description,
            priority=min(5, max(2, action.priority)),
            confidence_impact=float(action.confidence),
            related_entity_type=action.related_entity_type,
            related_entity_id=action.related_entity_id,
        ))

    ordered = sorted(
        pool.values(),
        key=lambda i: (i.priority, -i.confidence_impact, i.title.lower()),
    )
    insights = [
        DecisionCenterInsight(
            title=d.title,
            description=d.description,
            priority=d.priority,
            related_entity_type=d.related_entity_type,
            related_entity_id=d.related_entity_id,
        )
        for d in ordered[: max(1, int(limit))]
    ]

    return DecisionCenterSummary(
        overall_status=status,
        executive_summary=_executive_summary(
            top_area=top_area,
            deployment_risk_assessment=deployment_risk_assessment,
            test_recommendations=test_recommendations,
            impact_map=impact_map,
            storyline=storyline,
        ),
        confidence=_center_confidence(deployment_risk_assessment, test_recommendations, hypotheses),
        top_risk_level=risk_level,
        top_risk_score=risk_score,
        top_hypothesis=top_hyp,
        top_impacted_area=top_area,
        recommended_test_count=len(test_recommendations.recommendations) if test_recommendations else 0,
        recommended_action_count=len(recommended_actions),
        key_takeaways=insights,
    )
