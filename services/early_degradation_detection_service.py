# services/early_degradation_detection_service.py
"""
Observability OBS-01C — Early Degradation Detection (read-only).

Identifies negative quality trends before critical incidents using stored intelligence.
No execution, notifications, workflows, or external calls.
"""
from __future__ import annotations

import hashlib
import re
from typing import Any, Dict, List, Optional, Set, Tuple

from models.incident_models import (
    ContractRiskReport,
    DataJourneyReport,
    DegradationAssessment,
    DegradationSignal,
    DeploymentRiskAssessment,
    EarlyDegradationReport,
    ExecutiveQualityReport,
    HistoricalLearningReport,
    MultiEnvironmentReport,
    QualityHealthReport,
    QualityTrend,
    QualityTrendPoint,
    QualityTrendReport,
)

_SCOPE_TYPES = ("project", "environment", "module", "journey")
_STATUS_RANK = {
    "STABLE": 0,
    "DEGRADING": 1,
    "RAPID_DEGRADATION": 2,
    "CRITICAL_DEGRADATION": 3,
}
_PROJECTION_ORDER = ("LOW_RISK", "ELEVATED_RISK", "HIGH_RISK", "INCIDENT_LIKELY")
_DEGRADING_STATUSES = frozenset({"DEGRADING", "RAPID_DEGRADATION", "CRITICAL_DEGRADATION"})


def _slug(value: str) -> str:
    return re.sub(r"[^a-z0-9]+", "_", value.strip().lower()).strip("_")


def _signal_id(scope_type: str, scope_name: str) -> str:
    return f"degradation_signal:{_slug(scope_type)}:{_slug(scope_name)}"


def _assessment_id(scope_type: str, scope_name: str) -> str:
    return f"degradation_assessment:{_slug(scope_type)}:{_slug(scope_name)}"


def _severity_from_drop(drop: int) -> str:
    magnitude = abs(drop)
    if magnitude <= 5:
        return "LOW"
    if magnitude <= 15:
        return "MEDIUM"
    if magnitude <= 25:
        return "HIGH"
    return "CRITICAL"


def _step_drops(points: List[QualityTrendPoint]) -> List[int]:
    scores = [p.score for p in points]
    return [scores[i + 1] - scores[i] for i in range(len(scores) - 1)]


def _is_rapid_degradation(points: List[QualityTrendPoint]) -> bool:
    drops = _step_drops(points)
    if len(drops) < 2:
        return False
    if not all(d < 0 for d in drops):
        return False
    avg_drop = sum(abs(d) for d in drops) / len(drops)
    return avg_drop >= 4.0


def _degradation_status(points: List[QualityTrendPoint], score_delta: int, current_score: int) -> str:
    if score_delta >= -5:
        return "STABLE"
    if score_delta <= -25 or current_score <= 57:
        return "CRITICAL_DEGRADATION"
    if _is_rapid_degradation(points):
        return "RAPID_DEGRADATION"
    return "DEGRADING"


def _base_projection(status: str) -> str:
    return {
        "STABLE": "LOW_RISK",
        "DEGRADING": "ELEVATED_RISK",
        "RAPID_DEGRADATION": "HIGH_RISK",
        "CRITICAL_DEGRADATION": "INCIDENT_LIKELY",
    }.get(status, "LOW_RISK")


def _elevate_projection(projection: str, steps: int = 1) -> str:
    try:
        idx = _PROJECTION_ORDER.index(projection)
    except ValueError:
        idx = 0
    return _PROJECTION_ORDER[min(len(_PROJECTION_ORDER) - 1, idx + steps)]


def _scope_tokens(scope_name: str) -> Set[str]:
    raw = _slug(scope_name)
    return {raw, raw.replace("_", "")} if raw else set()


def _matches_scope(scope_name: str, candidate: str) -> bool:
    if not scope_name or not candidate:
        return False
    scope = _slug(scope_name)
    token = _slug(candidate)
    return scope == token or scope in token or token in scope


def _critical_contract_for_scope(
    scope_name: str,
    contract_risk_assessment: Optional[ContractRiskReport],
) -> bool:
    if not contract_risk_assessment:
        return False
    for assessment in contract_risk_assessment.assessments:
        level = str(assessment.overall_risk_level or "").upper()
        if level not in {"CRITICAL", "HIGH"}:
            continue
        contract_id = str(assessment.contract_id or "")
        modules = assessment.affected_modules or []
        journeys = assessment.affected_journeys or []
        if _matches_scope(scope_name, contract_id):
            return True
        if any(_matches_scope(scope_name, m) for m in modules):
            return True
        if any(_matches_scope(scope_name, j) for j in journeys):
            return True
    return False


def _environment_degraded_for_scope(
    scope_name: str,
    multi_environment: Optional[MultiEnvironmentReport],
) -> bool:
    if not multi_environment:
        return False
    degraded_statuses = {"DEGRADED", "AT_RISK", "UNHEALTHY", "CRITICAL", "HIGH_RISK"}
    for env in multi_environment.environments:
        if not _matches_scope(scope_name, env.name):
            continue
        if str(env.status or "").upper() in degraded_statuses:
            return True
    for signal in multi_environment.signals:
        if str(signal.severity or "").upper() in {"HIGH", "CRITICAL"}:
            if _matches_scope(scope_name, signal.title):
                return True
    return False


def _repeated_historical_incidents(
    scope_name: str,
    historical_learning: Optional[HistoricalLearningReport],
) -> bool:
    if not historical_learning or not historical_learning.similar_incidents:
        return False
    matches = 0
    for incident in historical_learning.similar_incidents:
        haystack = f"{incident.title} {incident.summary}".lower()
        token = scope_name.strip().lower()
        if token and token in haystack:
            matches += 1
    return matches >= 2 or (
        len(historical_learning.similar_incidents) >= 2
        and any(_matches_scope(scope_name, i.title) for i in historical_learning.similar_incidents)
    )


def _trend_degrading_for_scope(scope_name: str, quality_trends: Optional[QualityTrendReport]) -> bool:
    if not quality_trends:
        return False
    for trend in quality_trends.trends:
        if _matches_scope(scope_name, trend.scope_name) and trend.trend_direction == "DEGRADING":
            return True
    return quality_trends.overall_trend == "DEGRADING" and _matches_scope(scope_name, "Project")


def _risk_projection(
    status: str,
    *,
    scope_name: str,
    quality_trends: Optional[QualityTrendReport],
    multi_environment: Optional[MultiEnvironmentReport],
    contract_risk_assessment: Optional[ContractRiskReport],
    historical_learning: Optional[HistoricalLearningReport],
) -> str:
    projection = _base_projection(status)
    if status == "CRITICAL_DEGRADATION":
        return "INCIDENT_LIKELY"

    escalations = 0
    if _critical_contract_for_scope(scope_name, contract_risk_assessment):
        escalations += 1
    if _environment_degraded_for_scope(scope_name, multi_environment):
        escalations += 1
    if _repeated_historical_incidents(scope_name, historical_learning):
        escalations += 1
    if status != "STABLE" and _trend_degrading_for_scope(scope_name, quality_trends):
        escalations += 1

    if escalations >= 3:
        return "INCIDENT_LIKELY"
    if escalations >= 2:
        return _elevate_projection(projection, 2)
    if escalations >= 1:
        return _elevate_projection(projection, 1)
    return projection


def _related_entity(scope_type: str, scope_name: str) -> Tuple[Optional[str], Optional[str]]:
    if scope_type == "module":
        return "module", _slug(scope_name)
    if scope_type == "journey":
        return "data_journey", f"journey:{_slug(scope_name)}"
    if scope_type == "environment":
        return "environment", _slug(scope_name)
    if scope_type == "project":
        return "project", "project"
    return None, None


def _signal_confidence(points: List[QualityTrendPoint], base: float = 0.55) -> float:
    if len(points) < 2:
        return 0.0
    n_boost = min(0.2, (len(points) - 2) * 0.05)
    drops = _step_drops(points)
    same_sign = all(d <= 0 for d in drops) or all(d >= 0 for d in drops)
    consistency_boost = 0.1 if same_sign else 0.03
    return round(min(0.95, base + n_boost + consistency_boost), 2)


def _build_signal(
    scope_type: str,
    scope_name: str,
    points: List[QualityTrendPoint],
) -> DegradationSignal:
    current_score = points[-1].score
    previous_score = points[0].score
    score_delta = current_score - previous_score
    drop = abs(score_delta) if score_delta < 0 else 0
    velocity = round(drop / max(1, len(points) - 1), 2) if drop else 0.0
    entity_type, entity_id = _related_entity(scope_type, scope_name)

    if score_delta < 0:
        summary = (
            f"{scope_name} quality declined from {previous_score} to {current_score} "
            f"({score_delta} over {len(points) - 1} step(s))."
        )
    elif score_delta > 0:
        summary = f"{scope_name} quality improved from {previous_score} to {current_score}."
    else:
        summary = f"{scope_name} quality remained stable at {current_score}."

    return DegradationSignal(
        signal_id=_signal_id(scope_type, scope_name),
        scope_type=scope_type,
        scope_name=scope_name,
        current_score=current_score,
        previous_score=previous_score,
        score_delta=score_delta,
        degradation_velocity=velocity,
        severity=_severity_from_drop(score_delta),
        confidence=_signal_confidence(points),
        summary=summary,
        related_entity_type=entity_type,
        related_entity_id=entity_id,
    )


def _build_assessment(
    trend: QualityTrend,
    *,
    quality_trends: Optional[QualityTrendReport],
    multi_environment: Optional[MultiEnvironmentReport],
    contract_risk_assessment: Optional[ContractRiskReport],
    historical_learning: Optional[HistoricalLearningReport],
) -> Optional[DegradationAssessment]:
    points = trend.points
    if len(points) < 2:
        return None

    signal = _build_signal(trend.scope_type, trend.scope_name, points)
    status = _degradation_status(points, signal.score_delta, signal.current_score)
    projection = _risk_projection(
        status,
        scope_name=trend.scope_name,
        quality_trends=quality_trends,
        multi_environment=multi_environment,
        contract_risk_assessment=contract_risk_assessment,
        historical_learning=historical_learning,
    )
    confidence = round(min(0.95, (signal.confidence + trend.confidence) / 2), 2)

    return DegradationAssessment(
        assessment_id=_assessment_id(trend.scope_type, trend.scope_name),
        scope_type=trend.scope_type,
        scope_name=trend.scope_name,
        status=status,
        risk_projection=projection,
        confidence=confidence,
        signals=[signal],
    )


def _overall_status(assessments: List[DegradationAssessment]) -> str:
    if not assessments:
        return "STABLE"
    return max(assessments, key=lambda a: _STATUS_RANK.get(a.status, 0)).status


def _load_intelligence_from_latest_report(project_id: str) -> Dict[str, Any]:
    from services.db.incident_report_repository import incident_report_repo

    rows = incident_report_repo.list_reports(project_id=project_id, limit=1)
    if not rows:
        return {}
    report_id = str(rows[0].get("id") or "").strip()
    if not report_id:
        return {}
    full = incident_report_repo.get(report_id)
    return full if isinstance(full, dict) else {}


def build_early_degradation_report(
    project_id: str,
    *,
    quality_health: Optional[QualityHealthReport] = None,
    quality_trends: Optional[QualityTrendReport] = None,
    historical_learning: Optional[HistoricalLearningReport] = None,
    executive_quality_report: Optional[ExecutiveQualityReport] = None,
    multi_environment: Optional[MultiEnvironmentReport] = None,
    deployment_risk_assessment: Optional[DeploymentRiskAssessment] = None,
    contract_risk_assessment: Optional[ContractRiskReport] = None,
    data_journey_validation: Optional[DataJourneyReport] = None,
) -> Optional[EarlyDegradationReport]:
    pid = (project_id or "").strip().lower()
    if not pid:
        return None

    if quality_trends is None:
        from services.quality_trend_service import build_quality_trend_report

        quality_trends = build_quality_trend_report(
            pid,
            quality_health=quality_health,
            executive_quality_report=executive_quality_report,
            historical_learning=historical_learning,
        )

    if not quality_trends or not quality_trends.trends:
        return None

    if contract_risk_assessment is None or multi_environment is None or historical_learning is None:
        stored = _load_intelligence_from_latest_report(pid)
        if contract_risk_assessment is None and stored.get("contract_risk_assessment"):
            contract_risk_assessment = ContractRiskReport.model_validate(stored["contract_risk_assessment"])
        if multi_environment is None and stored.get("multi_environment"):
            multi_environment = MultiEnvironmentReport.model_validate(stored["multi_environment"])
        if historical_learning is None and stored.get("historical_learning"):
            historical_learning = HistoricalLearningReport.model_validate(stored["historical_learning"])
        if data_journey_validation is None and stored.get("data_journey_validation"):
            data_journey_validation = DataJourneyReport.model_validate(stored["data_journey_validation"])
        if deployment_risk_assessment is None and stored.get("deployment_risk_assessment"):
            deployment_risk_assessment = DeploymentRiskAssessment.model_validate(
                stored["deployment_risk_assessment"]
            )

    assessments: List[DegradationAssessment] = []
    for trend in quality_trends.trends:
        if trend.scope_type not in _SCOPE_TYPES:
            continue
        assessment = _build_assessment(
            trend,
            quality_trends=quality_trends,
            multi_environment=multi_environment,
            contract_risk_assessment=contract_risk_assessment,
            historical_learning=historical_learning,
        )
        if assessment:
            assessments.append(assessment)

    if not assessments:
        return None

    assessments.sort(
        key=lambda a: (
            -_STATUS_RANK.get(a.status, 0),
            -abs(a.signals[0].score_delta) if a.signals else 0,
            a.scope_name,
        )
    )

    degrading_areas = sum(1 for a in assessments if a.status in _DEGRADING_STATUSES)
    critical_areas = sum(1 for a in assessments if a.status == "CRITICAL_DEGRADATION")
    overall = _overall_status(assessments)
    confidences = [a.confidence for a in assessments if a.confidence > 0]
    confidence = round(sum(confidences) / len(confidences), 2) if confidences else 0.55

    if historical_learning and historical_learning.similar_incidents:
        confidence = round(min(0.95, confidence + 0.04), 2)
    if deployment_risk_assessment and deployment_risk_assessment.risk_level in {"high", "critical"}:
        confidence = round(min(0.95, confidence + 0.03), 2)

    digest = hashlib.sha256("|".join(a.assessment_id for a in assessments).encode("utf-8")).hexdigest()[:8]
    if degrading_areas == 0:
        summary = f"No active degradation signals detected across {len(assessments)} tracked scope(s). Ref: {digest}."
    elif critical_areas > 0:
        names = ", ".join(a.scope_name for a in assessments if a.status == "CRITICAL_DEGRADATION")[:120]
        summary = (
            f"Critical degradation detected in {critical_areas} area(s): {names}. "
            f"Validate before quality deteriorates further. Ref: {digest}."
        )
    else:
        names = ", ".join(a.scope_name for a in assessments if a.status in _DEGRADING_STATUSES)[:120]
        summary = (
            f"Early degradation detected in {degrading_areas} area(s): {names}. "
            f"Review trends before incidents occur. Ref: {digest}."
        )

    return EarlyDegradationReport(
        overall_status=overall,
        degrading_areas=degrading_areas,
        critical_areas=critical_areas,
        confidence=confidence,
        assessments=assessments,
        summary=summary,
    )
