# tests/test_early_degradation_detection_service.py
"""OBS-01C — Early Degradation Detection (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.incident_models import (
    ContractRiskAssessment,
    ContractRiskReport,
    HistoricalLearningReport,
    QualityTrend,
    QualityTrendPoint,
    QualityTrendReport,
    SimilarIncident,
)
from services.early_degradation_detection_service import build_early_degradation_report


def _points(scores):
    return [
        QualityTrendPoint(timestamp=f"2026-06-0{i + 1}T08:00:00+00:00", score=score, status="GOOD")
        for i, score in enumerate(scores)
    ]


def _trend(scope_name, scores, direction="DEGRADING"):
    scope_type = "module" if scope_name not in ("Project",) else "project"
    pts = _points(scores)
    return QualityTrend(
        trend_id=f"quality_trend:{scope_type}:{scope_name.lower()}",
        scope_type=scope_type,
        scope_name=scope_name,
        trend_direction=direction,
        score_change=pts[-1].score - pts[0].score,
        confidence=0.82,
        points=pts,
    )


def _quality_trends(*trends):
    return QualityTrendReport(
        overall_trend="DEGRADING",
        confidence=0.8,
        trends=list(trends),
        summary="",
    )


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report")
@patch("services.quality_trend_service._load_historical_reports", return_value=[])
def test_empty_history(mock_hist, mock_intel):
    mock_intel.return_value = {}
    assert build_early_degradation_report("demo") is None


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_stable_trend(_mock_intel):
    trends = _quality_trends(_trend("Authentication", [95, 94, 93, 92], "STABLE"))
    report = build_early_degradation_report("demo", quality_trends=trends)
    assert report is not None
    auth = next(a for a in report.assessments if a.scope_name == "Authentication")
    assert auth.status == "STABLE"
    assert auth.risk_projection == "LOW_RISK"
    assert report.degrading_areas == 0


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_degrading_trend(_mock_intel):
    trends = _quality_trends(_trend("Orders", [88, 85, 81, 77], "STABLE"))
    report = build_early_degradation_report("demo", quality_trends=trends)
    orders = next(a for a in report.assessments if a.scope_name == "Orders")
    assert orders.status == "DEGRADING"
    assert orders.risk_projection == "ELEVATED_RISK"


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_rapid_degradation(_mock_intel):
    trends = _quality_trends(_trend("Checkout", [92, 89, 84, 78], "STABLE"))
    report = build_early_degradation_report("demo", quality_trends=trends)
    checkout = next(a for a in report.assessments if a.scope_name == "Checkout")
    assert checkout.status == "RAPID_DEGRADATION"
    assert checkout.risk_projection == "HIGH_RISK"
    assert checkout.signals[0].severity == "MEDIUM"


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_critical_degradation(_mock_intel):
    trends = _quality_trends(_trend("Payments", [82, 75, 68, 57]))
    report = build_early_degradation_report("demo", quality_trends=trends)
    payments = next(a for a in report.assessments if a.scope_name == "Payments")
    assert payments.status == "CRITICAL_DEGRADATION"
    assert payments.risk_projection == "INCIDENT_LIKELY"
    assert payments.signals[0].score_delta == -25
    assert report.critical_areas == 1


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_projection_escalation(_mock_intel):
    trends = _quality_trends(_trend("Payments", [88, 86, 84, 82]))
    contract_risk = ContractRiskReport(
        assessments=[
            ContractRiskAssessment(
                assessment_id="a1",
                contract_id="payments",
                overall_risk_level="CRITICAL",
                risk_score=95,
                confidence=0.9,
                affected_modules=["Payments"],
            )
        ]
    )
    report = build_early_degradation_report(
        "demo",
        quality_trends=trends,
        contract_risk_assessment=contract_risk,
    )
    payments = next(a for a in report.assessments if a.scope_name == "Payments")
    assert payments.risk_projection in {"HIGH_RISK", "INCIDENT_LIKELY"}


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_historical_incident_influence(_mock_intel):
    trends = _quality_trends(_trend("Payments", [90, 88, 86, 84]))
    historical = HistoricalLearningReport(
        similar_incidents=[
            SimilarIncident(incident_id="i1", title="Payments outage", similarity_score=0.9, summary=""),
            SimilarIncident(incident_id="i2", title="Payments regression", similarity_score=0.85, summary=""),
        ]
    )
    report = build_early_degradation_report(
        "demo",
        quality_trends=trends,
        historical_learning=historical,
    )
    payments = next(a for a in report.assessments if a.scope_name == "Payments")
    assert payments.risk_projection in {"ELEVATED_RISK", "HIGH_RISK", "INCIDENT_LIKELY"}
    assert report.confidence >= 0.55


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_deterministic_output(_mock_intel):
    trends = _quality_trends(_trend("Payments", [82, 75, 68, 57]))
    first = build_early_degradation_report("demo", quality_trends=trends)
    second = build_early_degradation_report("demo", quality_trends=trends)
    assert first.model_dump() == second.model_dump()


@patch("services.early_degradation_detection_service._load_intelligence_from_latest_report", return_value={})
def test_no_external_calls(_mock_intel):
    trends = _quality_trends(_trend("Payments", [92, 89, 84, 78]))
    with patch("urllib.request.urlopen", side_effect=AssertionError("external HTTP")):
        with patch("requests.request", side_effect=AssertionError("external HTTP")):
            report = build_early_degradation_report("demo", quality_trends=trends)
    assert report is not None
