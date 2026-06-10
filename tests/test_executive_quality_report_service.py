# tests/test_executive_quality_report_service.py
"""ENT-02A — Executive Quality Reports (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    ContractRiskAssessment,
    ContractRiskReport,
    DataJourney,
    DataJourneyReport,
    DataJourneyResult,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    EnterpriseDependencyMap,
    DependencyNode,
    HistoricalLearningReport,
    ProjectIncidentInvestigationReport,
    RecommendedAction,
    RecommendedTest,
    SimilarIncident,
    TestRecommendationReport,
)
from services.executive_quality_report_service import (
    build_executive_quality_report,
    build_executive_report_id,
)


def _base(**overrides):
    base = {
        "project_id": "demo",
        "decision_center": None,
        "deployment_risk_assessment": None,
        "historical_learning": None,
        "test_recommendations": None,
        "recommended_actions": [],
        "contract_risk_assessment": None,
        "data_journey_validation": None,
        "enterprise_dependency_map": None,
    }
    base.update(overrides)
    return base


def test_empty_state_returns_none():
    assert build_executive_quality_report(**_base()) is None


def test_quality_score_calculation_low_deployment():
    report = build_executive_quality_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=20,
                risk_level="low",
                confidence=0.8,
                summary="Low deployment risk",
            ),
        )
    )
    assert report is not None
    assert report.overall_quality_score >= 25


def test_quality_score_critical_deployment():
    report = build_executive_quality_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=95,
                risk_level="critical",
                confidence=0.9,
                summary="Critical",
            ),
            decision_center=DecisionCenterSummary(
                overall_status="RED",
                executive_summary="Critical",
                confidence=0.85,
                top_risk_level="CRITICAL",
                top_risk_score=95,
            ),
        )
    )
    assert report.overall_quality_score <= 60


def test_risk_level_mapping():
    high_quality = build_executive_quality_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(risk_score=20, risk_level="low", confidence=0.8, summary=""),
            decision_center=DecisionCenterSummary(overall_status="GREEN", executive_summary="", confidence=0.8),
        )
    )
    assert high_quality.overall_risk_level == "LOW"

    low_quality = build_executive_quality_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(risk_score=95, risk_level="critical", confidence=0.9, summary=""),
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id="a1",
                        contract_id="contract:payments",
                        overall_risk_level="CRITICAL",
                        risk_score=95,
                        confidence=0.9,
                        summary="critical",
                    )
                ],
                summary="",
                confidence=0.9,
            ),
            data_journey_validation=DataJourneyReport(
                journeys=[DataJourney(journey_id="j1", name="Checkout Journey", business_area="checkout", stages=[])],
                results=[DataJourneyResult(journey_id="j1", status="BROKEN", completed_stages=1, total_stages=5, confidence=0.6, summary="")],
                summary="",
                confidence=0.6,
            ),
        )
    )
    assert low_quality.overall_risk_level in ("HIGH", "CRITICAL")


def test_contract_risk_influence():
    report = build_executive_quality_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(risk_score=30, risk_level="low", confidence=0.8, summary=""),
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id="a1",
                        contract_id="contract:payments_api",
                        overall_risk_level="CRITICAL",
                        risk_score=92,
                        confidence=0.88,
                        summary="Payments contract removed amount",
                        affected_tests=["Payments Regression"],
                    )
                ],
                summary="",
                confidence=0.88,
            ),
        )
    )
    assert report.critical_contract_count == 1
    assert any("contract" in r.lower() for r in report.top_risks)


def test_journey_influence():
    report = build_executive_quality_report(
        **_base(
            data_journey_validation=DataJourneyReport(
                journeys=[DataJourney(journey_id="journey:checkout", name="Checkout Journey", business_area="checkout", stages=[])],
                results=[
                    DataJourneyResult(
                        journey_id="journey:checkout",
                        status="BROKEN",
                        completed_stages=2,
                        total_stages=5,
                        confidence=0.7,
                        summary="broken",
                    )
                ],
                summary="",
                confidence=0.7,
            ),
        )
    )
    assert report.broken_journey_count == 1
    assert any("BROKEN" in r for r in report.top_risks)


def test_historical_influence():
    report = build_executive_quality_report(
        **_base(
            historical_learning=HistoricalLearningReport(
                similar_incidents=[
                    SimilarIncident(incident_id="i1", title="Payments outage", summary="", similarity_score=0.8),
                    SimilarIncident(incident_id="i2", title="Checkout regression", summary="", similarity_score=0.75),
                    SimilarIncident(incident_id="i3", title="API deploy failure", summary="", similarity_score=0.7),
                ],
                pattern_summary="Recurring payments incidents after API deploys.",
                confidence=0.82,
            ),
            deployment_risk_assessment=DeploymentRiskAssessment(risk_score=40, risk_level="medium", confidence=0.75, summary=""),
        )
    )
    assert "payments" in report.historical_pattern_summary.lower()
    assert report.quality_trend in ("elevated", "stable", "improving")


def test_deterministic_output():
    kwargs = _base(
        deployment_risk_assessment=DeploymentRiskAssessment(risk_score=30, risk_level="low", confidence=0.8, summary=""),
        test_recommendations=TestRecommendationReport(
            recommendations=[
                RecommendedTest(
                    recommendation_id="rec-1",
                    test_name="Payments Regression",
                    reason="payments",
                    priority=10,
                    confidence=0.8,
                )
            ],
            summary="",
            recommendation_confidence=0.8,
        ),
    )
    first = build_executive_quality_report(**kwargs)
    second = build_executive_quality_report(**kwargs)
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_template_summary_generation():
    report = build_executive_quality_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(risk_score=55, risk_level="medium", confidence=0.75, summary=""),
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id="a1",
                        contract_id="contract:payments_api",
                        overall_risk_level="CRITICAL",
                        risk_score=90,
                        confidence=0.85,
                        summary="",
                    ),
                    ContractRiskAssessment(
                        assessment_id="a2",
                        contract_id="contract:checkout_api",
                        overall_risk_level="CRITICAL",
                        risk_score=88,
                        confidence=0.85,
                        summary="",
                    ),
                ],
                summary="",
                confidence=0.85,
            ),
            data_journey_validation=DataJourneyReport(
                journeys=[DataJourney(journey_id="j1", name="Checkout Journey", business_area="checkout", stages=[])],
                results=[DataJourneyResult(journey_id="j1", status="BROKEN", completed_stages=1, total_stages=4, confidence=0.6, summary="")],
                summary="",
                confidence=0.6,
            ),
        )
    )
    assert "moderate quality risk" in report.executive_summary or "elevated" in report.executive_summary
    assert "contract" in report.executive_summary.lower()
    assert report.top_recommendations


def test_report_id_format():
    assert build_executive_report_id("demo", "abc12345").startswith("executive_quality_report:demo:")


def test_report_field_exists():
    report = ProjectIncidentInvestigationReport(project_id="demo", description="test")
    assert hasattr(report, "executive_quality_report")


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_investigate_includes_executive_quality_report(client: TestClient):
    with patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[]), patch(
        "services.incident_qa_investigator_service.gather_related_pr_analysis",
        return_value=[],
    ), patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[]), patch(
        "services.incident_qa_investigator_service.gather_knowledge_context",
        return_value=None,
    ), patch("services.incident_qa_investigator_service.gather_regressions", return_value=[]), patch(
        "services.incident_qa_investigator_service.gather_failure_clusters",
        return_value=[],
    ), patch("services.incident_qa_investigator_service.gather_failed_runs") as mock_runs, patch(
        "services.db.project_repository.project_repo.get_project",
        return_value=type("P", (), {"id": "demo", "name": "Demo"})(),
    ):
        from models.incident_models import RelatedRunSummary

        mock_runs.return_value = [
            RelatedRunSummary(
                run_id="run-1",
                test_id="PAY-001",
                status="failed",
                started_at="2026-06-10T10:00:00+00:00",
                module="payments",
                error_summary="fail",
            ),
        ]
        resp = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Payments quality risk after deploy", "module": "payments"},
        )
        assert resp.status_code == 200
        body = resp.json()
        assert "executive_quality_report" in body
        assert body["meta"]["analyze_only"] is True
