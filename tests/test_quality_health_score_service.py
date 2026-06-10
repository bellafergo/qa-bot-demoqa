# tests/test_quality_health_score_service.py
"""OBS-01A — Quality Health Score (read-only)."""
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
    DependencyNode,
    DeploymentRiskAssessment,
    EnterpriseDependencyMap,
    EnvironmentProfile,
    ExecutiveQualityReport,
    HistoricalLearningReport,
    IncidentImpactNode,
    MultiEnvironmentReport,
    ProjectIncidentInvestigationReport,
    RecommendedTest,
    SimilarIncident,
    TestRecommendationReport,
)
from services.quality_health_score_service import (
    build_quality_health_report,
    build_score_id,
)


def _base(**overrides):
    base = {
        "project_id": "demo",
        "executive_quality_report": None,
        "multi_environment": None,
        "decision_center": None,
        "deployment_risk_assessment": None,
        "contract_risk_assessment": None,
        "data_journey_validation": None,
        "database_validation": None,
        "enterprise_dependency_map": None,
        "historical_learning": None,
        "test_recommendations": None,
        "recommended_actions": [],
        "impact_map": [],
    }
    base.update(overrides)
    return base


def test_empty_state_returns_none():
    assert build_quality_health_report(**_base()) is None


def test_excellent_score():
    report = build_quality_health_report(
        **_base(
            executive_quality_report=ExecutiveQualityReport(
                report_id="eqr-1",
                generated_at="2026-06-10T10:00:00+00:00",
                overall_quality_score=95,
                overall_risk_level="LOW",
                confidence=0.9,
                executive_summary="Strong quality posture.",
                quality_trend="stable",
            ),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=10,
                risk_level="low",
                confidence=0.9,
                summary="Low deployment risk",
            ),
        )
    )
    assert report is not None
    assert report.overall_score >= 90
    assert report.overall_status == "EXCELLENT"


def test_good_score():
    report = build_quality_health_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=30,
                risk_level="medium",
                confidence=0.8,
                summary="Medium deployment risk",
            ),
            executive_quality_report=ExecutiveQualityReport(
                report_id="eqr-1",
                generated_at="2026-06-10T10:00:00+00:00",
                overall_quality_score=82,
                overall_risk_level="MEDIUM",
                confidence=0.8,
                executive_summary="Generally healthy.",
                quality_trend="stable",
            ),
        )
    )
    assert report is not None
    assert 75 <= report.overall_score <= 89
    assert report.overall_status == "GOOD"


def test_attention_score():
    report = build_quality_health_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=55,
                risk_level="high",
                confidence=0.8,
                summary="High deployment risk",
            ),
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="rec-1",
                        test_name="Smoke Suite",
                        reason="baseline",
                        priority=50,
                        confidence=0.7,
                    ),
                    RecommendedTest(
                        recommendation_id="rec-2",
                        test_name="Regression Suite",
                        reason="coverage",
                        priority=40,
                        confidence=0.7,
                    ),
                ],
                summary="",
                recommendation_confidence=0.7,
            ),
        )
    )
    assert report is not None
    assert 50 <= report.overall_score <= 74
    assert report.overall_status == "ATTENTION"


def test_high_risk_score():
    report = build_quality_health_report(
        **_base(
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id="a1",
                        contract_id="contract:payments_api",
                        overall_risk_level="CRITICAL",
                        risk_score=95,
                        confidence=0.9,
                        summary="Critical payments contract",
                        affected_modules=["Payments"],
                    )
                ],
                summary="",
                confidence=0.9,
            ),
            data_journey_validation=DataJourneyReport(
                journeys=[DataJourney(journey_id="j1", name="Checkout Journey", business_area="checkout", stages=[])],
                results=[
                    DataJourneyResult(
                        journey_id="j1",
                        status="BROKEN",
                        completed_stages=1,
                        total_stages=5,
                        confidence=0.6,
                        summary="broken",
                    )
                ],
                summary="",
                confidence=0.6,
            ),
            historical_learning=HistoricalLearningReport(
                similar_incidents=[
                    SimilarIncident(
                        incident_id="inc-1",
                        title="Payments failures",
                        summary="Payments failures",
                        similarity_score=0.82,
                        occurrence_timestamp="2026-05-01T10:00:00+00:00",
                    )
                ],
                pattern_summary="Repeated payments incidents",
                confidence=0.75,
            ),
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="rec-1",
                        test_name="Payments Regression Suite",
                        reason="payments",
                        priority=10,
                        confidence=0.8,
                    )
                ],
                summary="",
                recommendation_confidence=0.8,
            ),
            impact_map=[
                IncidentImpactNode(
                    title="Payments",
                    description="Payments module impacted",
                    severity="high",
                    confidence=0.8,
                )
            ],
        )
    )
    assert report is not None
    assert report.overall_score <= 49
    assert report.overall_status == "HIGH_RISK"
    payments = next(s for s in report.scores if s.scope_type == "module" and s.scope_name == "Payments")
    assert payments.score <= 49
    assert payments.status == "HIGH_RISK"
    assert payments.trend == "DEGRADING"


def test_critical_deployment_deduction():
    report = build_quality_health_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=95,
                risk_level="critical",
                confidence=0.9,
                summary="Critical deployment risk",
            ),
        )
    )
    project = next(s for s in report.scores if s.scope_type == "project")
    assert any("Deployment risk" in f.title for f in project.contributing_factors)
    assert report.overall_score <= 70


def test_contract_risk_deduction():
    report = build_quality_health_report(
        **_base(
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id="a1",
                        contract_id="contract:payments_api",
                        overall_risk_level="CRITICAL",
                        risk_score=95,
                        confidence=0.9,
                        summary="Removed amount field",
                    )
                ],
                summary="",
                confidence=0.9,
            ),
        )
    )
    contract = next(s for s in report.scores if s.scope_type == "contract")
    assert contract.score <= 75
    assert any("contract risk" in f.title.lower() for f in contract.contributing_factors)


def test_broken_journey_deduction():
    report = build_quality_health_report(
        **_base(
            data_journey_validation=DataJourneyReport(
                journeys=[DataJourney(journey_id="journey:checkout", name="Checkout Journey", business_area="checkout", stages=[])],
                results=[
                    DataJourneyResult(
                        journey_id="journey:checkout",
                        status="BROKEN",
                        completed_stages=1,
                        total_stages=5,
                        confidence=0.6,
                        summary="broken",
                    )
                ],
                summary="",
                confidence=0.6,
            ),
        )
    )
    journey = next(s for s in report.scores if s.scope_type == "journey")
    assert journey.score <= 75
    assert journey.trend == "DEGRADING"


def test_environment_score_generation():
    report = build_quality_health_report(
        **_base(
            multi_environment=MultiEnvironmentReport(
                environments=[
                    EnvironmentProfile(
                        environment_id="env:staging",
                        name="STAGING",
                        type="STAGING",
                        status="DEGRADED",
                        is_production=False,
                    )
                ],
                signals=[],
                comparisons=[],
                promotion_readiness=[],
                summary="",
                confidence=0.8,
            ),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=40,
                risk_level="medium",
                confidence=0.75,
                summary="Medium",
            ),
        )
    )
    env = next(s for s in report.scores if s.scope_type == "environment")
    assert env.scope_name == "STAGING"
    assert env.score < 100


def test_module_score_generation():
    report = build_quality_health_report(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Payments",
                    description="Payments failures",
                    severity="high",
                    confidence=0.8,
                )
            ],
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id="a1",
                        contract_id="contract:payments_api",
                        overall_risk_level="CRITICAL",
                        risk_score=95,
                        confidence=0.9,
                        summary="Critical",
                        affected_modules=["Payments"],
                    )
                ],
                summary="",
                confidence=0.9,
            ),
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="rec-1",
                        test_name="Payments Regression Suite",
                        reason="payments",
                        priority=10,
                        confidence=0.8,
                    )
                ],
                summary="",
                recommendation_confidence=0.8,
            ),
            historical_learning=HistoricalLearningReport(
                similar_incidents=[
                    SimilarIncident(
                        incident_id="inc-1",
                        title="Payments issue",
                        summary="Payments issue",
                        similarity_score=0.8,
                        occurrence_timestamp="2026-05-01T10:00:00+00:00",
                    )
                ],
                pattern_summary="Repeated payments incidents",
                confidence=0.75,
            ),
        )
    )
    module = next(s for s in report.scores if s.scope_type == "module")
    assert module.scope_name == "Payments"
    assert module.score <= 49


def test_deterministic_output():
    kwargs = _base(
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=55,
            risk_level="medium",
            confidence=0.75,
            summary="Medium",
        ),
    )
    first = build_quality_health_report(**kwargs)
    second = build_quality_health_report(**kwargs)
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_no_external_calls():
    import services.quality_health_score_service as mod

    source = open(mod.__file__, encoding="utf-8").read()
    assert "requests." not in source
    assert "httpx." not in source
    build_quality_health_report(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=20,
                risk_level="low",
                confidence=0.8,
                summary="Low",
            ),
        )
    )


def test_report_field_exists():
    report = ProjectIncidentInvestigationReport(project_id="demo", description="test")
    assert hasattr(report, "quality_health")


def test_deterministic_ids():
    assert build_score_id("project", "overall") == "quality_health:project:overall"


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_investigate_includes_quality_health(client: TestClient):
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
            json={"description": "Payments quality health check", "module": "payments"},
        )
        assert resp.status_code == 200
        body = resp.json()
        assert "quality_health" in body
        assert body["meta"]["analyze_only"] is True
