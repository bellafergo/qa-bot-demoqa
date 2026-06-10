# tests/test_environment_intelligence_service.py
"""ENT-01A — Multi-Environment Intelligence (read-only)."""
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
    DatabaseValidationCheck,
    DatabaseValidationReport,
    DeploymentRiskAssessment,
    ProjectIncidentInvestigationReport,
    RecommendedTest,
    TestRecommendationReport,
)
from services.environment_intelligence_service import (
    build_environment_id,
    build_multi_environment_intelligence,
    build_signal_id,
)


def _base(**overrides):
    base = {
        "project_metadata": None,
        "executive_quality_report": None,
        "decision_center": None,
        "deployment_risk_assessment": None,
        "contract_risk_assessment": None,
        "data_journey_validation": None,
        "database_validation": None,
        "enterprise_dependency_map": None,
        "api_contract_intelligence": None,
        "historical_learning": None,
        "test_recommendations": None,
        "recommended_actions": [],
        "recommended_tests": [],
        "browser_watch_alert_count": 0,
    }
    base.update(overrides)
    return base


def test_empty_state_returns_none():
    assert build_multi_environment_intelligence(**_base()) is None


def test_default_environments_generated():
    report = build_multi_environment_intelligence(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=40,
                risk_level="medium",
                confidence=0.75,
                summary="Medium deployment risk",
            ),
        )
    )
    assert report is not None
    names = {e.name for e in report.environments}
    assert names == {"QA", "STAGING", "PROD"}


def test_critical_signal_marks_environment_broken():
    staging_id = build_environment_id("staging")
    report = build_multi_environment_intelligence(
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
    staging = next(e for e in report.environments if e.environment_id == staging_id)
    assert staging.status == "BROKEN"


def test_high_risk_marks_environment_degraded():
    report = build_multi_environment_intelligence(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=80,
                risk_level="high",
                confidence=0.85,
                summary="High deployment risk",
            ),
        )
    )
    staging = next(e for e in report.environments if e.name == "STAGING")
    assert staging.status == "DEGRADED"


def test_healthy_status_when_no_blockers():
    report = build_multi_environment_intelligence(
        **_base(
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="rec-1",
                        test_name="Smoke Suite",
                        reason="baseline",
                        priority=50,
                        confidence=0.7,
                    )
                ],
                summary="",
                recommendation_confidence=0.7,
            ),
        )
    )
    qa = next(e for e in report.environments if e.name == "QA")
    assert qa.status in ("HEALTHY", "UNKNOWN", "DEGRADED")


def test_qa_to_staging_comparison():
    report = build_multi_environment_intelligence(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=80,
                risk_level="high",
                confidence=0.85,
                summary="High deployment risk",
            ),
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="rec-1",
                        test_name="QA Smoke",
                        reason="qa",
                        priority=20,
                        confidence=0.8,
                    )
                ],
                summary="",
                recommendation_confidence=0.8,
            ),
        )
    )
    comparison = next(c for c in report.comparisons if "QA" in c.comparison_type)
    assert comparison.risk_delta != 0
    assert "riskier" in comparison.summary.lower() or "healthier" in comparison.summary.lower()


def test_staging_to_prod_promotion_readiness_ready():
    report = build_multi_environment_intelligence(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=20,
                risk_level="low",
                confidence=0.8,
                summary="Low risk",
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
        )
    )
    readiness = next(
        p for p in report.promotion_readiness
        if p.source_environment_id.endswith("staging") and p.target_environment_id.endswith("prod")
    )
    assert readiness.readiness_status in ("READY", "CAUTION")


def test_staging_to_prod_promotion_readiness_blocked():
    report = build_multi_environment_intelligence(
        **_base(
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id="a1",
                        contract_id="contract:payments_api",
                        overall_risk_level="CRITICAL",
                        risk_score=95,
                        confidence=0.9,
                        summary="Removed amount",
                        affected_tests=["Payments Regression Suite"],
                    )
                ],
                summary="",
                confidence=0.9,
            ),
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
            database_validation=DatabaseValidationReport(
                checks=[
                    DatabaseValidationCheck(
                        check_id="check:checkout:validate_order_record_exists",
                        name="Validate Orders DB",
                        query="SELECT order_id FROM orders",
                    )
                ],
                results=[],
                summary="",
                confidence=0.75,
            ),
        )
    )
    readiness = next(
        p for p in report.promotion_readiness
        if p.source_environment_id.endswith("staging") and p.target_environment_id.endswith("prod")
    )
    assert readiness.readiness_status == "BLOCKED"
    assert readiness.readiness_score <= 60
    assert any("contract" in b.lower() or "checkout" in b.lower() for b in readiness.blockers)
    assert readiness.recommended_validations


def test_recommended_validations_from_test_recommendations():
    report = build_multi_environment_intelligence(
        **_base(
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
            database_validation=DatabaseValidationReport(
                checks=[
                    DatabaseValidationCheck(
                        check_id="check:checkout:orders",
                        name="Validate Orders DB",
                        query="SELECT 1",
                    )
                ],
                results=[],
                summary="",
                confidence=0.7,
            ),
        )
    )
    readiness = report.promotion_readiness[0]
    assert any("Payments" in v or "Orders" in v for v in readiness.recommended_validations)


def test_deterministic_ids():
    env_id = build_environment_id("staging")
    assert env_id == "env:staging"
    assert build_signal_id(env_id, "deployment_risk", "primary").startswith("signal:env:staging:")


def test_deterministic_output():
    kwargs = _base(
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=55,
            risk_level="medium",
            confidence=0.75,
            summary="Medium",
        ),
    )
    first = build_multi_environment_intelligence(**kwargs)
    second = build_multi_environment_intelligence(**kwargs)
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_no_external_calls():
    import services.environment_intelligence_service as mod

    source = open(mod.__file__, encoding="utf-8").read()
    assert "requests." not in source
    assert "httpx." not in source
    build_multi_environment_intelligence(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=30,
                risk_level="low",
                confidence=0.8,
                summary="Low",
            ),
        )
    )


def test_report_field_exists():
    report = ProjectIncidentInvestigationReport(project_id="demo", description="test")
    assert hasattr(report, "multi_environment")


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_investigate_includes_multi_environment(client: TestClient):
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
            json={"description": "Payments issue across environments", "module": "payments"},
        )
        assert resp.status_code == 200
        body = resp.json()
        assert "multi_environment" in body
        assert body["meta"]["analyze_only"] is True
