# tests/test_contract_risk_assessment_service.py
"""INT-02B — Contract Risk Assessment (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    ApiContract,
    ApiContractChangeAssessment,
    ApiContractReport,
    ContractChange,
    DataJourney,
    DataJourneyReport,
    DataJourneyResult,
    DeploymentRiskAssessment,
    HistoricalLearningReport,
    IncidentImpactNode,
    ProjectIncidentInvestigationReport,
    RecommendedTest,
    SimilarIncident,
    TestRecommendationReport,
)
from services.api_contract_intelligence_service import build_assessment_id, build_change_id, build_contract_id
from services.contract_risk_assessment_service import (
    build_business_assessment_id,
    build_contract_risk_assessment,
    build_factor_id,
)


def _contract_and_intel(changes: list[ContractChange]) -> ApiContractReport:
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    contract = ApiContract(
        contract_id=contract_id,
        service_name="Payments API",
        endpoint="/payments",
        method="POST",
        version="v2",
        request_schema={},
        response_schema={},
        source="impact_map",
        confidence=0.85,
    )
    return ApiContractReport(
        contracts=[contract],
        risk_assessments=[
            ApiContractChangeAssessment(
                assessment_id=build_assessment_id(contract_id),
                risk_level="HIGH",
                confidence=0.85,
                summary="Contract changes detected.",
                changes=changes,
            )
        ],
        summary="1 contract",
        confidence=0.85,
    )


def _change(change_type: str, field_name: str, **kwargs) -> ContractChange:
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    return ContractChange(
        change_id=build_change_id(contract_id, change_type, field_name),
        severity=kwargs.get("severity", "HIGH"),
        change_type=change_type,
        field_name=field_name,
        old_value=kwargs.get("old_value"),
        new_value=kwargs.get("new_value"),
        description=kwargs.get("description", f"{change_type} on {field_name}"),
    )


def _base_kwargs(**overrides):
    base = {
        "api_contract_intelligence": None,
        "deployment_risk_assessment": None,
        "historical_learning": None,
        "impact_map": [],
        "test_recommendations": None,
        "data_journey_validation": None,
        "decision_center": None,
    }
    base.update(overrides)
    return base


def test_empty_state_returns_none():
    assert build_contract_risk_assessment(**_base_kwargs()) is None
    assert build_contract_risk_assessment(**_base_kwargs(api_contract_intelligence=ApiContractReport())) is None


def test_added_field_risk():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel(
                [_change("added_field", "reference_code", severity="LOW")]
            ),
        )
    )
    assert report is not None
    assessment = report.assessments[0]
    assert assessment.risk_score == 10
    assert assessment.overall_risk_level == "LOW"


def test_removed_field_risk():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel(
                [_change("removed_field", "legacy_flag", severity="HIGH")]
            ),
        )
    )
    assert report is not None
    assessment = report.assessments[0]
    assert assessment.risk_score == 50
    assert assessment.overall_risk_level == "HIGH"


def test_rename_risk():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel(
                [
                    _change(
                        "rename",
                        "reference_id",
                        old_value="ref_id",
                        new_value="reference_id",
                    )
                ]
            ),
        )
    )
    assert report.assessments[0].risk_score == 60


def test_type_change_risk():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel(
                [_change("type_change", "reference_id", old_value="number", new_value="string", severity="CRITICAL")]
            ),
        )
    )
    assert report.assessments[0].risk_score == 80


def test_historical_multiplier():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel([_change("removed_field", "amount")]),
            historical_learning=HistoricalLearningReport(
                similar_incidents=[
                    SimilarIncident(incident_id="inc-1", title="Payments outage", summary="checkout failed", similarity_score=0.82),
                    SimilarIncident(incident_id="inc-2", title="Checkout regression", summary="amount missing", similarity_score=0.78),
                ],
                pattern_summary="Payments and checkout incidents recur after API deploys.",
                confidence=0.8,
            ),
        )
    )
    assessment = report.assessments[0]
    assert any(f.factor_id.endswith("historical_incidents") for f in assessment.factors)
    assert assessment.risk_score >= 60


def test_deployment_risk_multiplier():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel([_change("removed_field", "amount")]),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=88,
                risk_level="high",
                confidence=0.9,
                summary="High deployment risk",
            ),
        )
    )
    assessment = report.assessments[0]
    assert any(f.factor_id.endswith("high_deployment_risk") for f in assessment.factors)
    assert assessment.risk_score >= 65


def test_journey_impact_multiplier():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel([_change("removed_field", "amount")]),
            impact_map=[
                IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.9, related_entity_count=3),
                IncidentImpactNode(title="Payments", description="", severity="medium", confidence=0.8, related_entity_count=2),
            ],
            data_journey_validation=DataJourneyReport(
                journeys=[
                    DataJourney(
                        journey_id="journey:checkout",
                        name="Checkout Journey",
                        business_area="checkout",
                        stages=[],
                    ),
                    DataJourney(
                        journey_id="journey:payments",
                        name="Payments Journey",
                        business_area="payments",
                        stages=[],
                    ),
                ],
                results=[
                    DataJourneyResult(
                        journey_id="journey:checkout",
                        status="DEGRADED",
                        completed_stages=3,
                        total_stages=5,
                        confidence=0.7,
                        summary="degraded",
                    ),
                    DataJourneyResult(
                        journey_id="journey:payments",
                        status="BROKEN",
                        completed_stages=1,
                        total_stages=4,
                        confidence=0.6,
                        summary="broken",
                    ),
                ],
                summary="journeys",
                confidence=0.7,
            ),
        )
    )
    assessment = report.assessments[0]
    assert "Checkout" in assessment.affected_journeys
    assert "Payments" in assessment.affected_journeys
    assert any(f.factor_id.endswith("journey_impact") for f in assessment.factors)


def test_risk_level_mapping_critical():
    report = build_contract_risk_assessment(
        **_base_kwargs(
            api_contract_intelligence=_contract_and_intel([_change("removed_field", "amount")]),
            impact_map=[
                IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.9, related_entity_count=4),
                IncidentImpactNode(title="Payments", description="", severity="high", confidence=0.9, related_entity_count=3),
            ],
            data_journey_validation=DataJourneyReport(
                journeys=[
                    DataJourney(journey_id="journey:checkout", name="Checkout Journey", business_area="checkout", stages=[]),
                    DataJourney(journey_id="journey:payments", name="Payments Journey", business_area="payments", stages=[]),
                ],
                results=[],
                summary="journeys",
                confidence=0.7,
            ),
            historical_learning=HistoricalLearningReport(
                similar_incidents=[
                    SimilarIncident(
                        incident_id=f"inc-{i}",
                        title="Payments API failure",
                        summary="amount field",
                        similarity_score=0.8,
                    )
                    for i in range(3)
                ],
                pattern_summary="payments",
                confidence=0.85,
            ),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=92,
                risk_level="high",
                confidence=0.9,
                summary="High",
            ),
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="rec-1",
                        test_name="Payments Regression",
                        reason="payments contract change",
                        priority=10,
                        confidence=0.8,
                    ),
                    RecommendedTest(
                        recommendation_id="rec-2",
                        test_name="Checkout Smoke",
                        reason="checkout impacted",
                        priority=12,
                        confidence=0.75,
                    ),
                ],
                summary="tests",
                recommendation_confidence=0.8,
            ),
        )
    )
    assessment = report.assessments[0]
    assert assessment.overall_risk_level == "CRITICAL"
    assert assessment.risk_score >= 75
    assert "Payments Regression" in assessment.affected_tests
    assert "Checkout Smoke" in assessment.affected_tests


def test_deterministic_output():
    kwargs = _base_kwargs(
        api_contract_intelligence=_contract_and_intel([_change("removed_field", "amount")]),
        impact_map=[IncidentImpactNode(title="Payments", description="", severity="high", confidence=0.8, related_entity_count=2)],
    )
    first = build_contract_risk_assessment(**kwargs)
    second = build_contract_risk_assessment(**kwargs)
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_deterministic_ids():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    assert build_business_assessment_id(contract_id) == f"business_risk_assessment:{contract_id}"
    assert build_factor_id(contract_id, "journey_impact") == f"factor:{contract_id}:journey_impact"


def test_report_field_exists():
    report = ProjectIncidentInvestigationReport(project_id="demo", description="test")
    assert hasattr(report, "contract_risk_assessment")


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_investigate_includes_contract_risk_assessment(client: TestClient):
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
            json={"description": "Payments API contract broke checkout", "module": "payments"},
        )
        assert resp.status_code == 200
        body = resp.json()
        assert "contract_risk_assessment" in body
        assert body["meta"]["analyze_only"] is True
