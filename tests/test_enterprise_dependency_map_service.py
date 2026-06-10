# tests/test_enterprise_dependency_map_service.py
"""INT-04A — Enterprise Dependency Map (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    ApiContract,
    ApiContractChangeAssessment,
    ApiContractReport,
    ContractChange,
    ContractRiskAssessment,
    ContractRiskReport,
    DatabaseValidationCheck,
    DatabaseValidationReport,
    DataJourney,
    DataJourneyReport,
    DataJourneyResult,
    DeploymentRiskAssessment,
    IncidentImpactNode,
    JourneyStage,
    ProjectIncidentInvestigationReport,
    RecommendedTest,
    TestRecommendationReport,
)
from services.api_contract_intelligence_service import build_assessment_id, build_change_id, build_contract_id
from services.data_journey_validation_service import build_journey_id, build_stage_id
from services.enterprise_dependency_map_service import (
    build_edge_id,
    build_enterprise_dependency_map,
    build_node_id,
)


def _base(**overrides):
    base = {
        "impact_map": [],
        "data_journey_validation": None,
        "api_contract_intelligence": None,
        "contract_risk_assessment": None,
        "database_validation": None,
        "deployment_risk_assessment": None,
        "historical_learning": None,
        "decision_center": None,
        "test_recommendations": None,
        "recommended_tests": [],
    }
    base.update(overrides)
    return base


def _checkout_journey() -> DataJourneyReport:
    journey_id = build_journey_id("checkout")
    stages = [
        JourneyStage(stage_id=build_stage_id(journey_id, "ui"), name="Web UI", stage_type="ui", validation_check_ids=[]),
        JourneyStage(stage_id=build_stage_id(journey_id, "payments_api"), name="Payments API", stage_type="api", validation_check_ids=[]),
        JourneyStage(stage_id=build_stage_id(journey_id, "orders_db"), name="Orders DB", stage_type="database", validation_check_ids=[]),
        JourneyStage(stage_id=build_stage_id(journey_id, "inventory_db"), name="Inventory DB", stage_type="database", validation_check_ids=[]),
        JourneyStage(stage_id=build_stage_id(journey_id, "erp_sync"), name="ERP Sync", stage_type="erp", validation_check_ids=[]),
    ]
    return DataJourneyReport(
        journeys=[DataJourney(journey_id=journey_id, name="Checkout Journey", business_area="checkout", stages=stages)],
        results=[DataJourneyResult(journey_id=journey_id, status="DEGRADED", completed_stages=3, total_stages=5, confidence=0.7, summary="")],
        summary="checkout",
        confidence=0.7,
    )


def _auth_journey() -> DataJourneyReport:
    journey_id = build_journey_id("authentication")
    stages = [
        JourneyStage(stage_id=build_stage_id(journey_id, "login"), name="User Login", stage_type="ui", validation_check_ids=[]),
        JourneyStage(stage_id=build_stage_id(journey_id, "session_store"), name="Session Store", stage_type="database", validation_check_ids=[]),
        JourneyStage(stage_id=build_stage_id(journey_id, "audit_log"), name="Audit Log", stage_type="audit", validation_check_ids=[]),
    ]
    return DataJourneyReport(
        journeys=[DataJourney(journey_id=journey_id, name="Authentication Journey", business_area="authentication", stages=stages)],
        results=[DataJourneyResult(journey_id=journey_id, status="HEALTHY", completed_stages=3, total_stages=3, confidence=0.8, summary="")],
        summary="auth",
        confidence=0.8,
    )


def _payments_contract_intel() -> ApiContractReport:
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    return ApiContractReport(
        contracts=[
            ApiContract(
                contract_id=contract_id,
                service_name="Payments API",
                endpoint="/payments",
                method="POST",
                version="v2",
                confidence=0.85,
            )
        ],
        risk_assessments=[
            ApiContractChangeAssessment(
                assessment_id=build_assessment_id(contract_id),
                risk_level="HIGH",
                confidence=0.85,
                summary="Removed amount",
                changes=[
                    ContractChange(
                        change_id=build_change_id(contract_id, "removed_field", "amount"),
                        severity="HIGH",
                        change_type="removed_field",
                        field_name="amount",
                        description="Removed field 'amount'.",
                    )
                ],
            )
        ],
        summary="contract",
        confidence=0.85,
    )


def test_empty_state_returns_none():
    assert build_enterprise_dependency_map(**_base()) is None


def test_checkout_dependency_graph():
    report = build_enterprise_dependency_map(
        **_base(
            data_journey_validation=_checkout_journey(),
            impact_map=[IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.85, related_entity_count=3)],
        )
    )
    assert report is not None
    journey_nodes = [n for n in report.nodes if n.node_type == "journey"]
    assert any(n.name == "Checkout Journey" for n in journey_nodes)
    assert any(n.name == "Payments API" for n in report.nodes)
    assert any(e.relationship_type == "depends_on" for e in report.edges)
    assert len(report.edges) >= 4


def test_authentication_dependency_graph():
    report = build_enterprise_dependency_map(
        **_base(
            data_journey_validation=_auth_journey(),
            impact_map=[IncidentImpactNode(title="Authentication", description="", severity="medium", confidence=0.75, related_entity_count=2)],
        )
    )
    assert report is not None
    assert any(n.name == "Authentication Journey" for n in report.nodes)
    assert any(n.name == "Session Store" for n in report.nodes)
    assert any(n.name == "Audit Log" for n in report.nodes)


def test_contract_dependency_propagation():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    report = build_enterprise_dependency_map(
        **_base(
            data_journey_validation=_checkout_journey(),
            api_contract_intelligence=_payments_contract_intel(),
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id=f"business_risk_assessment:{contract_id}",
                        contract_id=contract_id,
                        overall_risk_level="CRITICAL",
                        risk_score=92,
                        confidence=0.9,
                        summary="Critical payments contract risk",
                        affected_journeys=["Checkout", "Payments"],
                        affected_modules=["Payments"],
                        affected_tests=["Payments Regression"],
                    )
                ],
                summary="critical",
                confidence=0.9,
            ),
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="rec-pay",
                        test_name="Payments Regression",
                        reason="payments contract",
                        priority=10,
                        confidence=0.8,
                    )
                ],
                summary="tests",
                recommendation_confidence=0.8,
            ),
        )
    )
    assert report is not None
    contract_nodes = [n for n in report.nodes if n.node_type == "contract"]
    assert contract_nodes and contract_nodes[0].risk_level == "CRITICAL"
    assert any(e.relationship_type == "uses" for e in report.edges)
    assert any(e.relationship_type == "validates" for e in report.edges)


def test_risk_propagation():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    report = build_enterprise_dependency_map(
        **_base(
            data_journey_validation=_checkout_journey(),
            api_contract_intelligence=_payments_contract_intel(),
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id=f"business_risk_assessment:{contract_id}",
                        contract_id=contract_id,
                        overall_risk_level="CRITICAL",
                        risk_score=95,
                        confidence=0.9,
                        summary="critical",
                        affected_journeys=["Checkout"],
                        affected_modules=[],
                        affected_tests=[],
                    )
                ],
                summary="",
                confidence=0.9,
            ),
        )
    )
    journey = next(n for n in report.nodes if n.name == "Checkout Journey")
    assert journey.risk_level in ("HIGH", "CRITICAL")


def test_blast_radius_calculation():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    report = build_enterprise_dependency_map(
        **_base(
            data_journey_validation=_checkout_journey(),
            api_contract_intelligence=_payments_contract_intel(),
            database_validation=DatabaseValidationReport(
                checks=[
                    DatabaseValidationCheck(
                        check_id="check:checkout:validate_order_record_exists",
                        name="Validate Checkout order record exists",
                        query="SELECT order_id FROM orders",
                    )
                ],
                results=[],
                summary="checks",
                confidence=0.75,
            ),
            contract_risk_assessment=ContractRiskReport(
                assessments=[
                    ContractRiskAssessment(
                        assessment_id=f"business_risk_assessment:{contract_id}",
                        contract_id=contract_id,
                        overall_risk_level="CRITICAL",
                        risk_score=95,
                        confidence=0.9,
                        summary="critical blast",
                        affected_journeys=["Checkout"],
                        affected_modules=["Checkout"],
                        affected_tests=["Payments Regression"],
                    )
                ],
                summary="",
                confidence=0.9,
            ),
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
    )
    assert "blast radius" in report.summary.lower() or "CRITICAL" in report.summary
    elevated = [n for n in report.nodes if n.risk_level in ("HIGH", "CRITICAL")]
    assert len(elevated) >= 2


def test_deterministic_ids():
    assert build_node_id("journey", "checkout") == "node:journey:checkout"
    source = build_node_id("journey", "checkout")
    target = build_node_id("api", "payments")
    assert build_edge_id(source, "depends_on", target) == f"edge:{source}:depends_on:{target}"


def test_deterministic_output():
    kwargs = _base(
        data_journey_validation=_checkout_journey(),
        api_contract_intelligence=_payments_contract_intel(),
        impact_map=[IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.8, related_entity_count=2)],
    )
    first = build_enterprise_dependency_map(**kwargs)
    second = build_enterprise_dependency_map(**kwargs)
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_no_external_discovery():
    import services.enterprise_dependency_map_service as mod

    source = open(mod.__file__, encoding="utf-8").read()
    assert "requests." not in source
    assert "httpx." not in source
    assert "urllib.request" not in source
    build_enterprise_dependency_map(
        **_base(
            data_journey_validation=_checkout_journey(),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=70,
                risk_level="high",
                confidence=0.8,
                summary="High",
            ),
        )
    )


def test_report_field_exists():
    report = ProjectIncidentInvestigationReport(project_id="demo", description="test")
    assert hasattr(report, "enterprise_dependency_map")


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_investigate_includes_enterprise_dependency_map(client: TestClient):
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
                test_id="CHK-001",
                status="failed",
                started_at="2026-06-10T10:00:00+00:00",
                module="checkout",
                error_summary="fail",
            ),
        ]
        resp = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Checkout broken after payments API change", "module": "checkout"},
        )
        assert resp.status_code == 200
        body = resp.json()
        assert "enterprise_dependency_map" in body
        assert body["meta"]["analyze_only"] is True
