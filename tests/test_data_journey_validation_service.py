# tests/test_data_journey_validation_service.py
"""INT-01B — Data Journey Validation (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    ApiContract,
    ApiContractReport,
    ContractRiskAssessment,
    DatabaseValidationCheck,
    DatabaseValidationReport,
    DatabaseValidationResult,
    DeploymentRiskAssessment,
    IncidentImpactNode,
    ProjectIncidentInvestigationReport,
)
from services.data_journey_validation_service import (
    build_data_journey_validation,
    build_journey_id,
    build_stage_id,
)
from services.database_validation_service import build_check_id


def _base(**overrides):
    base = {
        "impact_map": [],
        "database_validation": None,
        "api_contract_intelligence": None,
        "deployment_risk_assessment": None,
        "hypotheses": [],
    }
    base.update(overrides)
    return base


def _checkout_checks_with_results(success: bool = False) -> DatabaseValidationReport:
    area = "Checkout"
    checks = [
        DatabaseValidationCheck(
            check_id=build_check_id(area, "validate_order_record_exists"),
            name="Validate Checkout order record exists",
            query="SELECT order_id FROM orders WHERE order_id = :order_id",
            database_type="postgresql",
        ),
        DatabaseValidationCheck(
            check_id=build_check_id(area, "validate_payment_status"),
            name="Validate Checkout payment status",
            query="SELECT payment_status FROM payments WHERE order_id = :order_id",
            database_type="postgresql",
        ),
        DatabaseValidationCheck(
            check_id=build_check_id(area, "validate_inventory_reservation"),
            name="Validate Checkout inventory reservation",
            query="SELECT reservation_id FROM inventory_reservations WHERE order_id = :order_id",
            database_type="postgresql",
        ),
        DatabaseValidationCheck(
            check_id=build_check_id(area, "validate_erp_sync_status"),
            name="Validate Checkout ERP sync status",
            query="SELECT erp_sync_status FROM order_fulfillment WHERE order_id = :order_id",
            database_type="postgresql",
        ),
    ]
    status = "SUCCESS" if success else "PLANNED"
    return DatabaseValidationReport(
        checks=checks,
        results=[
            DatabaseValidationResult(check_id=c.check_id, status=status, summary="ok", confidence=0.8)
            for c in checks
        ],
        summary="checks",
        confidence=0.8,
    )


def _payments_contract() -> ApiContractReport:
    contract_id = "contract:payments_api_post_payments_vv2"
    return ApiContractReport(
        contracts=[
            ApiContract(
                contract_id=contract_id,
                service_name="Payments API",
                endpoint="/payments",
                method="POST",
                version="v2",
                request_schema={},
                response_schema={},
                source="impact_map",
                confidence=0.8,
            )
        ],
        risk_assessments=[
            ContractRiskAssessment(
                assessment_id=f"assessment:{contract_id}",
                risk_level="LOW",
                confidence=0.8,
                summary="Low risk",
                changes=[],
            )
        ],
        summary="contract",
        confidence=0.8,
    )


def test_no_journeys_returns_none():
    assert build_data_journey_validation(**_base()) is None


def test_checkout_healthy():
    report = build_data_journey_validation(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Checkout failures",
                    severity="medium",
                    confidence=0.8,
                    related_entity_count=3,
                ),
            ],
            database_validation=_checkout_checks_with_results(success=True),
            api_contract_intelligence=_payments_contract(),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=30,
                risk_level="low",
                confidence=0.7,
                summary="Low risk",
            ),
        )
    )
    assert report is not None
    checkout = next(r for r in report.results if r.journey_id == build_journey_id("checkout"))
    assert checkout.status == "HEALTHY"
    assert checkout.completed_stages == checkout.total_stages


def test_checkout_degraded():
    db = _checkout_checks_with_results(success=True)
    db.checks = [c for c in db.checks if "erp" not in c.check_id]
    db.results = [r for r in db.results if "erp" not in r.check_id]
    report = build_data_journey_validation(
        **_base(
            impact_map=[IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.8, related_entity_count=2)],
            database_validation=db,
            api_contract_intelligence=_payments_contract(),
        )
    )
    assert report is not None
    checkout = next(r for r in report.results if r.journey_id == build_journey_id("checkout"))
    assert checkout.status == "DEGRADED"
    assert checkout.missing_stages


def test_checkout_broken():
    report = build_data_journey_validation(
        **_base(
            impact_map=[IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.9, related_entity_count=5)],
            database_validation=DatabaseValidationReport(
                checks=[
                    DatabaseValidationCheck(
                        check_id=build_check_id("Checkout", "validate_order_record_exists"),
                        name="order",
                        query="SELECT order_id FROM orders WHERE order_id = :order_id",
                    )
                ],
                results=[],
                summary="partial",
                confidence=0.5,
            ),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=90,
                risk_level="critical",
                confidence=0.9,
                summary="Critical",
            ),
        )
    )
    assert report is not None
    checkout = next(r for r in report.results if r.journey_id == build_journey_id("checkout"))
    assert checkout.status == "BROKEN"


def test_payments_validation():
    area = "Payments"
    report = build_data_journey_validation(
        **_base(
            impact_map=[IncidentImpactNode(title="Payments", description="", severity="high", confidence=0.82, related_entity_count=4)],
            database_validation=DatabaseValidationReport(
                checks=[
                    DatabaseValidationCheck(
                        check_id=build_check_id(area, "validate_payment_transaction_exists"),
                        name="transaction",
                        query="SELECT transaction_id FROM payment_transactions WHERE transaction_id = :transaction_id",
                    ),
                    DatabaseValidationCheck(
                        check_id=build_check_id(area, "validate_reconciliation_status"),
                        name="reconciliation",
                        query="SELECT status FROM payment_reconciliation WHERE transaction_id = :transaction_id",
                    ),
                ],
                results=[
                    DatabaseValidationResult(
                        check_id=build_check_id(area, "validate_payment_transaction_exists"),
                        status="SUCCESS",
                        summary="ok",
                        confidence=0.8,
                    ),
                    DatabaseValidationResult(
                        check_id=build_check_id(area, "validate_reconciliation_status"),
                        status="SUCCESS",
                        summary="ok",
                        confidence=0.8,
                    ),
                ],
                summary="payments checks",
                confidence=0.8,
            ),
            api_contract_intelligence=_payments_contract(),
        )
    )
    assert report is not None
    payments = next(r for r in report.results if r.journey_id == build_journey_id("payments"))
    assert payments.total_stages == 4


def test_authentication_validation():
    area = "Authentication"
    report = build_data_journey_validation(
        **_base(
            impact_map=[IncidentImpactNode(title="Authentication", description="", severity="medium", confidence=0.75, related_entity_count=2)],
            database_validation=DatabaseValidationReport(
                checks=[
                    DatabaseValidationCheck(
                        check_id=build_check_id(area, "validate_active_session_exists"),
                        name="session",
                        query="SELECT session_id FROM user_sessions WHERE user_id = :user_id",
                    ),
                    DatabaseValidationCheck(
                        check_id=build_check_id(area, "validate_auth_audit_event"),
                        name="audit",
                        query="SELECT event_type FROM auth_audit_events WHERE user_id = :user_id",
                    ),
                ],
                results=[
                    DatabaseValidationResult(
                        check_id=build_check_id(area, "validate_active_session_exists"),
                        status="SUCCESS",
                        summary="ok",
                        confidence=0.8,
                    ),
                    DatabaseValidationResult(
                        check_id=build_check_id(area, "validate_auth_audit_event"),
                        status="SUCCESS",
                        summary="ok",
                        confidence=0.8,
                    ),
                ],
                summary="auth checks",
                confidence=0.8,
            ),
        )
    )
    assert report is not None
    auth = next(r for r in report.results if r.journey_id == build_journey_id("authentication"))
    assert auth.total_stages == 3


def test_consistency_failure():
    db = _checkout_checks_with_results(success=True)
    db.checks = [c for c in db.checks if "inventory" not in c.check_id]
    db.results = [r for r in db.results if "inventory" not in r.check_id]
    report = build_data_journey_validation(
        **_base(
            impact_map=[IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.9, related_entity_count=4)],
            database_validation=db,
            api_contract_intelligence=_payments_contract(),
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=92,
                risk_level="critical",
                confidence=0.9,
                summary="Critical deployment risk",
            ),
        )
    )
    assert report is not None
    checkout = next(r for r in report.results if r.journey_id == build_journey_id("checkout"))
    assert checkout.status in ("DEGRADED", "BROKEN")
    assert checkout.missing_stages or checkout.inconsistent_stages


def test_deterministic_ids():
    journey_id = build_journey_id("checkout")
    stage_id = build_stage_id(journey_id, "orders_db")
    assert journey_id == "journey:checkout"
    assert stage_id == "journey:checkout:stage:orders_db"


def test_deterministic_output():
    kwargs = _base(
        impact_map=[IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.8, related_entity_count=3)],
        database_validation=_checkout_checks_with_results(success=True),
        api_contract_intelligence=_payments_contract(),
    )
    first = build_data_journey_validation(**kwargs)
    second = build_data_journey_validation(**kwargs)
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_no_writes():
    with patch("services.db.database_connector_repository.database_connector_repo.insert_execution") as insert_exec:
        build_data_journey_validation(
            **_base(
                impact_map=[IncidentImpactNode(title="Checkout", description="", severity="high", confidence=0.8, related_entity_count=2)],
                database_validation=_checkout_checks_with_results(),
                api_contract_intelligence=_payments_contract(),
            )
        )
        insert_exec.assert_not_called()


def test_report_field_exists():
    report = ProjectIncidentInvestigationReport(project_id="demo", description="test")
    assert hasattr(report, "data_journey_validation")


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_investigate_includes_data_journey_validation(client: TestClient):
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
            json={"description": "Checkout broken after deploy", "module": "checkout"},
        )
        assert resp.status_code == 200
        body = resp.json()
        assert "data_journey_validation" in body
        assert body["meta"]["analyze_only"] is True
