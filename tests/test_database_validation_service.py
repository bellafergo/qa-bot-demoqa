# tests/test_database_validation_service.py
"""INT-01A — Database Validation Checks (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    IncidentImpactNode,
    ProjectIncidentInvestigationReport,
    RelatedRunSummary,
)
from services.database_validation_service import (
    build_check_id,
    build_database_validation,
    validate_query_safety,
)


def _base(**overrides):
    base = {
        "impact_map": [],
        "test_recommendations": None,
        "recommended_actions": [],
        "deployment_risk_assessment": None,
        "decision_center": None,
        "historical_learning": None,
        "hypotheses": [],
    }
    base.update(overrides)
    return base


def test_no_signals_returns_none():
    assert build_database_validation(**_base()) is None


def test_checkout_generates_order_payment_inventory_checks():
    report = build_database_validation(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Checkout instability",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=6,
                ),
            ],
        )
    )
    assert report is not None
    names = [c.name.lower() for c in report.checks]
    assert any("order" in n for n in names)
    assert any("payment" in n for n in names)
    assert any("inventory" in n for n in names)
    assert any("erp" in n for n in names)


def test_payments_generates_transaction_status_checks():
    report = build_database_validation(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Payments",
                    description="Payment failures",
                    severity="high",
                    confidence=0.8,
                    related_entity_count=5,
                ),
            ],
        )
    )
    assert report is not None
    names = [c.name.lower() for c in report.checks]
    assert any("transaction" in n for n in names)
    assert any("reconciliation" in n for n in names)


def test_authentication_generates_session_audit_checks():
    report = build_database_validation(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Authentication",
                    description="Auth failures",
                    severity="medium",
                    confidence=0.72,
                    related_entity_count=3,
                ),
            ],
        )
    )
    assert report is not None
    names = [c.name.lower() for c in report.checks]
    assert any("session" in n for n in names)
    assert any("audit" in n for n in names)
    assert any("token" in n for n in names)


def test_requires_user_approval_always_true():
    report = build_database_validation(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Checkout signals",
                    severity="high",
                    confidence=0.8,
                    related_entity_count=4,
                ),
            ],
        )
    )
    assert report is not None
    assert all(c.requires_user_approval is True for c in report.checks)
    assert all(r.read_only is True for r in report.results)
    assert all(r.executed_at is None for r in report.results)


def test_select_query_allowed():
    safe, reason = validate_query_safety(
        "SELECT payment_status FROM payments WHERE order_id = :order_id"
    )
    assert safe is True
    assert "approved" in reason.lower()

    safe_with, _ = validate_query_safety(
        "WITH recent AS (SELECT order_id FROM orders) SELECT * FROM recent"
    )
    assert safe_with is True


def test_unsafe_queries_rejected():
    unsafe_queries = [
        "DELETE FROM orders WHERE order_id = :order_id",
        "UPDATE payments SET status = 'paid'",
        "INSERT INTO orders (id) VALUES ('1')",
        "DROP TABLE orders",
        "CALL refresh_orders()",
        "EXEC sp_validate_order :order_id",
    ]
    for query in unsafe_queries:
        safe, reason = validate_query_safety(query)
        assert safe is False, query
        assert reason


def test_multiple_statements_rejected():
    safe, reason = validate_query_safety(
        "SELECT 1 FROM orders; DELETE FROM orders"
    )
    assert safe is False
    assert "multiple" in reason.lower()


def test_deterministic_check_ids():
    rid_a = build_check_id("checkout", "validate_payment_status")
    rid_b = build_check_id("checkout", "validate_payment_status")
    assert rid_a == rid_b
    assert rid_a == "dbcheck:checkout:validate_payment_status"


def test_deterministic_output():
    kwargs = _base(
        impact_map=[
            IncidentImpactNode(
                title="Checkout",
                description="Checkout signals",
                severity="high",
                confidence=0.84,
                related_entity_count=6,
            ),
        ],
    )
    a = build_database_validation(**kwargs)
    b = build_database_validation(**kwargs)
    assert a is not None and b is not None
    assert [c.check_id for c in a.checks] == [c.check_id for c in b.checks]
    assert a.summary == b.summary
    assert a.confidence == b.confidence


def test_backward_compat_report_without_database_validation():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.database_validation is None


@pytest.fixture()
def client():
    from app import app

    return TestClient(app)


def _mock_project():
    class _P:
        id = "demo"
        name = "Demo"
    return _P()


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_investigate_includes_database_validation(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="CHK-001",
            status="failed",
            started_at="2026-06-09T10:00:00+00:00",
            module="checkout",
            error_summary="fail",
        ),
    ]
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Checkout broken after deploy", "module": "checkout"},
    )
    assert r.status_code == 200
    body = r.json()
    assert "database_validation" in body
    assert body["meta"]["analyze_only"] is True
    if body.get("database_validation"):
        for check in body["database_validation"]["checks"]:
            assert check["requires_user_approval"] is True
            safe, _ = validate_query_safety(check["query"])
            assert safe is True
