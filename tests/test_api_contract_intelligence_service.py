# tests/test_api_contract_intelligence_service.py
"""INT-02A — API Contract Intelligence (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    DeploymentRiskAssessment,
    HistoricalLearningReport,
    IncidentImpactNode,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    SimilarIncident,
)
from services.api_contract_intelligence_service import (
    build_api_contract_intelligence,
    build_assessment_id,
    build_change_id,
    build_contract_id,
    calculate_contract_risk_level,
    detect_schema_changes,
)


def _base(**overrides):
    base = {
        "impact_map": [],
        "related_pr_analysis": [],
        "deployment_risk_assessment": None,
        "historical_learning": None,
        "test_recommendations": None,
        "recommended_actions": [],
        "hypotheses": [],
        "stored_specs": None,
        "explicit_contracts": None,
    }
    base.update(overrides)
    return base


def test_no_signals_returns_none():
    assert build_api_contract_intelligence(**_base()) is None


def test_added_field_detection():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    changes = detect_schema_changes(
        {"customer_id": "integer"},
        {"customer_id": "integer", "customer_name": "string"},
        contract_id,
    )
    assert len(changes) == 1
    assert changes[0].change_type == "added_field"
    assert changes[0].severity == "LOW"
    assert changes[0].field_name == "customer_name"


def test_removed_field_detection():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    changes = detect_schema_changes(
        {"customer_id": "integer", "amount": "number"},
        {"customer_id": "integer"},
        contract_id,
    )
    assert any(c.change_type == "removed_field" and c.severity == "HIGH" for c in changes)


def test_type_change_detection():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    changes = detect_schema_changes(
        {"amount": "number"},
        {"amount": "string"},
        contract_id,
    )
    assert len(changes) == 1
    assert changes[0].change_type == "type_change"
    assert changes[0].severity == "CRITICAL"


def test_rename_detection():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    changes = detect_schema_changes(
        {"amount": "number"},
        {"total_amount": "number"},
        contract_id,
    )
    assert len(changes) == 1
    assert changes[0].change_type == "rename"
    assert changes[0].severity == "HIGH"
    assert changes[0].old_value == "amount"
    assert changes[0].new_value == "total_amount"


def test_risk_level_calculation():
    changes = detect_schema_changes(
        {"amount": "number"},
        {"total_amount": "number"},
        build_contract_id("Payments API", "POST", "/payments", "v2"),
    )
    level = calculate_contract_risk_level(
        changes,
        DeploymentRiskAssessment(risk_score=85, risk_level="high", confidence=0.8, summary="High risk"),
        None,
        "Payments",
    )
    assert level == "CRITICAL"

    low_changes = detect_schema_changes(
        {"customer_id": "integer"},
        {"customer_id": "integer", "customer_name": "string"},
        build_contract_id("Payments API", "POST", "/payments", "v2"),
    )
    low_level = calculate_contract_risk_level(low_changes, None, None, "Payments")
    assert low_level == "LOW"


def test_deterministic_ids():
    contract_id = build_contract_id("Payments API", "POST", "/payments", "v2")
    change_id = build_change_id(contract_id, "removed_field", "amount")
    assessment_id = build_assessment_id(contract_id)
    assert contract_id == "contract:payments_api_post_payments_vv2"
    assert change_id.startswith("change:contract:payments_api_post_payments_vv2:removed_field:")
    assert assessment_id == "assessment:contract:payments_api_post_payments_vv2"


def test_deterministic_output():
    kwargs = _base(
        impact_map=[
            IncidentImpactNode(
                title="Payments",
                description="Payment failures",
                severity="high",
                confidence=0.82,
                related_entity_count=4,
            ),
        ],
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=78,
            risk_level="high",
            confidence=0.77,
            summary="Elevated deployment risk",
        ),
    )
    first = build_api_contract_intelligence(**kwargs)
    second = build_api_contract_intelligence(**kwargs)
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_historical_risk_influence():
    report = build_api_contract_intelligence(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Payments",
                    description="Payment failures",
                    severity="high",
                    confidence=0.8,
                    related_entity_count=3,
                ),
            ],
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=70,
                risk_level="medium",
                confidence=0.7,
                summary="Medium deployment risk",
            ),
            historical_learning=HistoricalLearningReport(
                pattern_summary="Prior payment contract regressions after field removals.",
                confidence=0.74,
                similar_incidents=[
                    SimilarIncident(
                        incident_id="inc-1",
                        title="Payment outage",
                        summary="Removed amount field caused payment failures",
                        similarity_score=0.81,
                        occurrence_timestamp="2025-01-01T00:00:00Z",
                    ),
                ],
            ),
        )
    )
    assert report is not None
    assessment = report.risk_assessments[0]
    assert assessment.risk_level in ("HIGH", "CRITICAL")
    assert "historical" in assessment.summary.lower()


def test_no_network_activity():
    with patch("httpx.Client") as mock_client:
        report = build_api_contract_intelligence(
            **_base(
                impact_map=[
                    IncidentImpactNode(
                        title="Payments",
                        description="Failures",
                        severity="high",
                        confidence=0.8,
                        related_entity_count=2,
                    ),
                ],
                related_pr_analysis=[
                    RelatedPRAnalysisSummary(
                        pr_number="42",
                        risk_signals=["openapi breaking change on /payments"],
                        impacted_modules=["Payments"],
                    ),
                ],
            )
        )
        mock_client.assert_not_called()
    assert report is not None


def test_payments_impact_generates_contract_report():
    report = build_api_contract_intelligence(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Payments",
                    description="Payment instability",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=5,
                ),
            ],
        )
    )
    assert report is not None
    assert report.contracts
    assert report.risk_assessments
    assert any(c.service_name == "Payments API" for c in report.contracts)


def test_explicit_contract_definition_path():
    report = build_api_contract_intelligence(
        **_base(
            explicit_contracts=[
                {
                    "service_name": "Payments API",
                    "endpoint": "/payments",
                    "method": "POST",
                    "version": "v2",
                    "old_properties": {"customer_id": "integer", "amount": "number"},
                    "new_properties": {"customer_id": "integer"},
                    "source": "explicit_definition",
                },
            ],
        )
    )
    assert report is not None
    removed = [c for a in report.risk_assessments for c in a.changes if c.change_type == "removed_field"]
    assert removed
    assert removed[0].field_name == "amount"


def test_report_includes_api_contract_intelligence_field():
    report = ProjectIncidentInvestigationReport(
        project_id="demo",
        description="Payments failing after deploy",
        impact_map=[
            IncidentImpactNode(
                title="Payments",
                description="Failures",
                severity="high",
                confidence=0.8,
                related_entity_count=2,
            ),
        ],
    )
    assert hasattr(report, "api_contract_intelligence")


@pytest.fixture
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
def test_investigate_includes_api_contract_intelligence(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    from models.incident_models import RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="PAY-001",
            status="failed",
            started_at="2026-06-09T10:00:00+00:00",
            module="payments",
            error_summary="payment failed",
        ),
    ]
    resp = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Payments failing after deploy", "module": "payments"},
    )
    assert resp.status_code == 200
    body = resp.json()
    assert "api_contract_intelligence" in body
    assert body["meta"]["analyze_only"] is True
