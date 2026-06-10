# tests/test_test_recommendation_service.py
"""II-05B — Test Recommendation Engine (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    DeploymentRiskAssessment,
    IncidentHypothesis,
    IncidentImpactNode,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
    RiskFactor,
)
from services.test_recommendation_service import build_recommendation_id, build_test_recommendations


def _base(**overrides):
    base = {
        "hypotheses": [],
        "evidence_correlation": None,
        "investigation_plan": [],
        "storyline": [],
        "impact_map": [],
        "related_runs": [],
        "related_pr_analysis": [],
        "browser_events": [],
        "clusters": [],
        "temporal_correlation": None,
        "deployment_risk_assessment": None,
        "recommended_actions": [],
    }
    base.update(overrides)
    return base


def test_no_evidence_returns_none():
    assert build_test_recommendations(**_base()) is None


def test_checkout_recommendation():
    report = build_test_recommendations(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Multiple checkout signals",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=8,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
            related_runs=[
                RelatedRunSummary(
                    run_id="RUN-1",
                    module="checkout",
                    test_name="Checkout submit",
                    status="failed",
                    error_summary="timeout",
                ),
            ],
            browser_events=[{"watch_id": "w1", "summary": "Checkout UI changed"}],
        )
    )
    assert report is not None
    names = [r.test_name for r in report.recommendations]
    assert any("Checkout" in n for n in names)
    assert report.recommendations[0].priority == 1
    assert all(r.requires_user_approval for r in report.recommendations)


def test_payments_recommendation():
    report = build_test_recommendations(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Payments",
                    description="Payment failures",
                    severity="medium",
                    confidence=0.7,
                    related_entity_count=4,
                ),
            ],
            clusters=[{"cluster_id": "c-pay", "module": "payments", "total_failures": 6}],
        )
    )
    assert report is not None
    assert any("Payments" in r.test_name for r in report.recommendations)


def test_browser_alert_recommendation():
    report = build_test_recommendations(
        **_base(
            browser_events=[
                {"watch_id": "watch_ui", "summary": "Login UI changed on auth page"},
            ],
        )
    )
    assert report is not None
    assert any(r.test_type == "ui_smoke" for r in report.recommendations)


def test_failure_cluster_recommendation():
    report = build_test_recommendations(
        **_base(
            clusters=[{"cluster_id": "cluster_7", "module": "checkout", "total_failures": 9}],
        )
    )
    assert report is not None
    rec = next(r for r in report.recommendations if "Checkout" in r.test_name)
    assert rec.test_type == "cluster_regression"
    assert rec.related_entity_type == "failure_cluster"


def test_deduplication_checkout_variants():
    report = build_test_recommendations(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout Flow",
                    description="signals",
                    severity="high",
                    confidence=0.8,
                    related_entity_count=5,
                ),
            ],
            related_runs=[
                RelatedRunSummary(run_id="RUN-1", module="checkout", status="failed", error_summary="x"),
            ],
            browser_events=[{"watch_id": "w1", "summary": "checkout flow regression"}],
        )
    )
    assert report is not None
    checkout_recs = [r for r in report.recommendations if "checkout" in r.test_name.lower()]
    assert len(checkout_recs) == 1


def test_sorting_by_priority_confidence_and_risk_reduction():
    report = build_test_recommendations(
        **_base(
            impact_map=[
                IncidentImpactNode(title="Checkout", description="d", severity="high", confidence=0.9, related_entity_count=10),
                IncidentImpactNode(title="Search", description="d", severity="medium", confidence=0.6, related_entity_count=2),
            ],
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=82,
                risk_level="critical",
                confidence=0.88,
                summary="critical",
                contributing_factors=[RiskFactor(title="x", description="d", weight=0.2)],
            ),
        )
    )
    assert report is not None
    priorities = [r.priority for r in report.recommendations]
    assert priorities == sorted(priorities)
    assert priorities[0] == 1
    if len(report.recommendations) >= 2:
        assert report.recommendations[0].confidence >= report.recommendations[1].confidence or (
            report.recommendations[0].estimated_risk_reduction
            >= report.recommendations[1].estimated_risk_reduction
        )


def test_confidence_and_risk_reduction_calculation():
    report = build_test_recommendations(
        **_base(
            related_pr_analysis=[
                RelatedPRAnalysisSummary(
                    pr_number="483",
                    provider="github",
                    pr_risk_score=85.0,
                    risk_level="HIGH",
                    impacted_modules=["checkout"],
                ),
            ],
            hypotheses=[
                IncidentHypothesis(statement="Checkout regression likely.", confidence=0.85, basis="evidence"),
            ],
        )
    )
    assert report is not None
    assert 0.0 < report.recommendation_confidence <= 1.0
    for rec in report.recommendations:
        assert 0.0 < rec.confidence <= 1.0
        assert 0.0 < rec.estimated_risk_reduction <= 1.0


def test_requires_user_approval_always_true():
    report = build_test_recommendations(
        **_base(
            related_runs=[
                RelatedRunSummary(run_id="RUN-1", module="auth", status="failed", error_summary="fail"),
            ],
        )
    )
    assert report is not None
    assert all(r.requires_user_approval is True for r in report.recommendations)


def test_recommendation_id_deterministic():
    rid_a = build_recommendation_id("smoke", "Checkout Smoke Suite", "failure_cluster", "cluster_7")
    rid_b = build_recommendation_id("smoke", "Checkout Smoke Suite", "failure_cluster", "cluster_7")
    assert rid_a == rid_b
    assert rid_a == "smoke:failure_cluster:cluster_7"


def test_backward_compat_report_without_test_recommendations():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.test_recommendations is None


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
def test_investigate_includes_test_recommendations(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="AUTH-014",
            status="failed",
            started_at="2026-06-09T10:00:00+00:00",
            module="auth",
            error_summary="fail",
        ),
    ]
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Login broken after deploy", "module": "auth"},
    )
    assert r.status_code == 200
    body = r.json()
    assert "test_recommendations" in body
    assert body["meta"]["analyze_only"] is True
    if body.get("test_recommendations"):
        for rec in body["test_recommendations"]["recommendations"]:
            assert rec["requires_user_approval"] is True
