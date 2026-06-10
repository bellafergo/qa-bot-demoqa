# tests/test_quality_decision_center_service.py
"""II-05C — Quality Decision Center (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    DeploymentRiskAssessment,
    IncidentHypothesis,
    IncidentImpactNode,
    InvestigationPlanItem,
    ProjectIncidentInvestigationReport,
    RecommendedAction,
    RecommendedTest,
    RelatedRunSummary,
    RiskFactor,
    TestRecommendationReport,
)
from services.quality_decision_center_service import build_decision_center


def _base(**overrides):
    base = {
        "hypotheses": [],
        "investigation_plan": [],
        "storyline": [],
        "impact_map": [],
        "recommended_actions": [],
        "deployment_risk_assessment": None,
        "test_recommendations": None,
    }
    base.update(overrides)
    return base


def test_no_intelligence_returns_none():
    assert build_decision_center(**_base()) is None
    assert build_decision_center(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    rank=1,
                    statement="Weak guess",
                    confidence=0.3,
                    basis="assumption",
                ),
            ],
        )
    ) is None


def test_deployment_risk_maps_to_red_status():
    center = build_decision_center(
        **_base(
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=82,
                risk_level="critical",
                confidence=0.88,
                summary="Critical deployment risk detected.",
                contributing_factors=[
                    RiskFactor(
                        title="Repeated Checkout Failures",
                        description="12 failures clustered.",
                        weight=0.24,
                        related_entity_type="failure_cluster",
                        related_entity_id="cluster_7",
                    ),
                ],
            ),
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Multiple checkout signals",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=8,
                ),
            ],
        )
    )
    assert center is not None
    assert center.overall_status == "RED"
    assert center.top_risk_level == "CRITICAL"
    assert center.top_risk_score == 82
    assert "Critical deployment risk" in center.executive_summary


def test_impact_map_fallback_status_without_dra():
    center = build_decision_center(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Auth",
                    description="Auth instability",
                    severity="high",
                    confidence=0.8,
                    related_entity_count=5,
                ),
                IncidentImpactNode(
                    title="Login",
                    description="Login failures",
                    severity="high",
                    confidence=0.75,
                    related_entity_count=4,
                ),
            ],
        )
    )
    assert center is not None
    assert center.overall_status == "ORANGE"
    assert center.top_impacted_area == "Auth"


def test_counts_for_tests_and_actions():
    center = build_decision_center(
        **_base(
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="smoke:run:RUN-1",
                        test_name="Auth Smoke Suite",
                        test_type="smoke",
                        priority=1,
                        confidence=0.85,
                        reason="Failed auth run",
                        requires_user_approval=True,
                    ),
                    RecommendedTest(
                        recommendation_id="smoke:run:RUN-2",
                        test_name="Login Smoke Suite",
                        test_type="smoke",
                        priority=2,
                        confidence=0.8,
                        reason="Login instability",
                        requires_user_approval=True,
                    ),
                ],
                summary="Run 2 focused test suite(s).",
                recommendation_confidence=0.82,
            ),
            recommended_actions=[
                RecommendedAction(
                    action_id="act-1",
                    title="Review Auth failures",
                    description="Inspect auth runs",
                    reason="Correlated failures",
                    priority=10,
                    confidence=0.78,
                    action_type="inspect_failed_run",
                    requires_user_approval=True,
                ),
            ],
            impact_map=[
                IncidentImpactNode(
                    title="Auth",
                    description="Auth signals",
                    severity="medium",
                    confidence=0.7,
                    related_entity_count=3,
                ),
            ],
        )
    )
    assert center is not None
    assert center.recommended_test_count == 2
    assert center.recommended_action_count == 1
    assert any("Smoke validation" in i.title for i in center.key_takeaways)


def test_top_hypothesis_truncation():
    long_statement = "X" * 120
    center = build_decision_center(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    rank=1,
                    statement=long_statement,
                    confidence=0.72,
                    basis="inference",
                ),
            ],
            investigation_plan=[
                InvestigationPlanItem(
                    title="Validate auth regression",
                    reason="Check auth module",
                    priority=80,
                ),
            ],
        )
    )
    assert center is not None
    assert center.top_hypothesis is not None
    assert len(center.top_hypothesis) == 96
    assert center.top_hypothesis.endswith("...")


def test_key_takeaways_from_upstream():
    center = build_decision_center(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    rank=1,
                    statement="Checkout timeout after deploy",
                    confidence=0.8,
                    basis="evidence",
                ),
            ],
            investigation_plan=[
                InvestigationPlanItem(
                    title="Inspect checkout cluster",
                    reason="Repeated checkout failures",
                    priority=85,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Checkout instability",
                    severity="high",
                    confidence=0.86,
                    related_entity_count=6,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=65,
                risk_level="high",
                confidence=0.8,
                summary="Elevated deployment risk.",
                contributing_factors=[
                    RiskFactor(
                        title="Failure Cluster in Checkout",
                        description="Cluster overlap",
                        weight=0.2,
                    ),
                ],
            ),
        )
    )
    assert center is not None
    assert len(center.key_takeaways) >= 2
    titles = [i.title for i in center.key_takeaways]
    assert any("Checkout" in t for t in titles)
    assert any("hypothesis" in t.lower() for t in titles)


def test_confidence_blend():
    center = build_decision_center(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    rank=1,
                    statement="Auth regression",
                    confidence=0.7,
                    basis="inference",
                ),
            ],
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=40,
                risk_level="medium",
                confidence=0.75,
                summary="Moderate risk.",
            ),
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="smoke:auth",
                        test_name="Auth Smoke Suite",
                        test_type="smoke",
                        priority=1,
                        confidence=0.8,
                        reason="Auth failures",
                        requires_user_approval=True,
                    ),
                ],
                summary="Run auth smoke.",
                recommendation_confidence=0.8,
            ),
            impact_map=[
                IncidentImpactNode(
                    title="Auth",
                    description="Auth signals",
                    severity="medium",
                    confidence=0.7,
                    related_entity_count=2,
                ),
            ],
        )
    )
    assert center is not None
    assert 0.0 < center.confidence <= 1.0


def test_deterministic_output():
    kwargs = _base(
        impact_map=[
            IncidentImpactNode(
                title="Payments",
                description="Payment failures",
                severity="high",
                confidence=0.82,
                related_entity_count=5,
            ),
        ],
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=55,
            risk_level="medium",
            confidence=0.7,
            summary="Moderate deployment risk.",
        ),
    )
    a = build_decision_center(**kwargs)
    b = build_decision_center(**kwargs)
    assert a is not None and b is not None
    assert a.overall_status == b.overall_status
    assert a.executive_summary == b.executive_summary
    assert a.confidence == b.confidence
    assert [i.title for i in a.key_takeaways] == [i.title for i in b.key_takeaways]


def test_backward_compat_report_without_decision_center():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.decision_center is None


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
def test_investigate_includes_decision_center(
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
    assert "decision_center" in body
    assert body["meta"]["analyze_only"] is True
    if body.get("decision_center"):
        assert body["decision_center"]["overall_status"] in ("GREEN", "YELLOW", "ORANGE", "RED")
        assert isinstance(body["decision_center"]["key_takeaways"], list)
    for action in body.get("recommended_actions") or []:
        assert action["requires_user_approval"] is True
    if body.get("test_recommendations"):
        for rec in body["test_recommendations"]["recommendations"]:
            assert rec["requires_user_approval"] is True
