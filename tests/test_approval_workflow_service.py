# tests/test_approval_workflow_service.py
"""II-06B — Approval Workflow Foundation (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    DeploymentRiskAssessment,
    IncidentImpactNode,
    ProjectIncidentInvestigationReport,
    RecommendedAction,
    RecommendedTest,
    RelatedRunSummary,
    TestRecommendationReport,
)
from services.approval_workflow_service import build_approval_id, build_approval_workflow


def _base(**overrides):
    base = {
        "recommended_actions": [],
        "test_recommendations": None,
        "deployment_risk_assessment": None,
        "impact_map": [],
        "created_at": "2026-06-10T12:00:00+00:00",
    }
    base.update(overrides)
    return base


def test_empty_state_returns_none():
    assert build_approval_workflow(**_base()) is None


def test_recommended_action_approvals():
    workflow = build_approval_workflow(
        **_base(
            recommended_actions=[
                RecommendedAction(
                    action_id="act-pr",
                    title="Analyze PR #483",
                    description="Review correlated pull request changes.",
                    reason="PR overlaps impacted modules.",
                    priority=10,
                    confidence=0.8,
                    action_type="analyze_pr",
                    requires_user_approval=True,
                    related_entity_type="pr_analysis",
                    related_entity_id="github:483",
                ),
                RecommendedAction(
                    action_id="act-bw",
                    title="Review Browser Watch",
                    description="Inspect browser alert history.",
                    reason="Browser alerts correlate with failures.",
                    priority=15,
                    confidence=0.75,
                    action_type="review_browser_watch",
                    requires_user_approval=True,
                    related_entity_type="browser_watch",
                    related_entity_id="watch_checkout",
                ),
            ],
        )
    )
    assert workflow is not None
    assert workflow.pending_count == 2
    assert workflow.approved_count == 0
    assert workflow.rejected_count == 0
    titles = [r.title for r in workflow.requests]
    assert "Analyze PR #483" in titles
    assert "Review Browser Watch" in titles


def test_recommended_test_approvals():
    workflow = build_approval_workflow(
        **_base(
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="smoke:checkout",
                        test_name="Checkout Smoke Suite",
                        test_type="smoke",
                        priority=1,
                        confidence=0.85,
                        reason="Validate checkout after incident signals.",
                        requires_user_approval=True,
                    ),
                    RecommendedTest(
                        recommendation_id="regression:payments",
                        test_name="Payments Regression Suite",
                        test_type="regression",
                        priority=2,
                        confidence=0.8,
                        reason="Payments instability detected.",
                        requires_user_approval=True,
                    ),
                ],
                summary="Run focused suites.",
                recommendation_confidence=0.82,
            ),
        )
    )
    assert workflow is not None
    assert len(workflow.requests) == 2
    assert all(r.status == "PENDING" for r in workflow.requests)
    assert any("Checkout Smoke Suite" in r.title for r in workflow.requests)


def test_deterministic_ids():
    rid_a = build_approval_id("analyze_pr", "Analyze PR #483", "pr_analysis", "github:483")
    rid_b = build_approval_id("analyze_pr", "Analyze PR #483", "pr_analysis", "github:483")
    assert rid_a == rid_b
    assert rid_a == "approval:analyze_pr:pr_analysis:github:483"

    fallback = build_approval_id("smoke", "Login Smoke Suite")
    assert fallback.startswith("approval:smoke:title:")


def test_status_defaults_to_pending():
    workflow = build_approval_workflow(
        **_base(
            recommended_actions=[
                RecommendedAction(
                    action_id="act-1",
                    title="Inspect Failure Cluster",
                    description="Review cluster details.",
                    reason="Repeated failures.",
                    priority=20,
                    confidence=0.7,
                    action_type="inspect_failure_cluster",
                    requires_user_approval=True,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
        )
    )
    assert workflow is not None
    assert workflow.requests[0].status == "PENDING"


def test_sorting_by_priority_then_title():
    workflow = build_approval_workflow(
        **_base(
            recommended_actions=[
                RecommendedAction(
                    action_id="act-z",
                    title="Review Timeline",
                    description="Walk timeline.",
                    reason="Temporal correlation.",
                    priority=40,
                    confidence=0.6,
                    action_type="review_timeline",
                    requires_user_approval=True,
                ),
                RecommendedAction(
                    action_id="act-a",
                    title="Analyze PR #483",
                    description="PR review.",
                    reason="PR overlap.",
                    priority=1,
                    confidence=0.85,
                    action_type="analyze_pr",
                    requires_user_approval=True,
                    related_entity_type="pr_analysis",
                    related_entity_id="github:483",
                ),
            ],
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="smoke:login",
                        test_name="Login Smoke Suite",
                        test_type="smoke",
                        priority=8,
                        confidence=0.8,
                        reason="Auth failures.",
                        requires_user_approval=True,
                    ),
                ],
                summary="Run login smoke.",
                recommendation_confidence=0.8,
            ),
        )
    )
    assert workflow is not None
    assert workflow.requests[0].title == "Analyze PR #483"
    titles = [r.title for r in workflow.requests]
    assert titles.index("Login Smoke Suite") < titles.index("Review Timeline")


def test_no_execution_metadata():
    workflow = build_approval_workflow(
        **_base(
            recommended_actions=[
                RecommendedAction(
                    action_id="act-1",
                    title="Run Checkout Smoke Suite",
                    description="Queue smoke tests.",
                    reason="Checkout instability.",
                    priority=12,
                    confidence=0.8,
                    action_type="run_test_suite",
                    requires_user_approval=True,
                ),
            ],
        )
    )
    assert workflow is not None
    dumped = workflow.model_dump()
    assert "execution" not in str(dumped).lower()
    assert "workflow_id" not in dumped
    assert "job_id" not in dumped


def test_deterministic_output():
    kwargs = _base(
        recommended_actions=[
            RecommendedAction(
                action_id="act-1",
                title="Analyze PR #483",
                description="PR review.",
                reason="Overlap.",
                priority=10,
                confidence=0.8,
                action_type="analyze_pr",
                requires_user_approval=True,
                related_entity_type="pr_analysis",
                related_entity_id="github:483",
            ),
        ],
    )
    a = build_approval_workflow(**kwargs)
    b = build_approval_workflow(**kwargs)
    assert a is not None and b is not None
    assert [r.approval_id for r in a.requests] == [r.approval_id for r in b.requests]
    assert a.pending_count == b.pending_count


def test_backward_compat_report_without_approval_workflow():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.approval_workflow is None


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
def test_investigate_includes_approval_workflow(
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
    assert "approval_workflow" in body
    assert body["meta"]["analyze_only"] is True
    if body.get("approval_workflow"):
        assert body["approval_workflow"]["pending_count"] >= 0
        for req in body["approval_workflow"]["requests"]:
            assert req["status"] == "PENDING"
