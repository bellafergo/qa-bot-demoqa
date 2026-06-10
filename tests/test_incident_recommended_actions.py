# tests/test_incident_recommended_actions.py
"""II-04A — Human Approved Actions (read-only recommendations)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentImpactNode,
    InvestigationPlanItem,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
)
from services.incident_recommended_actions_service import build_action_id, build_recommended_actions


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
        "timeline": [],
        "temporal_correlation": None,
    }
    base.update(overrides)
    return base


def test_build_action_id_with_entity():
    assert build_action_id(
        "analyze_pr",
        "pr_analysis",
        "github:483",
    ) == "analyze_pr:pr_analysis:github:483"


def test_build_action_id_without_entity_uses_stable_title_hash():
    a = build_action_id("review_timeline", title="Review Incident Timeline")
    b = build_action_id("review_timeline", title="Review Incident Timeline")
    c = build_action_id("review_timeline", title="Different Title")
    assert a == b
    assert a.startswith("review_timeline:title:")
    assert a != c


def test_action_ids_are_deterministic_across_builds():
    kwargs = _base(
        related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="483",
                provider="github",
                pr_risk_score=84.0,
                risk_level="HIGH",
            ),
        ],
    )
    ids_a = [a.action_id for a in build_recommended_actions(**kwargs)]
    ids_b = [a.action_id for a in build_recommended_actions(**kwargs)]
    assert ids_a == ids_b
    assert any(aid == "analyze_pr:pr_analysis:github:483" for aid in ids_a)


def test_no_evidence_returns_empty():
    actions = build_recommended_actions(**_base())
    assert actions == []


def test_pr_recommendation():
    actions = build_recommended_actions(
        **_base(
            related_pr_analysis=[
                RelatedPRAnalysisSummary(
                    pr_number="483",
                    provider="github",
                    pr_risk_score=84.0,
                    risk_level="HIGH",
                    impacted_modules=["checkout"],
                    reason="same module affected: checkout",
                ),
            ],
            evidence_correlation=EvidenceCorrelationSummary(
                total_correlations=1,
                strongest_source="pr_analysis",
                evidence=[
                    CorrelatedEvidence(
                        source="pr_analysis",
                        confidence=0.9,
                        title="PR Analysis",
                        detail="PR #483",
                        reason="High PR correlation",
                        related_entity_type="pr_analysis",
                        related_entity_id="github:483",
                    ),
                ],
            ),
        )
    )
    pr_actions = [a for a in actions if a.action_type == "analyze_pr"]
    assert pr_actions
    assert "483" in pr_actions[0].title
    assert pr_actions[0].requires_user_approval is True


def test_browser_recommendation():
    actions = build_recommended_actions(
        **_base(
            browser_events=[
                {"watch_id": "watch_123", "summary": "Checkout UI changed"},
            ],
            investigation_plan=[
                InvestigationPlanItem(
                    title="Inspect Browser Watch Alert",
                    reason="Alert on checkout route",
                    priority=75,
                    related_entity_type="browser_watch",
                    related_entity_id="watch_123",
                ),
            ],
        )
    )
    types = {a.action_type for a in actions}
    assert "review_browser_watch" in types
    assert "run_browser_probe" in types
    assert all(a.requires_user_approval for a in actions)


def test_failure_cluster_recommendation():
    actions = build_recommended_actions(
        **_base(
            clusters=[
                {
                    "cluster_id": "cluster_7",
                    "module": "checkout",
                    "total_failures": 8,
                    "root_cause_category": "timeout",
                },
            ],
            related_runs=[
                RelatedRunSummary(
                    run_id="RUN-1",
                    test_id="CHK-001",
                    module="checkout",
                    status="failed",
                    error_summary="timeout",
                ),
            ],
        )
    )
    cluster_actions = [a for a in actions if a.action_type == "inspect_failure_cluster"]
    assert cluster_actions
    assert cluster_actions[0].related_entity_id == "cluster_7"
    assert all(a.requires_user_approval for a in actions)


def test_impacted_area_recommendation():
    actions = build_recommended_actions(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Multiple signals point to Checkout as an impacted area.",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=12,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
        )
    )
    area_actions = [a for a in actions if a.action_type == "review_impacted_area"]
    assert area_actions
    assert "Checkout" in area_actions[0].title


def test_sorting_by_priority_and_confidence():
    actions = build_recommended_actions(
        **_base(
            related_pr_analysis=[
                RelatedPRAnalysisSummary(
                    pr_number="483",
                    provider="github",
                    pr_risk_score=90.0,
                    risk_level="HIGH",
                ),
            ],
            browser_events=[
                {"watch_id": "w1", "summary": "UI changed"},
            ],
            related_runs=[
                RelatedRunSummary(
                    run_id="RUN-1",
                    test_id="CHK-001",
                    module="checkout",
                    status="failed",
                    error_summary="fail",
                ),
            ],
        )
    )
    assert len(actions) >= 2
    priorities = [a.priority for a in actions]
    assert priorities == sorted(priorities)
    for action in actions:
        assert action.requires_user_approval is True


def test_all_actions_require_user_approval():
    actions = build_recommended_actions(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    statement="Auth regression likely.",
                    confidence=0.8,
                    basis="evidence",
                ),
            ],
            related_runs=[
                RelatedRunSummary(
                    run_id="RUN-9",
                    test_id="AUTH-1",
                    module="auth",
                    status="failed",
                    error_summary="error",
                ),
            ],
        )
    )
    assert actions
    assert all(a.requires_user_approval is True for a in actions)


def test_backward_compat_report_without_recommended_actions():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.recommended_actions == []


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
def test_investigate_includes_recommended_actions(
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
    assert "recommended_actions" in body
    assert isinstance(body["recommended_actions"], list)
    assert len(body["recommended_actions"]) >= 1
    assert body["meta"]["analyze_only"] is True
    for action in body["recommended_actions"]:
        assert action["requires_user_approval"] is True
    for action in body.get("actions_available") or []:
        assert action["requires_user_approval"] is True
