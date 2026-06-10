# tests/test_similar_incidents_service.py
"""II-06A — Historical Learning & Similar Incidents (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    DeploymentRiskAssessment,
    IncidentHypothesis,
    IncidentImpactNode,
    IncidentStorylineStep,
    ProjectIncidentInvestigationReport,
    RecommendedAction,
    RecommendedTest,
    RelatedRunSummary,
    TestRecommendationReport,
)
from services.similar_incidents_service import build_historical_learning


def _base(**overrides):
    base = {
        "hypotheses": [],
        "impact_map": [],
        "deployment_risk_assessment": None,
        "storyline": [],
        "recommended_actions": [],
        "test_recommendations": None,
        "failure_clusters": [],
        "previous_reports": [],
        "impacted_modules": [],
    }
    base.update(overrides)
    return base


def _historical_report(**overrides) -> ProjectIncidentInvestigationReport:
    data = {
        "id": "hist-0041",
        "created_at": "2026-06-08T10:00:00+00:00",
        "project_id": "demo",
        "description": "Checkout regression after deploy",
        "summary": "Checkout failures clustered after release.",
        "impact_map": [
            IncidentImpactNode(
                title="Checkout",
                description="Checkout instability",
                severity="high",
                confidence=0.8,
                related_entity_count=5,
                related_entity_type="failure_cluster",
                related_entity_id="cluster_7",
            ),
        ],
        "hypotheses": [
            IncidentHypothesis(
                id="H1",
                rank=1,
                statement="Checkout timeout regression",
                confidence=0.75,
                basis="inference",
            ),
        ],
        "storyline": [],
        "recommended_actions": [],
        "deployment_risk_assessment": DeploymentRiskAssessment(
            risk_score=60,
            risk_level="high",
            confidence=0.7,
            summary="Elevated risk.",
        ),
        "test_recommendations": TestRecommendationReport(
            recommendations=[
                RecommendedTest(
                    recommendation_id="smoke:checkout",
                    test_name="Checkout Smoke Suite",
                    test_type="smoke",
                    priority=1,
                    confidence=0.8,
                    reason="Checkout failures",
                    requires_user_approval=True,
                ),
            ],
            summary="Run checkout smoke.",
            recommendation_confidence=0.8,
        ),
    }
    data.update(overrides)
    return ProjectIncidentInvestigationReport.model_validate(data)


def test_no_history_returns_none():
    assert build_historical_learning(**_base()) is None


def test_similar_impacted_area():
    report = build_historical_learning(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    rank=1,
                    statement="Checkout timeout regression",
                    confidence=0.75,
                    basis="inference",
                ),
            ],
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=60,
                risk_level="high",
                confidence=0.7,
                summary="Elevated risk.",
            ),
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Checkout signals",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=6,
                ),
            ],
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="smoke:checkout",
                        test_name="Checkout Smoke Suite",
                        test_type="smoke",
                        priority=1,
                        confidence=0.8,
                        reason="Checkout failures",
                        requires_user_approval=True,
                    ),
                ],
                summary="Run checkout smoke.",
                recommendation_confidence=0.8,
            ),
            previous_reports=[_historical_report()],
        )
    )
    assert report is not None
    assert len(report.similar_incidents) == 1
    assert report.similar_incidents[0].similarity_score >= 0.40


def test_similar_failure_cluster():
    report = build_historical_learning(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Cluster overlap",
                    severity="high",
                    confidence=0.8,
                    related_entity_count=4,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
            failure_clusters=[{"cluster_id": "cluster_7", "module": "checkout", "total_failures": 5}],
            previous_reports=[_historical_report()],
        )
    )
    assert report is not None
    assert report.similar_incidents[0].similarity_score >= 0.50


def test_similar_browser_watch():
    report = build_historical_learning(
        **_base(
            storyline=[
                IncidentStorylineStep(
                    step_number=1,
                    title="Browser alert",
                    description="Checkout UI changed",
                    confidence=0.8,
                    related_entity_type="browser_watch",
                    related_entity_id="watch_checkout",
                ),
            ],
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="UI signals",
                    severity="medium",
                    confidence=0.7,
                    related_entity_count=2,
                ),
            ],
            previous_reports=[
                _historical_report(
                    storyline=[
                        IncidentStorylineStep(
                            step_number=1,
                            title="Browser alert",
                            description="Checkout UI changed",
                            confidence=0.75,
                            related_entity_type="browser_watch",
                            related_entity_id="watch_checkout",
                        ),
                    ],
                ),
            ],
        )
    )
    assert report is not None
    assert report.similar_incidents[0].similarity_score >= 0.40


def test_similar_deployment_risk_level():
    report = build_historical_learning(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    rank=1,
                    statement="Authentication login regression",
                    confidence=0.7,
                    basis="inference",
                ),
            ],
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=62,
                risk_level="high",
                confidence=0.72,
                summary="High deployment risk.",
            ),
            impact_map=[
                IncidentImpactNode(
                    title="Auth",
                    description="Auth signals",
                    severity="medium",
                    confidence=0.65,
                    related_entity_count=2,
                ),
            ],
            previous_reports=[
                _historical_report(
                    id="hist-auth-29",
                    description="Authentication incident",
                    hypotheses=[
                        IncidentHypothesis(
                            id="H1",
                            rank=1,
                            statement="Auth login failures after deploy",
                            confidence=0.72,
                            basis="inference",
                        ),
                    ],
                    deployment_risk_assessment=DeploymentRiskAssessment(
                        risk_score=58,
                        risk_level="high",
                        confidence=0.7,
                        summary="High risk.",
                    ),
                    impact_map=[
                        IncidentImpactNode(
                            title="Auth",
                            description="Auth instability",
                            severity="medium",
                            confidence=0.7,
                            related_entity_count=3,
                        ),
                    ],
                ),
            ],
        )
    )
    assert report is not None
    assert any(i.similarity_score >= 0.40 for i in report.similar_incidents)


def test_similarity_ordering():
    older = _historical_report(id="hist-old", created_at="2026-06-01T10:00:00+00:00")
    newer = _historical_report(id="hist-new", created_at="2026-06-09T10:00:00+00:00")
    report = build_historical_learning(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Checkout signals",
                    severity="high",
                    confidence=0.85,
                    related_entity_count=7,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
            failure_clusters=[{"cluster_id": "cluster_7", "module": "checkout"}],
            previous_reports=[older, newer],
        )
    )
    assert report is not None
    scores = [i.similarity_score for i in report.similar_incidents]
    assert scores == sorted(scores, reverse=True)


def test_similarity_threshold_filters_weak_matches():
    report = build_historical_learning(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Search",
                    description="Unrelated area",
                    severity="low",
                    confidence=0.5,
                    related_entity_count=1,
                ),
            ],
            deployment_risk_assessment=DeploymentRiskAssessment(
                risk_score=20,
                risk_level="low",
                confidence=0.5,
                summary="Low risk.",
            ),
            previous_reports=[
                _historical_report(
                    id="hist-unrelated",
                    impact_map=[
                        IncidentImpactNode(
                            title="Billing",
                            description="Different module",
                            severity="low",
                            confidence=0.5,
                            related_entity_count=1,
                        ),
                    ],
                    deployment_risk_assessment=DeploymentRiskAssessment(
                        risk_score=18,
                        risk_level="medium",
                        confidence=0.5,
                        summary="Different risk.",
                    ),
                    hypotheses=[
                        IncidentHypothesis(
                            id="H9",
                            rank=1,
                            statement="Unrelated billing issue",
                            confidence=0.4,
                            basis="assumption",
                        ),
                    ],
                ),
            ],
        )
    )
    assert report is None


def test_pattern_summary_generation():
    report = build_historical_learning(
        **_base(
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Checkout signals",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=6,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
            ],
            failure_clusters=[{"cluster_id": "cluster_7", "module": "checkout"}],
            test_recommendations=TestRecommendationReport(
                recommendations=[
                    RecommendedTest(
                        recommendation_id="smoke:checkout",
                        test_name="Checkout Smoke Suite",
                        test_type="smoke",
                        priority=1,
                        confidence=0.85,
                        reason="Validate checkout",
                        requires_user_approval=True,
                    ),
                ],
                summary="Run checkout smoke.",
                recommendation_confidence=0.85,
            ),
            previous_reports=[
                _historical_report(id="hist-41"),
                _historical_report(id="hist-37", created_at="2026-06-07T10:00:00+00:00"),
            ],
        )
    )
    assert report is not None
    assert report.pattern_summary
    assert "Checkout" in report.pattern_summary
    assert report.confidence > 0


def test_deterministic_output():
    kwargs = _base(
        hypotheses=[
            IncidentHypothesis(
                id="H1",
                rank=1,
                statement="Checkout timeout regression",
                confidence=0.75,
                basis="inference",
            ),
        ],
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=60,
            risk_level="high",
            confidence=0.7,
            summary="Elevated risk.",
        ),
        impact_map=[
            IncidentImpactNode(
                title="Checkout",
                description="Checkout signals",
                severity="high",
                confidence=0.84,
                related_entity_count=6,
            ),
        ],
        test_recommendations=TestRecommendationReport(
            recommendations=[
                RecommendedTest(
                    recommendation_id="smoke:checkout",
                    test_name="Checkout Smoke Suite",
                    test_type="smoke",
                    priority=1,
                    confidence=0.8,
                    reason="Checkout failures",
                    requires_user_approval=True,
                ),
            ],
            summary="Run checkout smoke.",
            recommendation_confidence=0.8,
        ),
        previous_reports=[_historical_report()],
    )
    a = build_historical_learning(**kwargs)
    b = build_historical_learning(**kwargs)
    assert a is not None and b is not None
    assert a.pattern_summary == b.pattern_summary
    assert [i.incident_id for i in a.similar_incidents] == [i.incident_id for i in b.similar_incidents]
    assert a.confidence == b.confidence


def test_backward_compat_report_without_historical_learning():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.historical_learning is None


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
def test_investigate_includes_historical_learning_field(
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
    assert "historical_learning" in body
    assert body["meta"]["analyze_only"] is True
    for action in body.get("recommended_actions") or []:
        assert action["requires_user_approval"] is True
