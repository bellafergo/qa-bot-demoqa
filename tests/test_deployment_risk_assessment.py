# tests/test_deployment_risk_assessment.py
"""II-05A — Deployment Risk Assessment (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentImpactNode,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
    TemporalCorrelationSummary,
)
from services.deployment_risk_assessment_service import build_deployment_risk_assessment


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
    }
    base.update(overrides)
    return base


def test_no_evidence_returns_none():
    assert build_deployment_risk_assessment(**_base()) is None


def test_low_risk_scenario():
    assessment = build_deployment_risk_assessment(
        **_base(
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
    assert assessment is not None
    assert assessment.risk_level == "low"
    assert assessment.risk_score <= 24
    assert assessment.confidence > 0


def test_medium_risk_scenario():
    assessment = build_deployment_risk_assessment(
        **_base(
            browser_events=[{"watch_id": "w1", "summary": "Checkout UI changed"}],
            related_runs=[
                RelatedRunSummary(run_id="RUN-1", module="checkout", status="failed", error_summary="fail"),
                RelatedRunSummary(run_id="RUN-2", module="checkout", status="failed", error_summary="fail"),
            ],
            evidence_correlation=EvidenceCorrelationSummary(
                total_correlations=2,
                strongest_source="failed_run",
                evidence=[
                    CorrelatedEvidence(
                        source="failed_run",
                        confidence=0.7,
                        title="Failed run",
                        detail="RUN-1",
                        reason="correlated",
                        related_entity_type="run",
                        related_entity_id="RUN-1",
                    ),
                ],
            ),
        )
    )
    assert assessment is not None
    assert assessment.risk_level in ("medium", "high")
    assert 25 <= assessment.risk_score <= 74 or assessment.risk_level == "medium"


def test_high_risk_scenario():
    assessment = build_deployment_risk_assessment(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    statement="Checkout regression likely from recent deploy.",
                    confidence=0.82,
                    basis="evidence",
                ),
            ],
            related_pr_analysis=[
                RelatedPRAnalysisSummary(
                    pr_number="483",
                    provider="github",
                    pr_risk_score=78.0,
                    risk_level="HIGH",
                    impacted_modules=["checkout"],
                    reason="same module affected: checkout",
                ),
            ],
            clusters=[{"cluster_id": "c1", "module": "checkout", "total_failures": 5}],
            browser_events=[{"watch_id": "w1", "summary": "Checkout UI changed"}],
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="Multiple signals",
                    severity="high",
                    confidence=0.84,
                    related_entity_count=8,
                    related_entity_type="failure_cluster",
                    related_entity_id="c1",
                ),
                IncidentImpactNode(
                    title="Payments",
                    description="Some signals",
                    severity="medium",
                    confidence=0.6,
                    related_entity_count=3,
                ),
            ],
            temporal_correlation=TemporalCorrelationSummary(
                signal="medium",
                reason="PR → alert → failure sequence within 2 hours",
            ),
        )
    )
    assert assessment is not None
    assert assessment.risk_level in ("high", "critical")
    assert assessment.risk_score >= 50


def test_critical_risk_scenario():
    assessment = build_deployment_risk_assessment(
        **_base(
            hypotheses=[
                IncidentHypothesis(
                    id="H1",
                    statement="Critical checkout regression.",
                    confidence=0.9,
                    basis="evidence",
                ),
            ],
            related_pr_analysis=[
                RelatedPRAnalysisSummary(
                    pr_number="483",
                    provider="github",
                    pr_risk_score=90.0,
                    risk_level="HIGH",
                    impacted_modules=["checkout"],
                ),
            ],
            clusters=[{"cluster_id": "cluster_7", "module": "checkout", "total_failures": 12}],
            browser_events=[
                {"watch_id": "w1", "summary": "Checkout UI changed"},
                {"watch_id": "w2", "summary": "Payment step changed"},
            ],
            related_runs=[
                RelatedRunSummary(run_id="RUN-1", module="checkout", status="failed", error_summary="e1"),
                RelatedRunSummary(run_id="RUN-2", module="checkout", status="failed", error_summary="e2"),
                RelatedRunSummary(run_id="RUN-3", module="checkout", status="failed", error_summary="e3"),
            ],
            impact_map=[
                IncidentImpactNode(
                    title="Checkout",
                    description="High impact",
                    severity="high",
                    confidence=0.9,
                    related_entity_count=12,
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster_7",
                ),
                IncidentImpactNode(
                    title="Payments",
                    description="Medium impact",
                    severity="high",
                    confidence=0.8,
                    related_entity_count=6,
                ),
            ],
            evidence_correlation=EvidenceCorrelationSummary(
                total_correlations=4,
                strongest_source="pr_analysis",
                evidence=[
                    CorrelatedEvidence(
                        source="pr_analysis",
                        confidence=0.92,
                        title="PR",
                        detail="PR #483",
                        reason="high",
                        related_entity_type="pr_analysis",
                        related_entity_id="github:483",
                    ),
                    CorrelatedEvidence(
                        source="failure_cluster",
                        confidence=0.88,
                        title="Cluster",
                        detail="cluster",
                        reason="cluster",
                        related_entity_type="failure_cluster",
                        related_entity_id="cluster_7",
                    ),
                ],
            ),
            temporal_correlation=TemporalCorrelationSummary(
                signal="strong",
                reason="PR analysis, browser alert, and failed runs within 30 min windows",
            ),
        )
    )
    assert assessment is not None
    assert assessment.risk_level == "critical"
    assert assessment.risk_score >= 75


def test_factor_ordering_by_weight():
    assessment = build_deployment_risk_assessment(
        **_base(
            clusters=[{"cluster_id": "c1", "module": "checkout", "total_failures": 8}],
            browser_events=[{"watch_id": "w1", "summary": "alert"}],
            related_runs=[
                RelatedRunSummary(run_id="RUN-1", module="checkout", status="failed", error_summary="fail"),
            ],
        )
    )
    assert assessment is not None
    weights = [f.weight for f in assessment.contributing_factors]
    assert weights == sorted(weights, reverse=True)


def test_confidence_calculation():
    assessment = build_deployment_risk_assessment(
        **_base(
            hypotheses=[
                IncidentHypothesis(statement="test", confidence=0.85, basis="evidence"),
            ],
            evidence_correlation=EvidenceCorrelationSummary(
                total_correlations=2,
                evidence=[
                    CorrelatedEvidence(
                        source="failed_run",
                        confidence=0.9,
                        title="t",
                        detail="d",
                        reason="r",
                    ),
                ],
            ),
            related_runs=[
                RelatedRunSummary(run_id="RUN-1", module="auth", status="failed", error_summary="x"),
            ],
        )
    )
    assert assessment is not None
    assert 0.0 < assessment.confidence <= 1.0


def test_deterministic_output():
    kwargs = _base(
        clusters=[{"cluster_id": "c1", "module": "checkout", "total_failures": 4}],
        browser_events=[{"watch_id": "w1", "summary": "UI change"}],
    )
    a = build_deployment_risk_assessment(**kwargs)
    b = build_deployment_risk_assessment(**kwargs)
    assert a is not None and b is not None
    assert a.risk_score == b.risk_score
    assert a.risk_level == b.risk_level
    assert a.confidence == b.confidence
    assert [f.title for f in a.contributing_factors] == [f.title for f in b.contributing_factors]


def test_backward_compat_report_without_deployment_risk():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.deployment_risk_assessment is None


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
def test_investigate_includes_deployment_risk_assessment(
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
    assert "deployment_risk_assessment" in body
    assert body["meta"]["analyze_only"] is True
    for action in body.get("actions_available") or []:
        assert action["requires_user_approval"] is True
    for action in body.get("recommended_actions") or []:
        assert action["requires_user_approval"] is True
