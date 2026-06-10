# tests/test_incident_storyline.py
"""II-03B — Incident Storyline (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentTimelineEvent,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    RelatedPRSummary,
    RelatedRunSummary,
    TemporalCorrelationSummary,
)
from services.incident_storyline_service import build_incident_storyline


def test_pr_driven_storyline():
    storyline = build_incident_storyline(
        hypotheses=[
            IncidentHypothesis(
                id="H1",
                rank=1,
                statement="PR #483 likely introduced the regression.",
                confidence=0.8,
                basis="evidence",
                supporting_refs=["pr_analysis:github:483"],
            ),
        ],
        evidence_correlation=EvidenceCorrelationSummary(
            total_correlations=1,
            strongest_source="pr_analysis",
            evidence=[
                CorrelatedEvidence(
                    source="pr_analysis",
                    confidence=0.92,
                    title="PR Analysis snapshot",
                    detail="PR Analysis references impacted module checkout",
                    reason="Recent code change correlated with incident evidence",
                    related_entity_type="pr_analysis",
                    related_entity_id="github:483",
                    timestamp="2026-06-09T08:00:00+00:00",
                ),
            ],
        ),
        related_runs=[],
        related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="483",
                provider="github",
                pr_risk_score=84.0,
                risk_level="HIGH",
                impacted_modules=["checkout"],
                reason="same module affected: checkout",
                analyzed_at="2026-06-09T08:00:00+00:00",
            ),
        ],
        related_prs=[],
        browser_events=[],
        clusters=[],
        timeline=[],
        temporal_correlation=None,
        incident_reported_at="2026-06-09T10:00:00+00:00",
    )
    assert len(storyline) >= 2
    assert storyline[0].title.startswith("PR #483")
    assert storyline[0].related_entity_type == "pr_analysis"
    assert storyline[0].related_entity_id == "github:483"
    assert storyline[-1].title == "Investigation conclusion"


def test_browser_alert_storyline():
    storyline = build_incident_storyline(
        hypotheses=[],
        evidence_correlation=EvidenceCorrelationSummary(
            total_correlations=1,
            strongest_source="browser_watch",
            evidence=[
                CorrelatedEvidence(
                    source="browser_watch",
                    confidence=0.88,
                    title="Browser alert detected",
                    detail="Watch CHECKOUT reported change",
                    reason="Browser Watch detected a visual change",
                    related_entity_type="browser_watch",
                    related_entity_id="watch_123",
                    timestamp="2026-06-09T09:00:00+00:00",
                ),
            ],
        ),
        related_runs=[],
        related_pr_analysis=[],
        related_prs=[],
        browser_events=[
            {
                "watch_id": "watch_123",
                "summary": "Checkout UI changed",
                "change_level": "visual",
                "timestamp": "2026-06-09T09:00:00+00:00",
            },
        ],
        clusters=[],
        timeline=[
            IncidentTimelineEvent(
                timestamp="2026-06-09T09:00:00+00:00",
                event_type="browser_watch_alert",
                title="Browser Watch alert",
                details="Checkout UI changed",
                source="browser_watch:watch_123",
            ),
        ],
        temporal_correlation=None,
        incident_reported_at="2026-06-09T10:00:00+00:00",
    )
    assert len(storyline) >= 1
    browser_steps = [s for s in storyline if s.related_entity_type == "browser_watch"]
    assert browser_steps
    assert browser_steps[0].related_entity_id == "watch_123"
    assert browser_steps[0].confidence >= 0.7


def test_failed_run_storyline():
    storyline = build_incident_storyline(
        hypotheses=[
            IncidentHypothesis(
                statement="Failed auth test explains the incident.",
                confidence=0.76,
                basis="evidence",
                supporting_refs=["run:RUN-99"],
            ),
        ],
        evidence_correlation=EvidenceCorrelationSummary(
            total_correlations=1,
            strongest_source="failed_run",
            evidence=[
                CorrelatedEvidence(
                    source="failed_run",
                    confidence=0.85,
                    title="Correlated failed run",
                    detail="Run RUN-99 failed",
                    reason="Run failed shortly before incident report",
                    related_entity_type="run",
                    related_entity_id="RUN-99",
                    timestamp="2026-06-09T09:30:00+00:00",
                ),
            ],
        ),
        related_runs=[
            RelatedRunSummary(
                run_id="RUN-99",
                test_id="AUTH-014",
                test_name="Auth login",
                status="failed",
                started_at="2026-06-09T09:30:00+00:00",
                error_summary="AssertionError: login button missing",
            ),
        ],
        related_pr_analysis=[],
        related_prs=[],
        browser_events=[],
        clusters=[],
        timeline=[],
        temporal_correlation=None,
        incident_reported_at="2026-06-09T10:00:00+00:00",
    )
    run_steps = [s for s in storyline if s.related_entity_type == "run"]
    assert run_steps
    assert run_steps[0].related_entity_id == "RUN-99"
    assert storyline[-1].title == "Investigation conclusion"


def test_mixed_evidence_storyline_ordering():
    storyline = build_incident_storyline(
        hypotheses=[
            IncidentHypothesis(
                statement="PR change led to UI alert and test failures.",
                confidence=0.82,
                basis="evidence",
            ),
        ],
        evidence_correlation=EvidenceCorrelationSummary(
            total_correlations=3,
            strongest_source="pr_analysis",
            evidence=[
                CorrelatedEvidence(
                    source="pr_analysis",
                    confidence=0.9,
                    title="PR Analysis",
                    detail="PR #483",
                    reason="PR correlated",
                    related_entity_type="pr_analysis",
                    related_entity_id="github:483",
                    timestamp="2026-06-09T08:00:00+00:00",
                ),
                CorrelatedEvidence(
                    source="browser_watch",
                    confidence=0.86,
                    title="Browser alert",
                    detail="UI changed",
                    reason="Alert after code change",
                    related_entity_type="browser_watch",
                    related_entity_id="watch_123",
                    timestamp="2026-06-09T09:00:00+00:00",
                ),
                CorrelatedEvidence(
                    source="failed_run",
                    confidence=0.84,
                    title="Failed run",
                    detail="RUN-99",
                    reason="Failures after alert",
                    related_entity_type="run",
                    related_entity_id="RUN-99",
                    timestamp="2026-06-09T09:30:00+00:00",
                ),
            ],
        ),
        related_runs=[
            RelatedRunSummary(
                run_id="RUN-99",
                test_id="AUTH-014",
                status="failed",
                started_at="2026-06-09T09:30:00+00:00",
                error_summary="AssertionError",
            ),
        ],
        related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="483",
                provider="github",
                pr_risk_score=80.0,
                risk_level="HIGH",
                analyzed_at="2026-06-09T08:00:00+00:00",
            ),
        ],
        related_prs=[],
        browser_events=[
            {
                "watch_id": "watch_123",
                "summary": "Checkout UI changed",
                "timestamp": "2026-06-09T09:00:00+00:00",
            },
        ],
        clusters=[
            {
                "cluster_id": "cluster-auth-1",
                "module": "auth",
                "total_failures": 4,
                "root_cause_category": "timeout",
            },
        ],
        timeline=[],
        temporal_correlation=TemporalCorrelationSummary(
            signal="strong",
            reason="PR analysis, browser alert, and failed runs within 30 min windows",
            event_chain=["PR analyzed", "Browser alert", "Failed run(s)"],
        ),
        incident_reported_at="2026-06-09T10:00:00+00:00",
    )
    assert len(storyline) >= 4
    types = [s.related_entity_type for s in storyline if s.related_entity_type]
    pr_idx = types.index("pr_analysis")
    browser_idx = types.index("browser_watch")
    run_idx = types.index("run")
    cluster_idx = types.index("failure_cluster")
    assert pr_idx < browser_idx < run_idx < cluster_idx
    assert storyline[-1].title == "Investigation conclusion"
    assert storyline[0].step_number == 1
    assert all(s.step_number == i + 1 for i, s in enumerate(storyline))


def test_empty_evidence_storyline():
    storyline = build_incident_storyline(
        hypotheses=[
            IncidentHypothesis(
                statement="No strong correlates found.",
                confidence=0.2,
                basis="assumption",
                supporting_refs=[],
            ),
        ],
        evidence_correlation=None,
        related_runs=[],
        related_pr_analysis=[],
        related_prs=[],
        browser_events=[],
        clusters=[],
        timeline=[],
        temporal_correlation=None,
    )
    assert storyline == []


def test_backward_compat_report_without_storyline():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.storyline == []


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
def test_investigate_includes_storyline(
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
    assert "storyline" in body
    assert isinstance(body["storyline"], list)
    assert len(body["storyline"]) >= 1
    assert body["meta"]["analyze_only"] is True
    for action in body.get("actions_available") or []:
        assert action["requires_user_approval"] is True
