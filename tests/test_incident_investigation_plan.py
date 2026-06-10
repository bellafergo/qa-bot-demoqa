# tests/test_incident_investigation_plan.py
"""II-03A — Recommended Investigation Plan (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
)
from services.incident_investigation_plan_service import build_investigation_plan


def test_build_plan_from_pr_analysis_correlation():
    plan = build_investigation_plan(
        hypotheses=[
            IncidentHypothesis(
                id="H1",
                rank=1,
                statement="PR may have introduced auth regression.",
                confidence=0.72,
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
                    confidence=0.62,
                    title="PR Analysis snapshot",
                    detail="PR Analysis references impacted module auth",
                    reason="Stored PR Analysis references the same impacted module identified by the investigation.",
                    related_entity_type="pr_analysis",
                    related_entity_id="github:483",
                ),
            ],
        ),
        related_runs=[],
        related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="483",
                provider="github",
                pr_risk_score=70.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                reason="same module affected: auth",
            ),
        ],
        browser_events=[],
        clusters=[],
    )
    assert len(plan) >= 1
    pr_steps = [p for p in plan if p.related_entity_type == "pr_analysis"]
    assert pr_steps
    assert "483" in pr_steps[0].title
    assert pr_steps[0].priority > 0


def test_build_plan_includes_failed_run():
    plan = build_investigation_plan(
        hypotheses=[
            IncidentHypothesis(
                id="H1",
                rank=1,
                statement="Recent failure may explain incident.",
                confidence=0.75,
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
                    reason="Run failed 10 minutes before the incident.",
                    related_entity_type="run",
                    related_entity_id="RUN-99",
                ),
            ],
        ),
        related_runs=[
            RelatedRunSummary(
                run_id="RUN-99",
                test_id="AUTH-014",
                status="failed",
                error_summary="AssertionError",
            ),
        ],
        related_pr_analysis=[],
        browser_events=[],
        clusters=[],
    )
    run_steps = [p for p in plan if p.related_entity_type == "run"]
    assert run_steps
    assert run_steps[0].title == "Inspect Latest Failed Run"
    assert run_steps[0].priority >= 85


def test_build_plan_browser_watch_and_cluster():
    plan = build_investigation_plan(
        hypotheses=[],
        evidence_correlation=EvidenceCorrelationSummary(
            total_correlations=2,
            strongest_source="browser_watch",
            evidence=[
                CorrelatedEvidence(
                    source="browser_watch",
                    confidence=0.75,
                    title="Browser alert detected",
                    detail="Watch AUTH_LOGIN reported change",
                    reason="Browser Watch alert detected on the same route referenced by the incident.",
                    related_entity_type="browser_watch",
                    related_entity_id="AUTH_LOGIN",
                ),
                CorrelatedEvidence(
                    source="failure_cluster",
                    confidence=0.77,
                    title="Failure cluster overlap",
                    detail="Failure cluster overlaps incident modules (auth, 3 failure(s), timeout)",
                    reason="Failure cluster overlaps incident modules and occurred within the investigation window.",
                    related_entity_type="failure_cluster",
                    related_entity_id="cluster-auth-1",
                ),
            ],
        ),
        related_runs=[],
        related_pr_analysis=[],
        browser_events=[],
        clusters=[],
    )
    sources = {p.related_entity_type for p in plan}
    assert "browser_watch" in sources
    assert "failure_cluster" in sources


def test_build_plan_empty_when_no_signals():
    plan = build_investigation_plan(
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
        browser_events=[],
        clusters=[],
    )
    assert plan == []


def test_backward_compat_report_without_investigation_plan():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.investigation_plan == []


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
def test_investigate_includes_investigation_plan(
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
    assert "investigation_plan" in body
    assert isinstance(body["investigation_plan"], list)
    assert len(body["investigation_plan"]) >= 1
    assert body["meta"]["analyze_only"] is True
    for action in body.get("actions_available") or []:
        assert action["requires_user_approval"] is True
