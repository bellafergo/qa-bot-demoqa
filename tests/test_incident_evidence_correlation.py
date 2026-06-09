# tests/test_incident_evidence_correlation.py
"""II-02A — Evidence Correlation Engine (read-only)."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    IncidentInvestigationRun,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
)
from services.incident_evidence_correlation_service import (
    build_evidence_correlation,
    correlate_api_failures,
    correlate_browser_alerts,
    correlate_browser_investigations,
    correlate_failed_runs,
    correlate_failure_clusters,
)


def test_correlate_failed_run_in_window():
    items = correlate_failed_runs(
        [
            RelatedRunSummary(
                run_id="RUN-123",
                test_id="AUTH-014",
                test_name="Login flow",
                status="failed",
                started_at="2026-06-09T12:00:00+00:00",
                error_summary="AssertionError: redirect failed",
                module="auth",
            ),
        ],
        incident_reported_at="2026-06-09T12:32:00+00:00",
    )
    assert len(items) == 1
    assert items[0].source == "failed_run"
    assert "RUN-123" in items[0].detail
    assert "32 minutes" in items[0].detail
    assert items[0].related_run_id == "RUN-123"


def test_correlate_browser_watch_alert():
    items = correlate_browser_alerts([
        {
            "watch_id": "AUTH_LOGIN",
            "summary": "DOM change on login form",
            "timestamp": "2026-06-09T13:22:00+00:00",
        },
    ])
    assert len(items) == 1
    assert items[0].source == "browser_watch"
    assert "AUTH_LOGIN" in items[0].detail


def test_correlate_browser_probe_investigation():
    items = correlate_browser_investigations(
        IncidentInvestigationRun(
            id="probe-1",
            created_at="2026-06-09T14:00:00+00:00",
            updated_at="2026-06-09T14:00:00+00:00",
            status="completed",
            incident_description="Login issue",
            probable_cause="HTTP 500 on /api/auth",
        ),
    )
    assert len(items) == 1
    assert items[0].source == "browser_probe"
    assert "Passive browser observation" in items[0].detail


def test_correlate_api_failures_from_step_evidence():
    run = RelatedRunSummary(
        run_id="run-api-1",
        test_id="API-001",
        status="failed",
        started_at="2026-06-09T10:00:00+00:00",
    )
    canonical = MagicMock()
    canonical.steps = [
        {"action": "api_call", "evidence": {"failure": {"type": "http_status_mismatch"}}},
    ]

    with patch("services.run_history_service.run_history_service") as mock_rh:
        mock_rh.get_run_unified.return_value = canonical
        items = correlate_api_failures([run])

    assert len(items) == 1
    assert items[0].source == "api_evidence"
    assert "HTTP failure evidence detected" in items[0].detail


def test_correlate_failure_cluster_overlap():
    items = correlate_failure_clusters(
        [{"module": "auth", "total_failures": 4, "root_cause_category": "timeout"}],
        impacted_modules=["auth"],
    )
    assert len(items) == 1
    assert items[0].source == "failure_cluster"
    assert "overlaps incident modules" in items[0].detail.lower()


def test_build_evidence_correlation_empty_state():
    summary = build_evidence_correlation(
        related_runs=[],
        browser_events=[],
        browser_investigation=None,
        clusters=[],
        related_pr_analysis=[],
    )
    assert summary.total_correlations == 0
    assert summary.strongest_source is None
    assert summary.evidence == []


def test_build_evidence_correlation_strongest_source():
    summary = build_evidence_correlation(
        related_runs=[
            RelatedRunSummary(
                run_id="r1",
                status="failed",
                started_at="2026-06-09T10:00:00+00:00",
            ),
        ],
        browser_events=[],
        browser_investigation=None,
        clusters=[],
        related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="42",
                provider="github",
                pr_risk_score=70.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                reason="same module affected: auth",
            ),
        ],
        incident_reported_at="2026-06-09T11:00:00+00:00",
    )
    assert summary.total_correlations >= 2
    assert summary.strongest_source == "failed_run"
    sources = {e.source for e in summary.evidence}
    assert "failed_run" in sources
    assert "pr_analysis" in sources


def test_backward_compat_old_report_without_evidence_correlation():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.3b", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.evidence_correlation is None


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
def test_investigate_includes_evidence_correlation(
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
    assert body.get("evidence_correlation") is not None
    assert body["evidence_correlation"]["total_correlations"] >= 1
    assert body["meta"]["analyze_only"] is True
    assert body["meta"]["engine_version"] == "incident-v1.4a"
    for action in body.get("actions_available") or []:
        assert action["requires_user_approval"] is True
