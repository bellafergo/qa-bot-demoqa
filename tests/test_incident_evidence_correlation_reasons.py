# tests/test_incident_evidence_correlation_reasons.py
"""II-02C — Evidence Correlation reasoning and drilldown metadata (read-only)."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

from models.incident_models import (
    CorrelatedEvidence,
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
    _correlate_pr_analysis,
    _correlate_system_memory,
)


def test_failed_run_reason_includes_timing_and_module():
    items = correlate_failed_runs(
        [
            RelatedRunSummary(
                run_id="RUN-123",
                test_id="AUTH-014",
                status="failed",
                started_at="2026-06-09T12:00:00+00:00",
                module="auth",
            ),
        ],
        incident_reported_at="2026-06-09T12:32:00+00:00",
    )
    assert len(items) == 1
    assert "32 minutes before the incident" in items[0].reason
    assert "AUTH" in items[0].reason
    assert items[0].related_entity_type == "run"
    assert items[0].related_entity_id == "RUN-123"


def test_browser_watch_reason():
    items = correlate_browser_alerts([
        {"watch_id": "AUTH_LOGIN", "summary": "DOM change", "timestamp": "2026-06-09T13:22:00+00:00"},
    ])
    assert items[0].reason == (
        "Browser Watch alert detected on the same route referenced by the incident."
    )
    assert items[0].related_entity_type == "browser_watch"
    assert items[0].related_entity_id == "AUTH_LOGIN"


def test_browser_probe_reason_and_drilldown():
    items = correlate_browser_investigations(
        IncidentInvestigationRun(
            id="probe-99",
            created_at="2026-06-09T14:00:00+00:00",
            updated_at="2026-06-09T14:00:00+00:00",
            status="completed",
            incident_description="Login issue",
        ),
    )
    assert items[0].reason == (
        "Browser probe captured evidence on the target URL under investigation."
    )
    assert items[0].related_entity_type == "browser_probe"
    assert items[0].related_entity_id == "probe-99"


def test_pr_analysis_reason():
    items = _correlate_pr_analysis([
        RelatedPRAnalysisSummary(
            pr_number="42",
            provider="github",
            pr_risk_score=70.0,
            risk_level="HIGH",
            impacted_modules=["auth"],
        ),
    ])
    assert items[0].reason == (
        "Stored PR Analysis references the same impacted module identified by the investigation."
    )
    assert items[0].related_entity_type == "pr_analysis"
    assert items[0].related_entity_id == "github:42"


def test_failure_cluster_reason():
    items = correlate_failure_clusters(
        [{"module": "auth", "cluster_id": "cluster-auth-1", "total_failures": 3}],
        impacted_modules=["auth"],
    )
    assert items[0].reason == (
        "Failure cluster overlaps incident modules and occurred within the investigation window."
    )
    assert items[0].related_entity_type == "failure_cluster"
    assert items[0].related_entity_id == "cluster-auth-1"


def test_system_memory_reason():
    items = _correlate_system_memory(["Route /login is critical for auth flows"])
    assert items[0].reason == (
        "System Memory contains route or module hints matching the incident context."
    )
    assert items[0].related_entity_type == "memory_hint"
    assert items[0].related_entity_id == "hint-0"


def test_api_evidence_reason():
    run = RelatedRunSummary(
        run_id="run-api-1",
        status="failed",
        started_at="2026-06-09T10:00:00+00:00",
        error_summary="HTTP 500 from /api/auth",
    )
    canonical = MagicMock()
    canonical.steps = [
        {"action": "api_call", "evidence": {"failure": {"type": "http_status_mismatch"}}},
    ]

    with patch("services.run_history_service.run_history_service") as mock_rh:
        mock_rh.get_run_unified.return_value = canonical
        items = correlate_api_failures([run])

    assert items[0].reason == "API evidence was extracted from a correlated failed run."
    assert items[0].related_entity_type == "run"
    assert items[0].related_entity_id == "run-api-1"


def test_drilldown_metadata_serialization():
    item = CorrelatedEvidence(
        source="failed_run",
        confidence=0.85,
        title="Correlated failed run",
        detail="Run RUN-1 failed",
        reason="Run failed 10 minutes before the incident.",
        related_entity_type="run",
        related_entity_id="RUN-1",
        related_run_id="RUN-1",
    )
    payload = item.model_dump()
    assert payload["reason"] == "Run failed 10 minutes before the incident."
    assert payload["related_entity_type"] == "run"
    assert payload["related_entity_id"] == "RUN-1"


def test_backward_compat_correlated_evidence_without_new_fields():
    item = CorrelatedEvidence(
        source="failed_run",
        confidence=0.85,
        title="Correlated failed run",
        detail="detail",
    )
    assert item.reason == ""
    assert item.related_entity_type is None
    assert item.related_entity_id is None


def test_backward_compat_report_without_reason_fields():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "evidence_correlation": {
            "total_correlations": 1,
            "strongest_source": "failed_run",
            "evidence": [{
                "source": "failed_run",
                "confidence": 0.85,
                "title": "Correlated failed run",
                "detail": "Run failed",
            }],
        },
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    ev = report.evidence_correlation.evidence[0]
    assert ev.reason == ""
    assert ev.related_entity_type is None


def test_build_evidence_correlation_populates_reasons():
    with patch("services.run_history_service.run_history_service") as mock_rh:
        mock_rh.get_run_unified.return_value = None
        summary = build_evidence_correlation(
            related_runs=[
                RelatedRunSummary(
                    run_id="r1",
                    status="failed",
                    started_at="2026-06-09T10:00:00+00:00",
                    module="auth",
                ),
            ],
            browser_events=[],
            browser_investigation=None,
            clusters=[],
            related_pr_analysis=[],
            incident_reported_at="2026-06-09T11:00:00+00:00",
        )
    assert summary.evidence[0].reason
    assert summary.evidence[0].related_entity_type == "run"
