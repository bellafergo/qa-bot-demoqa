# tests/test_incident_evidence_dedupe.py
"""II-02B — Evidence Correlation deduplication (read-only)."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

from models.incident_models import (
    CorrelatedEvidence,
    IncidentInvestigationRun,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
)
from services.incident_evidence_correlation_service import (
    build_evidence_correlation,
    dedupe_correlated_evidence,
)


def _item(
    source: str,
    confidence: float,
    *,
    related_run_id: str | None = None,
    timestamp: str | None = None,
) -> CorrelatedEvidence:
    return CorrelatedEvidence(
        source=source,
        confidence=confidence,
        title=f"{source} title",
        detail=f"{source} detail",
        timestamp=timestamp,
        related_run_id=related_run_id,
    )


def test_dedupe_failed_run_and_api_evidence_same_run_id():
    evidence = [
        _item("failed_run", 0.85, related_run_id="run-abc", timestamp="2026-06-09T10:00:00+00:00"),
        _item("api_evidence", 0.70, related_run_id="run-abc", timestamp="2026-06-09T10:00:00+00:00"),
    ]
    deduped = dedupe_correlated_evidence(evidence)
    assert len(deduped) == 1
    assert deduped[0].source == "failed_run"
    assert deduped[0].related_run_id == "run-abc"


def test_dedupe_failed_run_and_api_evidence_distinct_run_ids():
    evidence = [
        _item("failed_run", 0.85, related_run_id="run-a"),
        _item("api_evidence", 0.70, related_run_id="run-b"),
    ]
    deduped = dedupe_correlated_evidence(evidence)
    assert len(deduped) == 2
    sources = {item.source for item in deduped}
    assert sources == {"failed_run", "api_evidence"}


def test_dedupe_browser_probe_and_watch_same_signal():
    evidence = [
        _item("browser_probe", 0.80, timestamp="2026-06-09T14:00:00+00:00"),
        _item("browser_watch", 0.75, timestamp="2026-06-09T14:10:00+00:00"),
    ]
    deduped = dedupe_correlated_evidence(evidence)
    assert len(deduped) == 1
    assert deduped[0].source == "browser_probe"


def test_dedupe_browser_probe_and_watch_far_apart_keeps_both():
    evidence = [
        _item("browser_probe", 0.80, timestamp="2026-06-09T14:00:00+00:00"),
        _item("browser_watch", 0.75, timestamp="2026-06-09T16:00:00+00:00"),
    ]
    deduped = dedupe_correlated_evidence(evidence)
    assert len(deduped) == 2


def test_dedupe_pr_analysis_never_dedupes():
    evidence = [
        _item("pr_analysis", 0.62),
        _item("pr_analysis", 0.58),
    ]
    deduped = dedupe_correlated_evidence(evidence)
    assert len(deduped) == 2


def test_build_evidence_correlation_total_after_dedupe():
    run = RelatedRunSummary(
        run_id="run-dedupe-1",
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
        summary = build_evidence_correlation(
            related_runs=[run],
            browser_events=[],
            browser_investigation=None,
            clusters=[],
            related_pr_analysis=[],
            incident_reported_at="2026-06-09T11:00:00+00:00",
        )

    assert summary.total_correlations == 1
    assert len(summary.evidence) == 1
    assert summary.evidence[0].source == "failed_run"


def test_build_evidence_correlation_strongest_source_after_dedupe():
    with patch("services.run_history_service.run_history_service") as mock_rh:
        mock_rh.get_run_unified.return_value = None
        summary = build_evidence_correlation(
            related_runs=[
                RelatedRunSummary(
                    run_id="r1",
                    status="failed",
                    started_at="2026-06-09T10:00:00+00:00",
                ),
            ],
            browser_events=[],
            browser_investigation=IncidentInvestigationRun(
                id="probe-1",
                created_at="2026-06-09T14:00:00+00:00",
                updated_at="2026-06-09T14:00:00+00:00",
                status="completed",
                incident_description="Login issue",
            ),
            clusters=[],
            related_pr_analysis=[
                RelatedPRAnalysisSummary(
                    pr_number="42",
                    provider="github",
                    pr_risk_score=70.0,
                    risk_level="HIGH",
                    impacted_modules=["auth"],
                ),
            ],
            incident_reported_at="2026-06-09T11:00:00+00:00",
        )

    assert summary.strongest_source == "failed_run"
    assert summary.total_correlations == 3
    sources = {e.source for e in summary.evidence}
    assert sources == {"failed_run", "browser_probe", "pr_analysis"}


def test_build_evidence_correlation_browser_dedupe_updates_total():
    summary = build_evidence_correlation(
        related_runs=[],
        browser_events=[
            {
                "watch_id": "AUTH_LOGIN",
                "summary": "DOM change on login form",
                "timestamp": "2026-06-09T14:05:00+00:00",
            },
        ],
        browser_investigation=IncidentInvestigationRun(
            id="probe-1",
            created_at="2026-06-09T14:00:00+00:00",
            updated_at="2026-06-09T14:00:00+00:00",
            status="completed",
            incident_description="Login issue",
        ),
        clusters=[],
        related_pr_analysis=[],
    )
    assert summary.total_correlations == 1
    assert summary.strongest_source == "browser_probe"
    assert summary.evidence[0].source == "browser_probe"
