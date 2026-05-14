# tests/test_insights_timeline.py
from __future__ import annotations

from unittest.mock import MagicMock, patch

from services.insights_timeline_service import get_failure_timeline


def test_timeline_returns_buckets_and_deploy_placeholder():
    mock_run = MagicMock()
    mock_run.status = "failed"
    mock_run.started_at = "2026-05-10T12:00:00+00:00"

    with patch("services.insights_timeline_service.run_history_service") as rhs:
        rhs.list_runs.return_value = [mock_run]
        out = get_failure_timeline(project_id=None, days=7)

    assert len(out.buckets) == 7
    assert out.deploy_context.deploy_id is None
    assert out.cluster_recurrence_note == "insufficient_evidence"
    assert out.runs_analyzed >= 1
