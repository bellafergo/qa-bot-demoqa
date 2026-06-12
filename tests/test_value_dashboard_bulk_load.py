# tests/test_value_dashboard_bulk_load.py
"""V1.2 — Value dashboard bulk incident report loading."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

from services.value_dashboard_service import _load_incident_reports


def test_load_incident_reports_uses_bulk_list_full_reports():
    sample = [{"id": "r1", "project_id": "proj-a", "description": "d", "severity": "low", "summary": "s", "confidence": 0.5, "created_at": "2026-01-01T00:00:00+00:00"}]
    repo = MagicMock()
    repo.list_full_reports.return_value = sample

    with patch("services.db.incident_report_repository.incident_report_repo", repo):
        reports = _load_incident_reports("proj-a")

    repo.list_full_reports.assert_called_once_with(project_id="proj-a", limit=100)
    repo.get.assert_not_called()
    repo.list_reports.assert_not_called()
    assert len(reports) == 1
