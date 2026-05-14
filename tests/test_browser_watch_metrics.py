# tests/test_browser_watch_metrics.py
from __future__ import annotations

import uuid
from unittest.mock import patch

from models.browser_inspection_watch_models import BrowserInspectionWatchCreate
from models.browser_inspection_models import BrowserInspectionResult
from services.browser_inspection_watch_service import create_watch, execute_watch_tick, get_watch_metrics


def test_metrics_empty_watch():
    w = create_watch(BrowserInspectionWatchCreate(url="https://example.com"))
    m = get_watch_metrics(w.watch_id)
    assert m.total_runs == 0
    assert m.total_diffs == 0
    assert m.current_status == "never_run"


@patch("services.browser_inspection_watch_service.inspect_url_collect")
@patch("services.browser_inspection_watch_service.persist_light_browser_inspection")
def test_metrics_counts_run_completed(mock_persist, mock_collect):
    w = create_watch(BrowserInspectionWatchCreate(url="https://example.com"))
    ins = BrowserInspectionResult(
        inspection_id=str(uuid.uuid4()),
        url="https://example.com",
        final_url="https://example.com/",
        title="t",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    mock_collect.return_value = (ins, {}, {})
    mock_persist.return_value = (ins.inspection_id, True, None)
    execute_watch_tick(w.watch_id, force=True)
    m = get_watch_metrics(w.watch_id)
    assert m.total_runs == 1
    assert m.total_diffs == 0
