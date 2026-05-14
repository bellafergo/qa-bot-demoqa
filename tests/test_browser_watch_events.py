# tests/test_browser_watch_events.py
from __future__ import annotations

import uuid
from unittest.mock import patch

from fastapi.testclient import TestClient

from models.browser_inspection_watch_models import BrowserInspectionWatchCreate
from models.browser_inspection_models import BrowserInspectionResult
from services.browser_inspection_watch_service import create_watch, execute_watch_tick, list_watch_events


@patch("services.browser_inspection_watch_service.inspect_url_collect")
@patch("services.browser_inspection_watch_service.persist_light_browser_inspection")
def test_events_include_run_completed(mock_persist, mock_collect):
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
    ev = list_watch_events(w.watch_id, limit=20)
    types = {e.event_type for e in ev}
    assert "run_completed" in types
    assert "run_started" in types
    ev_sorted = sorted(ev, key=lambda e: (e.created_at, e.event_id))
    started = [e for e in ev_sorted if e.event_type == "run_started"]
    completed = [e for e in ev_sorted if e.event_type == "run_completed"]
    assert started and completed
    assert started[0].created_at <= completed[0].created_at
    assert getattr(started[0], "run_origin", None) == "cloud"
    assert getattr(completed[0], "run_origin", None) == "cloud"
    for e in ev:
        assert len(e.summary or "") < 5000


def test_events_endpoint_via_client():
    from app import app

    client = TestClient(app)
    r = client.post(
        "/browser-inspections/watch",
        json={"url": "https://example.com", "execution_mode": "cloud"},
    )
    assert r.status_code == 200
    wid = r.json()["watch_id"]
    resp = client.get(f"/browser-inspections/watch/{wid}/events")
    assert resp.status_code == 200
    assert isinstance(resp.json(), list)


def test_events_pagination_cursor_respects_order():
    from datetime import datetime, timedelta, timezone

    from services.db.browser_inspection_watch_repository import browser_inspection_watch_repo

    w = create_watch(BrowserInspectionWatchCreate(url="https://example.com"))
    base = datetime(2026, 5, 1, 12, 0, 0, tzinfo=timezone.utc)
    stamps = [(base + timedelta(seconds=i)).isoformat() for i in range(5)]

    def next_stamp():
        return stamps.pop(0)

    with patch("services.db.browser_inspection_watch_repository._utc_iso", side_effect=next_stamp):
        for i in range(5):
            browser_inspection_watch_repo.insert_event(
                event_id=f"evt-{i}",
                watch_id=w.watch_id,
                base_inspection_id=None,
                target_inspection_id="_",
                change_level="none",
                summary=f"e{i}",
                regression_signals=[],
                improvement_signals=[],
                alert_triggered=False,
                alert_kind=None,
                visual_meta=None,
                event_type="diff_generated",
            )

    page1, c1 = browser_inspection_watch_repo.list_events_page(w.watch_id, limit=2, cursor=None)
    assert [e["summary"] for e in page1] == ["e4", "e3"]
    assert c1
    page2, c2 = browser_inspection_watch_repo.list_events_page(w.watch_id, limit=2, cursor=c1)
    assert [e["summary"] for e in page2] == ["e2", "e1"]
    assert c2
    page3, c3 = browser_inspection_watch_repo.list_events_page(w.watch_id, limit=2, cursor=c2)
    assert [e["summary"] for e in page3] == ["e0"]
    assert c3 is None


def test_events_endpoint_paged_and_invalid_cursor():
    from datetime import datetime, timedelta, timezone

    from app import app
    from services.db.browser_inspection_watch_repository import browser_inspection_watch_repo

    client = TestClient(app)
    r = client.post("/browser-inspections/watch", json={"url": "https://example.org", "execution_mode": "cloud"})
    wid = r.json()["watch_id"]
    base = datetime(2026, 6, 1, 8, 0, 0, tzinfo=timezone.utc)
    stamps = [(base + timedelta(seconds=i)).isoformat() for i in range(3)]

    def next_stamp():
        return stamps.pop(0)

    with patch("services.db.browser_inspection_watch_repository._utc_iso", side_effect=next_stamp):
        for i in range(3):
            browser_inspection_watch_repo.insert_event(
                event_id=f"p-{i}",
                watch_id=wid,
                base_inspection_id=None,
                target_inspection_id="_",
                change_level="none",
                summary=f"s{i}",
                regression_signals=[],
                improvement_signals=[],
                alert_triggered=False,
                alert_kind=None,
                visual_meta=None,
                event_type="run_completed",
            )

    j = client.get(f"/browser-inspections/watch/{wid}/events", params={"paged": "true", "limit": 2}).json()
    assert "items" in j and "next_cursor" in j
    assert len(j["items"]) == 2
    j2 = client.get(
        f"/browser-inspections/watch/{wid}/events",
        params={"paged": "true", "limit": 2, "cursor": j["next_cursor"]},
    ).json()
    assert len(j2["items"]) == 1
    bad = client.get(f"/browser-inspections/watch/{wid}/events", params={"cursor": "not-a-cursor"})
    assert bad.status_code == 400


def test_watch_events_indexes_created():
    from sqlalchemy import text

    from services.db.sqlite_db import engine

    with engine.connect() as conn:
        rows = conn.execute(
            text(
                "SELECT name FROM sqlite_master WHERE type='index' "
                "AND tbl_name='browser_inspection_watch_events'"
            )
        ).fetchall()
    names = {r[0] for r in rows}
    assert "ix_bio_wevents_watch_created" in names
    assert "ix_bio_wevents_watch_event_type" in names


def test_init_catalog_db_idempotent():
    from services.db.init_db import init_catalog_db

    init_catalog_db()
    init_catalog_db()
