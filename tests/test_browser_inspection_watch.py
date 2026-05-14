# tests/test_browser_inspection_watch.py
from __future__ import annotations

import uuid
from unittest.mock import patch

import pytest
from fastapi import HTTPException
from fastapi.testclient import TestClient
from pydantic import ValidationError

from models.browser_inspection_watch_models import BrowserInspectionWatchCreate
from services.browser_inspection_watch_service import max_change_level, threshold_allows_alert


def test_max_change_level_orders():
    assert max_change_level("none", "high") == "high"
    assert max_change_level("medium", "low") == "medium"
    assert max_change_level("none", "none") == "none"


def test_threshold_allows_alert_rules():
    assert threshold_allows_alert("high", "high") is True
    assert threshold_allows_alert("high", "medium") is False
    assert threshold_allows_alert("medium", "medium") is True
    assert threshold_allows_alert("medium", "low") is False
    assert threshold_allows_alert("low", "low") is True
    assert threshold_allows_alert("low", "none") is False


def test_create_watch_invalid_execution_mode():
    with pytest.raises(ValidationError):
        BrowserInspectionWatchCreate(
            url="https://example.com",
            execution_mode="local_agent",  # type: ignore[arg-type]
        )


def test_post_watch_valid():
    from app import app

    client = TestClient(app)
    r = client.post(
        "/browser-inspections/watch",
        json={
            "url": "https://example.com",
            "project_id": "p-watch",
            "interval_minutes": 60,
            "change_threshold": "medium",
            "enabled": True,
            "execution_mode": "cloud",
        },
    )
    assert r.status_code == 200
    data = r.json()
    assert data.get("watch_id")
    assert data["url"] == "https://example.com"
    assert data.get("compare_mode") == "last"
    assert data.get("current_status") == "never_run"
    assert "last_change_level" in data
    assert "last_visual_change_level" in data
    assert "baseline_inspection_id" in data


def test_get_watch_list_200_not_shadowed_by_inspection_id_route():
    """GET /browser-inspections/watch must resolve to the watch list, not GET …/{inspection_id} with id='watch'."""
    from app import app

    client = TestClient(app)
    r = client.get("/browser-inspections/watch", params={"limit": 10})
    assert r.status_code == 200, r.text
    assert isinstance(r.json(), list)


def test_get_watch_list_with_project_id_query():
    from app import app

    client = TestClient(app)
    r = client.get("/browser-inspections/watch", params={"limit": 10, "project_id": "p-scope-test"})
    assert r.status_code == 200, r.text
    assert isinstance(r.json(), list)


def test_watch_persisted_roundtrip():
    from services.browser_inspection_watch_service import create_watch, get_watch

    w = create_watch(
        BrowserInspectionWatchCreate(
            url="https://example.org",
            project_id=None,
            interval_minutes=30,
            change_threshold="low",
            enabled=True,
        )
    )
    w2 = get_watch(w.watch_id)
    assert w2.watch_id == w.watch_id
    assert w2.interval_minutes == 30


@patch("services.browser_inspection_watch_service.inspect_url_collect")
@patch("services.browser_inspection_watch_service.persist_light_browser_inspection")
def test_run_now_creates_inspection(mock_persist, mock_collect):
    from services.browser_inspection_watch_service import create_watch, execute_watch_tick
    from models.browser_inspection_models import BrowserInspectionResult

    w = create_watch(BrowserInspectionWatchCreate(url="https://example.com"))
    ins = BrowserInspectionResult(
        inspection_id=str(uuid.uuid4()),
        url="https://example.com",
        final_url="https://example.com/",
        title="OK",
        status_code=200,
        inventory_counts={"links_count": 1},
        inspection_succeeded=True,
    )
    mock_collect.return_value = (ins, {}, {})
    mock_persist.return_value = (ins.inspection_id, True, None)

    out = execute_watch_tick(w.watch_id, force=True)
    assert out.inspection_id == ins.inspection_id
    assert out.diff_id is None
    assert out.alert_triggered is False


@patch("services.browser_inspection_watch_service.schedule_browser_inspection_watch_alert")
@patch("services.browser_inspection_watch_service.compare_browser_inspections")
@patch("services.browser_inspection_watch_service.inspect_url_collect")
@patch("services.browser_inspection_watch_service.persist_light_browser_inspection")
def test_run_now_second_pass_diff_and_alert_medium(mock_persist, mock_collect, mock_compare, mock_alert):
    from services.browser_inspection_watch_service import create_watch, execute_watch_tick
    from models.browser_inspection_models import BrowserInspectionResult
    from models.browser_inspection_diff_models import (
        BrowserInspectionChanges,
        BrowserInspectionDiffResponse,
        CountsDelta,
    )

    w = create_watch(
        BrowserInspectionWatchCreate(
            url="https://example.com",
            change_threshold="medium",
        )
    )
    first = BrowserInspectionResult(
        inspection_id="ins-first",
        url="https://example.com",
        final_url="https://example.com/",
        title="A",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    second = BrowserInspectionResult(
        inspection_id="ins-second",
        url="https://example.com",
        final_url="https://example.com/",
        title="B",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    mock_collect.side_effect = [(first, {}, {}), (second, {}, {})]
    mock_persist.side_effect = [(first.inspection_id, True, None), (second.inspection_id, True, None)]

    execute_watch_tick(w.watch_id, force=True)

    diff_resp = BrowserInspectionDiffResponse(
        base_inspection_id="ins-first",
        target_inspection_id="ins-second",
        change_level="high",
        summary="big",
        changes=BrowserInspectionChanges(counts_delta=CountsDelta()),
        regression_signals=["console_errors_increased"],
        improvement_signals=[],
    )
    mock_compare.return_value = diff_resp

    out = execute_watch_tick(w.watch_id, force=True)
    assert out.diff_id
    assert out.change_level == "high"
    assert out.alert_triggered is True
    mock_alert.assert_called_once()
    kw = mock_alert.call_args.kwargs
    assert "screenshot_b64" not in str(kw)
    assert kw.get("url")


@patch("services.browser_inspection_watch_service.schedule_browser_inspection_watch_alert")
@patch("services.browser_inspection_watch_service.compare_browser_inspections")
@patch("services.browser_inspection_watch_service.inspect_url_collect")
@patch("services.browser_inspection_watch_service.persist_light_browser_inspection")
def test_threshold_high_skips_medium_alert(mock_persist, mock_collect, mock_compare, mock_alert):
    from services.browser_inspection_watch_service import create_watch, execute_watch_tick
    from models.browser_inspection_models import BrowserInspectionResult
    from models.browser_inspection_diff_models import (
        BrowserInspectionChanges,
        BrowserInspectionDiffResponse,
    )

    w = create_watch(
        BrowserInspectionWatchCreate(
            url="https://example.com",
            change_threshold="high",
        )
    )
    ids = ["ins-a", "ins-b"]
    results = [
        BrowserInspectionResult(
            inspection_id=i,
            url="https://example.com",
            final_url="https://example.com/",
            title="t",
            status_code=200,
            inventory_counts={},
            inspection_succeeded=True,
        )
        for i in ids
    ]
    mock_collect.side_effect = [(results[0], {}, {}), (results[1], {}, {})]
    mock_persist.side_effect = [(results[0].inspection_id, True, None), (results[1].inspection_id, True, None)]
    execute_watch_tick(w.watch_id, force=True)

    mock_compare.return_value = BrowserInspectionDiffResponse(
        base_inspection_id=ids[0],
        target_inspection_id=ids[1],
        change_level="medium",
        summary="m",
        changes=BrowserInspectionChanges(),
        regression_signals=[],
        improvement_signals=[],
    )
    out = execute_watch_tick(w.watch_id, force=True)
    assert out.alert_triggered is False
    mock_alert.assert_not_called()


def test_run_now_disabled_returns_400():
    from services.browser_inspection_watch_service import create_watch, execute_watch_tick
    from fastapi import HTTPException

    w = create_watch(BrowserInspectionWatchCreate(url="https://example.com", enabled=False))
    with pytest.raises(HTTPException) as ei:
        execute_watch_tick(w.watch_id, force=False)
    assert ei.value.status_code == 400


def test_tick_due_bounded():
    from services.browser_inspection_watch_service import tick_due_watches

    with patch(
        "services.browser_inspection_watch_service.browser_inspection_watch_repo.list_watches",
        return_value=[],
    ):
        assert tick_due_watches(max_per_tick=5) == 0


def test_scheduler_start_respects_disable_env():
    import services.browser_inspection_watch_scheduler as sch

    sch._started = False
    sch._worker = None
    with patch.dict("os.environ", {"VANYA_BROWSER_WATCH_SCHEDULER": "0"}, clear=False):
        sch.ensure_watch_scheduler_started()
        sch.ensure_watch_scheduler_started()
    assert sch._worker is None


def test_scheduler_legacy_disable_env():
    import services.browser_inspection_watch_scheduler as sch

    sch._started = False
    sch._worker = None
    env = {k: v for k, v in __import__("os").environ.items() if k != "VANYA_BROWSER_WATCH_SCHEDULER"}
    with patch.dict("os.environ", env, clear=True):
        with patch.dict("os.environ", {"VANYA_BIO_WATCH_SCHEDULER": "0"}, clear=False):
            sch.ensure_watch_scheduler_started()
    assert sch._worker is None


@patch("services.browser_inspection_watch_service.watch_alert_try_reserve_slot")
@patch("services.browser_inspection_watch_service.schedule_browser_inspection_watch_alert")
@patch("services.browser_inspection_watch_service.compare_browser_inspections")
@patch("services.browser_inspection_watch_service.inspect_url_collect")
@patch("services.browser_inspection_watch_service.persist_light_browser_inspection")
def test_alert_dedupe_suppresses_second_send(
    mock_persist, mock_collect, mock_compare, mock_alert, mock_reserve
):
    from services.browser_inspection_watch_service import create_watch, execute_watch_tick
    from models.browser_inspection_models import BrowserInspectionResult
    from models.browser_inspection_diff_models import (
        BrowserInspectionChanges,
        BrowserInspectionDiffResponse,
    )
    from services.browser_watch_alert_dedupe import reset_watch_alert_dedupe_for_tests

    reset_watch_alert_dedupe_for_tests()
    mock_reserve.side_effect = [(True, ""), (False, "dedupe_equivalent_within_window")]

    w = create_watch(BrowserInspectionWatchCreate(url="https://example.com"))
    ids = ["ins-a", "ins-b", "ins-c"]
    results = [
        BrowserInspectionResult(
            inspection_id=i,
            url="https://example.com",
            final_url="https://example.com/",
            title="t",
            status_code=200,
            inventory_counts={},
            inspection_succeeded=True,
        )
        for i in ids
    ]
    mock_collect.side_effect = [(r, {}, {}) for r in results]
    mock_persist.side_effect = [(r.inspection_id, True, None) for r in results]

    diff_resp = BrowserInspectionDiffResponse(
        base_inspection_id=ids[0],
        target_inspection_id=ids[1],
        change_level="high",
        summary="boom",
        changes=BrowserInspectionChanges(),
        regression_signals=["console_errors_increased"],
        improvement_signals=[],
        visual_change_detected=True,
        visual_change_level="medium",
        visual_similarity_score=0.71,
    )
    mock_compare.return_value = diff_resp

    execute_watch_tick(w.watch_id, force=True)
    execute_watch_tick(w.watch_id, force=True)
    out = execute_watch_tick(w.watch_id, force=True)

    assert mock_alert.call_count == 1
    assert out.alert_dedupe_suppressed is True
    kw = mock_alert.call_args.kwargs
    assert kw.get("visual_change_level") == "medium"
    assert kw.get("visual_similarity_score") == pytest.approx(0.71)


def test_post_baseline_use_latest_via_client():
    from datetime import datetime, timezone

    from app import app
    from models.browser_inspection_models import BrowserInspectionResult
    from models.test_run import TestRun as StoredTestRun
    from services.browser_inspection_watch_service import execute_watch_tick
    from services.db.test_run_repository import test_run_repo

    client = TestClient(app)
    r = client.post("/browser-inspections/watch", json={"url": "https://example.com"})
    wid = r.json()["watch_id"]
    ins = BrowserInspectionResult(
        inspection_id="pinned-1",
        url="https://example.com",
        final_url="https://example.com/",
        title="t",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    with patch("services.browser_inspection_watch_service.inspect_url_collect", return_value=(ins, {}, {})), patch(
        "services.browser_inspection_watch_service.persist_light_browser_inspection",
        return_value=("pinned-1", True, None),
    ):
        execute_watch_tick(wid, force=True)
    test_run_repo.create_run(
        StoredTestRun(
            run_id="pinned-1",
            test_case_id="_browser_inspection",
            test_name="bio",
            executed_at=datetime.now(timezone.utc),
            environment="default",
            status="pass",
            duration_ms=0,
            evidence_url=None,
            evidence_id="pinned-1",
            logs=[],
            steps_result=[],
            meta={
                "source": "browser_inspection",
                "browser_inspection_summary": {
                    "inspection_id": "pinned-1",
                    "url": "https://example.com",
                    "final_url": "https://example.com/",
                    "title": "t",
                },
            },
        )
    )
    br = client.post(f"/browser-inspections/watch/{wid}/baseline", json={"use_latest": True})
    assert br.status_code == 200
    assert br.json().get("baseline_inspection_id") == "pinned-1"


def test_compare_mode_baseline_calls_diff_with_baseline_base():
    from services.browser_inspection_watch_service import create_watch, execute_watch_tick
    from models.browser_inspection_models import BrowserInspectionResult
    from models.browser_inspection_diff_models import BrowserInspectionChanges, BrowserInspectionDiffResponse

    w = create_watch(
        BrowserInspectionWatchCreate(url="https://example.com", compare_mode="baseline")  # type: ignore[arg-type]
    )
    ids = ["ins-a", "ins-b"]
    results = [
        BrowserInspectionResult(
            inspection_id=i,
            url="https://example.com",
            final_url="https://example.com/",
            title="t",
            status_code=200,
            inventory_counts={},
            inspection_succeeded=True,
        )
        for i in ids
    ]
    with patch("services.browser_inspection_watch_service.inspect_url_collect") as mc, patch(
        "services.browser_inspection_watch_service.persist_light_browser_inspection"
    ) as mp, patch("services.browser_inspection_watch_service.compare_browser_inspections") as mcmp:
        mc.side_effect = [(results[0], {}, {}), (results[1], {}, {})]
        mp.side_effect = [(results[0].inspection_id, True, None), (results[1].inspection_id, True, None)]
        mcmp.return_value = BrowserInspectionDiffResponse(
            base_inspection_id=ids[0],
            target_inspection_id=ids[1],
            change_level="low",
            summary="d",
            changes=BrowserInspectionChanges(),
            regression_signals=[],
            improvement_signals=[],
        )
        execute_watch_tick(w.watch_id, force=True)
        execute_watch_tick(w.watch_id, force=True)
        assert mcmp.call_count == 1
        req = mcmp.call_args[0][0]
        assert req.base_inspection_id == ids[0]


def test_set_baseline_rejects_project_mismatch():
    from datetime import datetime, timezone

    from models.test_run import TestRun as StoredTestRun
    from models.browser_inspection_watch_models import WatchBaselineSetRequest
    from services.browser_inspection_watch_service import create_watch, set_watch_baseline
    from services.db.test_run_repository import test_run_repo

    w = create_watch(
        BrowserInspectionWatchCreate(url="https://example.com", project_id="p-watch")  # type: ignore[arg-type]
    )
    rid = str(uuid.uuid4())
    test_run_repo.create_run(
        StoredTestRun(
            run_id=rid,
            test_case_id="_browser_inspection",
            test_name="bio",
            executed_at=datetime.now(timezone.utc),
            environment="default",
            status="pass",
            duration_ms=0,
            evidence_url=None,
            evidence_id=rid,
            logs=[],
            steps_result=[],
            meta={
                "source": "browser_inspection",
                "project_id": "other-project",
                "browser_inspection_summary": {
                    "inspection_id": rid,
                    "url": "https://example.com",
                    "final_url": "https://example.com/",
                    "title": "t",
                },
            },
        )
    )
    with pytest.raises(HTTPException) as ei:
        set_watch_baseline(w.watch_id, WatchBaselineSetRequest(inspection_id=rid, use_latest=False))
    assert ei.value.status_code == 400


def test_last_status_never_run_then_healthy():
    from services.browser_inspection_watch_service import create_watch, execute_watch_tick, get_watch
    from models.browser_inspection_models import BrowserInspectionResult

    w = create_watch(BrowserInspectionWatchCreate(url="https://example.com"))
    g0 = get_watch(w.watch_id)
    assert g0.last_status == "never_run"
    assert g0.current_status == "never_run"
    ins = BrowserInspectionResult(
        inspection_id=str(uuid.uuid4()),
        url="https://example.com",
        final_url="https://example.com/",
        title="t",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    with patch("services.browser_inspection_watch_service.inspect_url_collect", return_value=(ins, {}, {})), patch(
        "services.browser_inspection_watch_service.persist_light_browser_inspection",
        return_value=(ins.inspection_id, True, None),
    ):
        execute_watch_tick(w.watch_id, force=True)
    g1 = get_watch(w.watch_id)
    assert g1.last_status == "healthy"
    assert g1.current_status == "healthy"
