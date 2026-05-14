# tests/test_browser_inspection_persistence.py
from __future__ import annotations

import json
import uuid
from datetime import datetime, timezone
from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.app_map_models import AppMapResponse, InspectUrlMapRequest
from models.browser_inspection_models import BrowserInspectionResult, InspectUrlRequest, SelectorCandidate
from models.test_run import TestRun as StoredTestRun
from services.app_map_builder import build_app_map
from services.browser_inspection_persistence import (
    browser_inspection_persist_enabled,
    build_lightweight_run_payload,
    persist_light_browser_inspection,
)


def _fake_runner_payload(*, with_b64: bool = False):
    inv = {
        "inputs": [{"tag": "input", "type": "text", "visible": True}],
        "buttons": [],
        "links": [{"text": "A", "href": "/a", "visible": True}],
        "headings": [{"tag": "h1", "text": "Hi"}],
    }
    out = {
        "url": "https://example.com",
        "final_url": "https://example.com/",
        "title": "T",
        "status_code": 200,
        "inventory": inv,
        "extras": {"forms": [], "images_without_alt": [], "performance": {"domContentLoaded_ms": 42}},
        "screenshot_b64": "AAA" if with_b64 else None,
        "screenshot_logs": [],
        "console_errors": [{"text": "e" * 500, "location": "x"}],
        "network_errors": [{"url": "https://z", "failure": "f"}],
        "navigation_error": None,
    }
    return out


def test_build_payload_omits_screenshot_b64_even_if_inspection_had_upload():
    ins = BrowserInspectionResult(
        inspection_id="id-1",
        url="https://example.com",
        final_url="https://example.com/",
        title="T",
        status_code=200,
        screenshot_url="https://cdn.example/x.png",
        console_errors=[],
        network_errors=[],
        inventory_counts={"selector_candidates_count": 1},
        inspection_succeeded=True,
    )
    p = build_lightweight_run_payload(ins, inv={}, extras={}, project_id="p1", app_map=None)
    assert "screenshot_b64" not in p
    assert (p.get("meta") or {}).get("source") == "browser_inspection"
    bis = (p.get("meta") or {}).get("browser_inspection_summary") or {}
    assert bis.get("screenshot_url") == "https://cdn.example/x.png"


def test_build_payload_includes_app_map_summary():
    ins = BrowserInspectionResult(
        inspection_id="id-2",
        url="https://example.com",
        final_url="https://example.com/",
        title="T",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    m = AppMapResponse(
        inspection_id=ins.inspection_id,
        url=ins.url,
        final_url=ins.final_url,
        page_type=["login_page"],
        confidence="high",
        detected_patterns=["auth_form"],
        main_navigation=[{"text": "Home", "href": "/"}],
        primary_actions=[{"text": "Go", "kind": "submit"}],
        forms=[],
        tables=[],
        search_elements=[],
        risk_notes=["r1"],
        suggested_test_flows=["flow1"],
        selector_candidates=[SelectorCandidate(kind="a", selector="#x", priority="id")],
    )
    p = build_lightweight_run_payload(ins, inv={}, extras={}, project_id=None, app_map=m)
    ams = (p.get("meta") or {}).get("app_map_summary") or {}
    assert ams.get("page_type") == ["login_page"]
    assert ams.get("main_navigation_count") == 1
    assert "selector_candidates" not in ams


def test_persist_light_returns_warning_when_save_returns_none(monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    ins = BrowserInspectionResult(
        inspection_id="id-3",
        url="https://example.com",
        final_url="https://example.com/",
        title="",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    with patch("services.run_access.persist_run_payload", return_value=None):
        rid, ok, w = persist_light_browser_inspection(ins, inv={}, extras={}, project_id=None, app_map=None)
    assert rid is None and ok is False
    assert w and "persistence" in w.lower()


@pytest.mark.parametrize("with_b64", [False, True])
def test_inspect_url_endpoint_persists_light_metadata(with_b64, monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    captured: list = []

    def _grab(payload):
        captured.append(payload)
        return payload["run_id"]

    fake = _fake_runner_payload(with_b64=with_b64)
    from app import app

    with patch("services.run_access.persist_run_payload", side_effect=_grab):
        with patch("runners.browser_inspector_runner.run_browser_inspection", return_value=fake):
            client = TestClient(app)
            r = client.post("/inspect-url", json={"url": "https://example.com", "timeout_ms": 5000, "project_id": "px"})
    assert r.status_code == 200
    data = r.json()
    assert data["persisted"] is True
    assert data["persisted_run_id"] == data["inspection_id"]
    assert data.get("persistence_warning") in (None, "")
    assert len(captured) == 1
    p0 = captured[0]
    assert "screenshot_b64" not in p0
    assert "html" not in json.dumps(p0).lower() or "<html" not in json.dumps(p0).lower()
    meta = p0.get("meta") or {}
    assert meta.get("source") == "browser_inspection"
    bis = meta.get("browser_inspection_summary") or {}
    assert bis.get("counts", {}).get("links_count") == 1
    assert meta.get("app_map_summary") is None


def test_inspect_url_map_persists_app_map_summary(monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    captured: list = []

    def _grab(payload):
        captured.append(payload)
        return payload["run_id"]

    ins = BrowserInspectionResult(
        inspection_id=str(uuid.uuid4()),
        url="https://example.com",
        final_url="https://example.com/",
        title="Login",
        status_code=200,
        console_errors=[],
        network_errors=[],
        headings=[],
        links=[],
        buttons=[],
        inputs=[{"type": "password", "name": "p", "visible": True}],
        forms=[],
        images_without_alt=[],
        selector_candidates=[],
        performance={"domContentLoaded_ms": 10},
        warnings=[],
        inventory_counts={"inputs_count": 1, "selector_candidates_count": 0},
        inspection_succeeded=True,
    )

    def fake_collect(req: InspectUrlRequest):
        return ins, {"inputs": [{}], "links": [], "buttons": [], "headings": []}, {"forms": [], "images_without_alt": []}

    from app import app

    with patch("services.run_access.persist_run_payload", side_effect=_grab):
        with patch("services.app_map_builder.inspect_url_collect", side_effect=fake_collect):
            client = TestClient(app)
            resp = client.post("/inspect-url/map", json={"url": "https://example.com", "timeout_ms": 5000})
    assert resp.status_code == 200
    body = resp.json()
    assert body.get("persisted") is True
    assert body.get("persisted_run_id") == body.get("inspection_id")
    assert len(captured) == 1
    ams = (captured[0].get("meta") or {}).get("app_map_summary")
    assert isinstance(ams, dict)
    assert "login_page" in (ams.get("page_type") or [])


def test_inspect_url_survives_persist_failure_with_warning(monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    fake = _fake_runner_payload()
    from app import app

    with patch("services.run_access.persist_run_payload", return_value=None):
        with patch("runners.browser_inspector_runner.run_browser_inspection", return_value=fake):
            client = TestClient(app)
            r = client.post("/inspect-url", json={"url": "https://example.com", "timeout_ms": 5000})
    assert r.status_code == 200
    data = r.json()
    assert data["persisted"] is False
    assert data.get("persistence_warning")


def test_list_and_get_browser_inspections():
    from app import app

    rid = str(uuid.uuid4())
    meta = {
        "source": "browser_inspection",
        "test_case_id": "_browser_inspection",
        "execution_mode": "cloud",
        "project_id": "proj-z",
        "browser_inspection_summary": {
            "inspection_id": rid,
            "url": "https://example.com",
            "final_url": "https://example.com/",
            "title": "Hi",
            "status_code": 200,
            "screenshot_url": None,
            "console_errors": [],
            "network_errors": [],
            "counts": {"links_count": 2},
            "warnings": [],
            "inspection_succeeded": True,
        },
        "app_map_summary": {"page_type": ["unknown"], "confidence": "low"},
    }
    run = StoredTestRun(
        run_id=rid,
        test_case_id="_browser_inspection",
        test_name="[Browser inspection] Hi",
        executed_at=datetime.now(timezone.utc),
        environment="default",
        status="pass",
        duration_ms=0,
        evidence_url=None,
        evidence_id=rid,
        logs=[],
        steps_result=[],
        meta=meta,
    )
    from services.db.test_run_repository import test_run_repo

    test_run_repo.create_run(run)
    try:
        client = TestClient(app)
        lst = client.get("/browser-inspections", params={"project_id": "proj-z", "limit": 10})
        assert lst.status_code == 200
        items = lst.json().get("items") or []
        assert any(x.get("inspection_id") == rid for x in items)
        one = client.get(f"/browser-inspections/{rid}")
        assert one.status_code == 200
        detail = one.json()
        assert detail["inspection_id"] == rid
        assert detail["browser_inspection_summary"]["title"] == "Hi"
        assert detail["app_map_summary"]["page_type"] == ["unknown"]
    finally:
        # soft-delete not implemented for tests — merge overwrite with deleted marker unsupported; leave row (uuid rare collision)
        pass


def test_persist_disabled_skips_save(monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "0")
    assert browser_inspection_persist_enabled() is False
    ins = BrowserInspectionResult(
        inspection_id="x",
        url="https://example.com",
        final_url="https://example.com/",
        title="",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    with patch("services.run_access.persist_run_payload") as mp:
        rid, ok, w = persist_light_browser_inspection(ins, inv={}, extras={}, project_id=None, app_map=None)
    mp.assert_not_called()
    assert ok is False and rid is None and w is None


def test_build_app_map_persist_selector_count_in_summary(monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    captured: list = []

    def _grab(payload):
        captured.append(payload)
        return payload["run_id"]

    ins = BrowserInspectionResult(
        inspection_id=str(uuid.uuid4()),
        url="https://example.com",
        final_url="https://example.com/",
        title="",
        status_code=200,
        selector_candidates=[
            SelectorCandidate(kind="input", selector="#a", priority="id"),
            SelectorCandidate(kind="button", selector="#b", priority="id"),
        ],
        inventory_counts={"selector_candidates_count": 2},
        inspection_succeeded=True,
    )

    def fake_collect(req: InspectUrlRequest):
        return ins, {"inputs": [{}, {}], "links": [], "buttons": [], "headings": []}, {"forms": [], "images_without_alt": []}

    with patch("services.run_access.persist_run_payload", side_effect=_grab):
        with patch("services.app_map_builder.inspect_url_collect", side_effect=fake_collect):
            build_app_map(InspectUrlMapRequest(url="https://example.com"))
    assert captured
    cnt = (captured[0].get("meta") or {}).get("browser_inspection_summary", {}).get("counts", {})
    assert cnt.get("selector_candidates_count") == 2
