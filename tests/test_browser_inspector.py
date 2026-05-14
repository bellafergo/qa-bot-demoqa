# tests/test_browser_inspector.py
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.browser_inspection_models import InspectUrlRequest
from services.browser_inspector_service import build_selector_candidates


@pytest.fixture(autouse=True)
def _stub_browser_inspection_persist():
    """Avoid writing SQLite / Supabase during default inspector HTTP tests."""
    with patch(
        "services.browser_inspection_persistence.persist_light_browser_inspection",
        return_value=(None, False, None),
    ):
        yield


def test_build_selector_candidates_prioritizes_testid_and_caps():
    buttons = [{"text": "Go", "testid": "submit-btn", "visible": True}]
    inputs = [{"tag": "input", "name": "email", "type": "email", "visible": True}]
    links = [{"text": "Home", "href": "/home", "visible": True}]
    c = build_selector_candidates(inputs, buttons, links, max_items=50)
    assert len(c) >= 2
    kinds = [x.kind for x in c]
    assert "button" in kinds and "input" in kinds
    assert any(x.priority == "testid" for x in c)


@patch("runners.browser_inspector_runner.run_browser_inspection")
def test_inspect_url_structured_response(mock_run):
    mock_run.return_value = {
        "url": "https://example.com",
        "final_url": "https://example.com/",
        "title": "Ex",
        "status_code": 200,
        "inventory": {
            "inputs": [
                {
                    "tag": "input",
                    "type": "text",
                    "id": "q",
                    "visible": True,
                }
            ],
            "buttons": [{"tag": "button", "text": "Search", "visible": True}],
            "links": [{"text": "About", "href": "/about", "visible": True}],
            "headings": [{"tag": "h1", "text": "Welcome"}],
        },
        "extras": {
            "forms": [{"id": "f1", "name": None, "action": "/search", "method": "get", "field_count": 1}],
            "images_without_alt": [{"src": "https://example.com/x.png"}],
            "performance": {"domContentLoaded_ms": 12},
        },
        "screenshot_b64": None,
        "screenshot_logs": ["Screenshot PNG OK [attempt 1]"],
        "console_errors": [{"text": "boom", "location": None}],
        "network_errors": [{"url": "https://example.com/api", "failure": "net::ERR_FAILED"}],
        "navigation_error": None,
    }
    from services.browser_inspector_service import inspect_url

    r = inspect_url(InspectUrlRequest(url="https://example.com", timeout_ms=5000))
    assert r.inspection_id
    assert "example.com" in r.final_url
    assert r.title == "Ex"
    assert r.status_code == 200
    assert r.headings and r.headings[0]["tag"] == "h1"
    assert r.inputs and r.buttons and r.links
    assert r.forms and r.images_without_alt
    assert r.console_errors and r.network_errors
    assert r.performance.get("domContentLoaded_ms") == 12
    assert isinstance(r.selector_candidates, list)
    assert r.inventory_counts.get("links_count") == 1
    assert r.inspection_succeeded is True


def test_inspect_url_endpoint_localhost_400():
    from app import app

    client = TestClient(app)
    resp = client.post("/inspect-url", json={"url": "http://127.0.0.1:8080/", "timeout_ms": 5000})
    assert resp.status_code == 400


def test_inspect_url_endpoint_file_scheme_400():
    from app import app

    client = TestClient(app)
    resp = client.post("/inspect-url", json={"url": "file:///etc/passwd", "timeout_ms": 5000})
    assert resp.status_code == 400


def test_inspect_url_endpoint_ok_json_keys():
    from app import app

    fake = {
        "url": "https://example.com",
        "final_url": "https://example.com/",
        "title": "OK",
        "status_code": 200,
        "inventory": {"inputs": [], "buttons": [], "links": [], "headings": []},
        "extras": {"forms": [], "images_without_alt": [], "performance": {}},
        "screenshot_b64": None,
        "screenshot_logs": [],
        "console_errors": [],
        "network_errors": [],
        "navigation_error": None,
    }
    with patch("runners.browser_inspector_runner.run_browser_inspection", return_value=fake):
        client = TestClient(app)
        resp = client.post("/inspect-url", json={"url": "https://example.com", "timeout_ms": 5000})
    assert resp.status_code == 200
    data = resp.json()
    for key in (
        "inspection_id",
        "url",
        "final_url",
        "title",
        "status_code",
        "screenshot_url",
        "console_errors",
        "network_errors",
        "headings",
        "links",
        "buttons",
        "inputs",
        "forms",
        "images_without_alt",
        "selector_candidates",
        "performance",
        "warnings",
        "created_at",
        "persisted_run_id",
        "inventory_counts",
        "inspection_succeeded",
        "persisted",
        "persistence_warning",
    ):
        assert key in data


def test_inspect_url_request_timeout_bounds():
    from pydantic import ValidationError

    with pytest.raises(ValidationError):
        InspectUrlRequest(url="https://example.com", timeout_ms=500)
    InspectUrlRequest(url="https://example.com", timeout_ms=3000)


def test_browser_inspector_runner_has_no_direct_interactions():
    from pathlib import Path

    src = Path(__file__).resolve().parents[1] / "runners" / "browser_inspector_runner.py"
    text = src.read_text()
    for needle in (".click(", ".fill(", ".press(", ".check(", ".uncheck(", ".select_option("):
        assert needle not in text, f"unexpected interaction API: {needle}"
