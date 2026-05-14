# tests/test_app_map_builder.py
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient
from pydantic import ValidationError

from models.app_map_models import InspectUrlMapRequest
from models.browser_inspection_models import BrowserInspectionResult, InspectUrlRequest, SelectorCandidate
from services.dom_semantic_classifier import classify_semantic_map
from services.app_map_builder import build_app_map


@pytest.fixture(autouse=True)
def _stub_app_map_persist():
    with patch(
        "services.app_map_builder.persist_light_browser_inspection",
        return_value=(None, False, None),
    ):
        yield


def _minimal_inspection(**kwargs) -> BrowserInspectionResult:
    base = dict(
        inspection_id="test-id",
        url="https://example.com",
        final_url="https://example.com/",
        title="",
        status_code=200,
        screenshot_url=None,
        console_errors=[],
        network_errors=[],
        headings=[],
        links=[],
        buttons=[],
        inputs=[],
        forms=[],
        images_without_alt=[],
        selector_candidates=[],
        performance={},
        warnings=[],
        inventory_counts={},
        inspection_succeeded=True,
        persisted=False,
    )
    base.update(kwargs)
    return BrowserInspectionResult(**base)


def test_login_page_detection():
    ins = _minimal_inspection(
        title="Sign in",
        buttons=[{"text": "Sign In", "visible": True}],
        inputs=[
            {"type": "email", "name": "email", "visible": True},
            {"type": "password", "name": "password", "visible": True},
        ],
        links=[{"text": "Forgot password", "href": "/reset", "visible": True}],
    )
    extras = {"layout_hints": {"password_inputs": 1}}
    out = classify_semantic_map(ins, {}, extras)
    assert "login_page" in out["page_type"]


def test_dashboard_navigation_detection():
    links = [{"text": f"Nav {i}", "href": f"/p{i}", "visible": True} for i in range(16)]
    ins = _minimal_inspection(links=links, forms=[{"id": None, "field_count": 2, "method": "get"}])
    extras = {"layout_hints": {"nav_linkish": 10, "table_count": 0}}
    out = classify_semantic_map(ins, {}, extras)
    assert "dashboard" in out["page_type"] or "landing_page" in out["page_type"]
    assert out["main_navigation"]


def test_form_page_detection():
    forms = [{"id": "big", "field_count": 8, "method": "post", "name": "register"}]
    ins = _minimal_inspection(
        forms=forms,
        inputs=[{"type": "text", "name": f"f{i}", "visible": True} for i in range(8)],
    )
    out = classify_semantic_map(ins, {}, extras={})
    assert "form_page" in out["page_type"]


def test_search_interface_detection():
    ins = _minimal_inspection(
        inputs=[{"type": "search", "name": "q", "placeholder": "Search products", "visible": True}],
        links=[{"text": "Home", "href": "/", "visible": True}],
    )
    extras = {"layout_hints": {"search_inputs": 1}}
    out = classify_semantic_map(ins, {}, extras)
    assert "search_interface" in out["page_type"]
    assert out["search_elements"]


def test_unknown_fallback():
    ins = _minimal_inspection()
    out = classify_semantic_map(ins, {}, {})
    assert out["page_type"] == ["unknown"]


def test_inspect_url_map_endpoint_mocked():
    from app import app

    fake_inspection = _minimal_inspection(
        selector_candidates=[
            SelectorCandidate(kind="button", selector="#go", priority="id", label="Go"),
        ],
        links=[{"text": "A", "href": "/a", "visible": True}],
        buttons=[{"text": "Save", "visible": True}],
    )

    def fake_collect(req: InspectUrlRequest):
        inv = {"selects": []}
        extras = {
            "layout_hints": {
                "table_count": 0,
                "nav_linkish": 0,
                "search_inputs": 0,
                "dialog_like": 0,
                "required_fields": 0,
                "password_inputs": 0,
                "filter_like_inputs": 0,
            }
        }
        return fake_inspection, inv, extras

    with patch("services.app_map_builder.inspect_url_collect", side_effect=fake_collect):
        client = TestClient(app)
        r = client.post(
            "/inspect-url/map",
            json={"url": "https://example.com", "timeout_ms": 5000, "execution_mode": "cloud"},
        )
    assert r.status_code == 200
    data = r.json()
    assert data["inspection_id"] == "test-id"
    assert len(data["selector_candidates"]) == 1
    assert data["selector_candidates"][0]["selector"] == "#go"
    assert "persist_hints" in data
    assert data["persist_hints"]["source"] == "browser_inspection"


def test_execution_mode_cloud_only_valid():
    InspectUrlMapRequest(url="https://example.com", execution_mode="cloud")


def test_execution_mode_local_agent_rejected():
    with pytest.raises(ValidationError):
        InspectUrlMapRequest(url="https://example.com", execution_mode="local_agent")  # type: ignore[arg-type]


def test_localhost_still_blocked_on_map():
    from app import app

    client = TestClient(app)
    r = client.post("/inspect-url/map", json={"url": "http://localhost/", "timeout_ms": 5000})
    assert r.status_code == 400


def test_selector_candidates_propagate():
    cands = [
        SelectorCandidate(kind="input", selector="#email", priority="id", label="Email"),
    ]
    ins = _minimal_inspection(selector_candidates=cands, links=[{"text": "x", "href": "/y", "visible": True}])
    with patch("services.app_map_builder.inspect_url_collect", return_value=(ins, {}, {})):
        m = build_app_map(InspectUrlMapRequest(url="https://example.com"))
    assert len(m.selector_candidates) == 1
    assert m.selector_candidates[0].selector == "#email"


def test_execution_mode_hybrid_rejected():
    with pytest.raises(ValidationError):
        InspectUrlMapRequest(url="https://example.com", execution_mode="hybrid")  # type: ignore[arg-type]
