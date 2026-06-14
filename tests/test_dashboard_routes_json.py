# tests/test_dashboard_routes_json.py
"""Dashboard-related routes always return JSON with application/json content-type."""
from __future__ import annotations

import json

import pytest
from fastapi.testclient import TestClient

from app import app


@pytest.fixture
def client() -> TestClient:
    return TestClient(app)


DASHBOARD_PATHS = [
    "/dashboard/summary",
    "/dashboard/executive-risk-brief?project_id=demo",
    "/dashboard/recent-runs?limit=5",
    "/dashboard/recent-jobs?limit=5",
    "/execution/status",
    "/failure-intelligence/summary",
    "/analytics/runs/dashboard",
    "/evidences/summary",
]


@pytest.mark.parametrize("path", DASHBOARD_PATHS)
def test_dashboard_endpoints_return_json(client: TestClient, path: str) -> None:
    resp = client.get(path)
    content_type = resp.headers.get("content-type", "")
    assert "application/json" in content_type, f"{path} content-type={content_type!r}"
    body = resp.json()
    assert body is not None
    if resp.status_code >= 400:
        assert "detail" in body or "message" in body


def test_dashboard_summary_success_shape(client: TestClient) -> None:
    resp = client.get("/dashboard/summary")
    assert resp.status_code == 200
    data = resp.json()
    assert "total_test_cases" in data
    assert "total_runs" in data


def test_json_error_response_shape_on_forced_failure(monkeypatch, client: TestClient) -> None:
    from services import dashboard_service as ds_mod

    def boom(*_a, **_k):
        raise RuntimeError("simulated dashboard failure")

    monkeypatch.setattr(ds_mod.dashboard_service, "get_summary", boom)
    resp = client.get("/dashboard/summary")
    assert resp.status_code == 500
    assert "application/json" in resp.headers.get("content-type", "")
    payload = resp.json()
    assert payload.get("ok") is False
    assert payload.get("message")
    json.dumps(payload)
