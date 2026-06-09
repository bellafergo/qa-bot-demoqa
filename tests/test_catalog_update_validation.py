# tests/test_catalog_update_validation.py
"""PUT /tests/{id} — name validation on catalog updates."""
from __future__ import annotations

import pytest


@pytest.fixture()
def client():
    from fastapi.testclient import TestClient
    from app import app

    return TestClient(app)


def _create_test(client, tc_id: str) -> None:
    from services.db.init_db import init_catalog_db

    init_catalog_db()
    res = client.post(
        "/tests",
        json={
            "test_case_id": tc_id,
            "name": "Original name",
            "module": "demo",
            "type": "smoke",
            "priority": "medium",
            "steps": [{"action": "goto", "value": "https://example.com"}],
            "assertions": [],
        },
    )
    assert res.status_code == 201, res.text


def test_update_test_empty_name_rejected(client):
    tc_id = "TC-NAME-EMPTY-001"
    _create_test(client, tc_id)

    res = client.put(f"/tests/{tc_id}", json={"name": ""})
    assert res.status_code == 400
    assert "name" in res.json().get("detail", "").lower()

    got = client.get(f"/tests/{tc_id}").json()
    assert got["name"] == "Original name"


def test_update_test_whitespace_name_rejected(client):
    tc_id = "TC-NAME-WS-001"
    _create_test(client, tc_id)

    res = client.put(f"/tests/{tc_id}", json={"name": "   "})
    assert res.status_code == 400
    assert "name" in res.json().get("detail", "").lower()

    got = client.get(f"/tests/{tc_id}").json()
    assert got["name"] == "Original name"


def test_update_test_partial_without_name_allowed(client):
    tc_id = "TC-NAME-PARTIAL-001"
    _create_test(client, tc_id)

    res = client.put(f"/tests/{tc_id}", json={"priority": "high"})
    assert res.status_code == 200, res.text
    assert res.json()["priority"] == "high"
    assert res.json()["name"] == "Original name"


def test_update_test_name_trimmed(client):
    tc_id = "TC-NAME-TRIM-001"
    _create_test(client, tc_id)

    res = client.put(f"/tests/{tc_id}", json={"name": "  Trimmed name  "})
    assert res.status_code == 200, res.text
    assert res.json()["name"] == "Trimmed name"
