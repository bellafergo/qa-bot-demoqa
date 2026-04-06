# tests/test_from_run_catalog.py
"""POST /tests/from-run — persist catalog tests from executed runs."""
from __future__ import annotations

import uuid

import pytest


@pytest.fixture()
def client():
    from fastapi.testclient import TestClient
    from app import app

    return TestClient(app)


def test_create_from_run_duplicate_returns_409(client):
    from services.db.init_db import init_catalog_db
    from services.run_store import save_run

    init_catalog_db()
    evid = f"EV-{uuid.uuid4().hex[:10]}"
    save_run(
        {
            "evidence_id": evid,
            "run_id": evid,
            "status": "passed",
            "steps": [
                {"action": "goto", "url": "https://example.com"},
                {"action": "click", "selector": "#btn"},
            ],
        }
    )

    pid = "default"
    body = {"run_id": evid, "name": "From run A", "project_id": pid}
    r1 = client.post("/tests/from-run", json=body)
    assert r1.status_code == 201, r1.text

    r2 = client.post("/tests/from-run", json=body)
    assert r2.status_code == 409
    detail = r2.json().get("detail", "")
    assert "already exists" in detail.lower() or "catalog test" in detail.lower()


def test_create_from_run_persists_metadata(client):
    from services.db.init_db import init_catalog_db
    from services.run_store import save_run

    init_catalog_db()
    evid = f"EV-{uuid.uuid4().hex[:10]}"
    save_run(
        {
            "evidence_id": evid,
            "run_id": evid,
            "status": "passed",
            "steps": [{"action": "goto", "url": "https://example.org"}],
        }
    )

    project_id = "default"
    name = f"Run import {evid[:6]}"
    res = client.post(
        "/tests/from-run",
        json={"run_id": evid, "name": name, "project_id": project_id},
    )
    assert res.status_code == 201, res.text
    data = res.json()
    assert data.get("created_from") == "run"
    assert data.get("source_run_id") == evid
    assert data.get("project_id") == project_id
    assert data.get("name") == name

    listed = client.get(f"/tests?project_id={project_id}").json()
    ids = {x.get("test_case_id") for x in listed}
    assert data.get("test_case_id") in ids
