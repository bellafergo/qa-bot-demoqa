# tests/test_local_agent.py
from __future__ import annotations

import uuid

import pytest
from fastapi.testclient import TestClient

from services.db.local_agent_repository import local_agent_repo


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_register_agent_with_project_id(client: TestClient):
    r = client.post(
        "/local-agents/register",
        json={
            "project_id": "proj-alpha",
            "name": "edge-node-1",
            "capabilities": ["browser_inspection", "localhost_access"],
            "version": "0.0.1",
            "metadata": {"region": "mx"},
        },
    )
    assert r.status_code == 200, r.text
    data = r.json()
    assert data["project_id"] == "proj-alpha"
    assert data["name"] == "edge-node-1"
    assert "browser_inspection" in data["capabilities"]
    assert data["agent_token"].startswith("vla_")
    assert data["agent_id"]


def test_token_not_returned_after_registration(client: TestClient):
    r = client.post(
        "/local-agents/register",
        json={"project_id": "p1", "name": "n1"},
    )
    assert r.status_code == 200
    assert r.json()["agent_token"].startswith("vla_")
    agent_id = r.json()["agent_id"]

    g = client.get(f"/local-agents/{agent_id}")
    assert g.status_code == 200
    body = g.json()
    assert "agent_token" not in body
    assert "token_fingerprint" in body
    assert len(body["token_fingerprint"]) <= 16


def test_list_agents(client: TestClient):
    client.post("/local-agents/register", json={"project_id": "pl", "name": "L1"})
    r = client.get("/local-agents", params={"project_id": "pl"})
    assert r.status_code == 200
    rows = r.json()
    assert isinstance(rows, list)
    assert any(x.get("project_id") == "pl" for x in rows)


def test_heartbeat_updates_last_seen_and_status(client: TestClient):
    reg = client.post(
        "/local-agents/register",
        json={"project_id": "ph", "name": "hb1"},
    )
    assert reg.status_code == 200
    token = reg.json()["agent_token"]
    aid = reg.json()["agent_id"]

    h = client.post(
        f"/agent-api/{aid}/heartbeat",
        json={"agent_version": "0.1.0"},
        headers={"Authorization": f"Bearer {token}"},
    )
    assert h.status_code == 200, h.text
    d = h.json()
    assert d.get("status") == "online"
    assert d.get("last_seen_at")


def test_disable_agent(client: TestClient):
    reg = client.post("/local-agents/register", json={"project_id": "pd", "name": "d1"})
    token = reg.json()["agent_token"]
    aid = reg.json()["agent_id"]

    x = client.post(f"/local-agents/{aid}/disable")
    assert x.status_code == 200
    assert x.json().get("enabled") is False
    assert x.json().get("status") == "disabled"

    h = client.post(
        f"/agent-api/{aid}/heartbeat",
        json={},
        headers={"Authorization": f"Bearer {token}"},
    )
    assert h.status_code == 403


def test_poll_empty_jobs(client: TestClient):
    reg = client.post("/local-agents/register", json={"project_id": "pq", "name": "q1"})
    token = reg.json()["agent_token"]
    aid = reg.json()["agent_id"]

    p = client.post(
        f"/agent-api/{aid}/poll",
        json={"limit": 5},
        headers={"Authorization": f"Bearer {token}"},
    )
    assert p.status_code == 200
    assert p.json() == {"jobs": []}


def test_poll_disabled_agent_fails(client: TestClient):
    reg = client.post("/local-agents/register", json={"project_id": "px", "name": "x1"})
    token = reg.json()["agent_token"]
    aid = reg.json()["agent_id"]
    client.post(f"/local-agents/{aid}/disable")

    p = client.post(
        f"/agent-api/{aid}/poll",
        json={},
        headers={"Authorization": f"Bearer {token}"},
    )
    assert p.status_code == 403


def test_result_unknown_job(client: TestClient):
    reg = client.post("/local-agents/register", json={"project_id": "pr", "name": "r1"})
    token = reg.json()["agent_token"]
    aid = reg.json()["agent_id"]

    r = client.post(
        f"/agent-api/{aid}/jobs/{uuid.uuid4()}/result",
        json={"status": "succeeded", "result_ref": "ref-1"},
        headers={"Authorization": f"Bearer {token}"},
    )
    assert r.status_code == 404


def test_cloud_inspect_url_still_blocks_localhost(client: TestClient):
    resp = client.post("/inspect-url", json={"url": "http://localhost:3000/", "timeout_ms": 5000})
    assert resp.status_code == 400


def test_local_agent_watch_cloud_execution_disabled(client: TestClient):
    w = client.post(
        "/browser-inspections/watch",
        json={"url": "https://example.com", "project_id": "pw2", "execution_mode": "cloud"},
    )
    assert w.status_code == 200
    wid = w.json()["watch_id"]
    p = client.patch(
        f"/browser-inspections/watch/{wid}",
        json={"execution_mode": "local_agent"},
    )
    assert p.status_code == 200, p.text
    assert p.json().get("execution_mode") == "local_agent"

    run = client.post(f"/browser-inspections/watch/{wid}/run-now")
    assert run.status_code == 400
    detail = run.json().get("detail")
    assert "local_agent" in str(detail).lower()


def test_job_payload_too_large_rejected():
    with pytest.raises(ValueError, match="payload"):
        local_agent_repo.insert_job(
            job_id=str(uuid.uuid4()),
            project_id="p",
            agent_id=None,
            job_type="browser_inspection",
            target_url="https://example.com",
            payload_json="x" * (5000),
        )


def test_register_requires_admin_key_when_secret_set(client: TestClient, monkeypatch):
    monkeypatch.setenv("LOCAL_AGENT_REGISTER_SECRET", "super-secret-key")
    r = client.post("/local-agents/register", json={"project_id": "ps", "name": "s1"})
    assert r.status_code == 403

    ok = client.post(
        "/local-agents/register",
        json={"project_id": "ps", "name": "s1"},
        headers={"X-Vanya-Local-Agent-Register-Key": "super-secret-key"},
    )
    assert ok.status_code == 200, ok.text
    assert ok.json()["agent_token"].startswith("vla_")


def test_submit_result_wrong_project_forbidden(client: TestClient):
    reg = client.post("/local-agents/register", json={"project_id": "agent-proj", "name": "a1"})
    token = reg.json()["agent_token"]
    aid = reg.json()["agent_id"]
    jid = str(uuid.uuid4())
    local_agent_repo.insert_job(
        job_id=jid,
        project_id="other-proj",
        agent_id=None,
        job_type="browser_inspection",
        target_url="https://example.com",
        payload_json="{}",
    )
    r = client.post(
        f"/agent-api/{aid}/jobs/{jid}/result",
        json={"status": "succeeded"},
        headers={"Authorization": f"Bearer {token}"},
    )
    assert r.status_code == 403
