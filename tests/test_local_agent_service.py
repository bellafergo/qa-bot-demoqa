# tests/test_local_agent_service.py
"""INT-03A — Local Agents Foundation (read-only registration and heartbeat)."""
from __future__ import annotations

from datetime import datetime, timedelta, timezone
from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.local_agent_models import AgentInventory, LocalAgentFoundationHeartbeat
from services.db.local_agent_repository import local_agent_repo
from services.local_agent_service import (
    build_deterministic_agent_id,
    build_deterministic_agent_token,
    build_local_agent_report,
    hash_agent_token,
    mock_foundation_heartbeat,
    process_foundation_heartbeat,
    register_foundation_agent,
    resolve_foundation_status,
    row_to_foundation_view,
)


class _Req:
    headers: dict = {}


def _register_body(**overrides):
    base = {
        "project_id": "store-001",
        "name": "Store-001",
        "environment": "production",
        "version": "1.0.0",
        "capabilities": ["database_validation", "contract_validation"],
        "inventory": {
            "databases_detected": ["Oracle", "SQL Server"],
            "repositories_detected": [],
            "services_detected": [],
        },
    }
    base.update(overrides)
    return base


def test_agent_registration():
    from models.local_agent_models import LocalAgentFoundationRegistrationRequest

    resp = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(**_register_body()),
        _Req(),
    )
    assert resp.agent_id == "agent:store_001:store_001"
    assert resp.agent_token.startswith("agent_agent_store_001_store_001_")
    assert "database_validation" in resp.capabilities
    row = local_agent_repo.get_agent(resp.agent_id)
    assert row is not None
    assert row["metadata"]["environment"] == "production"


def test_duplicate_registration_returns_existing():
    from models.local_agent_models import LocalAgentFoundationRegistrationRequest

    body = LocalAgentFoundationRegistrationRequest(**_register_body(project_id="dup-proj", name="Dup-Agent"))
    first = register_foundation_agent(body, _Req())
    assert first.already_exists is False
    second = register_foundation_agent(body, _Req())
    assert second.already_exists is True
    assert second.agent_id == first.agent_id


def test_heartbeat_update():
    from models.local_agent_models import LocalAgentFoundationRegistrationRequest, LocalAgentMockHeartbeatRequest

    reg = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(**_register_body(project_id="hb-proj", name="HB-Agent")),
        _Req(),
    )
    view = mock_foundation_heartbeat(
        LocalAgentMockHeartbeatRequest(
            agent_id=reg.agent_id,
            version="1.0.1",
            capabilities=["database_validation", "contract_validation", "browser_probe"],
            inventory=AgentInventory(databases_detected=["Oracle"]),
        ),
        _Req(),
    )
    assert view.status == "ONLINE"
    assert view.last_heartbeat_at
    assert view.version == "1.0.1"
    assert "browser_probe" in view.capabilities


def test_offline_detection():
    agent_id = build_deterministic_agent_id("offline-proj", "Offline-Agent")
    now = datetime.now(timezone.utc)
    stale = (now - timedelta(minutes=30)).isoformat()
    row = {
        "agent_id": agent_id,
        "enabled": True,
        "status": "online",
        "last_seen_at": stale,
        "created_at": stale,
        "name": "Offline-Agent",
        "capabilities": ["database_validation"],
        "metadata": {"environment": "production"},
        "version": "1.0.0",
    }
    assert resolve_foundation_status(row, now=now) == "OFFLINE"
    assert row_to_foundation_view(row, now=now).status == "OFFLINE"


def test_capability_registration():
    from models.local_agent_models import LocalAgentFoundationRegistrationRequest

    resp = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(
            **_register_body(
                project_id="cap-proj",
                name="Cap-Agent",
                capabilities=[
                    "database_validation",
                    "contract_validation",
                    "filesystem_inventory",
                ],
            )
        ),
        _Req(),
    )
    assert set(resp.capabilities) == {
        "database_validation",
        "contract_validation",
        "filesystem_inventory",
    }


def test_inventory_registration():
    from models.local_agent_models import LocalAgentFoundationRegistrationRequest

    resp = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(**_register_body(project_id="inv-proj", name="Inv-Agent")),
        _Req(),
    )
    report = build_local_agent_report(project_id="inv-proj")
    inv = next(i for i in report.inventory if i.agent_id == resp.agent_id)
    assert "Oracle" in inv.databases_detected
    assert "SQL Server" in inv.databases_detected


def test_token_hashing():
    token = build_deterministic_agent_token("agent:store_001:store_001")
    th, fp = hash_agent_token(token)
    assert len(th) == 64
    assert len(fp) == 12
    assert token not in th
    assert token not in fp


def test_deterministic_ids():
    agent_id = build_deterministic_agent_id("Store-001", "Store-001")
    token_a = build_deterministic_agent_token(agent_id)
    token_b = build_deterministic_agent_token(agent_id)
    assert agent_id == "agent:store_001:store_001"
    assert token_a == token_b
    assert token_a.startswith("agent_agent_store_001_store_001_")


def test_foundation_heartbeat_auth_path():
    from models.local_agent_models import LocalAgentFoundationRegistrationRequest

    reg = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(**_register_body(project_id="auth-proj", name="Auth-Agent")),
        _Req(),
    )
    view = process_foundation_heartbeat(
        LocalAgentFoundationHeartbeat(
            agent_id=reg.agent_id,
            version="1.0.0",
            capabilities=["database_validation"],
        ),
        f"Bearer {reg.agent_token}",
    )
    assert view.status == "ONLINE"


def test_no_remote_execution_on_report():
    with patch("subprocess.run") as mock_run, patch("httpx.Client") as mock_client:
        report = build_local_agent_report(project_id="store-001")
        mock_run.assert_not_called()
        mock_client.assert_not_called()
    assert isinstance(report.summary, str)


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_foundation_register_endpoint(client: TestClient):
    r = client.post(
        "/local-agents/foundation/register",
        json=_register_body(project_id="api-proj", name="Api-Agent"),
    )
    assert r.status_code == 200, r.text
    data = r.json()
    assert data["agent_id"].startswith("agent:")
    assert data["agent_token"].startswith("agent_")
    assert "agent_token" in data
    assert data.get("already_exists") is False


def test_foundation_register_idempotent_endpoint(client: TestClient):
    payload = _register_body(project_id="api-idem", name="Idem-Agent")
    first = client.post("/local-agents/foundation/register", json=payload)
    second = client.post("/local-agents/foundation/register", json=payload)
    assert first.status_code == 200, first.text
    assert second.status_code == 200, second.text
    assert second.json().get("already_exists") is True
    assert second.json()["agent_id"] == first.json()["agent_id"]


def test_foundation_report_endpoint(client: TestClient):
    client.post(
        "/local-agents/foundation/register",
        json=_register_body(project_id="report-proj", name="Report-Agent"),
    )
    r = client.get("/local-agents/foundation/report", params={"project_id": "report-proj"})
    assert r.status_code == 200, r.text
    body = r.json()
    assert "agents" in body
    assert "inventory" in body
    assert body["agents"]
