# tests/test_database_connector_service.py
"""INT-03B — Secure database connectors (agent-mediated, read-only)."""
from __future__ import annotations

from datetime import datetime, timedelta, timezone
from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.database_connector_models import DatabaseConnectionRegistrationRequest
from models.incident_models import DatabaseValidationCheck
from fastapi import HTTPException

from models.local_agent_models import LocalAgentFoundationRegistrationRequest, LocalAgentMockHeartbeatRequest
from services.database_connector_service import (
    build_connection_id,
    build_execution_id,
    execute_validation_check,
    register_connection,
    simulate_approval,
    simulate_agent_readonly_execution,
)
from services.local_agent_service import mock_foundation_heartbeat, register_foundation_agent


class _Req:
    headers: dict = {}


def _register_agent(project_id: str, name: str):
    reg = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(
            project_id=project_id,
            name=name,
            environment="production",
            version="1.0.0",
            capabilities=["database_validation", "contract_validation"],
        ),
        _Req(),
    )
    mock_foundation_heartbeat(
        LocalAgentMockHeartbeatRequest(
            agent_id=reg.agent_id,
            version="1.0.0",
            capabilities=["database_validation", "contract_validation"],
            timestamp=datetime.now(timezone.utc).isoformat(),
        ),
        _Req(),
    )
    return reg


def _register_connection(agent_id: str, name: str = "Payments DB"):
    return register_connection(
        DatabaseConnectionRegistrationRequest(
            agent_id=agent_id,
            name=name,
            database_type="postgresql",
            host_label="payments-db.internal",
            database_name="payments",
        ),
        _Req(),
    )


def _sample_check(**overrides):
    base = {
        "check_id": "dbcheck:payments:validate_transaction_status",
        "name": "Validate payment transaction status",
        "description": "Confirm captured payments exist.",
        "query": "SELECT payment_status FROM payments WHERE order_id = :order_id",
        "database_type": "postgresql",
        "expected_result_type": "status_equals",
        "expected_value": "captured",
        "enabled": True,
        "requires_user_approval": True,
    }
    base.update(overrides)
    return DatabaseValidationCheck(**base)


def test_connector_registration():
    reg = _register_agent("conn-reg-proj", "Conn-Agent")
    conn = _register_connection(reg.agent_id)
    assert conn.connection_id == build_connection_id(reg.agent_id, "Payments DB")
    assert conn.database_type == "postgresql"
    assert conn.status in ("CONNECTED", "UNKNOWN")


def test_connector_status_offline_when_agent_stale():
    reg = _register_agent("conn-status-proj", "Status-Agent")
    conn = register_connection(
        DatabaseConnectionRegistrationRequest(
            agent_id=reg.agent_id,
            name="Stale DB",
            database_type="mysql",
            host_label="mysql.internal",
            database_name="orders",
        ),
        _Req(),
    )
    assert conn.status == "CONNECTED"


def test_safe_query_execution():
    reg = _register_agent("conn-exec-proj", "Exec-Agent")
    conn = _register_connection(reg.agent_id)
    check = _sample_check()
    simulate_approval(check.check_id, "APPROVED")
    result = execute_validation_check(check=check, connection_id=conn.connection_id)
    assert result.status == "SUCCESS"
    assert result.row_count > 0
    assert "password" not in result.summary.lower()


def test_unsafe_query_blocked():
    reg = _register_agent("conn-unsafe-proj", "Unsafe-Agent")
    conn = _register_connection(reg.agent_id)
    check = _sample_check(query="DELETE FROM payments WHERE order_id = :order_id")
    simulate_approval(check.check_id, "APPROVED")
    result = execute_validation_check(check=check, connection_id=conn.connection_id)
    assert result.status == "BLOCKED"
    assert result.row_count == 0


def test_missing_approval_blocked():
    reg = _register_agent("conn-approval-proj", "Approval-Agent")
    conn = _register_connection(reg.agent_id)
    check = _sample_check(check_id="dbcheck:payments:no_approval")
    result = execute_validation_check(check=check, connection_id=conn.connection_id)
    assert result.status == "REJECTED"


def test_missing_capability_blocked():
    reg = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(
            project_id="conn-cap-proj",
            name="No-Cap-Agent",
            capabilities=["contract_validation"],
        ),
        _Req(),
    )
    with pytest.raises(HTTPException) as exc:
        _register_connection(reg.agent_id)
    assert exc.value.status_code == 400


def test_result_summarization():
    row_count, summary = simulate_agent_readonly_execution(
        "SELECT payment_status FROM payments WHERE order_id = :order_id",
        "postgresql",
        "Validate payment transaction status",
    )
    assert row_count >= 1
    assert "Validate payment transaction status" in summary
    assert "jdbc" not in summary.lower()


def test_no_credential_leakage_in_execution_summary():
    reg = _register_agent("conn-leak-proj", "Leak-Agent")
    conn = _register_connection(reg.agent_id)
    check = _sample_check()
    simulate_approval(check.check_id, "APPROVED")
    result = execute_validation_check(check=check, connection_id=conn.connection_id)
    for token in ("password", "username", "postgres://", "jdbc:"):
        assert token not in result.summary.lower()


def test_deterministic_execution_ids():
    check_id = "dbcheck:payments:validate_transaction_status"
    connection_id = "dbconn:agent:payments_db"
    assert build_execution_id(check_id, connection_id) == build_execution_id(check_id, connection_id)
    assert build_execution_id(check_id, connection_id).startswith("dbexec:")


def test_no_direct_db_access():
    with patch(
        "services.database_connector_service.simulate_agent_readonly_execution",
        return_value=(1, "Agent-mediated read-only result."),
    ) as mock_agent_exec:
        reg = _register_agent("conn-direct-proj", "Direct-Agent")
        conn = _register_connection(reg.agent_id)
        check = _sample_check()
        simulate_approval(check.check_id, "APPROVED")
        execute_validation_check(check=check, connection_id=conn.connection_id)
        mock_agent_exec.assert_called_once()


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


def test_register_connection_endpoint(client: TestClient):
    reg = client.post(
        "/local-agents/foundation/register",
        json={
            "project_id": "api-conn-proj",
            "name": "Api-Conn-Agent",
            "capabilities": ["database_validation"],
        },
    )
    assert reg.status_code == 200, reg.text
    agent_id = reg.json()["agent_id"]
    client.post(
        "/local-agents/foundation/mock-heartbeat",
        json={"agent_id": agent_id, "version": "1.0.0"},
    )
    r = client.post(
        "/local-agents/database-connections/register",
        json={
            "agent_id": agent_id,
            "name": "Store DB",
            "database_type": "postgresql",
            "host_label": "store-db",
            "database_name": "store",
        },
    )
    assert r.status_code == 200, r.text
    assert r.json()["host_label"] == "store-db"
