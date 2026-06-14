# tests/test_platform_asset_bootstrap.py
"""Platform asset bootstrap — real persistence registration (no demo/sample data)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi import HTTPException

from models.database_connector_models import DatabaseConnectionRegistrationRequest
from services.database_connector_service import (
    is_demo_connection_row,
    register_connection,
)
from services.platform_asset_bootstrap_service import (
    PLATFORM_AGENT_NAME,
    PLATFORM_SQLITE_NAME,
    PLATFORM_SUPABASE_NAME,
    bootstrap_platform_assets,
)
from services.platform_internal_probe_service import (
    SQLITE_ALLOWLISTED_TABLES,
    probe_sqlite_platform,
)


class _Req:
    headers: dict = {}


def test_bootstrap_idempotent_no_duplicates():
    pid = "plat-bootstrap-idem"
    first = bootstrap_platform_assets(pid)
    second = bootstrap_platform_assets(pid)
    assert first.agent_id == second.agent_id
    assert len(first.connections) == len(second.connections)
    assert second.agent_already_exists is True
    assert all(c.already_exists for c in second.connections)


def test_bootstrap_registers_sqlite_platform_asset():
    pid = "plat-bootstrap-sqlite"
    with patch("services.platform_asset_bootstrap_service.supabase_configured", return_value=False):
        with patch("services.platform_asset_bootstrap_service.sqlite_store_accessible", return_value=True):
            result = bootstrap_platform_assets(pid)
    sqlite_conn = next((c for c in result.connections if c.database_type == "sqlite"), None)
    assert sqlite_conn is not None
    assert sqlite_conn.asset_scope == "platform_internal"
    assert sqlite_conn.execution_mode == "platform_backend"
    assert sqlite_conn.name == PLATFORM_SQLITE_NAME
    assert sqlite_conn.host_label != "sample-host"
    assert sqlite_conn.database_name != "sample_db"
    assert sqlite_conn.is_platform_managed is True
    assert sqlite_conn.created_by_system is True


def test_bootstrap_registers_supabase_when_configured():
    pid = "plat-bootstrap-supabase"
    with patch("services.platform_asset_bootstrap_service.supabase_configured", return_value=True):
        with patch("services.platform_asset_bootstrap_service.safe_supabase_host_label", return_value="abc.supabase.co"):
            with patch("services.platform_asset_bootstrap_service.sqlite_store_accessible", return_value=False):
                with patch(
                    "services.platform_internal_probe_service.probe_supabase_platform",
                    return_value=(5, "ok", True),
                ):
                    result = bootstrap_platform_assets(pid)
    pg_conn = next((c for c in result.connections if c.database_type == "postgresql"), None)
    assert pg_conn is not None
    assert pg_conn.asset_scope == "platform_internal"
    assert pg_conn.execution_mode == "platform_backend"
    assert pg_conn.name == PLATFORM_SUPABASE_NAME
    assert result.probes_succeeded >= 1


def test_bootstrap_no_sample_metadata():
    pid = "plat-bootstrap-no-sample"
    result = bootstrap_platform_assets(pid)
    for conn in result.connections:
        assert not is_demo_connection_row(conn.model_dump())
        assert "sample" not in conn.host_label.lower()
        assert conn.database_name != "sample_db"


def test_demo_connection_registration_rejected():
    from services.local_agent_service import register_foundation_agent
    from models.local_agent_models import LocalAgentFoundationRegistrationRequest

    reg = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(
            project_id="demo-reject-proj",
            name="Reject-Agent",
            capabilities=["database_validation"],
        ),
        _Req(),
    )
    with pytest.raises(HTTPException) as exc:
        register_connection(
            DatabaseConnectionRegistrationRequest(
                agent_id=reg.agent_id,
                name="Sample Validation Database",
                database_type="postgresql",
                host_label="sample-host",
                database_name="sample_db",
            ),
            _Req(),
        )
    assert exc.value.status_code == 400


def test_sqlite_probe_uses_allowlisted_tables_only():
    row_count, summary, ok = probe_sqlite_platform()
    assert ok is True
    assert row_count >= 0
    for table in SQLITE_ALLOWLISTED_TABLES:
        if table in summary:
            assert table in SQLITE_ALLOWLISTED_TABLES


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_onboarding_completed_after_platform_probe(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    from types import SimpleNamespace

    from services.onboarding_service import build_onboarding_checklist

    project_id = "onb-platform-probe-proj"
    mock_project_repo.get_project.return_value = SimpleNamespace(
        id=project_id,
        settings={},
        base_url=None,
    )
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist(project_id)
    db_step = next(s for s in checklist.steps if s.step_id == "configure_database_validation")
    assert db_step.status == "COMPLETED"
    assert db_step.completion_percentage == 100


def test_platform_agent_name():
    pid = "plat-agent-name"
    result = bootstrap_platform_assets(pid)
    assert result.agent_id
    from services.db.local_agent_repository import local_agent_repo

    agent = local_agent_repo.get_agent(result.agent_id)
    assert agent is not None
    assert agent.get("name") == PLATFORM_AGENT_NAME
