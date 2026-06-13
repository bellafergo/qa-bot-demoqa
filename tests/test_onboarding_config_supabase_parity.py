# tests/test_onboarding_config_supabase_parity.py
"""Supabase durability for onboarding configuration entities."""
from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import MagicMock, patch

from services.onboarding_service import build_onboarding_checklist


# ── mirror helpers ────────────────────────────────────────────────────────────


@patch("services.local_agent_supabase._get_supabase")
def test_persist_local_agent_supabase_upserts(mock_get_sb):
    from services.local_agent_supabase import persist_local_agent_supabase

    table = MagicMock()
    sb = MagicMock()
    sb.table.return_value = table
    mock_get_sb.return_value = sb

    ok = persist_local_agent_supabase(
        {
            "agent_id": "agent-1",
            "project_id": "demo",
            "name": "Demo Agent",
            "status": "offline",
            "capabilities": ["browser_inspection"],
            "version": "1.0",
            "last_seen_at": None,
            "created_at": "2026-06-06T10:00:00+00:00",
            "updated_at": "2026-06-06T10:00:00+00:00",
            "enabled": True,
            "metadata": {},
            "token_hash": "abc123",
            "token_fingerprint": "fp1",
        }
    )
    assert ok is True
    sb.table.assert_called_with("local_agents")
    table.upsert.assert_called_once()
    assert table.upsert.call_args.kwargs.get("on_conflict") == "agent_id"


@patch("services.browser_inspection_watch_supabase._get_supabase")
def test_persist_browser_watch_supabase_upserts(mock_get_sb):
    from services.browser_inspection_watch_supabase import persist_browser_inspection_watch_supabase

    table = MagicMock()
    sb = MagicMock()
    sb.table.return_value = table
    mock_get_sb.return_value = sb

    ok = persist_browser_inspection_watch_supabase(
        {
            "watch_id": "watch-1",
            "url": "https://example.com",
            "project_id": "demo",
            "interval_minutes": 60,
            "change_threshold": "medium",
            "enabled": True,
            "execution_mode": "cloud",
            "local_agent_id": None,
            "compare_mode": "last",
            "created_at": "2026-06-06T10:00:00+00:00",
            "updated_at": "2026-06-06T10:00:00+00:00",
            "last_status": "never_run",
        }
    )
    assert ok is True
    sb.table.assert_called_with("browser_inspection_watches")
    assert table.upsert.call_args.kwargs.get("on_conflict") == "watch_id"


@patch("services.database_connector_supabase._get_supabase")
def test_persist_database_connection_supabase_upserts(mock_get_sb):
    from services.database_connector_supabase import persist_database_connection_supabase

    table = MagicMock()
    sb = MagicMock()
    sb.table.return_value = table
    mock_get_sb.return_value = sb

    ok = persist_database_connection_supabase(
        {
            "connection_id": "conn-1",
            "agent_id": "agent-1",
            "name": "Demo DB",
            "database_type": "postgres",
            "host_label": "db.internal",
            "database_name": "app",
            "status": "UNKNOWN",
            "created_at": "2026-06-06T10:00:00+00:00",
        }
    )
    assert ok is True
    sb.table.assert_called_with("database_connections")
    assert table.upsert.call_args.kwargs.get("on_conflict") == "connection_id"


def test_persist_local_agent_supabase_no_client_returns_false():
    from services.local_agent_supabase import persist_local_agent_supabase

    with patch("services.local_agent_supabase._get_supabase", return_value=None):
        ok = persist_local_agent_supabase({"agent_id": "x"})
    assert ok is False


# ── repository read fallback ──────────────────────────────────────────────────


@patch("services.local_agent_supabase.supabase_onboarding_config_enabled", return_value=True)
@patch("services.local_agent_supabase.list_local_agents_supabase")
@patch("services.db.local_agent_repository.LocalAgentRepository._list_agents_sqlite")
def test_local_agent_repo_list_falls_back_to_supabase(mock_sqlite, mock_supa_list, _enabled):
    from services.db.local_agent_repository import LocalAgentRepository

    mock_sqlite.return_value = []
    mock_supa_list.return_value = [
        {
            "agent_id": "agent-1",
            "project_id": "demo",
            "name": "Demo Agent",
            "status": "offline",
            "capabilities": [],
            "version": None,
            "last_seen_at": None,
            "created_at": "2026-06-06T10:00:00+00:00",
            "updated_at": "2026-06-06T10:00:00+00:00",
            "enabled": True,
            "metadata": {},
            "token_hash": "abc",
            "token_fingerprint": "fp",
        }
    ]
    rows = LocalAgentRepository().list_agents(project_id="demo", limit=10)
    assert len(rows) == 1
    assert rows[0]["agent_id"] == "agent-1"


@patch("services.browser_inspection_watch_supabase.supabase_onboarding_config_enabled", return_value=True)
@patch("services.browser_inspection_watch_supabase.list_browser_inspection_watches_supabase")
@patch(
    "services.db.browser_inspection_watch_repository.BrowserInspectionWatchRepository._list_watches_sqlite"
)
def test_browser_watch_repo_list_falls_back_to_supabase(mock_sqlite, mock_supa_list, _enabled):
    from services.db.browser_inspection_watch_repository import BrowserInspectionWatchRepository

    mock_sqlite.return_value = []
    mock_supa_list.return_value = [
        {
            "watch_id": "watch-1",
            "url": "https://example.com",
            "project_id": "demo",
            "interval_minutes": 60,
            "change_threshold": "medium",
            "enabled": True,
            "execution_mode": "cloud",
            "local_agent_id": None,
            "compare_mode": "last",
            "created_at": "2026-06-06T10:00:00+00:00",
            "updated_at": "2026-06-06T10:00:00+00:00",
            "last_status": "never_run",
            "current_status": "never_run",
            "last_effective_change_level": None,
            "last_change_level": None,
            "last_visual_change_level": None,
            "last_alert_at": None,
            "last_run_error": None,
            "last_run_at": None,
            "last_inspection_id": None,
            "last_diff_id": None,
            "baseline_inspection_id": None,
            "baseline_set_at": None,
            "baseline_updated_by": None,
        }
    ]
    rows = BrowserInspectionWatchRepository().list_watches(project_id="demo", limit=10)
    assert len(rows) == 1
    assert rows[0]["watch_id"] == "watch-1"


@patch("services.database_connector_supabase.supabase_onboarding_config_enabled", return_value=True)
@patch("services.database_connector_supabase.list_database_connections_supabase")
@patch("services.db.database_connector_repository.DatabaseConnectorRepository._list_connections_sqlite")
def test_database_connector_repo_list_falls_back_to_supabase(mock_sqlite, mock_supa_list, _enabled):
    from services.db.database_connector_repository import DatabaseConnectorRepository

    mock_sqlite.return_value = []
    mock_supa_list.return_value = [
        {
            "connection_id": "conn-1",
            "agent_id": "agent-1",
            "name": "Demo DB",
            "database_type": "postgres",
            "host_label": "db.internal",
            "database_name": "app",
            "status": "UNKNOWN",
            "created_at": "2026-06-06T10:00:00+00:00",
        }
    ]
    rows = DatabaseConnectorRepository().list_connections(agent_id="agent-1", limit=10)
    assert len(rows) == 1
    assert rows[0]["connection_id"] == "conn-1"


@patch("services.local_agent_supabase.persist_local_agent_supabase", return_value=True)
def test_local_agent_repo_insert_mirrors_to_supabase(mock_persist):
    from services.db.local_agent_repository import LocalAgentRepository

    LocalAgentRepository().insert_agent(
        agent_id="agent-mirror",
        project_id="demo",
        name="Mirror Agent",
        capabilities_json='["browser_inspection"]',
        version="1.0",
        agent_meta_json="{}",
        token_hash="deadbeef",
        token_fingerprint="fp",
    )
    mock_persist.assert_called_once()
    assert mock_persist.call_args.args[0]["agent_id"] == "agent-mirror"


# ── onboarding durable reads ───────────────────────────────────────────────────


def _project(**settings):
    base = dict(settings)
    base_url = base.pop("base_url", None)
    return SimpleNamespace(
        id="demo",
        name="Demo",
        base_url=base_url,
        settings=base,
    )


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.database_connector_supabase.supabase_onboarding_config_enabled", return_value=True)
@patch("services.local_agent_supabase.supabase_onboarding_config_enabled", return_value=True)
@patch("services.browser_inspection_watch_supabase.supabase_onboarding_config_enabled", return_value=True)
@patch("services.database_connector_supabase.list_database_connections_supabase")
@patch("services.local_agent_supabase.list_local_agents_supabase")
@patch("services.browser_inspection_watch_supabase.list_browser_inspection_watches_supabase")
@patch("services.db.database_connector_repository.DatabaseConnectorRepository._list_connections_sqlite")
@patch("services.db.local_agent_repository.LocalAgentRepository._list_agents_sqlite")
@patch(
    "services.db.browser_inspection_watch_repository.BrowserInspectionWatchRepository._list_watches_sqlite"
)
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_onboarding_completed_from_supabase_when_sqlite_empty(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_sqlite,
    mock_agent_sqlite,
    mock_conn_sqlite,
    mock_watch_supa,
    mock_agent_supa,
    mock_conn_supa,
    _watch_enabled,
    _agent_enabled,
    _conn_enabled,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        github={"enabled": True, "owner": "acme", "repo": "app"},
    )
    mock_catalog_repo.list_test_cases.return_value = [{"id": "tc-1"}]
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 1}
    mock_watch_sqlite.return_value = []
    mock_agent_sqlite.return_value = []
    mock_conn_sqlite.return_value = []
    mock_watch_supa.return_value = [
        {
            "watch_id": "watch-1",
            "url": "https://example.com",
            "project_id": "demo",
            "interval_minutes": 60,
            "change_threshold": "medium",
            "enabled": True,
            "execution_mode": "cloud",
            "created_at": "2026-06-06T10:00:00+00:00",
            "updated_at": "2026-06-06T10:00:00+00:00",
            "last_status": "never_run",
            "current_status": "never_run",
            "compare_mode": "last",
            "local_agent_id": None,
            "last_effective_change_level": None,
            "last_change_level": None,
            "last_visual_change_level": None,
            "last_alert_at": None,
            "last_run_error": None,
            "last_run_at": None,
            "last_inspection_id": None,
            "last_diff_id": None,
            "baseline_inspection_id": None,
            "baseline_set_at": None,
            "baseline_updated_by": None,
        }
    ]
    mock_agent_supa.return_value = [
        {
            "agent_id": "agent-1",
            "project_id": "demo",
            "name": "Demo Agent",
            "status": "offline",
            "capabilities": [],
            "version": None,
            "last_seen_at": None,
            "created_at": "2026-06-06T10:00:00+00:00",
            "updated_at": "2026-06-06T10:00:00+00:00",
            "enabled": True,
            "metadata": {},
            "token_hash": "abc",
            "token_fingerprint": "fp",
        }
    ]

    def _conn_side_effect(*, agent_id=None, limit=200):
        if agent_id == "agent-1":
            return [
                {
                    "connection_id": "conn-1",
                    "agent_id": "agent-1",
                    "name": "Demo DB",
                    "database_type": "postgres",
                    "host_label": "db.internal",
                    "database_name": "app",
                    "status": "UNKNOWN",
                    "created_at": "2026-06-06T10:00:00+00:00",
                }
            ]
        return []

    mock_conn_supa.side_effect = _conn_side_effect
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    by_id = {s.step_id: s for s in checklist.steps}
    assert by_id["configure_browser_monitoring"].status == "COMPLETED"
    assert by_id["configure_local_agent"].status == "COMPLETED"
    assert by_id["configure_database_validation"].status == "COMPLETED"
