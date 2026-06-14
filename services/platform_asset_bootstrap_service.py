# services/platform_asset_bootstrap_service.py
"""
Generic platform asset bootstrap — registers real platform-owned persistence assets
for any project (idempotent, no demo/sample data).
"""
from __future__ import annotations

import json
import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from models.platform_asset_models import PlatformAssetBootstrapResponse

from services.database_connector_service import build_connection_id
from services.db.database_connector_repository import database_connector_repo
from services.db.local_agent_repository import local_agent_repo
from services.local_agent_service import (
    build_deterministic_agent_id,
    build_deterministic_agent_token,
    hash_agent_token,
)
from services.platform_internal_probe_service import (
    PLATFORM_PROBE_CHECK_ID,
    execute_platform_internal_probe,
    safe_sqlite_host_label,
    safe_supabase_host_label,
    sqlite_store_accessible,
    supabase_configured,
)

logger = logging.getLogger("vanya.platform_asset_bootstrap")

PLATFORM_AGENT_NAME = "Vanya Platform Agent"
PLATFORM_SUPABASE_NAME = "Platform Database (Supabase)"
PLATFORM_SQLITE_NAME = "Platform Operational Store (SQLite)"


def _utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def _ensure_platform_agent(project_id: str) -> Tuple[str, bool]:
    """Register idempotent platform agent. Returns (agent_id, already_exists)."""
    pid = (project_id or "").strip()
    agent_id = build_deterministic_agent_id(pid, PLATFORM_AGENT_NAME)
    existing = local_agent_repo.get_agent(agent_id)
    if existing:
        return agent_id, True

    caps = ["database_validation"]
    caps_json = json.dumps(caps, separators=(",", ":"))
    meta = {
        "agent_type": "platform",
        "environment": "production",
        "foundation": True,
        "platform_managed": True,
    }
    meta_json = json.dumps(meta, separators=(",", ":"))
    raw_token = build_deterministic_agent_token(agent_id)
    token_hash, fingerprint = hash_agent_token(raw_token)
    now = _utc_now()
    local_agent_repo.insert_agent(
        agent_id=agent_id,
        project_id=pid,
        name=PLATFORM_AGENT_NAME,
        capabilities_json=caps_json,
        version="1.0.0",
        agent_meta_json=meta_json,
        token_hash=token_hash,
        token_fingerprint=fingerprint,
    )
    local_agent_repo.update_agent(
        agent_id,
        status="online",
        last_seen_at=now,
    )
    return agent_id, False


def _register_platform_connection(
    *,
    agent_id: str,
    name: str,
    database_type: str,
    host_label: str,
    database_name: str,
) -> Tuple[Dict[str, Any], bool]:
    connection_id = build_connection_id(agent_id, name)
    existing = database_connector_repo.get_connection(connection_id)
    if existing:
        return existing, True

    database_connector_repo.insert_connection(
        connection_id=connection_id,
        agent_id=agent_id,
        name=name,
        database_type=database_type,
        host_label=host_label,
        database_name=database_name,
        status="PENDING_VALIDATION",
        asset_scope="platform_internal",
        execution_mode="platform_backend",
        is_platform_managed=True,
        created_by_system=True,
    )
    row = database_connector_repo.get_connection(connection_id) or {}
    return row, False


def _persist_probe_execution(
    *,
    connection: Dict[str, Any],
    row_count: int,
    summary: str,
    ok: bool,
) -> None:
    from services.database_connector_service import build_execution_id

    connection_id = str(connection.get("connection_id") or "")
    agent_id = str(connection.get("agent_id") or "")
    now = _utc_now()
    status = "SUCCESS" if ok else "FAILED"
    conn_status = "CONNECTED" if ok else "DEGRADED"
    if not ok and row_count == 0:
        conn_status = "ERROR"

    execution_id = build_execution_id(PLATFORM_PROBE_CHECK_ID, connection_id)
    if not database_connector_repo.get_execution(execution_id):
        database_connector_repo.insert_execution(
            execution_id=execution_id,
            check_id=PLATFORM_PROBE_CHECK_ID,
            connection_id=connection_id,
            agent_id=agent_id,
            executed_at=now,
            status=status,
            row_count=row_count,
            summary=summary[:512],
            confidence=0.9 if ok else 0.0,
            requires_user_approval=False,
            check_json=json.dumps({"probe": "platform_internal", "read_only": True}),
        )
    database_connector_repo.update_connection_probe(
        connection_id,
        status=conn_status,
        last_probe_at=now,
        last_probe_status=status,
        last_probe_summary=summary[:512],
    )


def run_platform_probes_for_connections(connections: List[Dict[str, Any]]) -> Tuple[int, int]:
    probes_run = 0
    probes_succeeded = 0
    for conn in connections:
        if str(conn.get("asset_scope") or "") != "platform_internal":
            continue
        probes_run += 1
        row_count, summary, ok = execute_platform_internal_probe(conn)
        _persist_probe_execution(connection=conn, row_count=row_count, summary=summary, ok=ok)
        if ok:
            probes_succeeded += 1
    return probes_run, probes_succeeded


def bootstrap_platform_assets(project_id: str) -> PlatformAssetBootstrapResponse:
    """
    Idempotently register platform agent + real platform persistence assets for any project.
    Runs read-only probes and persists validation evidence.
    """
    pid = (project_id or "").strip()
    if not pid:
        raise ValueError("project_id is required")

    agent_id, agent_exists = _ensure_platform_agent(pid)
    registered: List[Tuple[Dict[str, Any], bool]] = []
    any_new = not agent_exists

    if supabase_configured():
        row, existed = _register_platform_connection(
            agent_id=agent_id,
            name=PLATFORM_SUPABASE_NAME,
            database_type="postgresql",
            host_label=safe_supabase_host_label(),
            database_name="vanya_platform",
        )
        registered.append((row, existed))
        any_new = any_new or not existed

    if sqlite_store_accessible():
        row, existed = _register_platform_connection(
            agent_id=agent_id,
            name=PLATFORM_SQLITE_NAME,
            database_type="sqlite",
            host_label=safe_sqlite_host_label(),
            database_name="vanya_operational_store",
        )
        registered.append((row, existed))
        any_new = any_new or not existed

    probes_run, probes_succeeded = run_platform_probes_for_connections([r for r, _ in registered])

    from services.database_connector_service import _row_to_connection

    agent_row = local_agent_repo.get_agent(agent_id)
    connection_models = [
        _row_to_connection(r, agent_row, already_exists=existed)
        for r, existed in registered
    ]

    return PlatformAssetBootstrapResponse(
        project_id=pid,
        agent_id=agent_id,
        agent_already_exists=agent_exists,
        connections=connection_models,
        probes_run=probes_run,
        probes_succeeded=probes_succeeded,
        already_bootstrapped=agent_exists and not any_new and probes_succeeded > 0,
    )
