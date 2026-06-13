# services/database_connector_supabase.py
"""Supabase mirror for database_connections (best-effort upsert, never raises)."""
from __future__ import annotations

import logging
import time
from typing import Any, Dict, List, Optional

from services.onboarding_config_supabase import supabase_onboarding_config_enabled
from services.run_store_supabase import _get_supabase, _is_transient_supabase_error

logger = logging.getLogger("vanya.database_connector_supabase")

_TABLE = "database_connections"


def _first_row(res: Any) -> Optional[Dict[str, Any]]:
    rows = getattr(res, "data", None) or []
    if not rows:
        return None
    row = rows[0]
    return row if isinstance(row, dict) else dict(row)


def _row_to_connection(row: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "connection_id": str(row.get("connection_id") or ""),
        "agent_id": str(row.get("agent_id") or ""),
        "name": str(row.get("name") or ""),
        "database_type": str(row.get("database_type") or ""),
        "host_label": str(row.get("host_label") or ""),
        "database_name": str(row.get("database_name") or ""),
        "status": str(row.get("status") or "UNKNOWN"),
        "created_at": str(row.get("created_at") or ""),
    }


def _connection_to_supabase_row(connection: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "connection_id": str(connection.get("connection_id") or "").strip(),
        "agent_id": str(connection.get("agent_id") or "").strip(),
        "name": str(connection.get("name") or "").strip(),
        "database_type": str(connection.get("database_type") or "").strip(),
        "host_label": str(connection.get("host_label") or "").strip(),
        "database_name": str(connection.get("database_name") or "").strip(),
        "status": str(connection.get("status") or "UNKNOWN"),
        "created_at": str(connection.get("created_at") or ""),
    }


def persist_database_connection_supabase(connection: Dict[str, Any]) -> bool:
    """Upsert by connection_id. Never raises."""
    cid = str(connection.get("connection_id") or "").strip()
    if not cid:
        return False
    sb = _get_supabase()
    if sb is None:
        return False
    row = _connection_to_supabase_row(connection)
    last_err: Optional[Exception] = None
    for attempt in range(1, 4):
        try:
            sb.table(_TABLE).upsert(row, on_conflict="connection_id").execute()
            return True
        except Exception as e:
            last_err = e
            if attempt >= 3 or not _is_transient_supabase_error(e):
                break
            logger.warning(
                "persist_database_connection_supabase: transient error attempt %s/3 connection_id=%r — %s",
                attempt,
                cid,
                e,
            )
            time.sleep(0.15 * (2 ** (attempt - 1)))
    logger.error(
        "persist_database_connection_supabase: upsert failed connection_id=%r — %s",
        cid,
        last_err,
    )
    return False


def fetch_database_connection_supabase(connection_id: str) -> Optional[Dict[str, Any]]:
    cid = (connection_id or "").strip()
    if not cid or not supabase_onboarding_config_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        row = _first_row(sb.table(_TABLE).select("*").eq("connection_id", cid).limit(1).execute())
        return _row_to_connection(row) if row else None
    except Exception:
        logger.exception("database_connector_supabase: fetch failed connection_id=%r", cid)
        return None


def list_database_connections_supabase(
    *,
    agent_id: Optional[str] = None,
    limit: int = 200,
) -> List[Dict[str, Any]]:
    if not supabase_onboarding_config_enabled():
        return []
    limit = max(1, min(int(limit), 500))
    try:
        sb = _get_supabase()
        if sb is None:
            return []
        q = sb.table(_TABLE).select("*").order("created_at", desc=True).limit(limit)
        if agent_id is not None and str(agent_id).strip():
            q = q.eq("agent_id", str(agent_id).strip())
        rows = getattr(q.execute(), "data", None) or []
        return [_row_to_connection(r) for r in rows if isinstance(r, dict)]
    except Exception:
        logger.exception("database_connector_supabase: list failed agent_id=%r", agent_id)
        return []
