# services/local_agent_supabase.py
"""Supabase mirror for local_agents (best-effort upsert, never raises)."""
from __future__ import annotations

import json
import logging
import time
from typing import Any, Dict, List, Optional

from services.onboarding_config_supabase import supabase_onboarding_config_enabled
from services.run_store_supabase import _get_supabase, _is_transient_supabase_error

logger = logging.getLogger("vanya.local_agent_supabase")

_TABLE = "local_agents"


def _first_row(res: Any) -> Optional[Dict[str, Any]]:
    rows = getattr(res, "data", None) or []
    if not rows:
        return None
    row = rows[0]
    return row if isinstance(row, dict) else dict(row)


def _row_to_agent(row: Dict[str, Any]) -> Dict[str, Any]:
    caps: List[str] = []
    try:
        raw = row.get("capabilities_json") or "[]"
        parsed = json.loads(raw) if isinstance(raw, str) else raw
        caps = parsed if isinstance(parsed, list) else []
    except Exception:
        caps = []
    meta: Dict[str, Any] = {}
    try:
        raw = row.get("agent_meta_json") or "{}"
        parsed = json.loads(raw) if isinstance(raw, str) else raw
        meta = parsed if isinstance(parsed, dict) else {}
    except Exception:
        meta = {}
    return {
        "agent_id": str(row.get("agent_id") or ""),
        "project_id": row.get("project_id"),
        "name": str(row.get("name") or ""),
        "status": str(row.get("status") or "offline"),
        "capabilities": caps,
        "version": row.get("version"),
        "last_seen_at": row.get("last_seen_at"),
        "created_at": str(row.get("created_at") or ""),
        "updated_at": str(row.get("updated_at") or ""),
        "enabled": bool(row.get("enabled", 1)),
        "metadata": meta,
        "token_hash": str(row.get("token_hash") or ""),
        "token_fingerprint": str(row.get("token_fingerprint") or ""),
    }


def _agent_to_supabase_row(agent: Dict[str, Any]) -> Dict[str, Any]:
    caps = agent.get("capabilities")
    if not isinstance(caps, list):
        caps = []
    meta = agent.get("metadata")
    if not isinstance(meta, dict):
        meta = {}
    return {
        "agent_id": str(agent.get("agent_id") or "").strip(),
        "project_id": (str(agent.get("project_id")).strip() or None)
        if agent.get("project_id") is not None
        else None,
        "name": str(agent.get("name") or "").strip(),
        "status": str(agent.get("status") or "offline"),
        "capabilities_json": json.dumps(caps, ensure_ascii=True),
        "version": (str(agent.get("version")).strip() or None)
        if agent.get("version") is not None
        else None,
        "last_seen_at": agent.get("last_seen_at"),
        "created_at": str(agent.get("created_at") or ""),
        "updated_at": str(agent.get("updated_at") or ""),
        "enabled": 1 if agent.get("enabled", True) else 0,
        "agent_meta_json": json.dumps(meta, ensure_ascii=True, default=str),
        "token_hash": str(agent.get("token_hash") or "").strip().lower(),
        "token_fingerprint": str(agent.get("token_fingerprint") or "").strip(),
    }


def persist_local_agent_supabase(agent: Dict[str, Any]) -> bool:
    """Upsert by agent_id. Never raises."""
    aid = str(agent.get("agent_id") or "").strip()
    if not aid:
        return False
    sb = _get_supabase()
    if sb is None:
        return False
    row = _agent_to_supabase_row(agent)
    last_err: Optional[Exception] = None
    for attempt in range(1, 4):
        try:
            sb.table(_TABLE).upsert(row, on_conflict="agent_id").execute()
            return True
        except Exception as e:
            last_err = e
            if attempt >= 3 or not _is_transient_supabase_error(e):
                break
            logger.warning(
                "persist_local_agent_supabase: transient error attempt %s/3 agent_id=%r — %s",
                attempt,
                aid,
                e,
            )
            time.sleep(0.15 * (2 ** (attempt - 1)))
    logger.error("persist_local_agent_supabase: upsert failed agent_id=%r — %s", aid, last_err)
    return False


def fetch_local_agent_supabase(agent_id: str) -> Optional[Dict[str, Any]]:
    aid = (agent_id or "").strip()
    if not aid or not supabase_onboarding_config_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        row = _first_row(sb.table(_TABLE).select("*").eq("agent_id", aid).limit(1).execute())
        return _row_to_agent(row) if row else None
    except Exception:
        logger.exception("local_agent_supabase: fetch failed agent_id=%r", aid)
        return None


def fetch_local_agent_by_token_hash_supabase(token_hash: str) -> Optional[Dict[str, Any]]:
    th = (token_hash or "").strip().lower()
    if not th or not supabase_onboarding_config_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        row = _first_row(sb.table(_TABLE).select("*").eq("token_hash", th).limit(1).execute())
        return _row_to_agent(row) if row else None
    except Exception:
        logger.exception("local_agent_supabase: fetch by token_hash failed")
        return None


def fetch_local_agent_by_project_and_name_supabase(
    project_id: str,
    name: str,
) -> Optional[Dict[str, Any]]:
    pid = (project_id or "").strip()
    nm = (name or "").strip()
    if not pid or not nm or not supabase_onboarding_config_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        row = _first_row(
            sb.table(_TABLE)
            .select("*")
            .eq("project_id", pid)
            .eq("name", nm)
            .limit(1)
            .execute()
        )
        return _row_to_agent(row) if row else None
    except Exception:
        logger.exception(
            "local_agent_supabase: fetch by project/name failed project_id=%r name=%r",
            pid,
            nm,
        )
        return None


def list_local_agents_supabase(
    *,
    project_id: Optional[str] = None,
    limit: int = 100,
) -> List[Dict[str, Any]]:
    if not supabase_onboarding_config_enabled():
        return []
    limit = max(1, min(int(limit), 500))
    try:
        sb = _get_supabase()
        if sb is None:
            return []
        q = sb.table(_TABLE).select("*").order("created_at", desc=True).limit(limit)
        if project_id is not None and str(project_id).strip():
            q = q.eq("project_id", str(project_id).strip())
        rows = getattr(q.execute(), "data", None) or []
        return [_row_to_agent(r) for r in rows if isinstance(r, dict)]
    except Exception:
        logger.exception("local_agent_supabase: list failed project_id=%r", project_id)
        return []
