# services/db/project_repository_supabase.py
"""
Persist catalog projects in Supabase Postgres via the official Supabase REST client
(same stack as services/supabase_store.py and run_store_supabase.py).

Requires SUPABASE_URL + SUPABASE_SERVICE_ROLE_KEY (service role bypasses RLS for server-side CRUD).
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from services.db.project_errors import ProjectDuplicateIdError
from services.project_settings_service import dump_settings_json, parse_settings_json
from services.supabase_store import supabase_client

logger = logging.getLogger("vanya.db.project.supabase")

_TABLE = "projects"


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _parse_ts(raw: Any) -> datetime:
    if raw is None:
        return datetime.now(timezone.utc)
    s = str(raw).strip()
    if not s:
        return datetime.now(timezone.utc)
    try:
        return datetime.fromisoformat(s.replace("Z", "+00:00"))
    except Exception:
        return datetime.now(timezone.utc)


def _settings_from_row(raw: Any) -> Dict[str, Any]:
    """PostgREST may return JSONB as dict; parse_settings_json expects a JSON string."""
    if raw is None:
        return {}
    if isinstance(raw, dict):
        return raw
    return parse_settings_json(raw if isinstance(raw, str) else str(raw))


def _dict_to_project(d: Dict[str, Any]):
    from models.project import Project

    sid = str(d.get("id") or "").strip()
    return Project(
        id=sid,
        name=str(d.get("name") or ""),
        description=str(d.get("description") or ""),
        color=str(d.get("color") or "#6366f1"),
        base_url=d.get("base_url"),
        settings=_settings_from_row(d.get("settings_json")),
        created_at=_parse_ts(d.get("created_at")),
        updated_at=_parse_ts(d.get("updated_at")),
    )


def _is_unique_violation(exc: BaseException) -> bool:
    msg = str(exc).lower()
    if "23505" in msg:
        return True
    if "duplicate key" in msg:
        return True
    if "unique constraint" in msg:
        return True
    if "already exists" in msg and "project" in msg:
        return True
    return False


class ProjectRepositorySupabase:

    def _sb(self):
        sb = supabase_client()
        if sb is None:
            raise RuntimeError(
                "Supabase client unavailable — set SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY"
            )
        return sb

    def list_projects(self) -> List[Any]:
        sb = self._sb()
        res = sb.table(_TABLE).select("*").order("id", desc=False).execute()
        rows = res.data if isinstance(res.data, list) else []
        out = [_dict_to_project(r) for r in rows if isinstance(r, dict)]
        logger.info("project_repo.supabase: list_projects count=%s", len(out))
        return out

    def get_project(self, project_id: str) -> Optional[Any]:
        pid = (project_id or "").strip().lower()
        if not pid:
            return None
        sb = self._sb()
        res = sb.table(_TABLE).select("*").eq("id", pid).limit(1).execute()
        rows = res.data if isinstance(res.data, list) else []
        if not rows:
            return None
        return _dict_to_project(rows[0]) if isinstance(rows[0], dict) else None

    def create_project(self, p) -> Any:
        now = _utc_iso()
        settings_dict: Dict[str, Any] = {}
        if hasattr(p, "model_dump"):
            sd = p.model_dump().get("settings")
            if isinstance(sd, dict):
                settings_dict = sd
        elif getattr(p, "settings", None) is not None and isinstance(p.settings, dict):
            settings_dict = p.settings

        row = {
            "id": p.id,
            "name": p.name,
            "description": p.description or "",
            "color": p.color or "#6366f1",
            "base_url": p.base_url,
            "settings_json": dump_settings_json(settings_dict or {}),
            "created_at": now,
            "updated_at": now,
        }
        sb = self._sb()
        try:
            sb.table(_TABLE).insert(row).execute()
        except Exception as e:
            if _is_unique_violation(e):
                raise ProjectDuplicateIdError(str(e)) from e
            logger.exception("project_repo.supabase: insert failed id=%s", p.id)
            raise
        logger.info("project_repo.supabase: created project id=%s", p.id)
        return _dict_to_project(row)

    def update_project(self, project_id: str, data: Dict[str, Any]) -> Optional[Any]:
        pid = (project_id or "").strip().lower()
        now = _utc_iso()
        payload: Dict[str, Any] = {"updated_at": now}
        if "name" in data and data["name"] is not None:
            payload["name"] = data["name"]
        if "description" in data and data["description"] is not None:
            payload["description"] = data["description"]
        if "color" in data and data["color"] is not None:
            payload["color"] = data["color"]
        if "base_url" in data:
            payload["base_url"] = data["base_url"]
        if "settings" in data and data["settings"] is not None:
            payload["settings_json"] = dump_settings_json(data["settings"])

        sb = self._sb()
        sb.table(_TABLE).update(payload).eq("id", pid).execute()
        return self.get_project(pid)

    def delete_project(self, project_id: str) -> bool:
        pid = (project_id or "").strip().lower()
        existing = self.get_project(pid)
        if existing is None:
            return False
        sb = self._sb()
        sb.table(_TABLE).delete().eq("id", pid).execute()
        logger.info("project_repo.supabase: deleted project id=%s", pid)
        return True
