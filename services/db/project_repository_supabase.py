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

from services.db.project_errors import ProjectDuplicateIdError, ProjectSettingsPersistError
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


def _merged_settings_row(d: Dict[str, Any]) -> Dict[str, Any]:
    """
    PostgREST / legacy rows may expose JSON as settings_json (TEXT/JSONB) or settings.
    Merge with settings_json winning on key collisions.
    """
    a = _settings_from_row(d.get("settings"))
    b = _settings_from_row(d.get("settings_json"))
    if not a:
        return b
    if not b:
        return a
    merged = dict(a)
    merged.update(b)
    return merged


def _dict_to_project(d: Dict[str, Any]):
    from models.project import Project

    sid = str(d.get("id") or "").strip()
    return Project(
        id=sid,
        name=str(d.get("name") or ""),
        description=str(d.get("description") or ""),
        color=str(d.get("color") or "#6366f1"),
        base_url=d.get("base_url"),
        settings=_merged_settings_row(d),
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


def _error_blob(exc: BaseException) -> str:
    parts = [str(exc)]
    for attr in ("message", "details", "hint", "code"):
        v = getattr(exc, attr, None)
        if v is not None:
            parts.append(str(v))
    return " ".join(parts).lower()


def _is_schema_cache_column_error(exc: BaseException) -> bool:
    """
    PostgREST (Supabase) may reject payloads that reference a column not yet in its
    in-memory schema cache after DDL (e.g. new settings_json).
    """
    b = _error_blob(exc)
    return "schema cache" in b and ("column" in b or "settings_json" in b)


def _is_column_unavailable_error(exc: BaseException, column: str) -> bool:
    """True when PostgREST rejects a column (missing from DB or stale schema cache)."""
    if _is_schema_cache_column_error(exc):
        return True
    b = _error_blob(exc)
    col = (column or "").strip().lower()
    if not col:
        return False
    if col not in b:
        return False
    return any(
        token in b
        for token in (
            "pgrst204",
            "pgrst205",
            "could not find",
            "column",
            "schema cache",
        )
    )


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

        settings_str = dump_settings_json(settings_dict or {})
        row_full = {
            "id": p.id,
            "name": p.name,
            "description": p.description or "",
            "color": p.color or "#6366f1",
            "base_url": p.base_url,
            "settings_json": settings_str,
            "created_at": now,
            "updated_at": now,
        }
        row_min = {k: v for k, v in row_full.items() if k != "settings_json"}
        sb = self._sb()

        try:
            sb.table(_TABLE).insert(row_full).execute()
            logger.info("project_repo.supabase: created project id=%s (full row)", p.id)
            return _dict_to_project(row_full)
        except Exception as e:
            if _is_unique_violation(e):
                raise ProjectDuplicateIdError(str(e)) from e
            if not _is_schema_cache_column_error(e):
                logger.exception("project_repo.supabase: insert failed id=%s", p.id)
                raise

        # Stale PostgREST schema cache: insert without settings_json, then PATCH settings.
        logger.warning(
            "project_repo.supabase: insert hit schema-cache error for settings_json id=%s — "
            "retrying minimal insert + settings update (or run NOTIFY pgrst, 'reload schema')",
            p.id,
        )
        try:
            sb.table(_TABLE).insert(row_min).execute()
        except Exception as e2:
            if _is_unique_violation(e2):
                raise ProjectDuplicateIdError(str(e2)) from e2
            logger.exception("project_repo.supabase: minimal insert failed id=%s", p.id)
            raise

        try:
            sb.table(_TABLE).update(
                {"settings_json": settings_str, "updated_at": _utc_iso()},
            ).eq("id", p.id).execute()
            logger.info("project_repo.supabase: created project id=%s (minimal + settings patch)", p.id)
            got = self.get_project(p.id)
            return got if got is not None else _dict_to_project(row_full)
        except Exception as e3:
            if _is_schema_cache_column_error(e3):
                logger.warning(
                    "project_repo.supabase: settings_json still not in schema cache for id=%s — "
                    "returning optimistic response; run NOTIFY pgrst, 'reload schema' in Supabase SQL",
                    p.id,
                )
                return _dict_to_project(row_full)
            logger.exception("project_repo.supabase: settings patch failed id=%s", p.id)
            raise

    def _persist_settings(self, sb, pid: str, settings_dict: Dict[str, Any], now: str) -> None:
        """
        Write project settings to Supabase.

        Canonical column: ``settings_json`` (TEXT JSON string per migration.sql).
        Fallback: ``settings`` (JSONB) when PostgREST schema cache lacks settings_json.
        """
        settings_str = dump_settings_json(settings_dict)
        attempts: List[str] = []

        try:
            sb.table(_TABLE).update({"settings_json": settings_str, "updated_at": now}).eq("id", pid).execute()
            return
        except Exception as e:
            if _is_column_unavailable_error(e, "settings_json"):
                attempts.append(f"settings_json: {type(e).__name__}: {str(e)[:180]}")
                logger.warning(
                    "project_repo.supabase: settings_json update failed id=%s — trying settings fallback",
                    pid,
                )
            else:
                raise

        try:
            sb.table(_TABLE).update({"settings": settings_dict, "updated_at": now}).eq("id", pid).execute()
            logger.warning(
                "project_repo.supabase: persisted settings via legacy settings column id=%s",
                pid,
            )
            return
        except Exception as e:
            attempts.append(f"settings: {type(e).__name__}: {str(e)[:180]}")
            if not _is_column_unavailable_error(e, "settings"):
                raise

        raise ProjectSettingsPersistError(
            f"Could not persist settings for project '{pid}'. "
            "Run in Supabase SQL editor: "
            "ALTER TABLE public.projects ADD COLUMN IF NOT EXISTS settings_json TEXT; "
            "NOTIFY pgrst, 'reload schema'; "
            f"Details: {' | '.join(attempts)}"
        )

    def update_project(self, project_id: str, data: Dict[str, Any]) -> Optional[Any]:
        pid = (project_id or "").strip().lower()
        now = _utc_iso()
        meta: Dict[str, Any] = {}
        if "name" in data and data["name"] is not None:
            meta["name"] = data["name"]
        if "description" in data and data["description"] is not None:
            meta["description"] = data["description"]
        if "color" in data and data["color"] is not None:
            meta["color"] = data["color"]
        if "base_url" in data:
            meta["base_url"] = data["base_url"]

        settings_dict: Optional[Dict[str, Any]] = None
        if "settings" in data and data["settings"] is not None:
            settings_dict = data["settings"] if isinstance(data["settings"], dict) else {}

        sb = self._sb()
        if settings_dict is not None:
            self._persist_settings(sb, pid, settings_dict, now)
        if meta:
            sb.table(_TABLE).update({**meta, "updated_at": now}).eq("id", pid).execute()
        elif settings_dict is None:
            sb.table(_TABLE).update({"updated_at": now}).eq("id", pid).execute()

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
