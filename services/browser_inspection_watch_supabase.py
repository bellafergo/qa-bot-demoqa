# services/browser_inspection_watch_supabase.py
"""Supabase mirror for browser_inspection_watches (best-effort upsert, never raises)."""
from __future__ import annotations

import logging
import time
from typing import Any, Dict, List, Optional

from services.onboarding_config_supabase import supabase_onboarding_config_enabled
from services.run_store_supabase import _get_supabase, _is_transient_supabase_error

logger = logging.getLogger("vanya.browser_inspection_watch_supabase")

_TABLE = "browser_inspection_watches"


def _first_row(res: Any) -> Optional[Dict[str, Any]]:
    rows = getattr(res, "data", None) or []
    if not rows:
        return None
    row = rows[0]
    return row if isinstance(row, dict) else dict(row)


def _row_to_watch(row: Dict[str, Any]) -> Dict[str, Any]:
    lec = row.get("last_effective_change_level")
    return {
        "watch_id": str(row.get("watch_id") or ""),
        "url": str(row.get("url") or ""),
        "project_id": row.get("project_id"),
        "interval_minutes": int(row.get("interval_minutes") or 60),
        "change_threshold": str(row.get("change_threshold") or "medium"),
        "enabled": bool(row.get("enabled", 1)),
        "execution_mode": str(row.get("execution_mode") or "cloud"),
        "local_agent_id": row.get("local_agent_id"),
        "compare_mode": str(row.get("compare_mode") or "last"),
        "baseline_inspection_id": row.get("baseline_inspection_id"),
        "baseline_set_at": row.get("baseline_set_at"),
        "baseline_updated_by": row.get("baseline_updated_by"),
        "last_status": row.get("last_status") or "never_run",
        "current_status": row.get("last_status") or "never_run",
        "last_effective_change_level": lec,
        "last_change_level": lec,
        "last_visual_change_level": row.get("last_visual_change_level"),
        "last_alert_at": row.get("last_alert_at"),
        "last_run_error": row.get("last_run_error"),
        "created_at": str(row.get("created_at") or ""),
        "updated_at": str(row.get("updated_at") or ""),
        "last_run_at": row.get("last_run_at"),
        "last_inspection_id": row.get("last_inspection_id"),
        "last_diff_id": row.get("last_diff_id"),
    }


def _watch_to_supabase_row(watch: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "watch_id": str(watch.get("watch_id") or "").strip(),
        "url": str(watch.get("url") or "").strip(),
        "project_id": (str(watch.get("project_id")).strip() or None)
        if watch.get("project_id") is not None
        else None,
        "interval_minutes": int(watch.get("interval_minutes") or 60),
        "change_threshold": str(watch.get("change_threshold") or "medium"),
        "enabled": 1 if watch.get("enabled", True) else 0,
        "execution_mode": str(watch.get("execution_mode") or "cloud"),
        "local_agent_id": (str(watch.get("local_agent_id")).strip() or None)
        if watch.get("local_agent_id") is not None
        else None,
        "compare_mode": str(watch.get("compare_mode") or "last"),
        "baseline_inspection_id": watch.get("baseline_inspection_id"),
        "baseline_set_at": watch.get("baseline_set_at"),
        "baseline_updated_by": watch.get("baseline_updated_by"),
        "last_status": watch.get("last_status"),
        "last_effective_change_level": watch.get("last_effective_change_level"),
        "last_visual_change_level": watch.get("last_visual_change_level"),
        "last_alert_at": watch.get("last_alert_at"),
        "last_run_error": watch.get("last_run_error"),
        "created_at": str(watch.get("created_at") or ""),
        "updated_at": str(watch.get("updated_at") or ""),
        "last_run_at": watch.get("last_run_at"),
        "last_inspection_id": watch.get("last_inspection_id"),
        "last_diff_id": watch.get("last_diff_id"),
    }


def persist_browser_inspection_watch_supabase(watch: Dict[str, Any]) -> bool:
    """Upsert by watch_id. Never raises."""
    wid = str(watch.get("watch_id") or "").strip()
    if not wid:
        return False
    sb = _get_supabase()
    if sb is None:
        return False
    row = _watch_to_supabase_row(watch)
    last_err: Optional[Exception] = None
    for attempt in range(1, 4):
        try:
            sb.table(_TABLE).upsert(row, on_conflict="watch_id").execute()
            return True
        except Exception as e:
            last_err = e
            if attempt >= 3 or not _is_transient_supabase_error(e):
                break
            logger.warning(
                "persist_browser_inspection_watch_supabase: transient error attempt %s/3 watch_id=%r — %s",
                attempt,
                wid,
                e,
            )
            time.sleep(0.15 * (2 ** (attempt - 1)))
    logger.error(
        "persist_browser_inspection_watch_supabase: upsert failed watch_id=%r — %s",
        wid,
        last_err,
    )
    return False


def fetch_browser_inspection_watch_supabase(watch_id: str) -> Optional[Dict[str, Any]]:
    wid = (watch_id or "").strip()
    if not wid or not supabase_onboarding_config_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        row = _first_row(sb.table(_TABLE).select("*").eq("watch_id", wid).limit(1).execute())
        return _row_to_watch(row) if row else None
    except Exception:
        logger.exception("browser_inspection_watch_supabase: fetch failed watch_id=%r", wid)
        return None


def list_browser_inspection_watches_supabase(
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
        return [_row_to_watch(r) for r in rows if isinstance(r, dict)]
    except Exception:
        logger.exception(
            "browser_inspection_watch_supabase: list failed project_id=%r",
            project_id,
        )
        return []
