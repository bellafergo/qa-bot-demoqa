# services/pr_analysis_report_supabase.py
"""Supabase mirror for pr_analysis_reports (qa_runs-style best-effort upsert)."""
from __future__ import annotations

import logging
import time
from typing import Any, Dict, List, Optional

from services.run_store_supabase import _get_supabase, _is_transient_supabase_error

logger = logging.getLogger("vanya.pr_analysis_report_supabase")

_TABLE = "pr_analysis_reports"


def supabase_pr_analysis_reports_enabled() -> bool:
    from services.qa_runs_read import supabase_qa_runs_enabled

    return supabase_qa_runs_enabled()


def persist_pr_analysis_report_supabase(
    *,
    row_id: str,
    project_id: str,
    pr_id: str,
    provider: str,
    created_at: str,
    report_json: Dict[str, Any],
) -> bool:
    """Upsert by id. Never raises."""
    rid = (row_id or "").strip()
    if not rid:
        return False
    sb = _get_supabase()
    if sb is None:
        return False

    row = {
        "id": rid,
        "project_id": (project_id or "").strip().lower(),
        "pr_id": str(pr_id or "").strip(),
        "provider": (provider or "manual").strip().lower(),
        "created_at": created_at,
        "report_json": report_json if isinstance(report_json, dict) else {},
    }

    last_err: Optional[Exception] = None
    for attempt in range(1, 4):
        try:
            sb.table(_TABLE).upsert(row, on_conflict="id").execute()
            return True
        except Exception as e:
            last_err = e
            if attempt >= 3 or not _is_transient_supabase_error(e):
                break
            logger.warning(
                "persist_pr_analysis_report_supabase: transient error attempt %s/3 id=%r — %s",
                attempt,
                rid,
                e,
            )
            time.sleep(0.15 * (2 ** (attempt - 1)))
    logger.error("persist_pr_analysis_report_supabase: upsert failed id=%r — %s", rid, last_err)
    return False


def _first_row(res: Any) -> Optional[Dict[str, Any]]:
    rows = getattr(res, "data", None) or []
    if not rows:
        return None
    row = rows[0]
    return row if isinstance(row, dict) else dict(row)


def _row_to_entry(row: Dict[str, Any]) -> Dict[str, Any]:
    report_json = row.get("report_json")
    report = report_json if isinstance(report_json, dict) else {}
    return {
        "id": str(row.get("id") or ""),
        "project_id": str(row.get("project_id") or ""),
        "pr_id": str(row.get("pr_id") or ""),
        "provider": str(row.get("provider") or "manual"),
        "created_at": str(row.get("created_at") or ""),
        "report": report,
    }


def list_pr_analysis_reports_supabase(
    project_id: str,
    *,
    limit: int = 50,
) -> List[Dict[str, Any]]:
    pid = (project_id or "").strip().lower()
    if not pid or not supabase_pr_analysis_reports_enabled():
        return []
    limit = max(1, min(int(limit), 200))
    try:
        sb = _get_supabase()
        if sb is None:
            return []
        res = (
            sb.table(_TABLE)
            .select("id,project_id,pr_id,provider,created_at,report_json")
            .eq("project_id", pid)
            .order("created_at", desc=True)
            .limit(limit)
            .execute()
        )
        rows = getattr(res, "data", None) or []
        return [_row_to_entry(r) for r in rows if isinstance(r, dict)]
    except Exception:
        logger.exception("pr_analysis_report_supabase: list failed project_id=%r", pid)
        return []


def fetch_pr_analysis_report_supabase(
    project_id: str,
    pr_id: str,
    *,
    provider: Optional[str] = None,
) -> Optional[Dict[str, Any]]:
    pid = (project_id or "").strip().lower()
    pr = str(pr_id or "").strip()
    if not pid or not pr or not supabase_pr_analysis_reports_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        q = sb.table(_TABLE).select("*").eq("project_id", pid).eq("pr_id", pr)
        if provider:
            q = q.eq("provider", provider.strip().lower())
        row = _first_row(q.order("created_at", desc=True).limit(1).execute())
        return _row_to_entry(row) if row else None
    except Exception:
        logger.exception(
            "pr_analysis_report_supabase: fetch failed project_id=%r pr_id=%r",
            pid,
            pr,
        )
        return None
