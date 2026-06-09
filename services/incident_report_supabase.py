# services/incident_report_supabase.py
"""Supabase mirror for incident_investigation_reports (qa_runs-style best-effort upsert)."""
from __future__ import annotations

import logging
import time
from typing import Any, Dict, List, Optional

from services.run_store_supabase import _get_supabase, _is_transient_supabase_error

logger = logging.getLogger("vanya.incident_report_supabase")

_TABLE = "incident_investigation_reports"


def supabase_incident_reports_enabled() -> bool:
    from services.qa_runs_read import supabase_qa_runs_enabled

    return supabase_qa_runs_enabled()


def persist_incident_report_supabase(
    *,
    report_id: str,
    project_id: str,
    description: str,
    severity: str,
    summary: str,
    confidence: float,
    created_at: str,
    report_json: Dict[str, Any],
) -> bool:
    """Upsert by id. Never raises."""
    rid = (report_id or "").strip()
    if not rid:
        return False
    sb = _get_supabase()
    if sb is None:
        return False

    row = {
        "id": rid,
        "project_id": (project_id or "").strip().lower(),
        "description": (description or "").strip(),
        "severity": (severity or "medium").strip(),
        "summary": (summary or "").strip(),
        "confidence": float(confidence or 0),
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
                "persist_incident_report_supabase: transient error attempt %s/3 id=%r — %s",
                attempt,
                rid,
                e,
            )
            time.sleep(0.15 * (2 ** (attempt - 1)))
    logger.error("persist_incident_report_supabase: upsert failed id=%r — %s", rid, last_err)
    return False


def _first_row(res: Any) -> Optional[Dict[str, Any]]:
    rows = getattr(res, "data", None) or []
    if not rows:
        return None
    row = rows[0]
    return row if isinstance(row, dict) else dict(row)


def fetch_incident_report_supabase(report_id: str) -> Optional[Dict[str, Any]]:
    rid = (report_id or "").strip()
    if not rid or not supabase_incident_reports_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        row = _first_row(sb.table(_TABLE).select("*").eq("id", rid).limit(1).execute())
        return _row_to_record(row) if row else None
    except Exception:
        logger.exception("incident_report_supabase: fetch failed id=%r", rid)
        return None


def list_incident_reports_supabase(
    project_id: str,
    *,
    limit: int = 50,
) -> List[Dict[str, Any]]:
    pid = (project_id or "").strip().lower()
    if not pid or not supabase_incident_reports_enabled():
        return []
    limit = max(1, min(int(limit), 200))
    try:
        sb = _get_supabase()
        if sb is None:
            return []
        res = (
            sb.table(_TABLE)
            .select("id,project_id,description,severity,summary,confidence,created_at,report_json")
            .eq("project_id", pid)
            .order("created_at", desc=True)
            .limit(limit)
            .execute()
        )
        rows = getattr(res, "data", None) or []
        return [_row_to_record(r) for r in rows if isinstance(r, dict)]
    except Exception:
        logger.exception("incident_report_supabase: list failed project_id=%r", pid)
        return []


def fetch_incident_report_full_supabase(report_id: str) -> Optional[Dict[str, Any]]:
    """Full report document (report_json merged with index columns)."""
    rid = (report_id or "").strip()
    if not rid or not supabase_incident_reports_enabled():
        return None
    try:
        sb = _get_supabase()
        if sb is None:
            return None
        row = _first_row(sb.table(_TABLE).select("*").eq("id", rid).limit(1).execute())
        if not row:
            return None
        return _row_to_full_document(row)
    except Exception:
        logger.exception("incident_report_supabase: fetch full failed id=%r", rid)
        return None


def _row_to_record(row: Dict[str, Any]) -> Dict[str, Any]:
    conf = row.get("confidence")
    try:
        confidence = float(conf if conf is not None else 0)
    except (TypeError, ValueError):
        confidence = 0.0
    return {
        "id": str(row.get("id") or ""),
        "project_id": str(row.get("project_id") or ""),
        "description": str(row.get("description") or ""),
        "severity": str(row.get("severity") or "medium"),
        "summary": str(row.get("summary") or ""),
        "confidence": confidence,
        "created_at": str(row.get("created_at") or ""),
    }


def _row_to_full_document(row: Dict[str, Any]) -> Dict[str, Any]:
    rec = _row_to_record(row)
    report_json = row.get("report_json")
    if isinstance(report_json, dict):
        data = dict(report_json)
    else:
        data = {}
    data.setdefault("id", rec["id"])
    data.setdefault("project_id", rec["project_id"])
    data.setdefault("description", rec["description"])
    data.setdefault("severity", rec["severity"])
    data.setdefault("summary", rec["summary"])
    data.setdefault("created_at", rec["created_at"])
    data.setdefault("confidence", rec["confidence"])
    return data
