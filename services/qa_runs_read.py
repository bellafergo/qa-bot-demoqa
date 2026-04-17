# services/qa_runs_read.py
"""
Read path for public.qa_runs (Supabase). Used when SUPABASE_URL + service key are set.

Maps table rows to legacy-shaped dicts for GET /runs and to CanonicalRun via run_from_legacy_store.
"""
from __future__ import annotations

import json
import logging
from typing import Any, Dict, List, Optional

from models.run_contract import CanonicalRun
from services.run_mapper import run_from_legacy_store

logger = logging.getLogger("vanya.qa_runs_read")


def _safe_str(x: Any) -> str:
    try:
        return (str(x) if x is not None else "").strip()
    except Exception:
        return ""


def supabase_qa_runs_enabled() -> bool:
    from services.supabase_store import _is_configured  # noqa: SLF001

    return _is_configured()


def _parse_jsonb(val: Any) -> Dict[str, Any]:
    if val is None:
        return {}
    if isinstance(val, dict):
        return val
    if isinstance(val, str):
        try:
            out = json.loads(val)
            return out if isinstance(out, dict) else {}
        except Exception:
            return {}
    return {}


def _effective_test_case_id(row: Dict[str, Any], meta: Dict[str, Any]) -> str:
    return (
        (meta.get("test_case_id") or meta.get("test_id") or "")
        if isinstance(meta, dict)
        else ""
    ) or str(row.get("test_name") or "").strip()


def qa_runs_row_to_legacy_payload(row: Dict[str, Any]) -> Dict[str, Any]:
    """Merge qa_runs columns with optional full `result` json for evidence + mapper."""
    meta = _parse_jsonb(row.get("meta"))
    result = row.get("result")
    steps = row.get("steps")
    evid = str(row.get("evidence_id") or "").strip()
    # Canonical execution id: persisted column preferred, then embedded payload, never forced to evidence_id.
    col_run = str(row.get("run_id") or "").strip()

    if isinstance(result, dict):
        payload: Dict[str, Any] = {**result}
        canon_run = col_run or _safe_str(result.get("run_id")) or evid
        payload["run_id"] = canon_run
        if evid:
            payload.setdefault("evidence_id", evid)
        if isinstance(steps, list):
            payload["steps"] = steps
        elif not payload.get("steps"):
            payload["steps"] = []
        prev_meta = _parse_jsonb(payload.get("meta"))
        payload["meta"] = {**prev_meta, **meta} if (prev_meta or meta) else (prev_meta or meta or {})
        if row.get("status") is not None:
            payload["status"] = row.get("status")
        if row.get("duration_ms") is not None:
            payload["duration_ms"] = row.get("duration_ms")
        es = row.get("error_summary")
        if es is not None:
            payload["error_summary"] = es
            payload.setdefault("error_message", es)
        if row.get("evidence_url"):
            payload["evidence_url"] = row.get("evidence_url")
        if row.get("report_url"):
            payload["report_url"] = row.get("report_url")
        if row.get("test_name") is not None:
            payload["test_name"] = row.get("test_name")
        if row.get("created_at") is not None:
            payload["created_at"] = row.get("created_at")
        if row.get("thread_id") is not None:
            payload.setdefault("thread_id", row.get("thread_id"))
        tc = _effective_test_case_id(row, meta)
        if tc:
            payload["test_case_id"] = tc
        return payload

    steps_list = steps if isinstance(steps, list) else []
    tc = _effective_test_case_id(row, meta)
    canon_run = col_run or _safe_str(meta.get("run_id")) or evid
    return {
        "evidence_id": evid or None,
        "run_id": canon_run,
        "status": row.get("status") or "failed",
        "duration_ms": row.get("duration_ms"),
        "steps": steps_list,
        "meta": meta,
        "test_name": row.get("test_name"),
        "test_case_id": tc or None,
        "error_summary": row.get("error_summary"),
        "error_message": row.get("error_summary"),
        "evidence_url": row.get("evidence_url"),
        "report_url": row.get("report_url"),
        "created_at": row.get("created_at"),
        "thread_id": row.get("thread_id"),
        "source": (meta.get("source") if isinstance(meta, dict) else None) or "chat",
    }


def canonical_from_qa_row(row: Dict[str, Any]) -> CanonicalRun:
    return run_from_legacy_store(qa_runs_row_to_legacy_payload(row))


def fetch_qa_runs_legacy_payload(run_id: str) -> Optional[Dict[str, Any]]:
    """Single run as legacy dict for GET /runs/{run_id} (HTML + JSON merge). Resolves by ``run_id`` column."""
    rid = (run_id or "").strip()
    if not rid or not supabase_qa_runs_enabled():
        return None
    try:
        from services.supabase_store import supabase_client

        sb = supabase_client()
        if sb is None:
            return None
        res = (
            sb.table("qa_runs")
            .select("*")
            .eq("run_id", rid)
            .limit(1)
            .execute()
        )
        rows = getattr(res, "data", None) or []
        if not rows:
            return None
        return qa_runs_row_to_legacy_payload(rows[0] if isinstance(rows[0], dict) else dict(rows[0]))
    except Exception:
        logger.exception("qa_runs_read: fetch legacy payload failed for run_id=%r", run_id)
        return None


def fetch_qa_run_canonical(run_id: str) -> Optional[CanonicalRun]:
    p = fetch_qa_runs_legacy_payload(run_id)
    if not p:
        return None
    try:
        return run_from_legacy_store(p)
    except Exception:
        logger.exception("qa_runs_read: canonical mapping failed for %r", run_id)
        return None


def list_qa_runs_canonical(
    *,
    test_case_id: Optional[str] = None,
    project_id: Optional[str] = None,
    limit: int = 100,
) -> List[CanonicalRun]:
    if not supabase_qa_runs_enabled():
        return []
    limit = max(1, min(int(limit), 500))
    fetch_cap = min(500, max(limit * 5, limit))

    allowed: Optional[List[str]] = None
    if project_id:
        from services.db.catalog_repository import catalog_repo

        allowed = catalog_repo.list_test_case_ids_for_project(project_id)
        # Do NOT return early when allowed is empty: rows with meta.project_id
        # or unassigned rows may still qualify for this project.

    try:
        from services.supabase_store import supabase_client

        sb = supabase_client()
        if sb is None:
            return []
        res = (
            sb.table("qa_runs")
            .select("*")
            .order("created_at", desc=True)
            .limit(fetch_cap)
            .execute()
        )
        raw_rows = getattr(res, "data", None) or []
    except Exception:
        logger.exception("qa_runs_read: list failed")
        return []

    out: List[CanonicalRun] = []
    for row in raw_rows:
        if not isinstance(row, dict):
            continue
        meta = _parse_jsonb(row.get("meta"))
        tc = _effective_test_case_id(row, meta)
        if test_case_id:
            if tc != (test_case_id or "").strip():
                continue
        else:
            if tc == "_async":
                continue
        if project_id:
            row_project = meta.get("project_id") if isinstance(meta, dict) else None
            if row_project:
                # Row has explicit project association — use it as the authority.
                if row_project != project_id:
                    continue  # belongs to a different project
            else:
                # No explicit project in meta: fall back to catalog linkage.
                # IMPORTANT: only compare proper catalog IDs (meta.test_case_id /
                # meta.test_id) against the allowed set.  Do NOT use test_name as
                # a catalog key — test_name is a display label, not an ID, and
                # will never appear in catalog_repo results, causing valid runs to
                # be incorrectly excluded.
                catalog_tc = (
                    (meta.get("test_case_id") or meta.get("test_id") or "").strip()
                    if isinstance(meta, dict) else ""
                )
                if allowed and catalog_tc and catalog_tc not in allowed:
                    continue
                # No catalog_tc, empty allowed, or catalog match → include conservatively.
        try:
            cr = canonical_from_qa_row(row)
            if cr.artifacts:
                lean = cr.artifacts.model_copy(update={"screenshot_b64": None})
                cr = cr.model_copy(update={"artifacts": lean})
            out.append(cr)
        except Exception:
            logger.debug("qa_runs_read: skip bad row", exc_info=True)
            continue
        if len(out) >= limit:
            break
    return out
