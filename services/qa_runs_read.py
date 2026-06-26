# services/qa_runs_read.py
"""
Read path for public.qa_runs (Supabase). Used when SUPABASE_URL + service key are set.

Maps table rows to legacy-shaped dicts for GET /runs and to CanonicalRun via run_from_legacy_store.
"""
from __future__ import annotations

import json
import logging
import time
from typing import Any, Callable, Dict, List, Optional, TypeVar

from services.supabase_http import is_transient_supabase_transport_error

T = TypeVar("T")

from models.run_contract import CanonicalRun
from services.run_mapper import run_from_legacy_store

logger = logging.getLogger("vanya.qa_runs_read")

_QA_RUNS_MAX_ATTEMPTS = 3
_QA_RUNS_RETRY_BASE_S = 0.15


def _with_transport_retry(op: Callable[[], T], *, what: str) -> T:
    last: Optional[BaseException] = None
    for attempt in range(1, _QA_RUNS_MAX_ATTEMPTS + 1):
        try:
            return op()
        except Exception as e:
            last = e
            if attempt >= _QA_RUNS_MAX_ATTEMPTS or not is_transient_supabase_transport_error(e):
                logger.exception(
                    "qa_runs_read: %s failed (attempt %s/%s) type=%s",
                    what,
                    attempt,
                    _QA_RUNS_MAX_ATTEMPTS,
                    type(e).__name__,
                )
                raise
            sleep_s = _QA_RUNS_RETRY_BASE_S * (2 ** (attempt - 1))
            logger.warning(
                "qa_runs_read: transient transport error on %s (%s: %s) — retry in %.2fs",
                what,
                type(e).__name__,
                e,
                sleep_s,
            )
            time.sleep(sleep_s)
    assert last is not None
    raise last

# List queries must NOT pull ``result`` (full run payload) — keeps Supabase Disk IO low.
# Extended columns require migration.sql ALTERs; fall back when PostgREST reports 42703.
QA_RUNS_LIST_COLUMNS_EXTENDED = (
    "evidence_id,run_id,status,test_name,duration_ms,error_summary,"
    "evidence_url,report_url,meta,created_at,thread_id,steps,updated_at"
)
QA_RUNS_LIST_COLUMNS_BASE = (
    "evidence_id,status,duration_ms,error_summary,"
    "evidence_url,report_url,meta,updated_at"
)
QA_RUNS_LIST_COLUMNS = QA_RUNS_LIST_COLUMNS_EXTENDED


def _safe_str(x: Any) -> str:
    try:
        return (str(x) if x is not None else "").strip()
    except Exception:
        return ""


def supabase_qa_runs_enabled() -> bool:
    from services.supabase_store import _is_configured  # noqa: SLF001

    return _is_configured()


def _is_missing_column_error(exc: BaseException) -> bool:
    try:
        from postgrest.exceptions import APIError

        if isinstance(exc, APIError):
            payload = getattr(exc, "args", (None,))[0]
            if isinstance(payload, dict) and str(payload.get("code") or "") == "42703":
                return True
    except Exception:
        pass
    msg = str(exc).lower()
    return "42703" in msg and "does not exist" in msg


def _fetch_qa_runs_list_rows(fetch_cap: int) -> List[Dict[str, Any]]:
    """Select qa_runs list columns, falling back when optional migration columns are absent."""
    from services.supabase_store import supabase_client

    last_exc: Optional[BaseException] = None
    for columns in (QA_RUNS_LIST_COLUMNS_EXTENDED, QA_RUNS_LIST_COLUMNS_BASE):
        try:
            def _op(cols: str = columns):
                sb = supabase_client()
                if sb is None:
                    return []
                res = (
                    sb.table("qa_runs")
                    .select(cols)
                    .order("updated_at", desc=True)
                    .limit(fetch_cap)
                    .execute()
                )
                return getattr(res, "data", None) or []

            rows = _with_transport_retry(
                _op,
                what=f"list_qa_runs(limit={fetch_cap}, columns={columns!r})",
            )
            if columns == QA_RUNS_LIST_COLUMNS_BASE:
                logger.warning(
                    "qa_runs_read: using base column set — apply supabase/migration.sql "
                    "(run_id, test_name, created_at) for full run history in System Memory."
                )
            return rows
        except Exception as e:
            last_exc = e
            if columns == QA_RUNS_LIST_COLUMNS_BASE or not _is_missing_column_error(e):
                raise
            logger.warning(
                "qa_runs_read: column set %r unavailable (%s) — retrying with base columns",
                columns,
                e,
            )
    if last_exc is not None:
        raise last_exc
    return []


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
    if not col_run:
        col_run = evid

    if isinstance(result, dict):
        payload: Dict[str, Any] = {**result}
        if isinstance(result.get("logs"), list):
            payload.setdefault("logs", result.get("logs"))
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


def _qa_row_matches_filters(
    row: Dict[str, Any],
    *,
    test_case_id: Optional[str],
    project_id: Optional[str],
    allowed: Optional[List[str]],
) -> bool:
    """Same scoping rules as ``list_qa_runs_canonical`` (project + catalog ids; excludes _async, _browser_inspection, and meta.source browser_inspection)."""
    meta = _parse_jsonb(row.get("meta"))
    tc = _effective_test_case_id(row, meta)
    if test_case_id:
        if tc != (test_case_id or "").strip():
            return False
    else:
        if tc in ("_async", "_browser_inspection"):
            return False
        if isinstance(meta, dict) and meta.get("source") == "browser_inspection":
            return False
    if project_id:
        row_project = meta.get("project_id") if isinstance(meta, dict) else None
        if row_project:
            if row_project != project_id:
                return False
        else:
            catalog_tc = (
                (meta.get("test_case_id") or meta.get("test_id") or "").strip()
                if isinstance(meta, dict) else ""
            )
            if allowed and catalog_tc and catalog_tc not in allowed:
                return False
    return True


def fetch_qa_runs_legacy_payload(run_id: str) -> Optional[Dict[str, Any]]:
    """Single run as legacy dict — resolves by run_id, evidence_id, or meta correlation/request id."""
    return fetch_qa_run_by_lookup_id(run_id)


def _qa_first_row(res: Any) -> Optional[Dict[str, Any]]:
    rows = getattr(res, "data", None) or []
    if not rows:
        return None
    row = rows[0]
    return row if isinstance(row, dict) else dict(row)


def fetch_qa_run_by_lookup_id(lookup_id: str) -> Optional[Dict[str, Any]]:
    """
    Resolve a qa_runs row by run_id, evidence_id, meta.correlation_id,
    meta.request_id, or short prefix (>= 8 chars) on run_id / evidence_id.
    """
    lid = (lookup_id or "").strip()
    if not lid or not supabase_qa_runs_enabled():
        return None
    try:
        from services.supabase_store import supabase_client

        sb = supabase_client()
        if sb is None:
            return None
        table = sb.table("qa_runs")

        for col in ("run_id", "evidence_id"):
            row = _qa_first_row(table.select("*").eq(col, lid).limit(1).execute())
            if row:
                return qa_runs_row_to_legacy_payload(row)

        for meta_key in ("correlation_id", "request_id"):
            row = _qa_first_row(
                table.select("*").eq(f"meta->>{meta_key}", lid).limit(1).execute()
            )
            if row:
                return qa_runs_row_to_legacy_payload(row)

        if len(lid) >= 8:
            for col in ("run_id", "evidence_id"):
                row = _qa_first_row(
                    table.select("*").like(col, f"{lid}%").limit(1).execute()
                )
                if row:
                    return qa_runs_row_to_legacy_payload(row)
    except Exception:
        logger.exception("qa_runs_read: lookup failed for %r", lookup_id)
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
        raw_rows = _fetch_qa_runs_list_rows(fetch_cap)
    except Exception:
        logger.exception(
            "qa_runs_read: list failed after retries (project_id=%r, limit=%s)",
            project_id,
            limit,
        )
        return []

    out: List[CanonicalRun] = []
    for row in raw_rows:
        if not isinstance(row, dict):
            continue
        if not _qa_row_matches_filters(
            row, test_case_id=test_case_id, project_id=project_id, allowed=allowed
        ):
            continue
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
