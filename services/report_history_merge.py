# services/report_history_merge.py
"""
Merge SQLite report rows with Supabase mirrors for unified reads.

Conflict resolution: newest ``created_at`` wins; tie or invalid timestamp → Supabase.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Optional


def parse_report_created_at(ts: Any) -> Optional[datetime]:
    if ts is None or not str(ts).strip():
        return None
    s = str(ts).strip().replace("Z", "+00:00")
    try:
        dt = datetime.fromisoformat(s)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return dt
    except (TypeError, ValueError):
        return None


def pick_newer_record(
    *,
    sqlite_row: Optional[Dict[str, Any]],
    supabase_row: Optional[Dict[str, Any]],
    sort_key: str = "created_at",
) -> Optional[Dict[str, Any]]:
    """
    Resolve duplicate id across stores.

    - Newer ``created_at`` wins.
    - Invalid/missing timestamp on one side → the other wins.
    - Both invalid or equal → Supabase wins (legacy fallback).
    """
    if sqlite_row is None:
        return supabase_row
    if supabase_row is None:
        return sqlite_row

    sqlite_dt = parse_report_created_at(sqlite_row.get(sort_key))
    supabase_dt = parse_report_created_at(supabase_row.get(sort_key))

    if sqlite_dt is None and supabase_dt is None:
        return supabase_row
    if sqlite_dt is None:
        return supabase_row
    if supabase_dt is None:
        return sqlite_row
    if sqlite_dt > supabase_dt:
        return sqlite_row
    if supabase_dt > sqlite_dt:
        return supabase_row
    return supabase_row


def _sort_rows_by_created_at(
    rows: List[Dict[str, Any]],
    *,
    sort_key: str = "created_at",
) -> List[Dict[str, Any]]:
    min_dt = datetime.min.replace(tzinfo=timezone.utc)

    def _key(row: Dict[str, Any]) -> datetime:
        return parse_report_created_at(row.get(sort_key)) or min_dt

    return sorted(rows, key=_key, reverse=True)


def merge_sqlite_supabase_records(
    *,
    supabase_rows: List[Dict[str, Any]],
    sqlite_rows: List[Dict[str, Any]],
    limit: int,
    id_key: str = "id",
    sort_key: str = "created_at",
) -> List[Dict[str, Any]]:
    limit = max(1, min(int(limit), 500))
    if not supabase_rows:
        return _sort_rows_by_created_at(sqlite_rows, sort_key=sort_key)[:limit]

    by_supa: Dict[str, Dict[str, Any]] = {}
    for row in supabase_rows:
        rid = str(row.get(id_key) or "").strip()
        if rid:
            by_supa[rid] = row

    by_sqlite: Dict[str, Dict[str, Any]] = {}
    for row in sqlite_rows:
        rid = str(row.get(id_key) or "").strip()
        if rid:
            by_sqlite[rid] = row

    merged: List[Dict[str, Any]] = []
    for rid in set(by_supa) | set(by_sqlite):
        winner = pick_newer_record(
            sqlite_row=by_sqlite.get(rid),
            supabase_row=by_supa.get(rid),
            sort_key=sort_key,
        )
        if winner:
            merged.append(winner)

    return _sort_rows_by_created_at(merged, sort_key=sort_key)[:limit]
