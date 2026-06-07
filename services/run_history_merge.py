# services/run_history_merge.py
"""
Merge SQLite catalog runs with Supabase ``qa_runs`` for unified history reads.

When Supabase is configured, cloud rows are preferred for the same ``run_id``,
but SQLite rows are included when the mirror is empty, lagging, or failed to write.
"""
from __future__ import annotations

from typing import Dict, List, Optional

from models.run_contract import CanonicalRun
from services.qa_runs_read import supabase_qa_runs_enabled
from services.run_mapper import run_from_catalog_testrun


def merge_sqlite_supabase_runs(
    *,
    supabase_runs: List[CanonicalRun],
    sqlite_runs: List,
    limit: int,
) -> List[CanonicalRun]:
    """Order: newest SQLite first, then Supabase-only ids; dedupe by run_id (Supabase wins)."""
    limit = max(1, min(int(limit), 500))
    sqlite_crs = [run_from_catalog_testrun(r) for r in sqlite_runs]

    if not supabase_qa_runs_enabled() or not supabase_runs:
        return sqlite_crs[:limit]

    by_supa: Dict[str, CanonicalRun] = {cr.run_id: cr for cr in supabase_runs if cr.run_id}

    def _sort_key(cr: CanonicalRun) -> str:
        return str(cr.started_at or cr.finished_at or "")

    sqlite_sorted = sorted(sqlite_crs, key=_sort_key, reverse=True)
    supa_sorted = sorted(supabase_runs, key=_sort_key, reverse=True)
    used: set[str] = set()
    ordered: List[CanonicalRun] = []

    def _emit(cr: CanonicalRun) -> None:
        rid = cr.run_id
        if not rid or rid in used:
            return
        used.add(rid)
        ordered.append(by_supa.get(rid, cr))

    for sc in sqlite_sorted:
        _emit(sc)
        if len(ordered) >= limit:
            return ordered
    for cr in supa_sorted:
        _emit(cr)
        if len(ordered) >= limit:
            break
    return ordered
