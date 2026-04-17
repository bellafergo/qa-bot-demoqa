# services/run_history_service.py
"""
Run History Service — canonical read facade for persisted run history
======================================================================

Architecture (writes vs reads)
------------------------------
Layer                  | Role                              | Technology
-----------------------|-----------------------------------|-------------------
run_access.persist_*   | Preferred entry for new writes    | → run_store.save_run
run_store.save_run     | Hot cache + bridge to SQLite      | optional in-memory TTL (not SQLite)
run_bridge             | Chat/execute/API → SQLite         | durable rows; failures logged + meta flag
test_run_repo          | Source of truth for listings      | SQLite / SQLAlchemy (no auto TTL)
run_history_service    | Canonical reads (this module)     | qa_runs (if Supabase) else SQLite
Supabase               | Optional mirror                     | best-effort from save_run

Rules
-----
• **Listings** (dashboard, ``GET /test-runs``, analytics): when Supabase is
  configured, ``qa_runs`` via ``qa_runs_read``; otherwise SQLite → ``CanonicalRun``.

• **get_run(run_id)**: Supabase ``qa_runs`` by canonical ``run_id`` column when configured;
  else SQLite ``test_runs`` by ``run_id``. Use **get_run_unified** when the id might exist only in memory.

• **Evidence** ``GET /runs/{run_id}``: ``run_store`` (``get_run_by_id`` then ``get_run``), then ``qa_runs`` when
  Supabase is configured, else SQLite — see ``app.get_run_evidence``.

• New code should persist via ``services.run_access.persist_run_payload`` so all
  writes share one entry point.

Usage
-----
    from services.run_history_service import run_history_service

    runs  = run_history_service.list_runs(limit=50)       # List[CanonicalRun]
    run   = run_history_service.get_run("uuid")         # qa_runs or SQLite by run_id
    one   = run_history_service.get_run_unified("uuid") # run_store, then persisted
"""
from __future__ import annotations

import logging
from typing import List, Optional

from models.run_contract import CanonicalRun
from services.db.test_run_repository import test_run_repo
from services.qa_runs_read import (
    fetch_qa_run_canonical,
    list_qa_runs_canonical,
    supabase_qa_runs_enabled,
)
from services.run_mapper import normalize_run, run_from_catalog_testrun

logger = logging.getLogger("vanya.run_history")


class RunHistoryService:
    """
    Official facade for reading test run history.

    All public methods return CanonicalRun objects — the mapper is applied
    here, not at the route layer, so every caller gets consistent types.
    """

    # ── Official history queries ──────────────────────────────────────────────

    def list_runs(
        self,
        *,
        test_case_id: Optional[str] = None,
        project_id: Optional[str] = None,
        limit: int = 100,
    ) -> List[CanonicalRun]:
        """
        Return recent persisted runs, most recent first.

        When Supabase is configured, reads ``public.qa_runs`` only. Otherwise
        SQLite (catalog ``test_runs``).

        Parameters
        ----------
        test_case_id : optional filter — only runs for this test case
        project_id   : optional catalog project slug — only runs for tests in that project
        limit        : maximum records to return (default 100, max 500)
        """
        limit = max(1, min(int(limit), 500))
        if supabase_qa_runs_enabled():
            mapped = list_qa_runs_canonical(
                test_case_id=test_case_id,
                project_id=project_id,
                limit=limit,
            )
        else:
            runs = test_run_repo.list_runs(
                test_case_id=test_case_id,
                project_id=project_id,
                limit=limit,
            )
            mapped = [run_from_catalog_testrun(r) for r in runs]
        # Strip screenshot_b64 from list payloads — large base64 blobs are only
        # needed for individual evidence views, not for history listings.
        result = []
        for r in mapped:
            if r.artifacts is None:
                result.append(r)
                continue
            try:
                lean_artifacts = r.artifacts.model_copy(update={"screenshot_b64": None})
                result.append(r.model_copy(update={"artifacts": lean_artifacts}))
            except Exception:
                # Old/incomplete run — return as-is rather than fail the listing
                result.append(r)
        return result

    def get_run(self, run_id: str) -> Optional[CanonicalRun]:
        """
        Return a single run by canonical ``run_id``. With Supabase configured, resolves the
        ``run_id`` column on ``qa_runs``. Otherwise SQLite ``test_runs.run_id``.

        Returns None if not found. For ids that may exist only in memory, use
        ``get_run_unified`` or ``GET /runs/{run_id}``.
        """
        if supabase_qa_runs_enabled():
            cr = fetch_qa_run_canonical((run_id or "").strip())
            if cr is None:
                logger.debug("run_history_service: run_id %r not found in qa_runs", run_id)
            return cr
        run = test_run_repo.get_run(run_id)
        if run is None:
            logger.debug("run_history_service: run_id %r not found in SQLite", run_id)
            return None
        return run_from_catalog_testrun(run)

    def get_run_unified(self, run_id: str) -> Optional[CanonicalRun]:
        """
        Resolve a run from SQLite history first, then the in-memory run_store
        (evidence_id or alternate run_id index). Used to persist catalog tests
        from planner/chat/execute flows that may only exist in run_store.
        """
        from services.run_store import get_run, get_run_by_id

        rid = (run_id or "").strip()
        if not rid:
            return None
        raw = get_run_by_id(rid) or get_run(rid)
        if raw is not None:
            try:
                return normalize_run(raw)
            except Exception:
                logger.exception("run_history_service: normalize_run failed for %r", run_id)
                return None
        cr = self.get_run(rid)
        if cr is not None:
            return cr
        logger.debug("run_history_service: unified run_id %r not found", run_id)
        return None

    # ── Aggregate read helpers (delegated from existing services) ─────────────

    def count_by_status(self) -> dict:
        """
        Return {status_string: count} over all persisted runs.

        Note: counts reflect the legacy status strings stored in SQLite
        ('pass', 'fail', 'error') — not canonical values.  Use
        normalize_status() from run_mapper if you need canonical counts.
        """
        return test_run_repo.count_by_status()

    def get_last_run_at(self) -> Optional[str]:
        """ISO-8601 timestamp of the most recent persisted run, or None."""
        return test_run_repo.get_last_executed_at()


# Module-level singleton — import this everywhere
run_history_service = RunHistoryService()
