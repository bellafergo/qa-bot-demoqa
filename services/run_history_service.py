# services/run_history_service.py
"""
Run History Service — canonical read facade for persisted run history
======================================================================

Architecture (writes vs reads)
------------------------------
Layer                  | Role                              | Technology
-----------------------|-----------------------------------|-------------------
run_access.persist_*   | Preferred entry for new writes    | → run_store.save_run
run_store.save_run     | Write pipeline (memory + bridge)  | in-memory TTL + hooks
run_bridge             | Chat/execute/API → SQLite         | best-effort, on save_run
test_run_repo          | Durable rows + list_runs          | SQLite / SQLAlchemy
run_history_service    | Canonical reads (this module)     | SQLite + optional run_store
Supabase               | Optional mirror                     | best-effort from save_run

Rules
-----
• **Listings** (dashboard, ``GET /test-runs``, analytics): SQLite via
  ``list_runs`` → ``CanonicalRun``.

• **get_run(run_id)**: SQLite only. Use **get_run_unified** when the id might be
  ``evidence_id`` from chat/planner before or without a SQLite row.

• **Evidence HTML** ``GET /runs/{id}``: still checks ``run_store`` first (freshest
  blobs), then SQLite via this service — see ``app.get_run_evidence``.

• New code should persist via ``services.run_access.persist_run_payload`` so all
  writes share one entry point.

Usage
-----
    from services.run_history_service import run_history_service

    runs  = run_history_service.list_runs(limit=50)       # List[CanonicalRun]
    run   = run_history_service.get_run("uuid")         # SQLite only
    one   = run_history_service.get_run_unified("id")   # SQLite + run_store fallback
"""
from __future__ import annotations

import logging
from typing import List, Optional

from models.run_contract import CanonicalRun
from services.db.test_run_repository import test_run_repo
from services.run_mapper import run_from_catalog_testrun

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
        Return recent run records from SQLite (official persistent store),
        most recent first.

        Parameters
        ----------
        test_case_id : optional filter — only runs for this test case
        project_id   : optional catalog project slug — only runs for tests in that project
        limit        : maximum records to return (default 100, max 500)
        """
        limit = max(1, min(int(limit), 500))
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
        Return a single run record by its run_id from SQLite.

        Returns None if the run_id is not found in SQLite. For chat/planner ids
        that may exist only in ``run_store`` until bridged, use ``get_run_unified``
        or ``GET /runs/{evidence_id}``.
        """
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
        from services.run_mapper import normalize_run
        from services.run_store import get_run, get_run_by_id

        rid = (run_id or "").strip()
        if not rid:
            return None
        cr = self.get_run(rid)
        if cr is not None:
            return cr
        raw = get_run(rid) or get_run_by_id(rid)
        if raw is None:
            logger.debug("run_history_service: unified run_id %r not found", run_id)
            return None
        try:
            return normalize_run(raw)
        except Exception:
            logger.exception("run_history_service: normalize_run failed for %r", run_id)
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
