# services/run_access.py
"""
Unified run access — single entry points for persistence and canonical reads
=============================================================================

Use this module for **new** code paths instead of importing ``run_store`` or
``test_run_repo`` directly. Existing callers may migrate gradually.

Architecture (single logical pipeline)
--------------------------------------
1. **Writes** — Always go through ``persist_run_payload()`` → ``run_store.save_run``:
   - In-memory cache (PR/tag indexes, fast ``GET /runs``); optional TTL via
     ``RUNS_TTL_S`` only trims this cache — **never** SQLite rows.
   - Optional Supabase mirror (if configured); failures are logged.
   - ``run_bridge`` → SQLite ``test_run_repo`` for durable history (terminal and
     async states). Failures are logged at ERROR and ``meta.durable_persist_failed``.

2. **Durable history** — SQLite ``test_run_repo`` is the source of truth for
   listings (dashboard, analytics, ``GET /test-runs``). Rows are not TTL-purged
   by default. Optional future soft-delete uses ``deleted_at`` (no automatic deletes).

3. **Reads (canonical)** — ``get_canonical_run()`` → ``run_history_service.get_run_unified``:
   SQLite first, then in-memory ``run_store`` (only while a run is still hot / not yet bridged).

4. **Normalization** — ``CanonicalRun`` (``models/run_contract.py``) + ``run_mapper``.

Hot-path catalog execution still uses ``test_catalog_service._save_run`` → repo;
those rows are the same SQLite store; no duplicate semantics.

Example ``CanonicalRun``-aligned JSON (illustrative)
----------------------------------------------------
{
  "run_id": "uuid-canonical",
  "evidence_id": "EV-abc123",
  "test_id": "TC-LOGIN-001",
  "test_name": "Login flow",
  "source": "chat",
  "status": "passed",
  "started_at": "2026-04-06T12:00:00+00:00",
  "finished_at": "2026-04-06T12:01:30+00:00",
  "duration_ms": 90000,
  "steps_count": 5,
  "summary": "5/5 steps passed",
  "error_summary": null,
  "steps": [{"action": "goto", "status": "passed"}],
  "artifacts": {"screenshot_b64": null, "evidence_url": "https://...", "report_url": null},
  "meta": {"trigger_source": "chat", "base_url": "https://example.com"},
  "evidence_url": "https://...",
  "report_url": null
}
"""
from __future__ import annotations

from typing import Any, Dict, Optional

from models.run_contract import CanonicalRun
from services.run_history_service import run_history_service
from services.run_store import save_run


def persist_run_payload(payload: Dict[str, Any]) -> Optional[str]:
    """
    Persist a run dict from chat, planner, workers, or API execute.

    Returns the in-memory cache key (legacy name ``evidence_id``; often equal to ``run_id``) on success, or ``None`` if invalid.
    Side effects: memory store + optional SQLite bridge + optional Supabase.
    """
    return save_run(payload)


def get_canonical_run(run_id: str) -> Optional[CanonicalRun]:
    """
    Resolve a run by canonical ``run_id`` (and hot-cache aliases via ``get_run_unified``).

    Prefer this over ad-hoc ``get_run`` + ``run_history_service.get_run`` chains.
    """
    return run_history_service.get_run_unified((run_id or "").strip())
