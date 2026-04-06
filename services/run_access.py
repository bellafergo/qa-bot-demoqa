# services/run_access.py
"""
Unified run access — single entry points for persistence and canonical reads
=============================================================================

Use this module for **new** code paths instead of importing ``run_store`` or
``test_run_repo`` directly. Existing callers may migrate gradually.

Architecture (single logical pipeline)
--------------------------------------
1. **Writes** — Always go through ``persist_run_payload()`` → ``run_store.save_run``:
   - In-memory TTL cache (evidence polling, PR indexes).
   - Best-effort Supabase mirror (if configured).
   - ``run_bridge`` → SQLite ``test_run_repo`` for terminal states (and async
     states for cross-worker polling).

2. **Durable history** — SQLite ``test_run_repo`` is the source for listings
   (dashboard, analytics, ``GET /test-runs``).

3. **Reads (canonical)** — ``get_canonical_run()`` → ``run_history_service.get_run_unified``:
   SQLite first, then in-memory ``run_store`` (planner/chat before bridge completes).

4. **Normalization** — ``CanonicalRun`` (``models/run_contract.py``) + ``run_mapper``.

Hot-path catalog execution still uses ``test_catalog_service._save_run`` → repo;
those rows are the same SQLite store; no duplicate semantics.

Example ``CanonicalRun``-aligned JSON (illustrative)
----------------------------------------------------
{
  "run_id": "uuid-or-evidence-id",
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

    Returns ``evidence_id`` on success, or ``None`` if the payload is invalid.
    Side effects: memory store + optional SQLite bridge + optional Supabase.
    """
    return save_run(payload)


def get_canonical_run(run_id: str) -> Optional[CanonicalRun]:
    """
    Resolve a run by ``run_id`` or ``evidence_id`` for catalog/from-run flows.

    Prefer this over ad-hoc ``get_run`` + ``run_history_service.get_run`` chains.
    """
    return run_history_service.get_run_unified((run_id or "").strip())
