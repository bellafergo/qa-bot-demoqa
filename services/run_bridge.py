# services/run_bridge.py
"""
Run Bridge — Chat/Execute → SQLite
===================================
Best-effort persistence of runs that flow through run_store (chat, async execute,
PR-triggered) into the official SQLite store (test_run_repo / data/vanya.db).

This module is called by run_store.save_run() for completed/failed payloads so
that chat/execute runs appear in GET /test-runs alongside catalog runs.
After a successful persist, services.alerting may schedule a Slack auto-alert
for fail/error (same pattern as catalog runs via test_catalog_service._save_run).

Design constraints
------------------
• Best-effort only — bridge_run_to_sqlite() NEVER raises; failures return False
  and are logged at ERROR (run_store also sets meta.durable_persist_failed).
• Only final states are bridged; queued / running payloads are skipped.
• Catalog runs reach test_run_repo via catalog_service._save_run() and are mirrored
  to Supabase ``qa_runs`` there (not via run_store), so there is no double SQLite row
  from the bridge for the same catalog execution.
• test_case_id is required by the SQLite schema.  For chat/execute runs it
  defaults to "_chat" unless one is provided in meta.

Field mapping (run_store payload → TestRun)
--------------------------------------------
evidence_id / run_id  → run_id + evidence_id
status (arbitrary)    → mapped to "pass" / "fail" / "error"
started_at / created_at (ms epoch or ISO) → executed_at (datetime)
duration_ms           → duration_ms
steps / steps_result  → steps_result
logs                  → logs (capped at 500 entries)
evidence_url          → evidence_url
report_url            → report_url
meta                  → meta (enriched: source=chat, bridge_persisted=True)
screenshot_b64        → meta.screenshot_b64 (EvidenceCard reads it from there)
meta.test_case_id     → test_case_id (or "_chat")
meta.domain / prompt  → test_name

Source label
------------
All bridged runs have meta["source"] = "chat" so they can be distinguished from
catalog runs (meta["source"] = "catalog") in dashboards and analysis queries.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict

from models.test_run import TestRun
from services.db.test_run_repository import test_run_repo

logger = logging.getLogger("vanya.run_bridge")

# States that are intermediate (bridged separately via bridge_async_run_to_sqlite)
_ASYNC_STATUSES = frozenset({"queued", "running", "pending"})

# Map arbitrary run_store status strings → TestRun's status set
# TestRun accepts: "pass" | "fail" | "error" | "running" | "queued"
_STATUS_MAP: Dict[str, str] = {
    "passed":    "pass",
    "pass":      "pass",
    "completed": "pass",
    "ok":        "pass",
    "failed":    "fail",
    "fail":      "fail",
    "error":     "error",
    "partial":   "fail",
    "queued":    "queued",
    "running":   "running",
    "pending":   "queued",
    "planning":  "planning",
    "compiled":  "compiled",
    "canceled":  "canceled",
    "cancelled": "canceled",
}


def _map_status(raw: str) -> str:
    return _STATUS_MAP.get(str(raw or "").strip().lower(), "error")


def _to_datetime(value: Any) -> datetime:
    """Convert ms-epoch int, ISO string, or datetime to a timezone-aware datetime."""
    if isinstance(value, datetime):
        return value if value.tzinfo else value.replace(tzinfo=timezone.utc)
    if isinstance(value, (int, float)):
        return datetime.fromtimestamp(value / 1000, tz=timezone.utc)
    if isinstance(value, str):
        try:
            return datetime.fromisoformat(value)
        except ValueError:
            pass
    return datetime.now(timezone.utc)


def bridge_async_run_to_sqlite(payload: Dict[str, Any]) -> bool:
    """
    Persist queued/running runs to SQLite so GET /runs/{run_id} can resolve
    them even when the request hits a different process (e.g. multi-instance).
    Uses test_case_id="_async" to distinguish from catalog runs.
    Returns True on success, False on skip or error. Never raises.
    """
    try:
        raw_status = str(payload.get("status") or "").strip().lower()
        if raw_status not in _ASYNC_STATUSES:
            return False

        evidence_id = str(payload.get("evidence_id") or payload.get("run_id") or "").strip()
        if not evidence_id:
            return False

        meta: Dict[str, Any] = dict(payload.get("meta") or {})
        meta.setdefault("source", "chat")
        meta["evidence_id"] = evidence_id

        executed_at = _to_datetime(
            payload.get("created_at") or payload.get("started_at")
        )

        status_sql = _STATUS_MAP.get(raw_status, "queued")
        if status_sql not in ("queued", "running"):
            status_sql = "queued"

        tr = TestRun(
            run_id=evidence_id,
            test_case_id="_async",
            test_name=meta.get("test_name") or "Async Run",
            executed_at=executed_at,
            environment=meta.get("environment") or "default",
            status=status_sql,
            duration_ms=int(payload.get("duration_ms") or 0),
            evidence_url=None,
            report_url=None,
            evidence_id=evidence_id,
            logs=[],
            steps_result=[],
            meta=meta,
        )

        test_run_repo.create_run(tr)
        logger.debug("run_bridge: persisted async run %s status=%s", evidence_id, status_sql)
        return True

    except Exception as exc:
        logger.error(
            "run_bridge: failed to persist async %s — %s: %s",
            payload.get("evidence_id", "?"), type(exc).__name__, exc,
            exc_info=True,
        )
        return False


def bridge_run_to_sqlite(payload: Dict[str, Any]) -> bool:
    """
    Persist a run_store payload to SQLite test_run_repo.

    Called best-effort by run_store.save_run() for every completed/failed run.
    Returns True on success, False on skip or error.  Never raises.
    """
    try:
        # ── Skip intermediate states (handled by bridge_async_run_to_sqlite) ───
        raw_status = str(payload.get("status") or "").strip().lower()
        if raw_status in _ASYNC_STATUSES:
            return False

        # ── Require an evidence_id ────────────────────────────────────────────
        evidence_id = str(payload.get("evidence_id") or payload.get("run_id") or "").strip()
        if not evidence_id:
            logger.debug("run_bridge: skip — no evidence_id in payload")
            return False

        # ── Build enriched meta ───────────────────────────────────────────────
        meta: Dict[str, Any] = dict(payload.get("meta") or {})
        meta.setdefault("source", "chat")      # label for analytics
        meta["bridge_persisted"] = True
        meta["evidence_id"] = evidence_id

        # Move screenshot_b64 into meta so EvidenceCard finds it via
        # run_from_catalog_testrun → artifacts.screenshot_b64 = meta["screenshot_b64"]
        screenshot_b64 = (
            payload.get("screenshot_b64")
            or meta.get("screenshot_b64")
            or ""
        )
        if screenshot_b64 and not meta.get("screenshot_b64"):
            meta["screenshot_b64"] = screenshot_b64

        # Preserve base_url if present at top level
        if payload.get("base_url") and not meta.get("base_url"):
            meta["base_url"] = payload["base_url"]

        # ── Build TestRun ─────────────────────────────────────────────────────
        # test_case_id: prefer explicit value, fallback to sentinel
        test_case_id = (
            meta.get("test_case_id")
            or payload.get("test_case_id")
            or "_chat"
        )

        # test_name: try several sources in priority order
        test_name = (
            meta.get("test_name")
            or meta.get("domain")
            or (str(payload.get("prompt") or "")[:80].strip() or None)
            or "Chat / Execute Run"
        )

        executed_at = _to_datetime(
            payload.get("started_at") or payload.get("created_at")
        )

        tr = TestRun(
            run_id       = evidence_id,
            test_case_id = test_case_id,
            test_name    = test_name,
            executed_at  = executed_at,
            environment  = meta.get("environment") or "default",
            status       = _map_status(raw_status),
            duration_ms  = int(payload.get("duration_ms") or 0),
            evidence_url = payload.get("evidence_url") or meta.get("evidence_url"),
            report_url   = payload.get("report_url")   or meta.get("report_url"),
            evidence_id  = evidence_id,
            logs         = list(payload.get("logs") or [])[:500],   # cap volume
            steps_result = list(
                payload.get("steps") or payload.get("steps_result") or []
            ),
            meta         = meta,
        )

        test_run_repo.create_run(tr)
        logger.info(
            "run_bridge: persisted run %s → SQLite  source=chat status=%s",
            evidence_id, tr.status,
        )
        try:
            from services.alerting import schedule_slack_alert_on_failed_run

            schedule_slack_alert_on_failed_run(tr)
        except Exception:
            pass
        return True

    except Exception as exc:
        logger.error(
            "run_bridge: failed to persist %s — %s: %s",
            payload.get("evidence_id", "?"), type(exc).__name__, exc,
            exc_info=True,
        )
        return False
