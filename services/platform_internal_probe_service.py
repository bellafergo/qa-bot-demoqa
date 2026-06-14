# services/platform_internal_probe_service.py
"""Read-only probes for platform_internal database assets (no customer DB access)."""
from __future__ import annotations

import logging
import os
from typing import Dict, List, Tuple
from urllib.parse import urlparse

from sqlalchemy import text

from services.db.sqlite_db import get_session

logger = logging.getLogger("vanya.platform_internal_probe")

SQLITE_ALLOWLISTED_TABLES = (
    "database_connections",
    "local_agents",
    "database_validation_executions",
    "test_runs",
    "incident_investigation_reports",
    "browser_inspection_watches",
)

SUPABASE_ALLOWLISTED_TABLES = (
    "database_connections",
    "local_agents",
    "qa_runs",
    "incident_investigation_reports",
    "browser_inspection_watches",
)

PLATFORM_PROBE_CHECK_ID = "platform_probe:read_only_health"


def supabase_configured() -> bool:
    return bool((os.environ.get("SUPABASE_URL") or "").strip())


def sqlite_store_accessible() -> bool:
    path = (os.environ.get("VANYA_SQLITE_PATH") or "./data/vanya.db").strip()
    try:
        return os.path.isfile(path) and os.access(path, os.R_OK)
    except OSError:
        return False


def safe_supabase_host_label() -> str:
    raw = (os.environ.get("SUPABASE_URL") or "").strip()
    if not raw:
        return "supabase-unconfigured"
    try:
        host = urlparse(raw).hostname or "supabase"
        return host[:128]
    except Exception:
        return "supabase"


def safe_sqlite_host_label() -> str:
    return "vanya-platform-runtime"


def probe_sqlite_platform() -> Tuple[int, str, bool]:
    """Read-only COUNT(*) on allowlisted SQLite tables. Returns (row_count, summary, ok)."""
    counts: Dict[str, int] = {}
    try:
        with get_session() as s:
            for table in SQLITE_ALLOWLISTED_TABLES:
                try:
                    row = s.execute(text(f"SELECT COUNT(*) AS n FROM {table}")).first()
                    counts[table] = int(row[0]) if row else 0
                except Exception as exc:
                    logger.debug("sqlite probe skipped table=%s: %s", table, exc)
        if not counts:
            return 0, "SQLite probe: no allowlisted tables accessible.", False
        total = sum(counts.values())
        parts = ", ".join(f"{k}={v}" for k, v in sorted(counts.items()))
        return total, f"Read-only SQLite probe verified {len(counts)} table(s): {parts}.", True
    except Exception as exc:
        logger.warning("sqlite platform probe failed: %s", exc)
        return 0, "SQLite probe failed.", False


def probe_supabase_platform() -> Tuple[int, str, bool]:
    """Read-only count probes via PostgREST on allowlisted tables."""
    try:
        from services.run_store_supabase import _get_supabase
    except ImportError:
        return 0, "Supabase client unavailable.", False

    sb = _get_supabase()
    if sb is None:
        return 0, "Supabase not configured.", False

    counts: Dict[str, int] = {}
    for table in SUPABASE_ALLOWLISTED_TABLES:
        try:
            res = sb.table(table).select("*", count="exact").limit(1).execute()
            count_val = getattr(res, "count", None)
            if count_val is None:
                data = getattr(res, "data", None) or []
                count_val = len(data)
            counts[table] = int(count_val or 0)
        except Exception as exc:
            logger.debug("supabase probe skipped table=%s: %s", table, exc)

    if not counts:
        return 0, "Supabase probe: no allowlisted tables accessible.", False
    total = sum(counts.values())
    parts = ", ".join(f"{k}={v}" for k, v in sorted(counts.items()))
    return total, f"Read-only Supabase probe verified {len(counts)} table(s): {parts}.", True


def execute_platform_internal_probe(connection: Dict[str, object]) -> Tuple[int, str, bool]:
    """Run a platform-internal read-only probe for the given connection row."""
    db_type = str(connection.get("database_type") or "").lower()
    scope = str(connection.get("asset_scope") or "")
    mode = str(connection.get("execution_mode") or "")
    if scope != "platform_internal" or mode != "platform_backend":
        return 0, "Not a platform_internal asset.", False
    if db_type == "sqlite":
        return probe_sqlite_platform()
    if db_type == "postgresql":
        return probe_supabase_platform()
    return 0, f"Unsupported platform database_type: {db_type}", False
