# services/db/init_db.py
"""
Initialize all Vanya catalog/runs/jobs tables.

Safe to call multiple times — uses CREATE TABLE IF NOT EXISTS semantics
via SQLAlchemy's `Base.metadata.create_all(checkfirst=True)`.

Call once on application startup before any repository operations.
"""
from __future__ import annotations

import logging

logger = logging.getLogger("vanya.db.init")


def init_catalog_db() -> None:
    """Create all catalog/run/job tables if they do not exist."""
    # Import ORM models to register them with Base.metadata
    from services.db.sqlite_db import engine, Base
    from services.db import catalog_repository      # noqa: F401 — registers TestCaseRow
    from services.db import test_run_repository     # noqa: F401 — registers TestRunRow
    from services.db import orchestrator_job_repository  # noqa: F401 — registers OrchestratorJobRow

    Base.metadata.create_all(bind=engine, checkfirst=True)
    logger.info("db: catalog tables initialized (SQLite)")

    # ── Schema migrations (idempotent) ────────────────────────────────────────
    # Add test_type column to existing test_cases tables that pre-date this feature.
    # SQLite does not support ADD COLUMN IF NOT EXISTS, so we catch the error.
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(test_cases)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if "test_type" not in existing_cols:
                conn.execute(text("ALTER TABLE test_cases ADD COLUMN test_type TEXT DEFAULT 'ui'"))
                conn.commit()
                logger.info("db: migrated test_cases — added test_type column")
    except Exception:
        logger.exception("db: migration for test_type failed (non-fatal)")

    # ── orchestrator_jobs: parallel-execution block columns ───────────────────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(orchestrator_jobs)"))
            existing_cols = {row[1] for row in result.fetchall()}
            migrations = [
                ("retry_count",      "ALTER TABLE orchestrator_jobs ADD COLUMN retry_count INTEGER DEFAULT 0"),
                ("skipped_count",    "ALTER TABLE orchestrator_jobs ADD COLUMN skipped_count INTEGER DEFAULT 0"),
                ("scheduling_notes", "ALTER TABLE orchestrator_jobs ADD COLUMN scheduling_notes TEXT"),
            ]
            for col_name, sql in migrations:
                if col_name not in existing_cols:
                    conn.execute(text(sql))
                    logger.info("db: migrated orchestrator_jobs — added %s column", col_name)
            conn.commit()
    except Exception:
        logger.exception("db: migration for orchestrator_jobs parallel-execution columns failed (non-fatal)")
