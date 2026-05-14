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
    from services.db import catalog_repository           # noqa: F401 — registers TestCaseRow
    from services.db import test_run_repository          # noqa: F401 — registers TestRunRow
    from services.db import orchestrator_job_repository  # noqa: F401 — registers OrchestratorJobRow
    from services.db import draft_repository             # noqa: F401 — registers DraftRow
    from services.db import test_version_repository      # noqa: F401 — registers TestVersionRow
    from services.db import project_repository_sqlite       # noqa: F401 — registers ProjectRow on SQLite Base
    from services.db import browser_inspection_watch_repository  # noqa: F401 — browser_inspection_watches tables
    from services.db import local_agent_repository  # noqa: F401 — Phase 4A local_agents / local_agent_jobs

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

    # ── test_cases.project_id (multi-project catalog) ──────────────────────────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(test_cases)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if "project_id" not in existing_cols:
                conn.execute(
                    text(
                        "ALTER TABLE test_cases ADD COLUMN project_id TEXT DEFAULT 'default'"
                    )
                )
                conn.commit()
                logger.info("db: migrated test_cases — added project_id column")
    except Exception:
        logger.exception("db: migration for project_id failed (non-fatal)")

    # ── test_cases.created_from / source_run_id (persist tests from runs) ─────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(test_cases)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if "created_from" not in existing_cols:
                conn.execute(text("ALTER TABLE test_cases ADD COLUMN created_from TEXT"))
                logger.info("db: migrated test_cases — added created_from column")
            if "source_run_id" not in existing_cols:
                conn.execute(text("ALTER TABLE test_cases ADD COLUMN source_run_id TEXT"))
                logger.info("db: migrated test_cases — added source_run_id column")
            conn.commit()
    except Exception:
        logger.exception("db: migration for created_from/source_run_id failed (non-fatal)")

    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            conn.execute(
                text(
                    "CREATE UNIQUE INDEX IF NOT EXISTS uq_test_cases_source_run_id "
                    "ON test_cases(source_run_id) WHERE source_run_id IS NOT NULL"
                )
            )
            conn.commit()
    except Exception:
        logger.exception("db: unique index uq_test_cases_source_run_id failed (non-fatal)")

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
                ("context_json",     "ALTER TABLE orchestrator_jobs ADD COLUMN context_json TEXT"),
                ("parent_job_id",    "ALTER TABLE orchestrator_jobs ADD COLUMN parent_job_id TEXT"),
                ("project_id",      "ALTER TABLE orchestrator_jobs ADD COLUMN project_id TEXT DEFAULT 'default'"),
            ]
            for col_name, sql in migrations:
                if col_name not in existing_cols:
                    conn.execute(text(sql))
                    logger.info("db: migrated orchestrator_jobs — added %s column", col_name)
            if "project_id" in {row[1] for row in conn.execute(text("PRAGMA table_info(orchestrator_jobs)")).fetchall()}:
                conn.execute(
                    text("UPDATE orchestrator_jobs SET project_id = 'default' WHERE project_id IS NULL")
                )
            conn.commit()
    except Exception:
        logger.exception("db: migration for orchestrator_jobs parallel-execution columns failed (non-fatal)")

    # ── test_runs.deleted_at (soft-delete placeholder; no auto-purge) ─────────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(test_runs)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if "deleted_at" not in existing_cols:
                conn.execute(text("ALTER TABLE test_runs ADD COLUMN deleted_at TEXT"))
                conn.commit()
                logger.info("db: migrated test_runs — added deleted_at column (soft delete)")
    except Exception:
        logger.exception("db: migration for test_runs.deleted_at failed (non-fatal)")

    # ── projects.settings_json (login_profile + variables per project) ───────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(projects)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if "settings_json" not in existing_cols:
                conn.execute(text("ALTER TABLE projects ADD COLUMN settings_json TEXT"))
                conn.commit()
                logger.info("db: migrated projects — added settings_json column")
    except Exception:
        logger.exception("db: migration for projects.settings_json failed (non-fatal)")

    # ── browser_inspection_watch_events.visual_meta_json (Phase 3D) ─────────────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(browser_inspection_watch_events)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if existing_cols and "visual_meta_json" not in existing_cols:
                conn.execute(text("ALTER TABLE browser_inspection_watch_events ADD COLUMN visual_meta_json TEXT"))
                conn.commit()
                logger.info("db: migrated browser_inspection_watch_events — added visual_meta_json")
    except Exception:
        logger.exception("db: migration for browser_inspection_watch_events.visual_meta_json failed (non-fatal)")

    # ── browser_inspection_watches — Phase 3E baseline + status ────────────────
    _watch_cols = [
        ("compare_mode", "ALTER TABLE browser_inspection_watches ADD COLUMN compare_mode TEXT DEFAULT 'last'"),
        ("baseline_inspection_id", "ALTER TABLE browser_inspection_watches ADD COLUMN baseline_inspection_id TEXT"),
        ("baseline_set_at", "ALTER TABLE browser_inspection_watches ADD COLUMN baseline_set_at TEXT"),
        ("baseline_updated_by", "ALTER TABLE browser_inspection_watches ADD COLUMN baseline_updated_by TEXT"),
        ("last_status", "ALTER TABLE browser_inspection_watches ADD COLUMN last_status TEXT"),
        ("last_effective_change_level", "ALTER TABLE browser_inspection_watches ADD COLUMN last_effective_change_level TEXT"),
        ("last_alert_at", "ALTER TABLE browser_inspection_watches ADD COLUMN last_alert_at TEXT"),
        ("last_run_error", "ALTER TABLE browser_inspection_watches ADD COLUMN last_run_error TEXT"),
    ]
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(browser_inspection_watches)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if not existing_cols:
                pass
            else:
                for col_name, sql in _watch_cols:
                    if col_name not in existing_cols:
                        conn.execute(text(sql))
                        logger.info("db: migrated browser_inspection_watches — added %s", col_name)
                conn.commit()
    except Exception:
        logger.exception("db: migration for browser_inspection_watches Phase3E failed (non-fatal)")

    # ── browser_inspection_watches.local_agent_id (Phase 4E) ─────────────────────
    try:
        from sqlalchemy import text

        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(browser_inspection_watches)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if existing_cols and "local_agent_id" not in existing_cols:
                conn.execute(text("ALTER TABLE browser_inspection_watches ADD COLUMN local_agent_id TEXT"))
                conn.commit()
                logger.info("db: migrated browser_inspection_watches — added local_agent_id")
    except Exception:
        logger.exception("db: migration for browser_inspection_watches.local_agent_id failed (non-fatal)")

    # ── browser_inspection_watch_events.event_type (Phase 3E) ─────────────────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(browser_inspection_watch_events)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if existing_cols and "event_type" not in existing_cols:
                conn.execute(
                    text(
                        "ALTER TABLE browser_inspection_watch_events ADD COLUMN event_type TEXT DEFAULT 'diff_generated'"
                    )
                )
                conn.commit()
                logger.info("db: migrated browser_inspection_watch_events — added event_type")
    except Exception:
        logger.exception("db: migration for browser_inspection_watch_events.event_type failed (non-fatal)")

    # ── browser_inspection_watches.last_visual_change_level (Phase 3F) ─────────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            result = conn.execute(text("PRAGMA table_info(browser_inspection_watches)"))
            existing_cols = {row[1] for row in result.fetchall()}
            if existing_cols and "last_visual_change_level" not in existing_cols:
                conn.execute(text("ALTER TABLE browser_inspection_watches ADD COLUMN last_visual_change_level TEXT"))
                conn.commit()
                logger.info("db: migrated browser_inspection_watches — added last_visual_change_level")
    except Exception:
        logger.exception("db: migration for browser_inspection_watches.last_visual_change_level failed (non-fatal)")

    # ── browser_inspection_watch_events indexes (Phase 3F, idempotent) ──────────
    try:
        from sqlalchemy import text
        with engine.connect() as conn:
            conn.execute(
                text(
                    "CREATE INDEX IF NOT EXISTS ix_bio_wevents_watch_created "
                    "ON browser_inspection_watch_events(watch_id, created_at)"
                )
            )
            conn.execute(
                text(
                    "CREATE INDEX IF NOT EXISTS ix_bio_wevents_watch_event_type "
                    "ON browser_inspection_watch_events(watch_id, event_type)"
                )
            )
            conn.commit()
            logger.info("db: ensured browser_inspection_watch_events Phase3F indexes")
    except Exception:
        logger.exception("db: migration for browser_inspection_watch_events indexes failed (non-fatal)")
