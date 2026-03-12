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
