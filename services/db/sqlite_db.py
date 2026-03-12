# services/db/sqlite_db.py
"""
SQLite engine for the Vanya catalog/runs/jobs DB.

Separate from the existing local.db (threads/messages in db.py).

Configure path via:
  VANYA_SQLITE_PATH=./data/vanya.db   (default)

Upgrade path: replace the engine URL with a Postgres/Supabase DSN.
"""
from __future__ import annotations

import os
from contextlib import contextmanager
from pathlib import Path

from sqlalchemy import create_engine, event
from sqlalchemy.orm import declarative_base, sessionmaker, Session

# ── Path / engine setup ───────────────────────────────────────────────────────

VANYA_SQLITE_PATH: str = os.getenv("VANYA_SQLITE_PATH", "./data/vanya.db")

# Ensure the data directory exists when using a file-based path
_path_obj = Path(VANYA_SQLITE_PATH)
if _path_obj.name != ":memory:":
    _path_obj.parent.mkdir(parents=True, exist_ok=True)

engine = create_engine(
    f"sqlite:///{VANYA_SQLITE_PATH}",
    connect_args={"check_same_thread": False},
    pool_pre_ping=True,
)

# Enable WAL mode for better concurrent read/write performance
@event.listens_for(engine, "connect")
def _set_wal_mode(dbapi_conn, _):
    dbapi_conn.execute("PRAGMA journal_mode=WAL")
    dbapi_conn.execute("PRAGMA foreign_keys=ON")


SessionLocal = sessionmaker(bind=engine, autoflush=False, autocommit=False, expire_on_commit=False)

Base = declarative_base()


# ── Session helper ────────────────────────────────────────────────────────────

@contextmanager
def get_session() -> Session:
    """Context manager: opens a session, commits on exit, rolls back on error."""
    session: Session = SessionLocal()
    try:
        yield session
        session.commit()
    except Exception:
        session.rollback()
        raise
    finally:
        session.close()
