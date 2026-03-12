# tests/conftest.py
"""
Pytest configuration for Vanya test suite.

Sets up a temporary SQLite database before any test module is imported,
so that all services that use the catalog/run/job repositories point at
an isolated, ephemeral DB rather than the development data/vanya.db.
"""
import os
import tempfile

# ── Set VANYA_SQLITE_PATH before any service module is imported ───────────────
# This must happen at module level (not inside a fixture) because SQLAlchemy
# creates the engine at import time when sqlite_db.py is first imported.

_tmpdir = tempfile.mkdtemp(prefix="vanya_test_")
os.environ["VANYA_SQLITE_PATH"] = os.path.join(_tmpdir, "test_vanya.db")


def pytest_configure(config):
    """Create tables once, before any test collection begins."""
    try:
        from services.db.init_db import init_catalog_db
        init_catalog_db()
    except Exception as exc:
        print(f"[conftest] WARNING: init_catalog_db() failed: {exc}")
