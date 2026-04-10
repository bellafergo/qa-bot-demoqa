# services/db/catalog_repository.py
"""
Test catalog persistence facade.

- **Supabase** (Postgres via PostgREST) when ``SUPABASE_STRICT=1`` (e.g. Render production).
  Catalog survives redeploys; requires ``SUPABASE_URL`` + ``SUPABASE_SERVICE_ROLE_KEY``
  and table ``public.test_cases`` (see ``supabase/migration.sql``).

- **SQLite** when ``SUPABASE_STRICT`` is off or unset — local dev and tests.

The active backend is chosen on **first use** of ``catalog_repo`` (lazy), reading
``os.environ`` so tests can ``monkeypatch.setenv`` + ``reset_catalog_repository_impl()``.

ORM model ``TestCaseRow`` always lives in SQLite metadata for ``init_catalog_db()``;
strict mode does not create SQLite rows for catalog data.
"""
from __future__ import annotations

import logging
import os
from typing import Any, Optional

from services.db.catalog_repository_sqlite import CatalogRepositorySqlite, TestCaseRow

# Backward-compat name for tests / scripts that instantiate the SQLite repo directly.
CatalogRepository = CatalogRepositorySqlite

logger = logging.getLogger("vanya.db.catalog")

_impl: Optional[Any] = None


def reset_catalog_repository_impl() -> None:
    """
    Forget the cached repository implementation.

    Intended for tests after changing ``SUPABASE_STRICT`` or mocking Supabase.
    """
    global _impl
    _impl = None


def get_catalog_repository_impl() -> Any:
    """Return the active catalog repository (SQLite or Supabase)."""
    global _impl
    if _impl is not None:
        return _impl

    strict = (os.getenv("SUPABASE_STRICT", "0").strip() == "1")
    if strict:
        from services.db.catalog_repository_supabase import CatalogRepositorySupabase

        _impl = CatalogRepositorySupabase()
        logger.info("catalog_repo: active_backend=supabase (SUPABASE_STRICT=1)")
    else:
        _impl = CatalogRepositorySqlite()
        logger.info("catalog_repo: active_backend=sqlite (SUPABASE_STRICT off)")

    return _impl


class _CatalogRepoProxy:
    """Delegates all public repository methods to the lazily chosen implementation."""

    def __getattr__(self, name: str) -> Any:
        return getattr(get_catalog_repository_impl(), name)


catalog_repo = _CatalogRepoProxy()

__all__ = [
    "TestCaseRow",
    "CatalogRepository",
    "CatalogRepositorySqlite",
    "catalog_repo",
    "get_catalog_repository_impl",
    "reset_catalog_repository_impl",
]
