# tests/test_catalog_repository_facade.py
from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import MagicMock

import pytest

from services.db.catalog_repository import (
    get_catalog_repository_impl,
    reset_catalog_repository_impl,
)
from services.db.catalog_repository_sqlite import CatalogRepositorySqlite
from services.db.catalog_repository_supabase import CatalogRepositorySupabase


@pytest.fixture(autouse=True)
def _reset_catalog_impl():
    reset_catalog_repository_impl()
    yield
    reset_catalog_repository_impl()


def test_get_impl_uses_sqlite_when_strict_off(monkeypatch):
    monkeypatch.delenv("SUPABASE_STRICT", raising=False)
    impl = get_catalog_repository_impl()
    assert isinstance(impl, CatalogRepositorySqlite)


def test_get_impl_uses_supabase_when_strict_on(monkeypatch):
    monkeypatch.setenv("SUPABASE_STRICT", "1")
    mock_sb = MagicMock()
    mock_sb.table.return_value.select.return_value.limit.return_value.execute.return_value = SimpleNamespace(
        data=[],
    )
    monkeypatch.setattr(
        "services.db.catalog_repository_supabase.supabase_client",
        lambda: mock_sb,
    )
    impl = get_catalog_repository_impl()
    assert isinstance(impl, CatalogRepositorySupabase)


def test_supabase_create_and_get_round_trip(monkeypatch):
    """With mocked PostgREST, create + get use insert + select (no SQLite)."""
    monkeypatch.setenv("SUPABASE_STRICT", "1")
    stored: dict = {}

    tbl = MagicMock()

    def _insert_side_effect(payload: dict):
        ins = MagicMock()

        def _ex():
            stored.clear()
            stored.update(payload)
            return SimpleNamespace(data=[dict(payload)])

        ins.execute = _ex
        return ins

    tbl.insert.side_effect = _insert_side_effect

    sel = MagicMock()
    lim = MagicMock()

    def _select_execute():
        return SimpleNamespace(data=[dict(stored)] if stored else [])

    lim.execute.side_effect = _select_execute
    sel.eq.return_value.limit.return_value = lim
    tbl.select.return_value = sel

    mock_sb = MagicMock()
    mock_sb.table.return_value = tbl

    monkeypatch.setattr(
        "services.db.catalog_repository_supabase.supabase_client",
        lambda: mock_sb,
    )

    from models.test_case import TestCase, TestStep

    tc = TestCase(
        test_case_id="TC-SB-FACADE-001",
        name="Supabase facade",
        module="m",
        type="smoke",
        priority="low",
        steps=[TestStep(action="goto", url="https://example.com")],
        project_id="default",
    )

    repo = CatalogRepositorySupabase()
    repo.create_test_case(tc)
    assert tbl.insert.called
    assert stored.get("test_case_id") == "TC-SB-FACADE-001"

    got = repo.get_test_case("TC-SB-FACADE-001")
    assert got is not None
    assert got.test_case_id == "TC-SB-FACADE-001"
    assert got.name == "Supabase facade"


def test_catalog_repo_proxy_delegates_to_sqlite(monkeypatch):
    monkeypatch.delenv("SUPABASE_STRICT", raising=False)
    from services.db import catalog_repository as cr

    reset_catalog_repository_impl()
    empty = cr.catalog_repo.is_empty()
    assert isinstance(empty, bool)
