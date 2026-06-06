# tests/test_supabase_client_singleton.py
"""Ensure Supabase REST client is not recreated on every call (run_store hot path)."""
from __future__ import annotations

from unittest.mock import MagicMock, patch


def test_run_store_supabase_reuses_shared_client():
    shared = MagicMock(name="shared_supabase_client")

    with patch("services.supabase_store.is_supabase_configured", return_value=True), patch(
        "services.supabase_store.supabase_client", return_value=shared
    ) as mock_sb_client, patch("supabase.create_client") as mock_create:
        import services.run_store_supabase as rss

        rss._supabase_cached = None
        rss._supabase_init_logged = False

        a = rss._get_supabase()
        b = rss._get_supabase()

        assert a is shared
        assert b is shared
        mock_sb_client.assert_called()
        mock_create.assert_not_called()


def test_persist_run_supabase_does_not_create_client_each_call():
    shared = MagicMock(name="shared_supabase_client")
    table = MagicMock()
    shared.table.return_value = table
    table.upsert.return_value.execute.return_value = MagicMock(data=[{}])

    payload = {
        "evidence_id": "ev-singleton-test",
        "run_id": "ev-singleton-test",
        "status": "passed",
        "meta": {},
    }

    with patch("services.supabase_store.is_supabase_configured", return_value=True), patch(
        "services.supabase_store.supabase_client", return_value=shared
    ), patch("supabase.create_client") as mock_create:
        import services.run_store_supabase as rss

        rss._supabase_cached = None
        rss._supabase_init_logged = False

        assert rss.persist_run_supabase(payload) is True
        assert rss.persist_run_supabase(payload) is True

        mock_create.assert_not_called()
        assert shared.table.call_count == 2


def test_supabase_store_client_is_module_singleton():
    fake = MagicMock(name="created_once")

    import services.supabase_store as store

    store._supabase = None

    with patch("services.supabase_store._is_configured", return_value=True), patch(
        "supabase.create_client", return_value=fake
    ) as mock_create:
        first = store.supabase_client()
        second = store.supabase_client()

    assert first is fake
    assert second is fake
    assert mock_create.call_count == 1
