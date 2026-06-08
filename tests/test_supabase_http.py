# tests/test_supabase_http.py
"""HTTP/2 transport error classification and postgrest patch."""
from __future__ import annotations

from services.supabase_http import (
    exc_diagnostic,
    is_transient_supabase_transport_error,
    patch_postgrest_disable_http2,
)


def test_keyerror_int_is_transient_http2_stream():
    assert is_transient_supabase_transport_error(KeyError(3)) is True
    diag = exc_diagnostic(KeyError(3))
    assert diag["likely_http2_stream_id"] is True
    assert diag["keyerror_key"] == 3


def test_keyerror_string_is_not_http2_stream():
    assert is_transient_supabase_transport_error(KeyError("missing_field")) is False


def test_trailers_error_is_transient():
    assert is_transient_supabase_transport_error(
        RuntimeError("Trailers must have END_STREAM set.")
    )


def test_postgrest_patch_sets_http2_false():
    import inspect

    patch_postgrest_disable_http2()
    from postgrest._sync.client import SyncPostgrestClient

    src = inspect.getsource(SyncPostgrestClient.create_session)
    assert "http2=False" in src
    assert "http2=True" not in src
