# services/supabase_http.py
"""
Supabase / PostgREST HTTP transport hardening.

Root cause (production dashboard KeyError: 3):
  postgrest-py creates httpx.Client with http2=True. Under concurrent dashboard
  reads (summary + recent-runs + FI + analytics), multiplexed HTTP/2 streams race
  on the shared singleton connection. The h2 library then raises KeyError(<stream_id>)
  (e.g. KeyError: 3) when a frame references a closed/missing stream — alongside
  "Trailers must have END_STREAM set" and ConnectionTerminated errors.

Mitigation:
  1. Force HTTP/1.1 for PostgREST (no stream multiplexing on one socket).
  2. Classify transport errors for retries in qa_runs_read / catalog repo.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, Optional, TypeVar

logger = logging.getLogger("vanya.supabase_http")

_PATCHED = False

T = TypeVar("T")


def patch_postgrest_disable_http2() -> None:
    """Monkey-patch postgrest SyncPostgrestClient to use http2=False (idempotent)."""
    global _PATCHED
    if _PATCHED:
        return
    try:
        from postgrest._sync.client import SyncPostgrestClient
        from postgrest.utils import SyncClient

        def create_session_http1(
            self,
            base_url: str,
            headers: Dict[str, str],
            timeout: Any,
            verify: bool = True,
            proxy: Optional[str] = None,
        ) -> SyncClient:
            return SyncClient(
                base_url=base_url,
                headers=headers,
                timeout=timeout,
                verify=verify,
                proxy=proxy,
                follow_redirects=True,
                http2=False,
            )

        SyncPostgrestClient.create_session = create_session_http1  # type: ignore[method-assign]
        _PATCHED = True
        logger.info("postgrest: patched SyncPostgrestClient.create_session (http2=False)")
    except Exception:
        logger.exception("postgrest: failed to patch http2=False — Supabase may hit HTTP/2 stream races")


def is_transient_supabase_transport_error(exc: BaseException) -> bool:
    """
    True for HTTP/2 stream races and other retryable transport failures.

    KeyError: 3  →  HTTP/2 stream id 3 missing from h2.connection.streams (not app dict key).
    """
    if isinstance(exc, KeyError) and exc.args:
        key = exc.args[0]
        if isinstance(key, int) and key > 0:
            return True

    msg = (str(exc) or "").lower()
    name = (type(exc).__name__ or "").lower()
    blob = f"{name} {msg}"

    markers = (
        "trailers must have end_stream",
        "connectionterminated",
        "connection terminated",
        "connection lost",
        "connection reset",
        "broken pipe",
        "remote protocol error",
        "protocol error",
        "stream closed",
        "nosuchstream",
        "read timeout",
        "write timeout",
        "timed out",
        "timeout",
        "502",
        "503",
        "504",
        "bad gateway",
        "service unavailable",
    )
    return any(m in blob for m in markers)


def exc_diagnostic(exc: BaseException) -> Dict[str, Any]:
    """Structured fields for logs / JSON error extras (no secrets)."""
    out: Dict[str, Any] = {
        "error_type": type(exc).__name__,
        "error": str(exc) or type(exc).__name__,
    }
    if isinstance(exc, KeyError) and exc.args:
        key = exc.args[0]
        out["keyerror_key"] = key
        if isinstance(key, int):
            out["likely_http2_stream_id"] = True
    return out
