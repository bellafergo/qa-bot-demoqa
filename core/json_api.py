# core/json_api.py
"""Consistent JSON error responses for API routes."""
from __future__ import annotations

from typing import Any, Dict, Optional

import logging

from fastapi.responses import JSONResponse

_api_logger = logging.getLogger("vanya.api")


def json_error_response(
    status_code: int,
    message: str,
    *,
    error: Optional[str] = None,
    error_type: Optional[str] = None,
    extra: Optional[Dict[str, Any]] = None,
) -> JSONResponse:
    """
    Return a JSON body even on failure (never HTML/plain text from route handlers).

    Includes both ``detail`` (FastAPI convention) and ``message`` for clients.
    """
    body: Dict[str, Any] = {
        "ok": False,
        "detail": message,
        "message": message,
    }
    if error is not None:
        body["error"] = error
    if error_type is not None:
        body["error_type"] = error_type
    if extra:
        body.update(extra)
    return JSONResponse(status_code=status_code, content=body)


def json_error_from_exception(
    status_code: int,
    message: str,
    exc: BaseException,
    *,
    logger: Optional[logging.Logger] = None,
    context: Optional[Dict[str, Any]] = None,
) -> JSONResponse:
    """Log full traceback + return structured JSON (used by dashboard routes)."""
    try:
        from services.supabase_http import exc_diagnostic

        diag = exc_diagnostic(exc)
    except Exception:
        diag = {"error_type": type(exc).__name__, "error": str(exc) or type(exc).__name__}

    log = logger or _api_logger
    log.exception(
        "%s | context=%s | diagnostic=%s",
        message,
        context or {},
        diag,
    )

    extra = {k: v for k, v in diag.items() if k not in ("error", "error_type")}
    if context:
        extra = {**extra, **context}

    return json_error_response(
        status_code,
        message,
        error=str(diag.get("error") or exc),
        error_type=str(diag.get("error_type") or type(exc).__name__),
        extra=extra or None,
    )
