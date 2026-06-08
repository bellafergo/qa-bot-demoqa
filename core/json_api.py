# core/json_api.py
"""Consistent JSON error responses for API routes."""
from __future__ import annotations

from typing import Any, Dict, Optional

from fastapi.responses import JSONResponse


def json_error_response(
    status_code: int,
    message: str,
    *,
    error: Optional[str] = None,
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
    if extra:
        body.update(extra)
    return JSONResponse(status_code=status_code, content=body)
