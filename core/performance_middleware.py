# core/performance_middleware.py
"""Lightweight HTTP endpoint timing instrumentation."""
from __future__ import annotations

import logging
import time
from typing import Callable

from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import Response

from core.vanya_auth import is_public_path

logger = logging.getLogger("vanya.performance")

_SKIP_PREFIXES = (
    "/evidence",
    "/reports",
    "/favicon.ico",
)


def _should_record(path: str) -> bool:
    if not path or path == "/health":
        return False
    if is_public_path(path) and path.startswith("/health"):
        return False
    for prefix in _SKIP_PREFIXES:
        if path == prefix or path.startswith(prefix + "/"):
            return False
    return True


class PerformanceMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        if not _should_record(request.url.path):
            return await call_next(request)

        started = time.perf_counter()
        response = await call_next(request)
        elapsed_ms = round((time.perf_counter() - started) * 1000, 2)

        response.headers["x-process-time-ms"] = str(elapsed_ms)

        try:
            from services.endpoint_timing_service import record_endpoint_timing

            record_endpoint_timing(
                endpoint=request.url.path,
                method=request.method,
                duration_ms=elapsed_ms,
                status_code=response.status_code,
                request_id=getattr(request.state, "request_id", None),
            )
        except Exception:
            logger.debug("performance: record failed", exc_info=True)

        return response
