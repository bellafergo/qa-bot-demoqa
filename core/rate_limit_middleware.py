# core/rate_limit_middleware.py
"""
In-memory rate limiting for expensive FastAPI routes.

See .env.example for RATE_LIMIT_* variables. Not shared across workers (use Redis later).
"""
from __future__ import annotations

import logging
import os
import re
import threading
import time
from typing import Callable, Dict, Optional, Tuple

from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse, Response

logger = logging.getLogger("vanya.rate_limit")

_TESTS_SINGLE_RUN_RE = re.compile(r"^/tests/[^/]+/run$")


def _env_flag(name: str, default: str = "0") -> bool:
    return (os.getenv(name) or default).strip().lower() in ("1", "true", "yes", "on")


def _parse_limit(name: str, default_n: int, default_window_s: int) -> Tuple[int, int]:
    raw = (os.getenv(name) or "").strip()
    if not raw:
        return default_n, default_window_s
    try:
        a, b = raw.split("/", 1)
        return max(1, int(a)), max(1, int(b))
    except Exception:
        return default_n, default_window_s


def _normalize_path(path: str) -> str:
    if len(path) > 1 and path.endswith("/"):
        return path[:-1]
    return path or "/"


def _skip_path(path: str) -> bool:
    """Cheap / public infrastructure — no rate limit."""
    p = path
    prefixes = (
        "/health",
        "/meta",
        "/webhooks",
        "/evidence",
        "/reports",
        "/favicon.ico",
        "/docs",
        "/redoc",
        "/openapi.json",
    )
    if any(p.startswith(px) for px in prefixes):
        return True
    if p == "/app-explorer/health" or p.startswith("/app-explorer/health"):
        return True
    if p in ("/execution/health", "/execution/status"):
        return True
    return False


def _tier_for_request(method: str, path: str) -> Optional[str]:
    """
    Return rate-limit tier name or None if this request is not limited.
    Only POST (and PUT/PATCH for a few) are considered; GET list traffic stays unlimited.
    """
    m = method.upper()
    if m == "OPTIONS":
        return None
    if _skip_path(path):
        return None

    if m == "POST":
        if path in (
            "/execute_steps",
            "/execute_suite",
            "/execute_text",
            "/tests/run-suite",
            "/api-testing/run",
            "/risk-selection/select-and-run",
            "/execution/run-batch",
            "/execution/retry-failed",
        ):
            return "execute"
        if _TESTS_SINGLE_RUN_RE.match(path):
            return "execute"
        if path.startswith("/orchestrator/jobs"):
            return "execute"

        if path in ("/app-explorer/explore", "/app-explorer/explore-app"):
            return "explorer"
        if path.startswith("/exploration/"):
            return "explorer"

        if path in ("/chat_run", "/plan_from_text"):
            return "chat"
        if path.startswith(
            (
                "/test-generation/",
                "/pr-analysis/",
                "/rca/",
                "/business-risk/",
                "/test-data/",
                "/api-testing/",
                "/github/",
            )
        ):
            return "chat"
        if path in ("/drafts/generate", "/drafts/generate-from-pages"):
            return "chat"
        if path in ("/tests/from-run", "/tests/auto-fix-preview"):
            return "chat"
        if path == "/tests":
            return "chat"

    return None


def _actor_key(request: Request) -> str:
    """Stable bucket key: service M2M, then user id/email, then IP."""
    kind = getattr(request.state, "auth_kind", None)
    uid = (getattr(request.state, "user_id", None) or "").strip()
    email = (getattr(request.state, "email", None) or "").strip()

    if kind == "service" or uid == "service":
        return "svc:m2m"

    if uid and uid != "service":
        return f"user:{uid}"

    if email:
        return f"email:{email.lower()}"

    xfwd = (request.headers.get("x-forwarded-for") or "").split(",")[0].strip()
    if xfwd:
        return f"ip:{xfwd}"
    if request.client and request.client.host:
        return f"ip:{request.client.host}"
    return "ip:unknown"


class _SlidingWindowLimiter:
    """Fixed window counter per key (monotonic clock). Thread-safe."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        # composite_key -> (window_start_monotonic, count)
        self._windows: Dict[str, Tuple[float, int]] = {}

    def allow(self, key: str, max_count: int, window_s: float) -> bool:
        now = time.monotonic()
        with self._lock:
            start, cnt = self._windows.get(key, (now, 0))
            if now - start >= window_s:
                self._windows[key] = (now, 1)
                ok = True
            elif cnt < max_count:
                self._windows[key] = (start, cnt + 1)
                ok = True
            else:
                ok = False
            if len(self._windows) > 100_000:
                self._prune_unlocked(now, window_s * 3)
            return ok

    def _prune_unlocked(self, now: float, max_age: float) -> None:
        cutoff = now - max_age
        dead = [k for k, (st, _) in self._windows.items() if st < cutoff]
        for k in dead[:50_000]:
            self._windows.pop(k, None)


_limiter = _SlidingWindowLimiter()


class RateLimitMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        if not _env_flag("RATE_LIMIT_ENABLED", "0"):
            return await call_next(request)

        path = _normalize_path(request.url.path)
        tier = _tier_for_request(request.method, path)
        if tier is None:
            return await call_next(request)

        if tier == "execute":
            n, w = _parse_limit("RATE_LIMIT_EXECUTE", 10, 300)
        elif tier == "explorer":
            n, w = _parse_limit("RATE_LIMIT_EXPLORER", 5, 300)
        else:
            n, w = _parse_limit("RATE_LIMIT_CHAT", 20, 300)

        actor = _actor_key(request)
        composite = f"{tier}:{actor}"

        if not _limiter.allow(composite, n, float(w)):
            logger.info("rate_limit exceeded tier=%s actor=%s path=%s", tier, actor, path)
            return JSONResponse(
                status_code=429,
                content={"detail": "Rate limit exceeded"},
                headers={"Retry-After": str(int(w))},
            )

        return await call_next(request)
