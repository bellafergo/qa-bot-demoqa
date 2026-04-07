# core/auth_middleware.py
"""
ASGI middleware: require Supabase user JWT (or service token) except on public paths.
"""
from __future__ import annotations

import logging
from typing import Callable

from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse, Response

from core.vanya_auth import (
    apply_service_to_state,
    apply_user_to_state,
    auth_enforcement_enabled,
    check_email_domain_policy,
    docs_allowed_in_this_deployment,
    extract_user_email_from_claims,
    is_public_path,
    machine_request_authorized,
    resolve_allowed_email_domains,
    verify_supabase_user_jwt,
)

logger = logging.getLogger("vanya.auth")


def _json_401(detail: str) -> JSONResponse:
    return JSONResponse(status_code=401, content={"detail": detail})


def _json_403(detail: str) -> JSONResponse:
    return JSONResponse(status_code=403, content={"detail": detail})


class VanyaAuthMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        path = request.url.path

        if request.method == "OPTIONS":
            return await call_next(request)

        # Docs / OpenAPI — dev-style deployments only (minimal-safe default).
        if path in ("/docs", "/redoc", "/openapi.json"):
            if docs_allowed_in_this_deployment():
                return await call_next(request)
            return _json_401("Authentication required")

        if is_public_path(path):
            return await call_next(request)

        if not auth_enforcement_enabled():
            return await call_next(request)

        # ── Machine auth (Phase 2 stub — full API keys later) ───────────────
        if machine_request_authorized(request.headers):
            apply_service_to_state(request.state)
            return await call_next(request)

        # ── User Bearer JWT ─────────────────────────────────────────────────
        auth_header = request.headers.get("authorization") or request.headers.get("Authorization")
        if not auth_header or not auth_header.lower().startswith("bearer "):
            return _json_401("Authentication required. Send Authorization: Bearer <access_token>.")

        token = auth_header.split(" ", 1)[1].strip()
        if not token:
            return _json_401("Authentication required. Empty Bearer token.")

        try:
            claims = verify_supabase_user_jwt(token)
        except Exception as e:
            logger.info("JWT validation failed: %s", type(e).__name__)
            return _json_401("Invalid or expired token.")

        email = extract_user_email_from_claims(claims)
        domains = resolve_allowed_email_domains(claims=claims)
        ok_domain, msg = check_email_domain_policy(email, domains)
        if not ok_domain:
            return _json_403(msg or "Email domain policy denied access.")

        apply_user_to_state(request.state, claims)
        return await call_next(request)
