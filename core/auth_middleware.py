# core/auth_middleware.py
"""
ASGI middleware: require Supabase user JWT (or service token) except on public paths.
"""
from __future__ import annotations

import json
import logging
from typing import Callable

from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse, Response

from core.vanya_auth import (
    apply_service_to_state,
    apply_user_to_state,
    auth_enforcement_enabled,
    docs_allowed_in_this_deployment,
    email_domain_allowed,
    is_public_path,
    machine_request_authorized,
    verify_supabase_user_jwt,
)

logger = logging.getLogger("vanya.auth")


def _json_401(detail: str) -> JSONResponse:
    return JSONResponse(status_code=401, content={"detail": detail})


def _json_403(detail: str) -> JSONResponse:
    return JSONResponse(status_code=403, content={"detail": detail})


class VanyaAuthMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        print("AUTH HIT:", request.url.path)
        path = request.url.path
        enforcement = auth_enforcement_enabled()

        # TEMP diagnostic logging (remove after validating prod auth) — see vanya.auth.middleware
        def _log_allow(reason: str) -> None:
            logger.info(
                "vanya.auth.middleware path=%s method=%s enforcement=%s auth_skipped=true reason=%s",
                path,
                request.method,
                enforcement,
                reason,
            )

        def _log_auth_ok(kind: str) -> None:
            logger.info(
                "vanya.auth.middleware path=%s method=%s enforcement=%s auth_skipped=false authorized=%s",
                path,
                request.method,
                enforcement,
                kind,
            )

        if request.method == "OPTIONS":
            _log_allow("options_preflight")
            return await call_next(request)

        # Docs / OpenAPI — dev-style deployments only (minimal-safe default).
        if path in ("/docs", "/redoc", "/openapi.json"):
            if docs_allowed_in_this_deployment():
                _log_allow("openapi_docs_allowed")
                return await call_next(request)
            logger.info(
                "vanya.auth.middleware path=%s method=%s enforcement=%s auth_skipped=false reason=openapi_docs_blocked",
                path,
                request.method,
                enforcement,
            )
            return _json_401("Authentication required")

        if is_public_path(path):
            _log_allow("public_path")
            return await call_next(request)

        if not enforcement:
            _log_allow("enforcement_disabled")
            return await call_next(request)

        # ── Machine auth (Phase 2 stub — full API keys later) ───────────────
        if machine_request_authorized(request.headers):
            _log_auth_ok("service_token")
            apply_service_to_state(request.state)
            return await call_next(request)

        # ── User Bearer JWT ─────────────────────────────────────────────────
        auth_header = request.headers.get("authorization") or request.headers.get("Authorization")
        if not auth_header or not auth_header.lower().startswith("bearer "):
            logger.info(
                "vanya.auth.middleware path=%s method=%s enforcement=%s auth_skipped=false reason=missing_bearer",
                path,
                request.method,
                enforcement,
            )
            return _json_401("Authentication required. Send Authorization: Bearer <access_token>.")

        token = auth_header.split(" ", 1)[1].strip()
        if not token:
            logger.info(
                "vanya.auth.middleware path=%s method=%s enforcement=%s auth_skipped=false reason=empty_bearer",
                path,
                request.method,
                enforcement,
            )
            return _json_401("Authentication required. Empty Bearer token.")

        try:
            claims = verify_supabase_user_jwt(token)
        except Exception as e:
            logger.info(
                "vanya.auth.middleware path=%s enforcement=%s auth_skipped=false reason=jwt_invalid (%s)",
                path,
                enforcement,
                type(e).__name__,
            )
            logger.info("JWT validation failed: %s", type(e).__name__)
            return _json_401("Invalid or expired token.")

        email = claims.get("email")
        if isinstance(email, str):
            email = email.strip()
        else:
            email = None

        ok_domain, msg = email_domain_allowed(email)
        if not ok_domain:
            logger.info(
                "vanya.auth.middleware path=%s enforcement=%s auth_skipped=false reason=domain_forbidden",
                path,
                enforcement,
            )
            return _json_403(msg or "Email domain is not allowed for this deployment.")

        _log_auth_ok("bearer_jwt")
        apply_user_to_state(request.state, claims)
        return await call_next(request)
