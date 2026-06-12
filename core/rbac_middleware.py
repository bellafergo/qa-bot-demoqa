# core/rbac_middleware.py
"""RBAC enforcement middleware — resolves roles and denies unauthorized requests."""
from __future__ import annotations

import logging
from typing import Callable

from starlette.middleware.base import BaseHTTPMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse, Response

from core.rbac_enforcement import (
    apply_rbac_to_state,
    check_request_access,
    rbac_enforcement_enabled,
)
from core.vanya_auth import is_public_path

logger = logging.getLogger("vanya.rbac")


def _json_403(detail: str) -> JSONResponse:
    return JSONResponse(status_code=403, content={"detail": detail})


class RbacEnforcementMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        if request.method == "OPTIONS":
            return await call_next(request)

        path = request.url.path

        if not rbac_enforcement_enabled() or is_public_path(path):
            return await call_next(request)

        apply_rbac_to_state(request.state)

        role_name = getattr(request.state, "role_name", "VIEWER")
        permissions = set(getattr(request.state, "permissions", set()) or set())

        allowed, detail, required_perm = check_request_access(
            method=request.method,
            path=path,
            role_name=role_name,
            permissions=permissions,
        )

        if not allowed:
            self._record_access_denied(
                request=request,
                role_name=role_name,
                detail=detail,
                required_permission=required_perm,
            )
            logger.info(
                "RBAC denied %s %s role=%s reason=%s",
                request.method,
                path,
                role_name,
                detail,
            )
            return _json_403(detail)

        return await call_next(request)

    @staticmethod
    def _record_access_denied(
        *,
        request: Request,
        role_name: str,
        detail: str,
        required_permission: str | None,
    ) -> None:
        try:
            from services.audit_event_service import safe_record_event

            safe_record_event(
                event_type="ACCESS_DENIED",
                resource_type="SECURITY",
                resource_id=request.url.path,
                action=request.method.lower(),
                result="FAILURE",
                user_id=getattr(request.state, "user_id", None),
                user_email=getattr(request.state, "email", None),
                metadata={
                    "role_name": role_name,
                    "required_permission": required_permission,
                    "detail": detail,
                },
            )
        except Exception:
            logger.debug("RBAC: failed to record ACCESS_DENIED audit event", exc_info=True)
