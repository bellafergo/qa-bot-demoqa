# services/authorization_service.py
"""
Enterprise SEC-01F — RBAC permission enforcement.

Reuses SEC-01B role and permission catalog. Records audit events on denial.
"""
from __future__ import annotations

import logging
import os
from typing import Any, Dict, List, Optional, Set

from fastapi import HTTPException, Request

from models.rbac_models import RolePermission, UserRole
from services.auth_identity_service import auth_context_from_state
from services.rbac_service import get_role_permissions, resolve_user_role

logger = logging.getLogger("vanya.authorization")

_KNOWN_ROLES = frozenset({"VIEWER", "QA_ENGINEER", "QA_MANAGER", "RELEASE_MANAGER", "ADMIN"})


def is_enforcement_enabled() -> bool:
    """When auth enforcement is off, RBAC stays permissive to preserve local/dev flows."""
    from core.vanya_auth import auth_enforcement_enabled

    if not auth_enforcement_enabled():
        return False
    return os.getenv("VANYA_RBAC_ENFORCEMENT", "1").strip().lower() not in {"0", "false", "no"}


def _user_id_from_request(request: Optional[Request]) -> Optional[str]:
    if request is None:
        return None
    header_uid = (request.headers.get("X-Vanya-User-Id") or "").strip()
    return header_uid or None


def _role_from_request(request: Optional[Request]) -> Optional[str]:
    if request is None:
        return None
    header_role = (request.headers.get("X-Vanya-Role") or "").strip().upper()
    if header_role in _KNOWN_ROLES:
        return header_role
    state_role = getattr(request.state, "role_name", None)
    if state_role and str(state_role).upper() in _KNOWN_ROLES:
        return str(state_role).upper()
    return None


def resolve_current_user_role(
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    auth_kind: Optional[str] = None,
    role_name: Optional[str] = None,
    request: Optional[Request] = None,
) -> UserRole:
    resolved_role = role_name or _role_from_request(request)
    return resolve_user_role(
        user_id=user_id,
        email=email,
        auth_kind=auth_kind,
        role_name=resolved_role,
    )


def resolve_current_permissions(
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    auth_kind: Optional[str] = None,
    role_name: Optional[str] = None,
    request: Optional[Request] = None,
) -> List[str]:
    role = resolve_current_user_role(
        user_id=user_id,
        email=email,
        auth_kind=auth_kind,
        role_name=role_name,
        request=request,
    )
    return list(get_role_permissions(role.role_name).permissions)


def has_permission(
    permission: str,
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    auth_kind: Optional[str] = None,
    role_name: Optional[str] = None,
    request: Optional[Request] = None,
) -> bool:
    if not is_enforcement_enabled():
        return True
    perms = set(resolve_current_permissions(
        user_id=user_id,
        email=email,
        auth_kind=auth_kind,
        role_name=role_name,
        request=request,
    ))
    if "ADMIN" in perms:
        return True
    return str(permission or "").strip().upper() in perms


def _record_denial(
    *,
    event_type: str,
    permission: str,
    user_id: Optional[str],
    user_email: Optional[str],
    resource_type: str,
    resource_id: str,
    metadata: Optional[Dict[str, Any]] = None,
) -> None:
    from services.audit_event_service import safe_record_event

    safe_record_event(
        event_type=event_type,
        resource_type=resource_type,
        resource_id=resource_id,
        action="authorize",
        result="FAILURE",
        user_id=user_id,
        user_email=user_email,
        metadata={
            "permission": permission,
            **(metadata or {}),
        },
    )


def require_permission(
    permission: str,
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    auth_kind: Optional[str] = None,
    role_name: Optional[str] = None,
    request: Optional[Request] = None,
    resource_type: str = "SECURITY",
    resource_id: str = "authorization",
) -> None:
    """Raise HTTP 401/403 when permission is missing; audit the denial."""
    if not is_enforcement_enabled():
        return

    perm = str(permission or "").strip().upper()
    uid = str(user_id or "").strip()
    if request is not None:
        ctx = auth_context_from_state(request.state)
        uid = uid or str(ctx.get("user_id") or "").strip() or (_user_id_from_request(request) or "")
        email = email or ctx.get("email")
        auth_kind = auth_kind or ctx.get("auth_kind")

    if not uid or uid in {"anonymous", "unknown"}:
        if auth_kind != "service":
            _record_denial(
                event_type="UNAUTHORIZED_ACCESS",
                permission=perm,
                user_id=uid or "anonymous",
                user_email=email,
                resource_type=resource_type,
                resource_id=resource_id,
            )
            raise HTTPException(status_code=401, detail="Authentication required.")

    if has_permission(
        perm,
        user_id=uid,
        email=email,
        auth_kind=auth_kind,
        role_name=role_name,
        request=request,
    ):
        return

    _record_denial(
        event_type="PERMISSION_DENIED",
        permission=perm,
        user_id=uid or "anonymous",
        user_email=email,
        resource_type=resource_type,
        resource_id=resource_id,
        metadata={"role": resolve_current_user_role(
            user_id=uid,
            email=email,
            auth_kind=auth_kind,
            role_name=role_name,
            request=request,
        ).role_name},
    )
    raise HTTPException(status_code=403, detail=f"Permission denied: {perm}")


def require_permission_from_request(
    request: Request,
    permission: str,
    *,
    resource_type: str = "SECURITY",
    resource_id: Optional[str] = None,
) -> None:
    ctx = auth_context_from_state(request.state)
    require_permission(
        permission,
        user_id=ctx.get("user_id"),
        email=ctx.get("email"),
        auth_kind=ctx.get("auth_kind"),
        request=request,
        resource_type=resource_type,
        resource_id=resource_id or request.url.path,
    )


def build_resolved_permissions_response(
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    auth_kind: Optional[str] = None,
    request: Optional[Request] = None,
) -> RolePermission:
    role = resolve_current_user_role(
        user_id=user_id,
        email=email,
        auth_kind=auth_kind,
        request=request,
    )
    return RolePermission(
        role_name=role.role_name,
        permissions=resolve_current_permissions(
            user_id=role.user_id,
            email=email,
            auth_kind=auth_kind,
            role_name=role.role_name,
            request=request,
        ),
    )
