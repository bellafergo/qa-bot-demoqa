# core/rbac_enforcement.py
"""
RBAC enforcement — role resolution from existing auth identities and permission checks.

Role resolution order:
  1. Service token actor → ADMIN
  2. JWT / env claim ``vanya_role`` (app_metadata, user_metadata, top-level)
  3. ``VANYA_RBAC_ROLE_BY_USER_ID`` env map
  4. ``VANYA_RBAC_ROLE_BY_EMAIL`` env map
  5. Default → VIEWER

Enforcement is active when JWT auth is enabled and ``VANYA_RBAC_ENFORCEMENT`` is not disabled.
"""
from __future__ import annotations

import logging
import os
from typing import Any, Dict, FrozenSet, List, Optional, Set, Tuple

from core.vanya_auth import auth_enforcement_enabled

logger = logging.getLogger("vanya.rbac")

_VALID_ROLES = frozenset({
    "VIEWER",
    "QA_ENGINEER",
    "QA_MANAGER",
    "RELEASE_MANAGER",
    "ADMIN",
})

_ROLE_PERMISSION_MATRIX: Dict[str, Set[str]] = {
    "VIEWER": {
        "VIEW_DASHBOARD",
        "VIEW_INCIDENTS",
        "VIEW_REPORTS",
    },
    "QA_ENGINEER": {
        "VIEW_DASHBOARD",
        "VIEW_INCIDENTS",
        "VIEW_REPORTS",
        "VIEW_RELEASE_INTELLIGENCE",
    },
    "QA_MANAGER": {
        "VIEW_DASHBOARD",
        "VIEW_INCIDENTS",
        "VIEW_REPORTS",
        "VIEW_RELEASE_INTELLIGENCE",
        "APPROVE_ACTIONS",
        "SEND_REPORTS",
    },
    "RELEASE_MANAGER": {
        "VIEW_DASHBOARD",
        "VIEW_INCIDENTS",
        "VIEW_REPORTS",
        "VIEW_RELEASE_INTELLIGENCE",
        "APPROVE_ACTIONS",
        "SEND_REPORTS",
        "MANAGE_INTEGRATIONS",
    },
    "ADMIN": {
        "VIEW_DASHBOARD",
        "VIEW_INCIDENTS",
        "VIEW_RELEASE_INTELLIGENCE",
        "VIEW_REPORTS",
        "SEND_REPORTS",
        "MANAGE_INTEGRATIONS",
        "MANAGE_SECURITY",
        "APPROVE_ACTIONS",
        "ADMIN",
    },
}


def rbac_enforcement_enabled() -> bool:
    """RBAC enforcement follows auth — disabled when auth is off."""
    if not auth_enforcement_enabled():
        return False
    raw = (os.getenv("VANYA_RBAC_ENFORCEMENT") or "1").strip().lower()
    return raw not in ("0", "false", "no", "off")


def _normalize_role(value: Any) -> Optional[str]:
    key = str(value or "").strip().upper()
    if key in _VALID_ROLES:
        return key
    return None


def _parse_role_map(env_name: str) -> Dict[str, str]:
    raw = (os.getenv(env_name) or "").strip()
    if not raw:
        return {}
    out: Dict[str, str] = {}
    for part in raw.split(","):
        piece = part.strip()
        if not piece or "=" not in piece:
            continue
        key, role = piece.split("=", 1)
        norm_key = key.strip().lower()
        norm_role = _normalize_role(role)
        if norm_key and norm_role:
            out[norm_key] = norm_role
    return out


def _role_from_claims(claims: Optional[Dict[str, Any]]) -> Optional[str]:
    if not isinstance(claims, dict):
        return None
    candidates = [
        claims.get("vanya_role"),
        (claims.get("app_metadata") or {}).get("vanya_role")
        if isinstance(claims.get("app_metadata"), dict)
        else None,
        (claims.get("app_metadata") or {}).get("role")
        if isinstance(claims.get("app_metadata"), dict)
        else None,
        (claims.get("user_metadata") or {}).get("vanya_role")
        if isinstance(claims.get("user_metadata"), dict)
        else None,
    ]
    for candidate in candidates:
        role = _normalize_role(candidate)
        if role:
            return role
    return None


def resolve_role_name(
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    auth_kind: Optional[str] = None,
    claims: Optional[Dict[str, Any]] = None,
) -> str:
    """Resolve application RBAC role for an authenticated identity."""
    if auth_kind == "service":
        return "ADMIN"

    from_claims = _role_from_claims(claims)
    if from_claims:
        return from_claims

    uid = str(user_id or "").strip()
    if uid:
        by_user = _parse_role_map("VANYA_RBAC_ROLE_BY_USER_ID")
        if uid.lower() in by_user:
            return by_user[uid.lower()]

    em = str(email or "").strip().lower()
    if em:
        by_email = _parse_role_map("VANYA_RBAC_ROLE_BY_EMAIL")
        if em in by_email:
            return by_email[em]

    return "VIEWER"


def permissions_for_role(role_name: str) -> FrozenSet[str]:
    key = _normalize_role(role_name) or "VIEWER"
    return frozenset(_ROLE_PERMISSION_MATRIX.get(key, _ROLE_PERMISSION_MATRIX["VIEWER"]))


def user_has_permission(*, permissions: Set[str], permission: str) -> bool:
    if "ADMIN" in permissions:
        return True
    return permission in permissions


def apply_rbac_to_state(state: Any) -> None:
    """Attach role_name and permissions to request.state after auth."""
    role_name = resolve_role_name(
        user_id=getattr(state, "user_id", None),
        email=getattr(state, "email", None),
        auth_kind=getattr(state, "auth_kind", None),
        claims=getattr(state, "auth_claims", None),
    )
    perms = permissions_for_role(role_name)
    state.role_name = role_name
    state.permissions = set(perms)


def check_request_access(
    *,
    method: str,
    path: str,
    role_name: str,
    permissions: Set[str],
) -> Tuple[bool, str, Optional[str]]:
    """
    Return (allowed, detail_message, required_permission).
    ``required_permission`` is set when a specific permission was required.
    """
    from core.rbac_policy import required_permissions_for_request

    required = required_permissions_for_request(method=method, path=path)
    m = (method or "GET").upper()

    if required is None:
        if m in ("GET", "HEAD", "OPTIONS"):
            if user_has_permission(permissions=permissions, permission="VIEW_DASHBOARD"):
                return True, "", None
            return False, "Missing permission: VIEW_DASHBOARD", "VIEW_DASHBOARD"
        if role_name == "VIEWER":
            return False, "Viewer role cannot perform write operations", None
        return True, "", None

    for perm in required:
        if user_has_permission(permissions=permissions, permission=perm):
            return True, "", perm

    detail = f"Missing permission: {required[0]}"
    return False, detail, required[0]
