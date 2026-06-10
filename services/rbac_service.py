# services/rbac_service.py
"""
Enterprise SEC-01B — Role-based access control foundation (read-only).

Defines default roles, permissions, and resolution helpers.
No authorization enforcement or access denial.
"""
from __future__ import annotations

import logging
from typing import Dict, List, Optional, Set

from models.rbac_models import (
    Permission,
    RBACReadinessReport,
    Role,
    RolePermission,
    RolesResponse,
    PermissionsResponse,
    UserRole,
)

logger = logging.getLogger("vanya.rbac")

_PERMISSIONS: tuple[tuple[str, str, str], ...] = (
    ("VIEW_DASHBOARD", "View Dashboard", "Access executive dashboard views."),
    ("VIEW_INCIDENTS", "View Incidents", "Access incident intelligence and investigator."),
    ("VIEW_RELEASE_INTELLIGENCE", "View Release Intelligence", "Access release readiness compositor views."),
    ("VIEW_REPORTS", "View Reports", "Access executive and scheduled reports."),
    ("SEND_REPORTS", "Send Reports", "Deliver executive reports through configured channels."),
    ("MANAGE_INTEGRATIONS", "Manage Integrations", "Configure GitHub, Azure DevOps, and connector integrations."),
    ("MANAGE_SECURITY", "Manage Security", "Configure authentication, SSO, and security settings."),
    ("APPROVE_ACTIONS", "Approve Actions", "Approve gated operational actions."),
    ("ADMIN", "Admin", "Full administrative access across the platform."),
)

_ROLES: tuple[tuple[str, str, str], ...] = (
    ("VIEWER", "Viewer", "Read-only access to dashboard, incidents, and reports."),
    ("QA_ENGINEER", "QA Engineer", "Viewer access plus release intelligence views."),
    ("QA_MANAGER", "QA Manager", "QA Engineer access plus approvals and report delivery."),
    ("RELEASE_MANAGER", "Release Manager", "QA Manager access plus integration management."),
    ("ADMIN", "Admin", "Full platform permissions."),
)

_DEFAULT_ROLE = "VIEWER"

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
    "ADMIN": {p[0] for p in _PERMISSIONS},
}


def list_permissions() -> List[Permission]:
    return [
        Permission(permission_id=pid, permission_name=name, description=desc)
        for pid, name, desc in _PERMISSIONS
    ]


def list_roles() -> List[Role]:
    return [
        Role(role_id=rid.lower(), role_name=rid, description=desc)
        for rid, name, desc in _ROLES
    ]


def get_role_permissions(role_name: str) -> RolePermission:
    key = str(role_name or "").strip().upper()
    perms = sorted(_ROLE_PERMISSION_MATRIX.get(key, set()))
    return RolePermission(role_name=key or _DEFAULT_ROLE, permissions=perms)


def resolve_user_role(*, user_id: Optional[str] = None) -> UserRole:
    """
    Resolve role for a user. Foundation sprint: unassigned users default to VIEWER.
    No persistent role assignments yet.
    """
    uid = str(user_id or "").strip() or "anonymous"
    return UserRole(user_id=uid, role_name=_DEFAULT_ROLE)


def is_rbac_configured() -> bool:
    """True when default role and permission catalog is ready."""
    report = build_rbac_readiness_report()
    return report.default_roles_ready


def build_rbac_readiness_report() -> RBACReadinessReport:
    roles = list_roles()
    permissions = list_permissions()
    default_roles_ready = len(roles) >= 5 and len(permissions) >= 9
    enforcement_enabled = False
    readiness_score = 50 if default_roles_ready else 0

    summary = (
        f"RBAC foundation defines {len(roles)} roles and {len(permissions)} permissions. "
        f"Enforcement is {'enabled' if enforcement_enabled else 'disabled'}. "
        f"Readiness score {readiness_score}%."
    )

    return RBACReadinessReport(
        role_count=len(roles),
        permission_count=len(permissions),
        default_roles_ready=default_roles_ready,
        enforcement_enabled=enforcement_enabled,
        readiness_score=readiness_score,
        summary=summary,
    )


def build_roles_response() -> RolesResponse:
    roles = list_roles()
    return RolesResponse(roles=roles, total=len(roles))


def build_permissions_response() -> PermissionsResponse:
    permissions = list_permissions()
    return PermissionsResponse(permissions=permissions, total=len(permissions))
