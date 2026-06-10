# tests/test_rbac_service.py
"""SEC-01B — RBAC foundation."""
from __future__ import annotations

from services.rbac_service import (
    build_rbac_readiness_report,
    get_role_permissions,
    is_rbac_configured,
    list_permissions,
    list_roles,
    resolve_user_role,
)


def test_list_roles():
    roles = list_roles()
    names = {r.role_name for r in roles}
    assert names == {"VIEWER", "QA_ENGINEER", "QA_MANAGER", "RELEASE_MANAGER", "ADMIN"}
    assert len(roles) == 5


def test_list_permissions():
    permissions = list_permissions()
    ids = {p.permission_id for p in permissions}
    assert "VIEW_DASHBOARD" in ids
    assert "ADMIN" in ids
    assert len(permissions) >= 9


def test_role_permission_matrix():
    viewer = get_role_permissions("VIEWER")
    assert viewer.permissions == ["VIEW_DASHBOARD", "VIEW_INCIDENTS", "VIEW_REPORTS"]

    engineer = get_role_permissions("QA_ENGINEER")
    assert "VIEW_RELEASE_INTELLIGENCE" in engineer.permissions
    assert "SEND_REPORTS" not in engineer.permissions

    manager = get_role_permissions("QA_MANAGER")
    assert "APPROVE_ACTIONS" in manager.permissions
    assert "SEND_REPORTS" in manager.permissions
    assert "MANAGE_INTEGRATIONS" not in manager.permissions

    release_mgr = get_role_permissions("RELEASE_MANAGER")
    assert "MANAGE_INTEGRATIONS" in release_mgr.permissions

    admin = get_role_permissions("ADMIN")
    assert len(admin.permissions) >= 9
    assert "ADMIN" in admin.permissions


def test_resolve_user_role_defaults_to_viewer():
    role = resolve_user_role(user_id="user-abc")
    assert role.user_id == "user-abc"
    assert role.role_name == "VIEWER"


def test_rbac_readiness_report():
    report = build_rbac_readiness_report()
    assert report.role_count == 5
    assert report.permission_count >= 9
    assert report.default_roles_ready is True
    assert report.enforcement_enabled is False
    assert report.readiness_score == 50


def test_is_rbac_configured():
    assert is_rbac_configured() is True
