# tests/test_rbac_enforcement.py
"""V1.2 — RBAC activation and enforcement."""
from __future__ import annotations

import pytest

from core.rbac_enforcement import (
    check_request_access,
    permissions_for_role,
    rbac_enforcement_enabled,
    resolve_role_name,
)
from services.rbac_service import build_rbac_readiness_report, build_user_security_context


def test_resolve_role_from_email_env(monkeypatch):
    monkeypatch.setenv(
        "VANYA_RBAC_ROLE_BY_EMAIL",
        "admin@corp.com=ADMIN,qa@corp.com=QA_ENGINEER",
    )
    assert resolve_role_name(email="admin@corp.com") == "ADMIN"
    assert resolve_role_name(email="qa@corp.com") == "QA_ENGINEER"
    assert resolve_role_name(email="viewer@corp.com") == "VIEWER"


def test_resolve_role_from_jwt_claims():
    role = resolve_role_name(
        claims={"app_metadata": {"vanya_role": "RELEASE_MANAGER"}},
    )
    assert role == "RELEASE_MANAGER"


def test_service_token_gets_admin():
    assert resolve_role_name(auth_kind="service") == "ADMIN"


def test_viewer_cannot_write():
    perms = set(permissions_for_role("VIEWER"))
    allowed, detail, _ = check_request_access(
        method="POST",
        path="/integrations/slack/config",
        role_name="VIEWER",
        permissions=perms,
    )
    assert allowed is False
    assert "Viewer" in detail or "MANAGE_INTEGRATIONS" in detail


def test_release_manager_can_manage_integrations():
    perms = set(permissions_for_role("RELEASE_MANAGER"))
    allowed, _, _ = check_request_access(
        method="POST",
        path="/integrations/slack/config",
        role_name="RELEASE_MANAGER",
        permissions=perms,
    )
    assert allowed is True


def test_rbac_readiness_reflects_env(monkeypatch):
    monkeypatch.delenv("VANYA_AUTH_ENABLED", raising=False)
    assert rbac_enforcement_enabled() is False
    report = build_rbac_readiness_report()
    assert report.enforcement_enabled is False

    monkeypatch.setenv("VANYA_AUTH_ENABLED", "1")
    monkeypatch.setenv("SUPABASE_URL", "https://example.supabase.co")
    assert rbac_enforcement_enabled() is True
    report = build_rbac_readiness_report()
    assert report.enforcement_enabled is True


def test_build_user_security_context():
    ctx = build_user_security_context(
        user_id="user-1",
        email="qa@corp.com",
        claims={"app_metadata": {"vanya_role": "QA_MANAGER"}},
    )
    assert ctx.role_name == "QA_MANAGER"
    assert "SEND_REPORTS" in ctx.permissions
    assert "MANAGE_INTEGRATIONS" not in ctx.permissions


def test_access_denied_audit_event_type_allowed():
    from models.audit_models import AuditEventType

    assert "ACCESS_DENIED" in AuditEventType.__args__
