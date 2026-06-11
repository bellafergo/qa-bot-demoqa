# tests/test_authorization_service.py
"""SEC-01F — RBAC permission enforcement."""
from __future__ import annotations

import pytest
from fastapi import HTTPException
from fastapi.testclient import TestClient

from services import audit_event_service as audit_svc
from services.authorization_service import (
    has_permission,
    is_enforcement_enabled,
    require_permission,
    resolve_current_permissions,
)
from services.rbac_service import get_role_permissions


@pytest.fixture
def enforcement_on(monkeypatch):
    monkeypatch.setattr(
        "services.authorization_service.is_enforcement_enabled",
        lambda: True,
    )


def test_enforcement_disabled_when_auth_off():
    assert is_enforcement_enabled() is False


def test_viewer_permissions(enforcement_on):
    perms = resolve_current_permissions(user_id="viewer-1", role_name="VIEWER")
    assert perms == get_role_permissions("VIEWER").permissions
    assert has_permission("VIEW_REPORTS", user_id="viewer-1", role_name="VIEWER") is True
    assert has_permission("SEND_REPORTS", user_id="viewer-1", role_name="VIEWER") is False
    assert has_permission("MANAGE_SECURITY", user_id="viewer-1", role_name="VIEWER") is False


def test_qa_engineer_permissions(enforcement_on):
    assert has_permission("VIEW_INCIDENTS", user_id="qa-1", role_name="QA_ENGINEER") is True
    assert has_permission("VIEW_RELEASE_INTELLIGENCE", user_id="qa-1", role_name="QA_ENGINEER") is True
    assert has_permission("SEND_REPORTS", user_id="qa-1", role_name="QA_ENGINEER") is False
    assert has_permission("MANAGE_INTEGRATIONS", user_id="qa-1", role_name="QA_ENGINEER") is False


def test_release_manager_permissions(enforcement_on):
    assert has_permission("SEND_REPORTS", user_id="rm-1", role_name="RELEASE_MANAGER") is True
    assert has_permission("MANAGE_INTEGRATIONS", user_id="rm-1", role_name="RELEASE_MANAGER") is True
    assert has_permission("MANAGE_SECURITY", user_id="rm-1", role_name="RELEASE_MANAGER") is False


def test_admin_permissions(enforcement_on):
    assert has_permission("MANAGE_SECURITY", user_id="admin-1", role_name="ADMIN") is True
    assert has_permission("ANY_PERMISSION", user_id="admin-1", role_name="ADMIN") is True


def test_require_permission_denied_records_audit(enforcement_on):
    with pytest.raises(HTTPException) as exc:
        require_permission(
            "MANAGE_SECURITY",
            user_id="viewer-1",
            role_name="VIEWER",
            resource_type="SECURITY",
            resource_id="sso",
        )
    assert exc.value.status_code == 403

    listed = audit_svc.list_events(event_type="PERMISSION_DENIED")
    assert listed.total == 1
    assert listed.events[0].metadata["permission"] == "MANAGE_SECURITY"
    assert listed.events[0].result == "FAILURE"


def test_require_permission_unauthorized_records_audit(enforcement_on):
    with pytest.raises(HTTPException) as exc:
        require_permission(
            "VIEW_REPORTS",
            user_id="anonymous",
            resource_type="REPORTS",
            resource_id="demo",
        )
    assert exc.value.status_code == 401

    listed = audit_svc.list_events(event_type="UNAUTHORIZED_ACCESS")
    assert listed.total == 1
    assert listed.events[0].metadata["permission"] == "VIEW_REPORTS"


def test_me_permissions_endpoint():
    from app import app

    client = TestClient(app)
    response = client.get(
        "/security/me/permissions",
        headers={"X-Vanya-Role": "QA_MANAGER", "X-Vanya-User-Id": "mgr-1"},
    )
    assert response.status_code == 200
    body = response.json()
    assert body["role_name"] == "QA_MANAGER"
    assert "SEND_REPORTS" in body["permissions"]
    assert "MANAGE_INTEGRATIONS" not in body["permissions"]


def test_sso_route_denied_for_viewer(enforcement_on):
    from app import app

    client = TestClient(app)
    response = client.get(
        "/security/sso/providers",
        headers={"X-Vanya-Role": "VIEWER", "X-Vanya-User-Id": "viewer-1"},
    )
    assert response.status_code == 403
    listed = audit_svc.list_events(event_type="PERMISSION_DENIED")
    assert listed.total >= 1
