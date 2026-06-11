# tests/test_auth_identity_service.py
"""SEC-01A — Authentication & SSO foundation."""
from __future__ import annotations

from services.auth_identity_service import (
    build_security_readiness_report,
    get_session_provider,
    get_user_identity,
    list_enabled_providers,
    list_identity_providers,
    resolve_authentication_source,
)


def test_local_provider_enabled():
    enabled = list_enabled_providers()
    assert len(enabled) == 1
    assert enabled[0].provider_id == "local"
    assert enabled[0].provider_type == "LOCAL"
    assert enabled[0].enabled is True


def test_provider_catalog_includes_sso_foundation():
    all_providers = list_identity_providers(enabled_only=False)
    types = {p.provider_type for p in all_providers}
    assert types == {"LOCAL", "GOOGLE", "MICROSOFT", "OKTA", "SAML"}
    sso = [p for p in all_providers if p.provider_type != "LOCAL"]
    assert all(not p.enabled for p in sso)


def test_resolve_authentication_source_local():
    assert resolve_authentication_source() == "LOCAL"
    assert resolve_authentication_source(auth_kind="user") == "LOCAL"


def test_resolve_authentication_source_sso():
    assert resolve_authentication_source(auth_kind="sso", provider_type="MICROSOFT") == "MICROSOFT"


def test_get_user_identity_from_context():
    identity = get_user_identity(
        user_id="user-123",
        email="cto@example.com",
        auth_kind="user",
    )
    assert identity.user_id == "user-123"
    assert identity.provider_type == "LOCAL"
    assert identity.email == "cto@example.com"
    assert identity.identity_id.startswith("identity:local:")


def test_get_session_provider():
    session = get_session_provider(
        user_id="user-123",
        auth_kind="user",
        session_id="session:test",
    )
    assert session.session_id == "session:test"
    assert session.user_id == "user-123"
    assert session.provider_type == "LOCAL"
    assert session.login_time
    assert session.last_activity


def test_security_readiness_report_defaults():
    report = build_security_readiness_report(
        user_id="user-123",
        email="cto@example.com",
        auth_kind="user",
    )
    assert report.authentication_method == "LOCAL"
    assert report.sso_ready is False
    assert report.available_sso_providers == ["MICROSOFT", "GOOGLE", "OKTA"]
    assert report.configured_sso_providers == []
    assert report.audit_ready is False
    assert report.rbac_ready is True
    assert report.security_score == 50
    assert report.active_provider_type == "LOCAL"
    assert "LOCAL ONLY" in report.summary
    assert "RBAC is configured" in report.summary


def test_service_identity():
    identity = get_user_identity(auth_kind="service")
    assert identity.user_id == "service"
    assert identity.display_name == "Service Token"
