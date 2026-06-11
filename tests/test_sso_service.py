# tests/test_sso_service.py
"""SEC-01D — Enterprise SSO activation tests."""
from __future__ import annotations

import pytest

from services import sso_service as svc
from services.auth_identity_service import build_security_readiness_report, list_identity_providers


def test_microsoft_validation_requires_fields():
    result = svc.validate_provider_config(provider="MICROSOFT", client_id="", tenant_id="")
    assert result.valid is False
    assert "client_id" in result.message.lower()

    result = svc.validate_provider_config(
        provider="MICROSOFT",
        client_id="ms-client-id",
        tenant_id="not-a-valid-tenant",
    )
    assert result.valid is False


def test_microsoft_validation_success():
    result = svc.validate_provider_config(
        provider="MICROSOFT",
        client_id="ms-client-id",
        tenant_id="11111111-1111-1111-1111-111111111111",
    )
    assert result.valid is True

    config = svc._to_provider_config("MICROSOFT")
    assert config.configured is True
    assert config.validated is True


def test_google_validation():
    invalid = svc.validate_provider_config(provider="GOOGLE", client_id="")
    assert invalid.valid is False

    valid = svc.validate_provider_config(provider="GOOGLE", client_id="google-client-id.apps.googleusercontent.com")
    assert valid.valid is True
    assert svc._to_provider_config("GOOGLE").validated is True


def test_okta_validation():
    invalid = svc.validate_provider_config(provider="OKTA", client_id="okta-client", issuer="http://bad")
    assert invalid.valid is False

    valid = svc.validate_provider_config(
        provider="OKTA",
        client_id="okta-client",
        issuer="https://example.okta.com/oauth2/default",
    )
    assert valid.valid is True
    assert svc._to_provider_config("OKTA").validated is True


def test_login_url_generation():
    svc.validate_provider_config(
        provider="MICROSOFT",
        client_id="ms-client-id",
        tenant_id="common",
    )
    login = svc.build_login_url(provider="MICROSOFT")
    assert login.provider == "MICROSOFT"
    assert "login.microsoftonline.com/common/oauth2/v2.0/authorize" in login.login_url
    assert "client_id=ms-client-id" in login.login_url

    svc.validate_provider_config(provider="GOOGLE", client_id="google-client-id")
    google_login = svc.build_login_url(provider="GOOGLE")
    assert "accounts.google.com/o/oauth2/v2/auth" in google_login.login_url

    svc.validate_provider_config(
        provider="OKTA",
        client_id="okta-client",
        issuer="https://example.okta.com/oauth2/default",
    )
    okta_login = svc.build_login_url(provider="OKTA")
    assert "example.okta.com/oauth2/default/v1/authorize" in okta_login.login_url


def test_readiness_updates_when_sso_validated():
    report_before = build_security_readiness_report()
    assert report_before.sso_ready is False
    assert report_before.authentication_method == "LOCAL"
    assert report_before.available_sso_providers == ["MICROSOFT", "GOOGLE", "OKTA"]

    svc.validate_provider_config(
        provider="MICROSOFT",
        client_id="ms-client-id",
        tenant_id="common",
    )

    report_after = build_security_readiness_report()
    assert report_after.sso_ready is True
    assert report_after.authentication_method == "HYBRID"
    assert "MICROSOFT" in report_after.configured_sso_providers
    assert "SSO READY" in report_after.summary

    providers = list_identity_providers(enabled_only=False)
    microsoft = next(p for p in providers if p.provider_type == "MICROSOFT")
    local = next(p for p in providers if p.provider_type == "LOCAL")
    assert local.enabled is True
    assert microsoft.enabled is True
