# tests/test_oauth_authentication_service.py
"""EC-01G — Real enterprise SSO authentication."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

import jwt
import pytest

from services import oauth_authentication_service as svc
from services import audit_event_service as audit_svc


@pytest.fixture(autouse=True)
def _sso_secrets(monkeypatch):
    monkeypatch.setenv("SSO_MICROSOFT_CLIENT_SECRET", "ms-secret")
    monkeypatch.setenv("SSO_GOOGLE_CLIENT_SECRET", "google-secret")
    monkeypatch.setenv("SSO_OKTA_CLIENT_SECRET", "okta-secret")
    monkeypatch.setenv("VANYA_SSO_JWT_SECRET", "test-jwt-secret")
    monkeypatch.setenv("VANYA_SSO_STATE_SECRET", "test-state-secret")


@pytest.fixture
def validated_microsoft(monkeypatch):
    from services import sso_service as sso_svc

    monkeypatch.setattr(
        sso_svc,
        "_to_provider_config",
        lambda provider: MagicMock(
            provider="MICROSOFT",
            enabled=True,
            configured=True,
            validated=True,
            client_id="ms-client",
            tenant_id="11111111-1111-1111-1111-111111111111",
            issuer=None,
        ),
    )
    monkeypatch.setattr(sso_svc, "_redirect_uri", lambda: "http://localhost:5173/auth/callback")


def test_login_url_generation(validated_microsoft):
    login = svc.build_authenticated_login_url(provider="MICROSOFT")
    assert login.provider == "MICROSOFT"
    assert "login.microsoftonline.com" in login.login_url
    assert "state=" in login.login_url


def test_oauth_state_validation(validated_microsoft):
    state = svc.generate_oauth_state("MICROSOFT")
    svc.validate_oauth_state(state, expected_provider="MICROSOFT")

    with pytest.raises(ValueError):
        svc.validate_oauth_state(state, expected_provider="GOOGLE")


def make_test_id_token(payload: dict) -> str:
    return jwt.encode(payload, "test-secret", algorithm="HS256")


@patch("services.oauth_authentication_service.httpx.Client")
def test_token_exchange_and_identity_resolution(mock_client_cls, validated_microsoft):
    id_token = make_test_id_token({
        "sub": "external-123",
        "email": "user@example.com",
        "name": "Test User",
    })

    token_response = MagicMock()
    token_response.status_code = 200
    token_response.json.return_value = {"id_token": id_token, "access_token": "provider-token"}

    mock_client = MagicMock()
    mock_client.__enter__.return_value = mock_client
    mock_client.post.return_value = token_response
    mock_client_cls.return_value = mock_client

    tokens = svc.exchange_authorization_code(provider="MICROSOFT", code="auth-code")
    identity = svc.resolve_provider_identity(provider="MICROSOFT", token_response=tokens)

    assert identity.email == "user@example.com"
    assert identity.external_id == "external-123"
    assert identity.provider_type == "MICROSOFT"


@patch("services.oauth_authentication_service.httpx.Client")
def test_session_creation(mock_client_cls, validated_microsoft):
    id_token = make_test_id_token({"sub": "external-456", "email": "cto@example.com", "name": "CTO"})
    token_response = MagicMock()
    token_response.status_code = 200
    token_response.json.return_value = {"id_token": id_token}

    mock_client = MagicMock()
    mock_client.__enter__.return_value = mock_client
    mock_client.post.return_value = token_response
    mock_client_cls.return_value = mock_client

    state = svc.generate_oauth_state("MICROSOFT")
    session = svc.complete_sso_callback(provider="MICROSOFT", code="auth-code", state=state)

    assert session.session_id.startswith("session:sso:")
    assert session.user_id.startswith("sso:microsoft:")
    assert session.access_token
    assert session.provider_type == "MICROSOFT"

    claims = svc.verify_sso_access_token(session.access_token)
    assert claims["email"] == "cto@example.com"


@patch("services.oauth_authentication_service.httpx.Client")
def test_login_failure_records_audit(mock_client_cls, validated_microsoft):
    token_response = MagicMock()
    token_response.status_code = 400
    token_response.json.return_value = {"error": "invalid_grant"}

    mock_client = MagicMock()
    mock_client.__enter__.return_value = mock_client
    mock_client.post.return_value = token_response
    mock_client_cls.return_value = mock_client

    state = svc.generate_oauth_state("MICROSOFT")
    with pytest.raises(ValueError):
        svc.complete_sso_callback(provider="MICROSOFT", code="bad-code", state=state)

    listed = audit_svc.list_events(event_type="SSO_LOGIN_FAILED")
    assert listed.total >= 1


@patch("services.oauth_authentication_service.httpx.Client")
def test_login_success_records_audit(mock_client_cls, validated_microsoft):
    id_token = make_test_id_token({"sub": "external-789", "email": "qa@example.com", "name": "QA User"})
    token_response = MagicMock()
    token_response.status_code = 200
    token_response.json.return_value = {"id_token": id_token}

    mock_client = MagicMock()
    mock_client.__enter__.return_value = mock_client
    mock_client.post.return_value = token_response
    mock_client_cls.return_value = mock_client

    state = svc.generate_oauth_state("MICROSOFT")
    svc.complete_sso_callback(provider="MICROSOFT", code="good-code", state=state)

    success = audit_svc.list_events(event_type="SSO_LOGIN_SUCCESS")
    resolved = audit_svc.list_events(event_type="SSO_IDENTITY_RESOLVED")
    assert success.total >= 1
    assert resolved.total >= 1


def test_auth_sso_routes_login_url(validated_microsoft):
    from fastapi.testclient import TestClient
    from app import app

    client = TestClient(app)
    response = client.get("/auth/sso/login-url", params={"provider": "MICROSOFT"})
    assert response.status_code == 200
    assert "login_url" in response.json()
