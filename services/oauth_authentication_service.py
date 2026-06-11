# services/oauth_authentication_service.py
"""
EC-01G — Real enterprise SSO authentication (OAuth authorization code flow).

Hybrid authentication: preserves Supabase/local login; adds Microsoft, Google, and Okta.
Provider access tokens are used only for identity resolution and are never stored.
"""
from __future__ import annotations

import base64
import hashlib
import hmac
import json
import logging
import os
import secrets
import threading
import time
from datetime import datetime, timezone
from typing import Any, Dict, Optional, Tuple
from urllib.parse import urlencode

import httpx

from models.auth_models import (
    AuthenticationSession,
    SSOProviderType,
    SSOLoginUrl,
    UserIdentity,
)

logger = logging.getLogger("vanya.oauth_auth")

_STATE_TTL_SECONDS = 600
_SESSION_TTL_SECONDS = 60 * 60 * 12
_LOCK = threading.RLock()
_SESSIONS: Dict[str, Dict[str, Any]] = {}


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _state_secret() -> str:
    return (
        os.getenv("VANYA_SSO_STATE_SECRET")
        or os.getenv("VANYA_SERVICE_TOKEN")
        or "vanya-sso-dev-state"
    ).strip()


def _jwt_secret() -> str:
    return (
        os.getenv("VANYA_SSO_JWT_SECRET")
        or os.getenv("VANYA_SERVICE_TOKEN")
        or "vanya-sso-dev-jwt"
    ).strip()


def _client_secret(provider: SSOProviderType) -> str:
    from core.settings import settings

    mapping = {
        "MICROSOFT": settings.SSO_MICROSOFT_CLIENT_SECRET,
        "GOOGLE": settings.SSO_GOOGLE_CLIENT_SECRET,
        "OKTA": settings.SSO_OKTA_CLIENT_SECRET,
    }
    return (mapping.get(provider) or "").strip()


def _provider_config(provider: SSOProviderType):
    from services.sso_service import _redirect_uri, _to_provider_config

    config = _to_provider_config(provider)
    if not config.configured or not config.validated:
        raise ValueError(f"{provider} SSO is not configured and validated.")
    if not config.enabled:
        raise ValueError(f"{provider} SSO is disabled.")
    secret = _client_secret(provider)
    if not secret:
        raise ValueError(f"{provider} client_secret is not configured on the server.")
    return config, _redirect_uri(), secret


def generate_oauth_state(provider: SSOProviderType) -> str:
    payload = {
        "provider": provider,
        "nonce": secrets.token_urlsafe(16),
        "exp": int(time.time()) + _STATE_TTL_SECONDS,
    }
    raw = base64.urlsafe_b64encode(json.dumps(payload, separators=(",", ":")).encode()).decode().rstrip("=")
    sig = hmac.new(_state_secret().encode(), raw.encode(), hashlib.sha256).hexdigest()
    return f"{raw}.{sig}"


def validate_oauth_state(state: str, *, expected_provider: SSOProviderType) -> None:
    token = (state or "").strip()
    if "." not in token:
        raise ValueError("Invalid OAuth state.")
    raw, sig = token.rsplit(".", 1)
    expected_sig = hmac.new(_state_secret().encode(), raw.encode(), hashlib.sha256).hexdigest()
    if not hmac.compare_digest(sig, expected_sig):
        raise ValueError("OAuth state signature mismatch.")

    padded = raw + "=" * (-len(raw) % 4)
    try:
        payload = json.loads(base64.urlsafe_b64decode(padded.encode()).decode())
    except Exception as exc:
        raise ValueError("Invalid OAuth state payload.") from exc

    if str(payload.get("provider") or "").upper() != expected_provider:
        raise ValueError("OAuth state provider mismatch.")
    if int(payload.get("exp") or 0) < int(time.time()):
        raise ValueError("OAuth state has expired.")


def build_authenticated_login_url(*, provider: SSOProviderType) -> SSOLoginUrl:
    """Build provider authorize URL with signed OAuth state."""
    config, redirect_uri, _secret = _provider_config(provider)
    state = generate_oauth_state(provider)

    if provider == "MICROSOFT":
        tenant = config.tenant_id or "common"
        base = f"https://login.microsoftonline.com/{tenant}/oauth2/v2.0/authorize"
        params = {
            "client_id": config.client_id,
            "response_type": "code",
            "redirect_uri": redirect_uri,
            "response_mode": "query",
            "scope": "openid profile email",
            "state": state,
        }
    elif provider == "GOOGLE":
        base = "https://accounts.google.com/o/oauth2/v2/auth"
        params = {
            "client_id": config.client_id,
            "response_type": "code",
            "redirect_uri": redirect_uri,
            "scope": "openid email profile",
            "access_type": "offline",
            "state": state,
        }
    else:
        base = f"{(config.issuer or '').rstrip('/')}/v1/authorize"
        params = {
            "client_id": config.client_id,
            "response_type": "code",
            "redirect_uri": redirect_uri,
            "scope": "openid profile email",
            "state": state,
        }

    login = SSOLoginUrl(provider=provider, login_url=f"{base}?{urlencode(params)}")
    from services.audit_event_service import safe_record_event

    safe_record_event(
        event_type="SSO_LOGIN_URL_GENERATED",
        resource_type="SECURITY",
        resource_id=provider,
        action="generate_login_url",
        result="SUCCESS",
        metadata={"redirect_uri": redirect_uri, "flow": "oauth_authentication"},
    )
    return login


def exchange_authorization_code(*, provider: SSOProviderType, code: str) -> Dict[str, Any]:
    """Exchange OAuth authorization code for tokens (access/id). Tokens are not persisted."""
    config, redirect_uri, secret = _provider_config(provider)
    auth_code = (code or "").strip()
    if not auth_code:
        raise ValueError("Authorization code is required.")

    if provider == "MICROSOFT":
        tenant = config.tenant_id or "common"
        token_url = f"https://login.microsoftonline.com/{tenant}/oauth2/v2.0/token"
    elif provider == "GOOGLE":
        token_url = "https://oauth2.googleapis.com/token"
    else:
        token_url = f"{(config.issuer or '').rstrip('/')}/v1/token"

    payload = {
        "client_id": config.client_id,
        "client_secret": secret,
        "grant_type": "authorization_code",
        "code": auth_code,
        "redirect_uri": redirect_uri,
    }

    try:
        with httpx.Client(timeout=30) as client:
            response = client.post(token_url, data=payload)
    except httpx.RequestError as exc:
        logger.warning("sso token exchange network error provider=%s: %s", provider, type(exc).__name__)
        raise ValueError("Could not reach provider token endpoint.") from exc

    if response.status_code >= 400:
        logger.warning("sso token exchange failed provider=%s status=%s", provider, response.status_code)
        raise ValueError("Provider token exchange failed.")

    data = response.json()
    if not isinstance(data, dict):
        raise ValueError("Provider token response was invalid.")
    return data


def _decode_unverified_jwt(token: str) -> Dict[str, Any]:
    import jwt

    try:
        payload = jwt.decode(token, options={"verify_signature": False})
    except Exception as exc:
        raise ValueError("Could not parse provider identity token.") from exc
    if not isinstance(payload, dict):
        raise ValueError("Provider identity token payload was invalid.")
    return payload


def _identity_from_id_token(provider: SSOProviderType, id_token: str) -> Dict[str, str]:
    claims = _decode_unverified_jwt(id_token)
    email = (
        str(claims.get("email") or claims.get("preferred_username") or claims.get("upn") or "").strip()
    )
    external_id = str(claims.get("sub") or "").strip()
    display_name = str(claims.get("name") or claims.get("given_name") or "").strip()
    if not external_id:
        raise ValueError("Provider identity token did not include a subject.")
    return {
        "provider": provider,
        "external_id": external_id,
        "email": email or None,
        "display_name": display_name or None,
    }


def _identity_from_userinfo(
    *,
    provider: SSOProviderType,
    access_token: str,
    userinfo_url: str,
) -> Dict[str, str]:
    token = (access_token or "").strip()
    if not token:
        raise ValueError("Provider access token missing for identity resolution.")
    try:
        with httpx.Client(timeout=30) as client:
            response = client.get(userinfo_url, headers={"Authorization": f"Bearer {token}"})
    except httpx.RequestError as exc:
        raise ValueError("Could not reach provider userinfo endpoint.") from exc
    if response.status_code >= 400:
        raise ValueError("Provider userinfo request failed.")
    data = response.json()
    if not isinstance(data, dict):
        raise ValueError("Provider userinfo response was invalid.")
    email = str(data.get("email") or "").strip() or None
    external_id = str(data.get("sub") or data.get("id") or "").strip()
    display_name = str(data.get("name") or data.get("given_name") or "").strip() or None
    if not external_id:
        raise ValueError("Provider userinfo did not include a subject.")
    return {
        "provider": provider,
        "external_id": external_id,
        "email": email,
        "display_name": display_name,
    }


def resolve_provider_identity(*, provider: SSOProviderType, token_response: Dict[str, Any]) -> UserIdentity:
    """Resolve enterprise identity from provider token response without storing provider tokens."""
    id_token = str(token_response.get("id_token") or "").strip()
    access_token = str(token_response.get("access_token") or "").strip()

    if id_token:
        raw = _identity_from_id_token(provider, id_token)
    elif provider == "GOOGLE" and access_token:
        raw = _identity_from_userinfo(
            provider=provider,
            access_token=access_token,
            userinfo_url="https://openidconnect.googleapis.com/v1/userinfo",
        )
    elif provider == "OKTA" and access_token:
        from services.sso_service import _to_provider_config

        issuer = (_to_provider_config(provider).issuer or "").rstrip("/")
        raw = _identity_from_userinfo(
            provider=provider,
            access_token=access_token,
            userinfo_url=f"{issuer}/v1/userinfo",
        )
    elif provider == "MICROSOFT" and access_token:
        raw = _identity_from_userinfo(
            provider=provider,
            access_token=access_token,
            userinfo_url="https://graph.microsoft.com/oidc/userinfo",
        )
    else:
        raise ValueError("Provider token response did not include identity claims.")

    from services.auth_identity_service import get_user_identity

    user_id = f"sso:{provider.lower()}:{raw['external_id']}"
    identity = get_user_identity(
        user_id=user_id,
        email=raw.get("email"),
        display_name=raw.get("display_name"),
        auth_kind="sso",
        external_id=raw["external_id"],
        provider_type=provider,
    )

    from services.audit_event_service import safe_record_event

    safe_record_event(
        event_type="SSO_IDENTITY_RESOLVED",
        resource_type="SECURITY",
        resource_id=provider,
        action="resolve_identity",
        result="SUCCESS",
        user_id=user_id,
        user_email=identity.email,
        metadata={"external_id": raw["external_id"]},
    )
    return identity


def issue_sso_access_token(identity: UserIdentity) -> str:
    """Issue a Vanya SSO session JWT (not a provider token)."""
    import jwt

    now = int(time.time())
    payload = {
        "sub": identity.user_id,
        "email": identity.email,
        "name": identity.display_name,
        "provider": identity.provider_type,
        "external_id": identity.external_id,
        "auth_kind": "sso",
        "iss": "vanya-sso",
        "aud": "vanya",
        "iat": now,
        "exp": now + _SESSION_TTL_SECONDS,
    }
    return jwt.encode(payload, _jwt_secret(), algorithm="HS256")


def verify_sso_access_token(token: str) -> Dict[str, Any]:
    import jwt

    payload = jwt.decode(
        token,
        _jwt_secret(),
        algorithms=["HS256"],
        audience="vanya",
        issuer="vanya-sso",
        options={"require": ["exp", "sub"]},
        leeway=60,
    )
    if not isinstance(payload, dict):
        raise ValueError("Invalid SSO session token.")
    return payload


def create_authenticated_session(*, identity: UserIdentity) -> AuthenticationSession:
    """Create an in-memory Vanya session and issue an SSO access token."""
    access_token = issue_sso_access_token(identity)
    now = _utc_now_iso()
    session_id = f"session:sso:{secrets.token_urlsafe(12)}"

    with _LOCK:
        _SESSIONS[session_id] = {
            "session_id": session_id,
            "user_id": identity.user_id,
            "email": identity.email,
            "display_name": identity.display_name,
            "provider_type": identity.provider_type,
            "external_id": identity.external_id,
            "login_time": now,
            "last_activity": now,
            "expires_at": int(time.time()) + _SESSION_TTL_SECONDS,
        }

    return AuthenticationSession(
        session_id=session_id,
        user_id=identity.user_id,
        provider_type=identity.provider_type,
        login_time=now,
        last_activity=now,
        access_token=access_token,
    )


def complete_sso_callback(
    *,
    provider: SSOProviderType,
    code: str,
    state: str,
) -> AuthenticationSession:
    """Validate state, exchange code, resolve identity, and create session."""
    try:
        validate_oauth_state(state, expected_provider=provider)
        token_response = exchange_authorization_code(provider=provider, code=code)
        identity = resolve_provider_identity(provider=provider, token_response=token_response)
        session = create_authenticated_session(identity=identity)

        from services.audit_event_service import safe_record_event

        safe_record_event(
            event_type="SSO_LOGIN_SUCCESS",
            resource_type="SECURITY",
            resource_id=provider,
            action="login",
            result="SUCCESS",
            user_id=identity.user_id,
            user_email=identity.email,
            metadata={"session_id": session.session_id},
        )
        return session
    except Exception as exc:
        from services.audit_event_service import safe_record_event

        safe_record_event(
            event_type="SSO_LOGIN_FAILED",
            resource_type="SECURITY",
            resource_id=provider,
            action="login",
            result="FAILURE",
            metadata={"error": str(exc)},
        )
        raise


def get_identity_for_session(session_id: str) -> Optional[UserIdentity]:
    sid = (session_id or "").strip()
    if not sid:
        return None
    with _LOCK:
        row = _SESSIONS.get(sid)
    if not row:
        return None
    if int(row.get("expires_at") or 0) < int(time.time()):
        return None

    from services.auth_identity_service import get_user_identity

    return get_user_identity(
        user_id=row["user_id"],
        email=row.get("email"),
        display_name=row.get("display_name"),
        auth_kind="sso",
        external_id=row.get("external_id"),
        provider_type=row.get("provider_type"),
    )


def list_login_ready_providers() -> list[SSOProviderType]:
    from services.sso_service import list_sso_provider_configs

    return [
        p.provider
        for p in list_sso_provider_configs().providers
        if p.configured and p.validated and p.enabled and _client_secret(p.provider)
    ]
