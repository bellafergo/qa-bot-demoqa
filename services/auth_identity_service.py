# services/auth_identity_service.py
"""
Enterprise SEC-01A — Unified identity abstraction (architecture foundation).

Maps current Supabase/local authentication to the identity model without
replacing or migrating existing login. No external SSO calls.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from models.auth_models import (
    AuthenticationSession,
    IdentityProvider,
    IdentityProvidersResponse,
    SecurityReadinessReport,
    UserIdentity,
)

logger = logging.getLogger("vanya.auth_identity")

_PROVIDER_CATALOG: Tuple[Tuple[str, str, str], ...] = (
    ("local", "Local Authentication", "LOCAL"),
    ("google", "Google Workspace", "GOOGLE"),
    ("microsoft", "Microsoft Entra ID", "MICROSOFT"),
    ("okta", "Okta", "OKTA"),
    ("saml", "SAML 2.0", "SAML"),
)

_SSO_PROVIDER_TYPES = frozenset({"GOOGLE", "MICROSOFT", "OKTA", "SAML"})


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _build_provider_catalog() -> List[IdentityProvider]:
    from services.sso_service import is_provider_enabled

    providers: List[IdentityProvider] = []
    for provider_id, provider_name, provider_type in _PROVIDER_CATALOG:
        if provider_type == "LOCAL":
            enabled = True
        elif provider_type in _SSO_PROVIDER_TYPES:
            enabled = is_provider_enabled(provider_type)
        else:
            enabled = False
        providers.append(
            IdentityProvider(
                provider_id=provider_id,
                provider_name=provider_name,
                provider_type=provider_type,  # type: ignore[arg-type]
                enabled=enabled,
            )
        )
    return providers


def list_identity_providers(*, enabled_only: bool = False) -> List[IdentityProvider]:
    """Return identity provider registry entries."""
    providers = _build_provider_catalog()
    if enabled_only:
        return [p for p in providers if p.enabled]
    return providers


def list_enabled_providers() -> List[IdentityProvider]:
    """Return providers enabled for authentication."""
    return list_identity_providers(enabled_only=True)


def resolve_authentication_source(
    *,
    auth_kind: Optional[str] = None,
) -> str:
    """
    Resolve active authentication source for the current request context.
    Foundation sprint: always LOCAL (Supabase JWT maps to local identity layer).
    """
    _ = auth_kind
    return "LOCAL"


def get_user_identity(
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    display_name: Optional[str] = None,
    auth_kind: Optional[str] = None,
    external_id: Optional[str] = None,
) -> UserIdentity:
    """Build a read-only user identity view from request context."""
    provider_type = resolve_authentication_source(auth_kind=auth_kind)
    uid = str(user_id or "").strip() or "anonymous"
    resolved_external = external_id or (uid if uid not in ("anonymous", "service") else None)
    identity_id = f"identity:{provider_type.lower()}:{uid}"

    if auth_kind == "service":
        return UserIdentity(
            identity_id=identity_id,
            user_id="service",
            provider_type="LOCAL",  # type: ignore[arg-type]
            external_id="service",
            email=None,
            display_name="Service Token",
        )

    name = (display_name or "").strip() or None
    if not name and email:
        name = str(email).split("@")[0]

    return UserIdentity(
        identity_id=identity_id,
        user_id=uid,
        provider_type=provider_type,  # type: ignore[arg-type]
        external_id=resolved_external,
        email=(email or "").strip() or None,
        display_name=name,
    )


def get_session_provider(
    *,
    user_id: Optional[str] = None,
    auth_kind: Optional[str] = None,
    session_id: Optional[str] = None,
    login_time: Optional[str] = None,
    last_activity: Optional[str] = None,
) -> AuthenticationSession:
    """Return session ownership metadata for the current authentication context."""
    provider_type = resolve_authentication_source(auth_kind=auth_kind)
    uid = str(user_id or "").strip() or "anonymous"
    sid = (session_id or "").strip() or f"session:{provider_type.lower()}:{uid}"
    now = _utc_now_iso()

    return AuthenticationSession(
        session_id=sid,
        user_id=uid,
        provider_type=provider_type,  # type: ignore[arg-type]
        login_time=login_time or now,
        last_activity=last_activity or now,
    )


def _security_score(*, sso_ready: bool, audit_ready: bool, rbac_ready: bool) -> int:
    score = 25  # LOCAL authentication foundation
    if sso_ready:
        score += 25
    if audit_ready:
        score += 25
    if rbac_ready:
        score += 25
    return min(100, score)


def build_security_readiness_report(
    *,
    user_id: Optional[str] = None,
    email: Optional[str] = None,
    auth_kind: Optional[str] = None,
) -> SecurityReadinessReport:
    """Deterministic enterprise security readiness snapshot (foundation only)."""
    from services.sso_service import (
        available_sso_providers,
        configured_sso_providers,
        is_sso_ready,
    )

    providers = list_identity_providers()
    sso_ready = is_sso_ready()
    available = available_sso_providers()
    configured = configured_sso_providers()
    audit_ready = False
    from services.rbac_service import build_rbac_readiness_report

    rbac_report = build_rbac_readiness_report()
    rbac_ready = rbac_report.default_roles_ready
    active_provider = resolve_authentication_source(auth_kind=auth_kind)

    method = "LOCAL"
    if sso_ready:
        method = "HYBRID"

    score = _security_score(sso_ready=sso_ready, audit_ready=audit_ready, rbac_ready=rbac_ready)

    identity = get_user_identity(user_id=user_id, email=email, auth_kind=auth_kind)
    sso_status = "SSO READY" if sso_ready else "LOCAL ONLY"
    summary = (
        f"Authentication status: {sso_status} ({method} via {active_provider}). "
        f"Configured SSO providers: {len(configured)} of {len(available)}. "
        f"RBAC {'is configured' if rbac_ready else 'is not configured'}. "
        f"Security score {score}/100."
    )
    if identity.email:
        summary += f" Active identity: {identity.email}."

    return SecurityReadinessReport(
        authentication_method=method,  # type: ignore[arg-type]
        sso_ready=sso_ready,
        audit_ready=audit_ready,
        rbac_ready=rbac_ready,
        security_score=score,
        active_provider_type=active_provider,  # type: ignore[arg-type]
        available_sso_providers=available,
        configured_sso_providers=configured,
        summary=summary,
    )


def build_identity_providers_response(*, enabled_only: bool = True) -> IdentityProvidersResponse:
    providers = list_identity_providers(enabled_only=enabled_only)
    return IdentityProvidersResponse(providers=providers, total=len(providers))


def auth_context_from_state(state: Any) -> Dict[str, Optional[str]]:
    """Extract optional auth context from Starlette request.state."""
    return {
        "user_id": getattr(state, "user_id", None),
        "email": getattr(state, "email", None),
        "auth_kind": getattr(state, "auth_kind", None),
    }
