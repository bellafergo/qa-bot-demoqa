# api/routes/auth_sso_routes.py
"""EC-01G — Enterprise SSO authentication routes."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException, Query, Request

from models.auth_models import AuthenticationSession, SSOProviderType, SSOLoginUrl, UserIdentity
from services.auth_identity_service import auth_context_from_state, get_user_identity
from services.oauth_authentication_service import (
    build_authenticated_login_url,
    complete_sso_callback,
    list_login_ready_providers,
)

logger = logging.getLogger("vanya.auth_sso_routes")

router = APIRouter(prefix="/auth/sso", tags=["auth-sso"])


@router.get("/login-url", response_model=SSOLoginUrl)
def get_sso_auth_login_url(
    provider: SSOProviderType = Query(..., description="Enterprise SSO provider"),
):
    """Return an OAuth authorize URL for a configured and validated provider."""
    try:
        return build_authenticated_login_url(provider=provider)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    except Exception as exc:
        logger.exception("GET /auth/sso/login-url failed provider=%s", provider)
        raise HTTPException(status_code=500, detail=f"Login URL generation failed: {exc}") from exc


@router.get("/callback", response_model=AuthenticationSession)
def get_sso_auth_callback(
    provider: SSOProviderType = Query(..., description="Enterprise SSO provider"),
    code: str = Query(..., description="OAuth authorization code"),
    state: str = Query(..., description="Signed OAuth state"),
):
    """Exchange OAuth code, resolve identity, and create a Vanya SSO session."""
    try:
        return complete_sso_callback(provider=provider, code=code, state=state)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    except Exception as exc:
        logger.exception("GET /auth/sso/callback failed provider=%s", provider)
        raise HTTPException(status_code=500, detail=f"SSO callback failed: {exc}") from exc


@router.get("/me", response_model=UserIdentity)
def get_sso_auth_me(request: Request):
    """Return the authenticated user identity for the current session."""
    ctx = auth_context_from_state(request.state)
    uid = str(ctx.get("user_id") or "").strip()
    if not uid or uid in {"anonymous", "unknown"}:
        raise HTTPException(status_code=401, detail="Authentication required.")

    return get_user_identity(
        user_id=uid,
        email=ctx.get("email"),
        display_name=ctx.get("display_name"),
        auth_kind=ctx.get("auth_kind"),
        external_id=ctx.get("external_id"),
        provider_type=ctx.get("provider_type"),
    )


@router.get("/providers")
def get_sso_login_providers():
    """Return SSO providers ready for end-user login."""
    return {"providers": list_login_ready_providers(), "total": len(list_login_ready_providers())}
