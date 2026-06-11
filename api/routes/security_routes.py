# api/routes/security_routes.py
"""Enterprise SEC-01A/B — Security, authentication, and RBAC foundation (read-only)."""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, HTTPException, Query, Request
from pydantic import BaseModel

from models.auth_models import (
    IdentityProvidersResponse,
    SecurityReadinessReport,
    SSOLoginUrl,
    SSOProviderType,
    SSOProvidersResponse,
    SSOValidationResult,
)
from models.rbac_models import PermissionsResponse, RBACReadinessReport, RolesResponse
from services.auth_identity_service import (
    auth_context_from_state,
    build_identity_providers_response,
    build_security_readiness_report,
)
from services.rbac_service import (
    build_permissions_response,
    build_rbac_readiness_report,
    build_roles_response,
)
from services.sso_service import build_login_url, list_sso_provider_configs, validate_provider_config

logger = logging.getLogger("vanya.security_routes")


class SSOValidateRequest(BaseModel):
    provider: SSOProviderType
    enabled: bool = True
    client_id: Optional[str] = None
    tenant_id: Optional[str] = None
    issuer: Optional[str] = None

router = APIRouter(prefix="/security", tags=["security"])


@router.get("/readiness", response_model=SecurityReadinessReport)
def get_security_readiness(request: Request):
    """Return deterministic enterprise security readiness metadata."""
    ctx = auth_context_from_state(request.state)
    return build_security_readiness_report(
        user_id=ctx.get("user_id"),
        email=ctx.get("email"),
        auth_kind=ctx.get("auth_kind"),
    )


@router.get("/providers", response_model=IdentityProvidersResponse)
def get_security_providers(
    enabled_only: bool = Query(True, description="Return only enabled identity providers"),
):
    """Return identity provider registry entries."""
    return build_identity_providers_response(enabled_only=enabled_only)


@router.get("/rbac", response_model=RBACReadinessReport)
def get_rbac_readiness():
    """Return RBAC foundation readiness metadata."""
    return build_rbac_readiness_report()


@router.get("/roles", response_model=RolesResponse)
def get_security_roles():
    """Return default RBAC roles."""
    return build_roles_response()


@router.get("/permissions", response_model=PermissionsResponse)
def get_security_permissions():
    """Return RBAC permission catalog."""
    return build_permissions_response()


@router.get("/sso/providers", response_model=SSOProvidersResponse)
def get_sso_providers():
    """Return configured enterprise SSO providers (SEC-01D)."""
    return list_sso_provider_configs()


@router.post("/sso/validate", response_model=SSOValidationResult)
def post_sso_validate(body: SSOValidateRequest):
    """Validate enterprise SSO provider configuration without login."""
    return validate_provider_config(
        provider=body.provider,
        client_id=body.client_id,
        tenant_id=body.tenant_id,
        issuer=body.issuer,
        enabled=body.enabled,
    )


@router.get("/sso/login-url", response_model=SSOLoginUrl)
def get_sso_login_url(
    provider: SSOProviderType = Query(..., description="Enterprise SSO provider"),
):
    """Generate OAuth login URL for a validated provider (no token exchange)."""
    try:
        return build_login_url(provider=provider)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    except Exception as exc:
        logger.exception("GET /security/sso/login-url failed provider=%s", provider)
        raise HTTPException(status_code=500, detail=f"Login URL generation failed: {exc}") from exc
