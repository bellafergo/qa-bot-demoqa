# api/routes/security_routes.py
"""Enterprise SEC-01A — Authentication & SSO foundation (read-only)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, Query, Request

from models.auth_models import IdentityProvidersResponse, SecurityReadinessReport
from services.auth_identity_service import (
    auth_context_from_state,
    build_identity_providers_response,
    build_security_readiness_report,
)

logger = logging.getLogger("vanya.security_routes")

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
