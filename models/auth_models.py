# models/auth_models.py
"""
Enterprise SEC-01A — Authentication & SSO foundation.

Identity provider abstraction and security readiness metadata only.
No SAML/OAuth login implementation in this sprint.
"""
from __future__ import annotations

from typing import List, Literal, Optional

from pydantic import BaseModel, Field

ProviderType = Literal["LOCAL", "GOOGLE", "MICROSOFT", "OKTA", "SAML"]
AuthenticationMethod = Literal["LOCAL", "SSO", "HYBRID"]


class IdentityProvider(BaseModel):
    provider_id: str
    provider_name: str
    provider_type: ProviderType
    enabled: bool = False


class UserIdentity(BaseModel):
    identity_id: str
    user_id: str
    provider_type: ProviderType = "LOCAL"
    external_id: Optional[str] = None
    email: Optional[str] = None
    display_name: Optional[str] = None


class AuthenticationSession(BaseModel):
    session_id: str
    user_id: str
    provider_type: ProviderType = "LOCAL"
    login_time: Optional[str] = None
    last_activity: Optional[str] = None


class SecurityReadinessReport(BaseModel):
    authentication_method: AuthenticationMethod = "LOCAL"
    sso_ready: bool = False
    audit_ready: bool = False
    rbac_ready: bool = False
    security_score: int = Field(default=25, ge=0, le=100)
    active_provider_type: ProviderType = "LOCAL"
    summary: str = ""


class IdentityProvidersResponse(BaseModel):
    providers: List[IdentityProvider] = Field(default_factory=list)
    total: int = Field(default=0, ge=0)
