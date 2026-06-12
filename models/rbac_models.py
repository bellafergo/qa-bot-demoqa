# models/rbac_models.py
"""
Enterprise SEC-01B — RBAC foundation.

Roles, permissions, and readiness metadata only. No enforcement.
"""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel, Field


class Permission(BaseModel):
    permission_id: str
    permission_name: str
    description: str = ""


class Role(BaseModel):
    role_id: str
    role_name: str
    description: str = ""


class RolePermission(BaseModel):
    role_name: str
    permissions: List[str] = Field(default_factory=list)


class UserRole(BaseModel):
    user_id: str
    role_name: str


class RBACReadinessReport(BaseModel):
    role_count: int = Field(default=0, ge=0)
    permission_count: int = Field(default=0, ge=0)
    default_roles_ready: bool = True
    enforcement_enabled: bool = False
    readiness_score: int = Field(default=50, ge=0, le=100)
    summary: str = ""


class RolesResponse(BaseModel):
    roles: List[Role] = Field(default_factory=list)
    total: int = Field(default=0, ge=0)


class UserSecurityContext(BaseModel):
    user_id: str
    email: Optional[str] = None
    role_name: str
    permissions: List[str] = Field(default_factory=list)
    enforcement_enabled: bool = False


class PermissionsResponse(BaseModel):
    permissions: List[Permission] = Field(default_factory=list)
    total: int = Field(default=0, ge=0)
