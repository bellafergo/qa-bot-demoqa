# models/database_connector_models.py
"""INT-03B — Secure read-only database connectors (agent-mediated)."""
from __future__ import annotations

from typing import List, Literal, Optional

from pydantic import BaseModel, Field

from models.incident_models import DatabaseValidationCheck

ConnectionStatus = Literal[
    "CONNECTED",
    "DISCONNECTED",
    "UNKNOWN",
    "DEGRADED",
    "ERROR",
    "PENDING_VALIDATION",
]
AssetScope = Literal["platform_internal", "customer_external"]
ExecutionMode = Literal["platform_backend", "local_agent"]
ExecutionStatus = Literal["SUCCESS", "FAILED", "BLOCKED", "REJECTED"]


class DatabaseConnection(BaseModel):
    connection_id: str
    agent_id: str
    name: str
    database_type: str
    host_label: str
    database_name: str
    status: ConnectionStatus = "UNKNOWN"
    created_at: str
    asset_scope: AssetScope = "customer_external"
    execution_mode: ExecutionMode = "local_agent"
    last_probe_at: Optional[str] = None
    last_probe_status: Optional[str] = None
    last_probe_summary: Optional[str] = None
    is_platform_managed: bool = False
    created_by_system: bool = False
    already_exists: bool = False


class DatabaseConnectionRegistrationRequest(BaseModel):
    agent_id: str = Field(..., min_length=1, max_length=256)
    name: str = Field(..., min_length=1, max_length=256)
    database_type: Literal["postgresql", "mysql", "sqlserver", "sqlite"] = "postgresql"
    host_label: str = Field(..., min_length=1, max_length=256)
    database_name: str = Field(..., min_length=1, max_length=256)
    asset_scope: AssetScope = "customer_external"
    execution_mode: ExecutionMode = "local_agent"


class DatabaseValidationExecution(BaseModel):
    execution_id: str
    check_id: str
    connection_id: str
    executed_at: str
    status: ExecutionStatus
    row_count: int = Field(default=0, ge=0)
    summary: str = ""
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    requires_user_approval: bool = True


class DatabaseValidationExecuteRequest(BaseModel):
    check_id: str = Field(..., min_length=1, max_length=256)
    connection_id: str = Field(..., min_length=1, max_length=256)
    row_count: Optional[int] = Field(default=None, ge=0)
    summary: Optional[str] = Field(default=None, max_length=512)
    status: Optional[ExecutionStatus] = None


class DatabaseValidationExecuteResponse(BaseModel):
    execution_id: str
    check_id: str
    connection_id: str
    status: ExecutionStatus
    row_count: int = Field(default=0, ge=0)
    summary: str = ""


class DatabaseValidationExecutionRequest(BaseModel):
    check: DatabaseValidationCheck
    connection_id: str = Field(..., min_length=1, max_length=256)


class ApprovalSimulationRequest(BaseModel):
    check_id: str = Field(..., min_length=1, max_length=256)
    status: Literal["APPROVED", "REJECTED", "PENDING"] = "APPROVED"
