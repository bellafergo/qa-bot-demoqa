# api/routes/database_connector_routes.py
"""INT-03B — Secure database connectors (admin + agent API)."""
from __future__ import annotations

from typing import List, Optional

from fastapi import APIRouter, Request

from models.database_connector_models import (
    ApprovalSimulationRequest,
    DatabaseConnection,
    DatabaseConnectionRegistrationRequest,
    DatabaseValidationExecution,
    DatabaseValidationExecutionRequest,
)
from models.incident_models import ApprovalRequest
from services import database_connector_service

admin_router = APIRouter(prefix="/local-agents/database-connections", tags=["database-connectors"])
validation_router = APIRouter(prefix="/database-validation", tags=["database-validation"])


@admin_router.post("/register", response_model=DatabaseConnection)
def register_database_connection(
    body: DatabaseConnectionRegistrationRequest,
    request: Request,
) -> DatabaseConnection:
    return database_connector_service.register_connection(body, request)


@admin_router.get("", response_model=List[DatabaseConnection])
def list_database_connections(
    request: Request,
    agent_id: Optional[str] = None,
    limit: int = 200,
) -> List[DatabaseConnection]:
    return database_connector_service.list_connections(agent_id=agent_id, limit=limit, request=request)


@validation_router.get("/executions", response_model=List[DatabaseValidationExecution])
def list_database_validation_executions(
    request: Request,
    connection_id: Optional[str] = None,
    limit: int = 100,
) -> List[DatabaseValidationExecution]:
    return database_connector_service.list_executions(connection_id=connection_id, limit=limit, request=request)


@validation_router.post("/executions", response_model=DatabaseValidationExecution)
def execute_database_validation(
    body: DatabaseValidationExecutionRequest,
    request: Request,
) -> DatabaseValidationExecution:
    return database_connector_service.trigger_validation_execution(body, request)


@validation_router.post("/approvals/simulate", response_model=ApprovalRequest)
def simulate_database_validation_approval(
    body: ApprovalSimulationRequest,
    request: Request,
) -> ApprovalRequest:
    database_connector_service.require_local_agent_admin(request)
    return database_connector_service.simulate_approval(body.check_id, body.status)
