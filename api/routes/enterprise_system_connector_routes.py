# api/routes/enterprise_system_connector_routes.py
"""INT-03D — Enterprise systems connectors (admin)."""
from __future__ import annotations

from typing import List, Optional

from fastapi import APIRouter, Request

from models.enterprise_system_connector_models import (
    EnterpriseSystemConnector,
    EnterpriseSystemRegistrationRequest,
    EnterpriseSystemValidationCreateRequest,
    EnterpriseValidationRequest,
)
from services import enterprise_system_connector_service

admin_router = APIRouter(prefix="/local-agents/enterprise-systems", tags=["enterprise-systems"])
validation_router = APIRouter(prefix="/enterprise-system-validations", tags=["enterprise-system-validations"])


@admin_router.post("/register", response_model=EnterpriseSystemConnector)
def register_enterprise_system(
    body: EnterpriseSystemRegistrationRequest,
    request: Request,
) -> EnterpriseSystemConnector:
    return enterprise_system_connector_service.register_enterprise_system(body, request)


@admin_router.get("", response_model=List[EnterpriseSystemConnector])
def list_enterprise_systems(
    request: Request,
    agent_id: Optional[str] = None,
    limit: int = 200,
) -> List[EnterpriseSystemConnector]:
    return enterprise_system_connector_service.list_enterprise_systems(
        agent_id=agent_id,
        limit=limit,
        request=request,
    )


@validation_router.post("", response_model=EnterpriseValidationRequest)
def create_enterprise_system_validation(
    body: EnterpriseSystemValidationCreateRequest,
    request: Request,
) -> EnterpriseValidationRequest:
    return enterprise_system_connector_service.create_enterprise_validation(body, request)


@validation_router.get("", response_model=List[EnterpriseValidationRequest])
def list_enterprise_system_validations(
    request: Request,
    connector_id: Optional[str] = None,
    limit: int = 200,
) -> List[EnterpriseValidationRequest]:
    return enterprise_system_connector_service.list_enterprise_validations(
        connector_id=connector_id,
        limit=limit,
        request=request,
    )
