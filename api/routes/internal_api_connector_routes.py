# api/routes/internal_api_connector_routes.py
"""INT-03C — Internal API connectors (admin)."""
from __future__ import annotations

from typing import List, Optional

from fastapi import APIRouter, Request

from models.internal_api_connector_models import (
    ApiValidationCreateRequest,
    ApiValidationRequest,
    InternalApiConnector,
    InternalApiConnectorRegistrationRequest,
)
from services import internal_api_connector_service

admin_router = APIRouter(prefix="/local-agents/internal-apis", tags=["internal-api-connectors"])
validation_router = APIRouter(prefix="/internal-api-validations", tags=["internal-api-validations"])


@admin_router.post("/register", response_model=InternalApiConnector)
def register_internal_api_connector(
    body: InternalApiConnectorRegistrationRequest,
    request: Request,
) -> InternalApiConnector:
    return internal_api_connector_service.register_internal_api_connector(body, request)


@admin_router.get("", response_model=List[InternalApiConnector])
def list_internal_api_connectors(
    request: Request,
    agent_id: Optional[str] = None,
    limit: int = 200,
) -> List[InternalApiConnector]:
    return internal_api_connector_service.list_internal_api_connectors(
        agent_id=agent_id,
        limit=limit,
        request=request,
    )


@validation_router.post("", response_model=ApiValidationRequest)
def create_internal_api_validation(
    body: ApiValidationCreateRequest,
    request: Request,
) -> ApiValidationRequest:
    return internal_api_connector_service.create_validation_request(body, request)


@validation_router.get("", response_model=List[ApiValidationRequest])
def list_internal_api_validations(
    request: Request,
    connector_id: Optional[str] = None,
    limit: int = 200,
) -> List[ApiValidationRequest]:
    return internal_api_connector_service.list_validation_requests(
        connector_id=connector_id,
        limit=limit,
        request=request,
    )
