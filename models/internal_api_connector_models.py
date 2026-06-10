# models/internal_api_connector_models.py
"""INT-03C — Internal API connectors (agent-mediated, read-only foundation)."""
from __future__ import annotations

from typing import List, Literal, Optional

from pydantic import BaseModel, Field

ApiType = Literal["REST", "SOAP", "GRAPHQL", "RPC", "OTHER"]
ConnectorStatus = Literal["ACTIVE", "INACTIVE", "UNKNOWN"]
HttpMethod = Literal["GET", "POST", "PUT", "PATCH", "DELETE"]
ValidationType = Literal[
    "availability_check",
    "contract_validation",
    "schema_validation",
    "response_structure_validation",
]
ValidationStatus = Literal["PENDING", "APPROVED", "BLOCKED", "COMPLETED"]


class InternalApiConnector(BaseModel):
    connector_id: str
    agent_id: str
    name: str
    api_type: ApiType = "REST"
    environment: str
    status: ConnectorStatus = "UNKNOWN"
    base_url_label: str
    endpoint_count: int = Field(default=0, ge=0)
    read_only: bool = True


class ApiEndpoint(BaseModel):
    endpoint_id: str
    connector_id: str
    method: HttpMethod
    path_label: str
    category: str = ""
    read_only: bool = True


class ApiValidationRequest(BaseModel):
    request_id: str
    connector_id: str
    endpoint_id: str
    validation_type: ValidationType
    status: ValidationStatus = "PENDING"
    requires_user_approval: bool = True


class ApiConnectorReport(BaseModel):
    connectors: List[InternalApiConnector] = Field(default_factory=list)
    endpoints: List[ApiEndpoint] = Field(default_factory=list)
    validations: List[ApiValidationRequest] = Field(default_factory=list)
    summary: str = ""


class ApiEndpointInput(BaseModel):
    method: HttpMethod
    path_label: str = Field(..., min_length=1, max_length=512)
    category: str = Field(default="", max_length=128)


class InternalApiConnectorRegistrationRequest(BaseModel):
    agent_id: str = Field(..., min_length=1, max_length=256)
    name: str = Field(..., min_length=1, max_length=256)
    api_type: ApiType = "REST"
    environment: str = Field(..., min_length=1, max_length=64)
    base_url_label: str = Field(..., min_length=1, max_length=512)
    endpoints: List[ApiEndpointInput] = Field(default_factory=list)
    status: ConnectorStatus = "ACTIVE"


class ApiValidationCreateRequest(BaseModel):
    connector_id: str = Field(..., min_length=1, max_length=256)
    endpoint_id: str = Field(..., min_length=1, max_length=256)
    validation_type: ValidationType = "availability_check"
