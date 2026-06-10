# models/enterprise_system_connector_models.py
"""INT-03D — Enterprise systems connectors (agent-mediated, read-only foundation)."""
from __future__ import annotations

from typing import List, Literal, Optional

from pydantic import BaseModel, Field

SystemType = Literal[
    "SAP_ECC",
    "SAP_S4",
    "ORACLE_EBS",
    "ORACLE_FUSION",
    "NETSUITE",
    "DYNAMICS",
    "MAINFRAME",
    "LEGACY",
    "OTHER",
]
ConnectorStatus = Literal["ACTIVE", "INACTIVE", "UNKNOWN"]
EnterpriseValidationType = Literal[
    "interface_validation",
    "contract_validation",
    "integration_validation",
    "data_consistency_validation",
]
ValidationStatus = Literal["PENDING", "APPROVED", "BLOCKED", "COMPLETED"]


class EnterpriseSystemConnector(BaseModel):
    connector_id: str
    agent_id: str
    system_name: str
    system_type: SystemType = "OTHER"
    environment: str
    status: ConnectorStatus = "UNKNOWN"
    read_only: bool = True


class EnterpriseSystemModule(BaseModel):
    module_id: str
    connector_id: str
    module_name: str
    category: str = ""
    criticality: str = "MEDIUM"


class EnterpriseValidationRequest(BaseModel):
    request_id: str
    connector_id: str
    module_id: str
    validation_type: EnterpriseValidationType
    status: ValidationStatus = "PENDING"
    requires_user_approval: bool = True


class EnterpriseSystemReport(BaseModel):
    connectors: List[EnterpriseSystemConnector] = Field(default_factory=list)
    modules: List[EnterpriseSystemModule] = Field(default_factory=list)
    validations: List[EnterpriseValidationRequest] = Field(default_factory=list)
    summary: str = ""


class EnterpriseSystemModuleInput(BaseModel):
    module_name: str = Field(..., min_length=1, max_length=128)
    category: str = Field(default="", max_length=128)
    criticality: str = Field(default="MEDIUM", max_length=32)


class EnterpriseSystemRegistrationRequest(BaseModel):
    agent_id: str = Field(..., min_length=1, max_length=256)
    system_name: str = Field(..., min_length=1, max_length=256)
    system_type: SystemType = "OTHER"
    environment: str = Field(..., min_length=1, max_length=64)
    modules: List[EnterpriseSystemModuleInput] = Field(default_factory=list)
    status: ConnectorStatus = "ACTIVE"


class EnterpriseSystemValidationCreateRequest(BaseModel):
    connector_id: str = Field(..., min_length=1, max_length=256)
    module_id: str = Field(..., min_length=1, max_length=256)
    validation_type: EnterpriseValidationType = "interface_validation"
