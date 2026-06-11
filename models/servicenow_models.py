# models/servicenow_models.py
"""Read-only ServiceNow discovery models (SNOW-01A)."""
from __future__ import annotations

from datetime import datetime
from typing import List, Optional

from pydantic import BaseModel, Field


class ServiceNowConnectionStatus(BaseModel):
    connected: bool = False
    instance_url: Optional[str] = None
    username: Optional[str] = None
    incident_count: int = 0
    change_count: int = 0
    service_count: int = 0
    cmdb_count: int = 0
    last_sync: Optional[datetime] = None


class ServiceNowIncident(BaseModel):
    number: str
    short_description: str = ""
    state: str = ""
    priority: str = ""
    assignment_group: Optional[str] = None
    opened_at: Optional[str] = None


class ServiceNowChange(BaseModel):
    number: str
    short_description: str = ""
    state: str = ""
    risk: str = ""
    planned_start: Optional[str] = None
    planned_end: Optional[str] = None


class ServiceNowService(BaseModel):
    name: str
    business_criticality: str = ""
    operational_status: str = ""


class ServiceNowCI(BaseModel):
    name: str
    class_name: str = ""
    operational_status: str = ""


class ServiceNowIncidentsResponse(BaseModel):
    incidents: List[ServiceNowIncident] = Field(default_factory=list)
    total: int = 0


class ServiceNowChangesResponse(BaseModel):
    changes: List[ServiceNowChange] = Field(default_factory=list)
    total: int = 0


class ServiceNowServicesResponse(BaseModel):
    services: List[ServiceNowService] = Field(default_factory=list)
    total: int = 0


class ServiceNowCMDBResponse(BaseModel):
    items: List[ServiceNowCI] = Field(default_factory=list)
    total: int = 0
