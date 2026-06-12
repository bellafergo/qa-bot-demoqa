# models/audit_models.py
"""Enterprise SEC-01E — Centralized audit event models."""
from __future__ import annotations

from datetime import datetime
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

AuditEventType = Literal[
    "SSO_PROVIDER_VALIDATED",
    "SSO_LOGIN_URL_GENERATED",
    "REPORT_PREVIEWED",
    "REPORT_SENT",
    "INTEGRATION_CONFIG_UPDATED",
    "INTEGRATION_VALIDATED",
    "INCIDENT_INVESTIGATION_STARTED",
    "INCIDENT_INVESTIGATION_COMPLETED",
    "RELEASE_READINESS_VIEWED",
    "ACCESS_DENIED",
]

AuditResourceType = Literal["SECURITY", "REPORTS", "INTEGRATIONS", "INCIDENTS", "RELEASES"]
AuditResult = Literal["SUCCESS", "FAILURE"]


class AuditEvent(BaseModel):
    event_id: str
    timestamp: datetime
    user_id: str
    user_email: Optional[str] = None
    event_type: AuditEventType
    resource_type: AuditResourceType
    resource_id: str
    action: str
    result: AuditResult = "SUCCESS"
    metadata: Dict[str, Any] = Field(default_factory=dict)


class AuditEventSummary(BaseModel):
    total_events: int = 0
    event_types: Dict[str, int] = Field(default_factory=dict)
    latest_event: Optional[AuditEvent] = None


class AuditEventsResponse(BaseModel):
    events: List[AuditEvent] = Field(default_factory=list)
    total: int = Field(default=0, ge=0)
