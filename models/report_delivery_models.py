# models/report_delivery_models.py
"""
Enterprise ENT-02C — Executive Report Delivery.

Transforms existing executive report previews into deliverable payloads.
No new intelligence generation.
"""
from __future__ import annotations

from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

ReportDeliveryType = Literal[
    "EXECUTIVE_QUALITY",
    "RELEASE_READINESS",
    "WEEKLY_QUALITY_BRIEF",
    "INCIDENT_REVIEW",
]

ReportDeliveryChannel = Literal["email", "slack", "teams"]


class ReportDeliveryPreviewRequest(BaseModel):
    report_type: ReportDeliveryType
    channel: ReportDeliveryChannel
    recipient: Optional[str] = None


class ReportDeliverySendRequest(BaseModel):
    report_type: ReportDeliveryType
    channel: ReportDeliveryChannel
    recipient: Optional[str] = None
    recipients: Optional[List[str]] = None
    channel_override: Optional[str] = None
    requires_user_approval: bool = False


class ReportDeliveryRequest(BaseModel):
    request_id: str
    project_id: str
    report_type: str
    channel: str
    recipient: Optional[str] = None
    created_at: str


class ReportDeliveryPreview(BaseModel):
    channel: str
    report_type: str
    subject: str
    summary: str
    payload: Dict[str, Any] = Field(default_factory=dict)


class ReportDeliveryResult(BaseModel):
    success: bool
    channel: str
    report_type: str
    delivered_at: Optional[str] = None
    error: Optional[str] = None
    request: Optional[ReportDeliveryRequest] = None
