# models/platform_observability_models.py
"""OBS-02A — Vanya self observability models (read-only platform health)."""
from __future__ import annotations

from typing import List, Literal, Optional

from pydantic import BaseModel, Field

PlatformHealthStatus = Literal["HEALTHY", "DEGRADED", "UNHEALTHY", "UNKNOWN"]


class PlatformHealthMetric(BaseModel):
    metric_name: str
    status: PlatformHealthStatus
    value: str
    summary: str


class IntegrationHealthSummary(BaseModel):
    healthy: int = 0
    degraded: int = 0
    disconnected: int = 0


class PlatformHealthArea(BaseModel):
    """Aggregated health for one platform surface."""

    status: PlatformHealthStatus = "UNKNOWN"
    summary: str = ""
    metrics: List[PlatformHealthMetric] = Field(default_factory=list)
    integration_summary: Optional[IntegrationHealthSummary] = None


class PlatformObservabilityReport(BaseModel):
    generated_at: str
    api_health: PlatformHealthArea
    authentication_health: PlatformHealthArea
    integration_health: PlatformHealthArea
    report_delivery_health: PlatformHealthArea
    incident_investigation_health: PlatformHealthArea
    top_platform_risks: List[str] = Field(default_factory=list)
    executive_summary: str = ""
