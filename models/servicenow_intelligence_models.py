# models/servicenow_intelligence_models.py
"""Read-only ServiceNow intelligence models (SNOW-01B)."""
from __future__ import annotations

from typing import List, Literal, Optional

from pydantic import BaseModel, Field

ServiceNowConfidence = Literal["LOW", "MEDIUM", "HIGH"]
ServiceNowEntityType = Literal["incident", "change", "service", "cmdb"]


class ServiceNowCorrelation(BaseModel):
    entity_type: ServiceNowEntityType
    entity_id: str
    correlation_reason: str = ""
    capability: str = ""
    confidence: ServiceNowConfidence = "LOW"


class ServiceNowIntelligenceReport(BaseModel):
    connected: bool = False
    incident_correlations: List[ServiceNowCorrelation] = Field(default_factory=list)
    change_correlations: List[ServiceNowCorrelation] = Field(default_factory=list)
    service_correlations: List[ServiceNowCorrelation] = Field(default_factory=list)
    cmdb_correlations: List[ServiceNowCorrelation] = Field(default_factory=list)
    top_operational_risks: List[str] = Field(default_factory=list)
    executive_summary: str = ""
    data_gaps: List[str] = Field(default_factory=list)
