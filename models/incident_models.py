# models/incident_models.py
"""Autonomous Incident Investigator — request/response contracts (MVP)."""
from __future__ import annotations

from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

IncidentStatus = Literal["running", "completed", "failed"]
IncidentSeverity = Literal["critical", "high", "medium", "low", "info"]
IncidentReproduced = Literal["true", "false", "unknown"]
SuspectedArea = Literal["frontend", "backend", "network", "auth", "data", "unknown"]


class InvestigateIncidentRequest(BaseModel):
    incident_description: str = Field(..., min_length=3, max_length=4000)
    target_url: Optional[str] = Field(default=None, max_length=2048)
    project_id: Optional[str] = Field(default=None, max_length=128)
    module: Optional[str] = Field(default=None, max_length=256)
    max_steps: int = Field(default=5, ge=1, le=20)
    credentials_mode: str = Field(default="none", max_length=32)
    allow_destructive_actions: bool = False
    timeout_ms: int = Field(default=30_000, ge=5_000, le=120_000)

    model_config = {"extra": "ignore"}


class IncidentInvestigationRun(BaseModel):
    id: str
    created_at: str
    updated_at: str
    status: IncidentStatus
    incident_description: str
    target_url: Optional[str] = None
    project_id: Optional[str] = None
    module: Optional[str] = None
    severity: IncidentSeverity = "info"
    reproduced: IncidentReproduced = "unknown"
    suspected_area: SuspectedArea = "unknown"
    suspected_endpoint: Optional[str] = None
    symptom_observed: str = ""
    probable_cause: str = ""
    console_errors: List[Dict[str, Any]] = Field(default_factory=list)
    network_errors: List[Dict[str, Any]] = Field(default_factory=list)
    http_errors: List[Dict[str, Any]] = Field(default_factory=list)
    screenshot_url: Optional[str] = None
    screenshot_b64: Optional[str] = None
    steps_executed: List[str] = Field(default_factory=list)
    diagnosis_summary: str = ""
    recommendations: List[str] = Field(default_factory=list)
    reproduction_steps: List[str] = Field(default_factory=list)
    raw_evidence: Dict[str, Any] = Field(default_factory=dict)
    meta: Dict[str, Any] = Field(default_factory=dict)
    error_message: Optional[str] = None

    model_config = {"extra": "ignore"}


class IncidentInvestigationListResponse(BaseModel):
    items: List[IncidentInvestigationRun] = Field(default_factory=list)
    total: int = 0
