# models/scheduled_report_models.py
"""
Enterprise ENT-02B — Scheduled Executive Reports (read-only foundation).

Report definitions, scheduling metadata, and deterministic previews.
No delivery, schedulers, or external messaging.
"""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel, Field


class ExecutiveReportSchedule(BaseModel):
    schedule_id: str
    name: str
    frequency: str
    enabled: bool = True
    report_type: str
    recipients_count: int = Field(default=0, ge=0)
    next_run_preview: Optional[str] = None


class ExecutiveReportPreview(BaseModel):
    preview_id: str
    title: str
    generated_at: str
    quality_score: int = Field(default=0, ge=0, le=100)
    quality_trend: str = "UNKNOWN"
    risk_level: str = "LOW"
    executive_summary: str = ""
    top_risks: List[str] = Field(default_factory=list)
    top_recommendations: List[str] = Field(default_factory=list)
    incident_count: int = Field(default=0, ge=0)
    critical_contract_count: int = Field(default=0, ge=0)
    broken_journey_count: int = Field(default=0, ge=0)
    jira_blocker_count: int = Field(default=0, ge=0)
    jira_blocker_keys: List[str] = Field(default_factory=list)


class ExecutiveReportCenter(BaseModel):
    schedules: List[ExecutiveReportSchedule] = Field(default_factory=list)
    latest_preview: Optional[ExecutiveReportPreview] = None
    total_schedules: int = Field(default=0, ge=0)
