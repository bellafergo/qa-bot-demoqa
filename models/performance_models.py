# models/performance_models.py
"""Lightweight API performance timing models."""
from __future__ import annotations

from datetime import datetime
from typing import List, Optional

from pydantic import BaseModel, Field


class EndpointTimingRecord(BaseModel):
    endpoint: str
    method: str
    duration_ms: float = Field(ge=0)
    status_code: int = 200
    timestamp: datetime
    request_id: Optional[str] = None


class SlowEndpointStat(BaseModel):
    method: str
    endpoint: str
    request_count: int = Field(ge=0)
    avg_duration_ms: float = Field(ge=0)
    max_duration_ms: float = Field(ge=0)
    latest_at: Optional[datetime] = None


class SlowEndpointsReport(BaseModel):
    total_requests: int = Field(default=0, ge=0)
    endpoints: List[SlowEndpointStat] = Field(default_factory=list)
    generated_at: Optional[datetime] = None
