# api/routes/audit_routes.py
"""Enterprise SEC-01E — Audit trail routes (read-only queries)."""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, Query

from models.audit_models import AuditEventSummary, AuditEventsResponse
from services.audit_event_service import build_audit_summary, list_events

logger = logging.getLogger("vanya.audit_routes")

router = APIRouter(prefix="/audit", tags=["audit"])


@router.get("/events", response_model=AuditEventsResponse)
def get_audit_events(
    event_type: Optional[str] = Query(None, description="Filter by event type"),
    resource_type: Optional[str] = Query(None, description="Filter by resource type"),
    user_id: Optional[str] = Query(None, description="Filter by user id"),
    limit: int = Query(100, ge=1, le=500, description="Maximum events to return"),
):
    """Return centralized audit events with optional filters."""
    return list_events(
        event_type=event_type,
        resource_type=resource_type,
        user_id=user_id,
        limit=limit,
    )


@router.get("/summary", response_model=AuditEventSummary)
def get_audit_summary():
    """Return audit trail summary statistics."""
    return build_audit_summary()
