# api/routes/integrations_routes.py
"""
Integration connector routes.

GET  /integrations                              — list all connectors
GET  /integrations/{connector_id}              — get connector status
POST /integrations/{connector_id}/health-check — run health check
POST /integrations/{connector_id}/enable       — enable connector
POST /integrations/{connector_id}/disable      — disable connector
POST /integrations/{connector_id}/config       — update config (secrets consumed, not returned)
GET  /integrations/{connector_id}/actions      — list supported actions
GET  /integrations/alerting/ready              — check if alerting can be sent (for UI)
POST /integrations/send-alert                  — send alert (requires human confirmation in UI)
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field, AliasChoices

from models.connector import (
    ConnectorConfig,
    ConnectorConfigUpdate,
    ConnectorStatus,
    ConnectorSummary,
)
from services.integration_dispatcher import integration_dispatcher
from services.integration_service import integration_service

logger = logging.getLogger("vanya.integrations")

router = APIRouter(prefix="/integrations", tags=["integrations"])


def _not_found(connector_id: str) -> HTTPException:
    return HTTPException(status_code=404, detail=f"Connector '{connector_id}' not found")


# ── Alerting (must be defined before /{connector_id} to avoid path conflict) ───

class SendAlertRequest(BaseModel):
    """Request for POST /integrations/send-alert. Requires explicit UI confirmation."""
    connector_id: str = Field(
        default="slack",
        validation_alias=AliasChoices("connector_id", "connector"),
        description="Target connector id (slack|email|teams)",
    )
    job_id:       Optional[str] = Field(None, description="Job ID for context")
    run_id:       Optional[str] = Field(None, description="Run ID for context")
    channel:      Optional[str] = Field(None, description="Override channel (else from config)")
    subject:      Optional[str] = Field(None, description="Optional title/subject for connectors that support it")
    recipients:   Optional[List[str]] = Field(default=None, description="Email recipients (for email connector)")
    message:      str = Field(..., description="Alert message text")
    metadata:     Optional[Dict[str, Any]] = Field(default=None, description="Optional extra context")


class CreateTicketRequest(BaseModel):
    title: str = Field(..., description="Ticket title")
    description: str = Field(..., description="Ticket description")
    priority: str = Field(default="medium", description="Ticket priority")
    context: Optional[Dict[str, Any]] = Field(default=None, description="Optional context payload")


@router.get("/readiness")
def readiness() -> Dict[str, Any]:
    """Readiness snapshot for all connectors."""
    return {"connectors": integration_dispatcher.readiness()}


@router.get("/alerting/ready")
def alerting_ready() -> Dict[str, Any]:
    """Backward-compatible readiness endpoint used by current ExecutionPage."""
    return integration_dispatcher.alerting_ready_legacy()


@router.post("/send-alert")
def send_alert(body: SendAlertRequest) -> Dict[str, Any]:
    """
    Send an alert via the specified connector.
    ONLY call after explicit human confirmation in the UI.
    Does NOT auto-send; this endpoint is purely reactive.
    """
    # Build message with context
    parts = [body.message]
    if body.job_id:
        parts.append(f"Job: `{body.job_id}`")
    if body.run_id:
        parts.append(f"Run: `{body.run_id}`")
    text = "\n".join(parts)
    ok, result = integration_dispatcher.send_alert(
        connector_id=body.connector_id,
        subject=body.subject,
        message=text,
        recipients=body.recipients,
        channel=body.channel,
        context={"job_id": body.job_id, "run_id": body.run_id, "metadata": body.metadata or {}},
    )
    if not ok:
        raise HTTPException(status_code=502, detail=result.get("error") or result.get("message") or "Alert dispatch failed")
    return {"ok": True, **result}


@router.post("/create-ticket")
def create_ticket(body: CreateTicketRequest) -> Dict[str, Any]:
    """
    Create a ticket via ITSM generic connector.
    This endpoint is reactive and must only be called after explicit user approval.
    """
    ok, result = integration_dispatcher.create_ticket(
        title=body.title,
        description=body.description,
        priority=body.priority,
        context=body.context or {},
    )
    if not ok:
        raise HTTPException(status_code=502, detail=result.get("error") or "Ticket creation failed")
    return {"ok": True, **result}


# ── List ──────────────────────────────────────────────────────────────────────

@router.get("", response_model=List[ConnectorSummary])
def list_integrations():
    """Return a summary of all registered connectors."""
    return integration_service.list_connectors()


# ── Single connector status ───────────────────────────────────────────────────

@router.get("/{connector_id}", response_model=ConnectorStatus)
def get_integration(connector_id: str):
    """Return the current status for a single connector."""
    try:
        return integration_service.get_connector_status(connector_id)
    except KeyError:
        raise _not_found(connector_id)


# ── Health check ──────────────────────────────────────────────────────────────

@router.post("/{connector_id}/health-check", response_model=ConnectorStatus)
def run_health_check(connector_id: str):
    """Run a fresh health check and return the updated status."""
    try:
        return integration_service.run_health_check(connector_id)
    except KeyError:
        raise _not_found(connector_id)


# ── Enable ────────────────────────────────────────────────────────────────────

@router.post("/{connector_id}/enable", response_model=ConnectorConfig)
def enable_integration(connector_id: str):
    """Enable a connector."""
    try:
        return integration_service.enable(connector_id)
    except KeyError:
        raise _not_found(connector_id)


# ── Disable ───────────────────────────────────────────────────────────────────

@router.post("/{connector_id}/disable", response_model=ConnectorConfig)
def disable_integration(connector_id: str):
    """Disable a connector."""
    try:
        return integration_service.disable(connector_id)
    except KeyError:
        raise _not_found(connector_id)


# ── Config update ─────────────────────────────────────────────────────────────

@router.post("/{connector_id}/config", response_model=ConnectorConfig)
def update_config(connector_id: str, body: ConnectorConfigUpdate):
    """
    Update connector configuration.
    Secrets (token, api_key) are consumed server-side and stored only as
    presence flags.  They are NEVER echoed back in the response.
    """
    try:
        return integration_service.update_config(connector_id, body)
    except KeyError:
        raise _not_found(connector_id)


# ── Actions ───────────────────────────────────────────────────────────────────

@router.get("/{connector_id}/actions", response_model=Dict[str, Any])
def get_actions(connector_id: str):
    """Return the list of supported actions for a connector."""
    try:
        actions = integration_service.get_actions(connector_id)
        return {"connector_id": connector_id, "actions": actions}
    except KeyError:
        raise _not_found(connector_id)
