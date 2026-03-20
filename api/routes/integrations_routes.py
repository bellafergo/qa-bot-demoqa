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
import os
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from models.connector import (
    ConnectorConfig,
    ConnectorConfigUpdate,
    ConnectorStatus,
    ConnectorSummary,
)
from services.integration_service import integration_service

logger = logging.getLogger("vanya.integrations")

router = APIRouter(prefix="/integrations", tags=["integrations"])


def _not_found(connector_id: str) -> HTTPException:
    return HTTPException(status_code=404, detail=f"Connector '{connector_id}' not found")


# ── Alerting (must be defined before /{connector_id} to avoid path conflict) ───

class SendAlertRequest(BaseModel):
    """Request for POST /integrations/send-alert. Requires explicit UI confirmation."""
    connector_id: str = Field(default="slack", description="Target connector (only 'slack' supported)")
    job_id:       Optional[str] = Field(None, description="Job ID for context")
    run_id:       Optional[str] = Field(None, description="Run ID for context")
    channel:      Optional[str] = Field(None, description="Override channel (else from config)")
    message:      str = Field(..., description="Alert message text")


@router.get("/alerting/ready")
def alerting_ready() -> Dict[str, Any]:
    """
    Check if alerting can be sent. Used by UI to enable/disable Send alert button.
    Returns { ready: bool, connector_id?: str, reason?: str }.
    """
    token = (os.environ.get("SLACK_BOT_TOKEN") or "").strip()
    if not token:
        return {"ready": False, "reason": "SLACK_BOT_TOKEN not configured"}
    try:
        cfg = integration_service.get_config("slack")
        if not cfg.enabled:
            return {"ready": False, "reason": "Slack connector is disabled"}
        if not cfg.channel:
            return {"ready": False, "reason": "Slack channel not configured"}
        return {"ready": True, "connector_id": "slack"}
    except KeyError:
        return {"ready": False, "reason": "Slack connector not found"}


@router.post("/send-alert")
def send_alert(body: SendAlertRequest) -> Dict[str, Any]:
    """
    Send an alert via the specified connector.
    ONLY call after explicit human confirmation in the UI.
    Does NOT auto-send; this endpoint is purely reactive.
    """
    if body.connector_id != "slack":
        raise HTTPException(status_code=400, detail=f"Only 'slack' connector supported, got {body.connector_id!r}")

    try:
        cfg = integration_service.get_config("slack")
    except KeyError:
        raise HTTPException(status_code=404, detail="Slack connector not found")

    if not cfg.enabled:
        raise HTTPException(status_code=503, detail="Slack connector is disabled. Enable it in Integrations.")

    channel = (body.channel or cfg.channel or "").strip()
    if not channel:
        raise HTTPException(status_code=400, detail="No channel specified. Configure in Integrations or pass in request.")

    token = (os.environ.get("SLACK_BOT_TOKEN") or "").strip()
    if not token:
        raise HTTPException(
            status_code=503,
            detail="Slack token not configured. Set SLACK_BOT_TOKEN environment variable.",
        )

    # Build message with context
    parts = [body.message]
    if body.job_id:
        parts.append(f"Job: `{body.job_id}`")
    if body.run_id:
        parts.append(f"Run: `{body.run_id}`")
    text = "\n".join(parts)

    from connectors.slack_connector import SlackConnector
    from connectors.registry import registry

    connector = registry.get("slack")
    if connector is None or not isinstance(connector, SlackConnector):
        raise HTTPException(status_code=500, detail="Slack connector not available")

    ok, msg = connector.send_alert(
        channel=channel,
        text=text,
        job_id=body.job_id,
        run_id=body.run_id,
    )
    if not ok:
        raise HTTPException(status_code=502, detail=msg)
    return {"ok": True, "channel": channel, "message": msg}


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
