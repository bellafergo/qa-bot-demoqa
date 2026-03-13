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
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List

from fastapi import APIRouter, HTTPException

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
