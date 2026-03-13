# models/connector.py
"""
Pydantic models for the Integration Connector Framework.

Secrets (tokens, api keys) are NEVER stored in config or returned in responses.
Instead, presence flags (token_present, api_key_present) are used throughout.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


# ── Type aliases ──────────────────────────────────────────────────────────────

ConnectorId   = Literal["jira", "github", "slack", "qmetry"]
AuthType      = Literal["token", "basic", "api_key", "none"]
HealthStatus  = Literal["ok", "degraded", "unreachable", "unconfigured", "unknown"]


# ── Stored config (persisted, secrets-free) ───────────────────────────────────

class ConnectorConfig(BaseModel):
    """
    Persisted configuration for one connector.
    Raw secrets are NEVER stored here — only presence flags.
    """
    connector_id:    str
    enabled:         bool           = False
    base_url:        Optional[str]  = None
    auth_type:       AuthType       = "token"
    token_present:   bool           = False     # True if a token was supplied
    api_key_present: bool           = False     # True if an api_key was supplied
    workspace:       Optional[str]  = None      # Jira/QMetry org slug
    project_key:     Optional[str]  = None      # Jira project key
    channel:         Optional[str]  = None      # Slack channel id / name
    extra:           Dict[str, Any] = Field(default_factory=dict)

    model_config = {"extra": "ignore"}


# ── Request body for config updates ──────────────────────────────────────────

class ConnectorConfigUpdate(BaseModel):
    """
    Write-only update body.  Token / api_key values are consumed and discarded;
    only the presence flag is stored.  No secrets are echoed back.
    """
    enabled:     Optional[bool]           = None
    base_url:    Optional[str]            = None
    auth_type:   Optional[AuthType]       = None
    token:       Optional[str]            = None   # consumed, never stored/returned
    api_key:     Optional[str]            = None   # consumed, never stored/returned
    workspace:   Optional[str]            = None
    project_key: Optional[str]            = None
    channel:     Optional[str]            = None
    extra:       Optional[Dict[str, Any]] = None

    model_config = {"extra": "ignore"}


# ── Runtime status (returned by health checks) ────────────────────────────────

class ConnectorStatus(BaseModel):
    """Live status snapshot for one connector."""
    connector_id:       str
    connector_name:     str
    enabled:            bool
    health:             HealthStatus       = "unknown"
    last_check_at:      Optional[datetime] = None
    last_check_message: Optional[str]      = None
    config_summary:     Dict[str, Any]     = Field(default_factory=dict)


# ── Summary (list view) ───────────────────────────────────────────────────────

class ConnectorSummary(BaseModel):
    """Lightweight connector record for list endpoints."""
    connector_id:      str
    connector_name:    str
    description:       str
    enabled:           bool
    health:            HealthStatus       = "unknown"
    last_check_at:     Optional[datetime] = None
    supported_actions: List[str]          = Field(default_factory=list)
