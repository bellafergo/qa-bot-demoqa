from __future__ import annotations

import os
from datetime import datetime, timezone
from typing import Any, Dict, List, Tuple

import httpx

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


def _env_bool(name: str, default: bool = False) -> bool:
    raw = (os.getenv(name) or "").strip().lower()
    if not raw:
        return default
    return raw in {"1", "true", "yes", "on"}


class TeamsConnector(BaseConnector):
    connector_id = "teams"
    connector_name = "Microsoft Teams"
    description = "Send alerts to Microsoft Teams via Incoming Webhook."

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not _env_bool("TEAMS_ENABLED", False):
            return False, "TEAMS_ENABLED is false"
        webhook = (os.getenv("TEAMS_WEBHOOK_URL") or "").strip()
        if not webhook:
            return False, "TEAMS_WEBHOOK_URL is required"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        ok, msg = self.validate_config(config)
        return ("ok", "Teams connector ready") if ok else ("unconfigured", msg)

    def get_status(self, config: ConnectorConfig) -> ConnectorStatus:
        health, msg = self.health_check(config)
        return ConnectorStatus(
            connector_id=self.connector_id,
            connector_name=self.connector_name,
            enabled=config.enabled,
            health=health,
            last_check_at=datetime.now(timezone.utc),
            last_check_message=msg,
            config_summary={
                "webhook_configured": bool((os.getenv("TEAMS_WEBHOOK_URL") or "").strip()),
            },
        )

    def supported_actions(self) -> List[str]:
        return ["send_alert", "send_notification"]

    def send_alert(self, *, subject: str, message: str) -> Tuple[bool, str]:
        webhook = (os.getenv("TEAMS_WEBHOOK_URL") or "").strip()
        if not webhook:
            return False, "Teams webhook not configured"
        text = f"**{subject or 'Vanya alert'}**\n\n{message or ''}"
        try:
            with httpx.Client(timeout=15.0) as client:
                resp = client.post(webhook, json={"text": text})
            if not resp.is_success:
                return False, f"Teams webhook failed ({resp.status_code})"
            return True, "Alert sent successfully"
        except Exception as exc:
            return False, str(exc)
