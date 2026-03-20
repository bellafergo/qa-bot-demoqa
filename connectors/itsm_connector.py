from __future__ import annotations

import os
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

import httpx

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


def _env_bool(name: str, default: bool = False) -> bool:
    raw = (os.getenv(name) or "").strip().lower()
    if not raw:
        return default
    return raw in {"1", "true", "yes", "on"}


class ITSMConnector(BaseConnector):
    connector_id = "itsm"
    connector_name = "ITSM (Generic)"
    description = "Create incidents/tickets in a generic ITSM endpoint."

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not _env_bool("ITSM_ENABLED", False):
            return False, "ITSM_ENABLED is false"
        base_url = (os.getenv("ITSM_BASE_URL") or "").strip()
        if not base_url:
            return False, "ITSM_BASE_URL is required"
        if not ((os.getenv("ITSM_API_KEY") or "").strip() or (os.getenv("ITSM_TOKEN") or "").strip()):
            return False, "ITSM_API_KEY or ITSM_TOKEN is required"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        ok, msg = self.validate_config(config)
        return ("ok", "ITSM connector ready") if ok else ("unconfigured", msg)

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
                "base_url": os.getenv("ITSM_BASE_URL"),
                "default_queue": os.getenv("ITSM_DEFAULT_QUEUE"),
                "auth_configured": bool((os.getenv("ITSM_API_KEY") or "").strip() or (os.getenv("ITSM_TOKEN") or "").strip()),
            },
        )

    def supported_actions(self) -> List[str]:
        return ["create_ticket"]

    def create_ticket(
        self,
        *,
        title: str,
        description: str,
        priority: str = "medium",
        context: Optional[Dict[str, Any]] = None,
    ) -> Tuple[bool, Dict[str, Any]]:
        base_url = (os.getenv("ITSM_BASE_URL") or "").strip().rstrip("/")
        api_key = (os.getenv("ITSM_API_KEY") or "").strip()
        token = (os.getenv("ITSM_TOKEN") or "").strip()
        queue = (os.getenv("ITSM_DEFAULT_QUEUE") or "").strip()
        if not base_url:
            return False, {"error": "ITSM_BASE_URL is not configured"}
        auth = api_key or token
        if not auth:
            return False, {"error": "ITSM_API_KEY or ITSM_TOKEN is not configured"}
        body = {
            "title": title,
            "description": description,
            "priority": priority,
            "queue": queue or None,
            "context": context or {},
        }
        headers = {
            "Authorization": f"Bearer {auth}",
            "Content-Type": "application/json",
        }
        try:
            with httpx.Client(timeout=20.0) as client:
                resp = client.post(f"{base_url}/tickets", json=body, headers=headers)
            data = resp.json() if resp.text else {}
            if not resp.is_success:
                return False, {"error": data.get("detail") or data.get("message") or f"ITSM request failed ({resp.status_code})"}
            ticket_id = data.get("ticket_id") or data.get("id")
            return True, {"ticket_id": ticket_id, "raw": data}
        except Exception as exc:
            return False, {"error": str(exc)}
