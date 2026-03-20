from __future__ import annotations

import os
import smtplib
from datetime import datetime, timezone
from email.message import EmailMessage
from typing import Any, Dict, List, Optional, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


def _env_bool(name: str, default: bool = False) -> bool:
    raw = (os.getenv(name) or "").strip().lower()
    if not raw:
        return default
    return raw in {"1", "true", "yes", "on"}


class EmailConnector(BaseConnector):
    connector_id = "email"
    connector_name = "Email"
    description = "Send operational alerts over SMTP email."

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not _env_bool("EMAIL_ENABLED", False):
            return False, "EMAIL_ENABLED is false"
        host = (os.getenv("EMAIL_HOST") or "").strip()
        from_addr = (os.getenv("EMAIL_FROM") or "").strip()
        if not host:
            return False, "EMAIL_HOST is required"
        if not from_addr:
            return False, "EMAIL_FROM is required"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        ok, msg = self.validate_config(config)
        return ("ok", "Email connector ready") if ok else ("unconfigured", msg)

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
                "host": os.getenv("EMAIL_HOST"),
                "port": os.getenv("EMAIL_PORT"),
                "from": os.getenv("EMAIL_FROM"),
                "tls": _env_bool("EMAIL_USE_TLS", True),
            },
        )

    def supported_actions(self) -> List[str]:
        return ["send_alert", "send_notification"]

    def send_alert(
        self,
        *,
        subject: str,
        message: str,
        recipients: List[str],
    ) -> Tuple[bool, str]:
        host = (os.getenv("EMAIL_HOST") or "").strip()
        port = int((os.getenv("EMAIL_PORT") or "587").strip() or "587")
        username = (os.getenv("EMAIL_USERNAME") or "").strip()
        password = (os.getenv("EMAIL_PASSWORD") or "").strip()
        from_addr = (os.getenv("EMAIL_FROM") or "").strip()
        use_tls = _env_bool("EMAIL_USE_TLS", True)
        if not host or not from_addr:
            return False, "Email connector not configured (EMAIL_HOST/EMAIL_FROM)"
        if not recipients:
            return False, "recipients is required for email alerts"

        try:
            msg = EmailMessage()
            msg["Subject"] = subject or "Vanya alert"
            msg["From"] = from_addr
            msg["To"] = ", ".join(recipients)
            msg.set_content(message or "")

            with smtplib.SMTP(host, port, timeout=15) as smtp:
                if use_tls:
                    smtp.starttls()
                if username:
                    smtp.login(username, password)
                smtp.send_message(msg)
            return True, "Alert sent successfully"
        except Exception as exc:
            return False, str(exc)
