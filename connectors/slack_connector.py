# connectors/slack_connector.py
"""
Slack connector stub.

Implements the BaseConnector contract with:
- Config validation (token + channel required)
- Deterministic health check
- Test notification stub / send hook
"""
from __future__ import annotations

import os
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


class SlackConnector(BaseConnector):
    connector_id   = "slack"
    connector_name = "Slack"
    description    = "Send test result notifications and alerts to Slack channels."

    # ── Contract ──────────────────────────────────────────────────────────────

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        has_token = config.token_present or bool((os.getenv("SLACK_BOT_TOKEN") or "").strip())
        if not has_token:
            return False, "A Slack bot token (xoxb-…) is required"
        if not config.channel:
            return False, "A target channel (e.g. #qa-alerts) is required"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        valid, msg = self.validate_config(config)
        if not valid:
            return "unconfigured", msg
        # Stub: real impl would call POST https://slack.com/api/auth.test
        channel = config.channel or "unknown"
        return "ok", f"Slack stub OK — channel: {channel}"

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
                "channel":       config.channel,
                "token_present": config.token_present,
                "workspace":     config.workspace,
            },
        )

    def supported_actions(self) -> List[str]:
        return [
            "send_notification",
            "send_test_summary",
            "send_failure_alert",
            "send_run_complete",
        ]

    # ── Extension points ──────────────────────────────────────────────────────

    def build_notification_payload(
        self,
        *,
        channel: str,
        text: str,
        status: Optional[str] = None,
        run_id: Optional[str] = None,
        evidence_url: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Build a Slack Block Kit message payload."""
        color = {"pass": "#36a64f", "fail": "#e01e5a", "error": "#ff6f00"}.get(
            str(status or "").lower(), "#cccccc"
        )
        blocks: List[Dict[str, Any]] = [
            {
                "type": "section",
                "text": {"type": "mrkdwn", "text": text},
            }
        ]
        if run_id:
            blocks.append({
                "type": "context",
                "elements": [{"type": "mrkdwn", "text": f"Run ID: `{run_id}`"}],
            })
        if evidence_url:
            blocks.append({
                "type": "actions",
                "elements": [{
                    "type": "button",
                    "text": {"type": "plain_text", "text": "View Evidence"},
                    "url": evidence_url,
                }],
            })
        return {
            "channel":     channel,
            "text":        text,
            "attachments": [{"color": color, "blocks": blocks}],
        }

    def send_notification_stub(
        self,
        channel: str,
        text: str,
        status: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Extension point: send a Slack message. Stub returns expected response shape."""
        return {
            "sent":    True,
            "channel": channel,
            "text":    text,
            "status":  status,
            "stub":    True,
        }

    def send_alert(
        self,
        channel: str,
        text: str,
        *,
        token: Optional[str] = None,
        job_id: Optional[str] = None,
        run_id: Optional[str] = None,
        status: Optional[str] = None,
        evidence_url: Optional[str] = None,
    ) -> Tuple[bool, str]:
        """
        Send an alert to Slack. Uses real Slack API when token is provided.
        Token can come from SLACK_BOT_TOKEN env var if not passed.
        Returns (success: bool, message: str).
        """
        tok = (token or "").strip() or (os.environ.get("SLACK_BOT_TOKEN") or "").strip()
        if not tok:
            return False, "Slack token not configured. Set SLACK_BOT_TOKEN env var or configure in Integrations."

        try:
            import httpx

            payload = self.build_notification_payload(
                channel=channel,
                text=text,
                status=status,
                run_id=run_id,
                evidence_url=evidence_url,
            )
            # Slack API expects JSON with channel, text; optional blocks
            body: Dict[str, Any] = {
                "channel": payload["channel"],
                "text":    payload["text"],
            }
            if payload.get("attachments"):
                body["attachments"] = payload["attachments"]

            with httpx.Client(timeout=15.0) as client:
                resp = client.post(
                    "https://slack.com/api/chat.postMessage",
                    headers={
                        "Authorization": f"Bearer {tok}",
                        "Content-Type":  "application/json",
                    },
                    json=body,
                )
            data = resp.json() if resp.text else {}
            if not resp.is_success:
                return False, data.get("error", resp.text or resp.reason_phrase or "Unknown error")
            if not data.get("ok"):
                return False, data.get("error", "Slack API returned ok=false")
            return True, "Alert sent successfully"
        except Exception as exc:
            return False, str(exc)
