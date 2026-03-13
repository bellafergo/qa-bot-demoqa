# connectors/slack_connector.py
"""
Slack connector stub.

Implements the BaseConnector contract with:
- Config validation (token + channel required)
- Deterministic health check
- Test notification stub / send hook
"""
from __future__ import annotations

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
        if not config.token_present:
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
