# connectors/jira_connector.py
"""
Jira connector — integration framework adapter (JIRA-01A read-only foundation).

Implements the BaseConnector contract with:
- Local config validation (base_url + token + workspace)
- Health check that delegates to real Jira validation when credentials are configured
- Write-operation payload builders (stubs for future JIRA-01B+; not invoked in JIRA-01A)

Canonical read-only discovery (projects, issues, epics, releases, fix versions) lives in
services/jira_integration_service.py — not in this connector.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


class JiraConnector(BaseConnector):
    connector_id   = "jira"
    connector_name = "Jira"
    description    = "Discover Jira projects and issues (read-only). Write actions planned for a future sprint."

    # ── Contract ──────────────────────────────────────────────────────────────

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not config.base_url:
            return False, "base_url is required (e.g. https://yourorg.atlassian.net)"
        if not config.token_present:
            return False, "A Jira API token is required"
        if not config.workspace:
            return False, "workspace (Jira org/user email) is required"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        """Delegate to jira_integration_service when enabled and configured."""
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        valid, msg = self.validate_config(config)
        if not valid:
            return "unconfigured", msg
        from services.jira_integration_service import validate_jira_connection

        status = validate_jira_connection()
        if status.connected:
            return "ok", f"Jira connected at {status.server_url}"
        if config.base_url:
            return "degraded", "Jira configured but connection could not be validated"
        return "unconfigured", msg

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
                "base_url":      config.base_url,
                "workspace":     config.workspace,
                "project_key":   config.project_key,
                "token_present": config.token_present,
                "auth_type":     config.auth_type,
            },
        )

    def supported_actions(self) -> List[str]:
        return [
            "list_projects",
            "create_issue",
            "push_test_result",
            "attach_evidence",
            "transition_issue",
        ]

    # ── Write stubs (JIRA-01B+ — not used in read-only JIRA-01A) ───────────────

    def build_issue_payload(
        self,
        *,
        project_key: str,
        summary: str,
        description: str,
        issue_type: str = "Bug",
        priority: str = "Medium",
        labels: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Build a Jira create-issue payload (ready to POST to REST API)."""
        return {
            "fields": {
                "project":     {"key": project_key},
                "summary":     summary,
                "description": {"version": 1, "type": "doc", "content": [
                    {"type": "paragraph", "content": [{"type": "text", "text": description}]}
                ]},
                "issuetype":   {"name": issue_type},
                "priority":    {"name": priority},
                "labels":      labels or [],
            }
        }

    def push_result_stub(
        self,
        run_id: str,
        status: str,
        project_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Extension point: push a test run result to Jira.
        Stub returns what the real call would produce.
        """
        return {
            "pushed":      True,
            "run_id":      run_id,
            "project_key": project_key or "QA",
            "issue_key":   f"QA-{abs(hash(run_id)) % 9000 + 1000}",
            "status":      status,
            "stub":        True,
        }
