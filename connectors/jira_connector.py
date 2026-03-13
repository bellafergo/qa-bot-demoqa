# connectors/jira_connector.py
"""
Jira connector stub.

Implements the BaseConnector contract with:
- Config validation (base_url + token required)
- Deterministic health check (no real network call)
- list-projects stub / payload builder
- Extension point: push_result(run, project_key)
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


class JiraConnector(BaseConnector):
    connector_id   = "jira"
    connector_name = "Jira"
    description    = "Push test results and create tickets in Jira projects."

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
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        valid, msg = self.validate_config(config)
        if not valid:
            return "unconfigured", msg
        # Stub: real impl would call GET {base_url}/rest/api/3/myself
        return "ok", f"Jira stub reachable at {config.base_url}"

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

    # ── Extension points ──────────────────────────────────────────────────────

    def list_projects_stub(self) -> List[Dict[str, Any]]:
        """Stub: returns a deterministic list of mock Jira projects."""
        return [
            {"key": "QA",   "name": "QA Platform",  "type": "software"},
            {"key": "WEB",  "name": "Web Frontend",  "type": "software"},
            {"key": "AUTH", "name": "Auth Service",  "type": "software"},
        ]

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
