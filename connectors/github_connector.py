# connectors/github_connector.py
"""
GitHub connector stub.

Implements the BaseConnector contract with:
- Config validation (token + workspace/org required)
- Deterministic health check
- Repo validation stub
- Extension points: PR hook, status check push
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


class GitHubConnector(BaseConnector):
    connector_id   = "github"
    connector_name = "GitHub"
    description    = "Trigger test runs from PR events and post status checks back to GitHub."

    # ── Contract ──────────────────────────────────────────────────────────────

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not config.token_present:
            return False, "A GitHub personal access token or app token is required"
        if not config.workspace:
            return False, "workspace (GitHub org or username) is required"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        valid, msg = self.validate_config(config)
        if not valid:
            return "unconfigured", msg
        # Stub: real impl would call GET https://api.github.com/user or /orgs/{org}
        org = config.workspace or "unknown"
        return "ok", f"GitHub stub OK — org: {org}"

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
                "workspace":     config.workspace,
                "token_present": config.token_present,
                "auth_type":     config.auth_type,
                "base_url":      config.base_url or "https://api.github.com",
            },
        )

    def supported_actions(self) -> List[str]:
        return [
            "validate_repo",
            "post_status_check",
            "create_pr_comment",
            "trigger_workflow",
            "list_open_prs",
        ]

    # ── Extension points ──────────────────────────────────────────────────────

    def validate_repo_stub(self, org: str, repo: str) -> Dict[str, Any]:
        """Stub: returns deterministic repo metadata."""
        return {
            "org":          org,
            "repo":         repo,
            "full_name":    f"{org}/{repo}",
            "default_branch": "main",
            "private":      False,
            "stub":         True,
        }

    def build_status_check_payload(
        self,
        *,
        state: str,
        description: str,
        context: str = "vanya/qa-bot",
        target_url: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Build a GitHub Commit Status payload (POST /repos/:owner/:repo/statuses/:sha)."""
        payload: Dict[str, Any] = {
            "state":       state,          # pending | success | failure | error
            "description": description[:140],
            "context":     context,
        }
        if target_url:
            payload["target_url"] = target_url
        return payload

    def post_pr_comment_stub(
        self,
        pr_number: int,
        body: str,
        repo: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Extension point: post a PR comment. Stub returns expected response shape."""
        return {
            "posted":      True,
            "pr_number":   pr_number,
            "repo":        repo,
            "body_length": len(body),
            "stub":        True,
        }
