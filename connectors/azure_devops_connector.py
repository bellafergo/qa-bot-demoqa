# connectors/azure_devops_connector.py
"""Azure DevOps connector — OAuth integration card (real PR flow via project routes)."""
from __future__ import annotations

from datetime import datetime, timezone
from typing import List, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus
from services.azure_devops_oauth_service import is_azure_devops_oauth_configured


class AzureDevOpsConnector(BaseConnector):
    connector_id = "azure_devops"
    connector_name = "Azure DevOps"
    description = "Connect via Microsoft OAuth to list open PRs and run PR Intelligence on Azure Repos."

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if is_azure_devops_oauth_configured():
            return True, "Server OAuth is configured — connect per project in Integrations"
        return False, "Azure OAuth env vars are not configured on the server"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        if is_azure_devops_oauth_configured():
            return "ok", "Azure DevOps OAuth configured on server"
        return "unconfigured", "Set AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, AZURE_TENANT_ID, AZURE_REDIRECT_URI"

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
                "oauth_configured": is_azure_devops_oauth_configured(),
                "provider": "microsoft_oauth",
            },
        )

    def supported_actions(self) -> List[str]:
        return [
            "list_open_prs",
            "analyze_pull_request",
            "list_repositories",
        ]
