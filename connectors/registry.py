# connectors/registry.py
"""
Connector registry — single source of truth for all available connectors.

To add a new connector:
  1. Create connectors/<name>_connector.py implementing BaseConnector
  2. Import it here and append to _ALL_CONNECTORS
"""
from __future__ import annotations

from typing import Dict, List, Optional

from connectors import BaseConnector
from connectors.jira_connector import JiraConnector
from connectors.github_connector import GitHubConnector
from connectors.slack_connector import SlackConnector
from connectors.qmetry_connector import QMetryConnector
from connectors.email_connector import EmailConnector
from connectors.teams_connector import TeamsConnector
from connectors.itsm_connector import ITSMConnector

_ALL_CONNECTORS: List[BaseConnector] = [
    JiraConnector(),
    GitHubConnector(),
    SlackConnector(),
    QMetryConnector(),
    EmailConnector(),
    TeamsConnector(),
    ITSMConnector(),
]


class ConnectorRegistry:
    """Immutable registry of all connectors keyed by connector_id."""

    def __init__(self, connectors: List[BaseConnector]) -> None:
        self._registry: Dict[str, BaseConnector] = {
            c.connector_id: c for c in connectors
        }

    def all(self) -> List[BaseConnector]:
        """Return all registered connectors in insertion order."""
        return list(self._registry.values())

    def get(self, connector_id: str) -> Optional[BaseConnector]:
        """Return the connector for the given id, or None if not found."""
        return self._registry.get(connector_id)

    def ids(self) -> List[str]:
        return list(self._registry.keys())


# Module-level singleton — import this everywhere
registry = ConnectorRegistry(_ALL_CONNECTORS)
