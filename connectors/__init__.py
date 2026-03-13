# connectors/__init__.py
"""
Base connector contract for the Vanya Integration Layer.

All connectors must subclass BaseConnector and implement every abstract method.
Connector implementations live in sibling modules (jira_connector.py, etc.).
"""
from __future__ import annotations

from abc import ABC, abstractmethod
from typing import List, Tuple

from models.connector import ConnectorConfig, ConnectorStatus, ConnectorSummary


class BaseConnector(ABC):
    """
    Abstract base class every connector must implement.

    Attributes (class-level, must be set by subclasses):
        connector_id   — stable machine identifier, e.g. "jira"
        connector_name — human-readable name, e.g. "Jira"
        description    — one-sentence description shown in the UI
    """

    connector_id:   str
    connector_name: str
    description:    str

    # ── Contract ──────────────────────────────────────────────────────────────

    @abstractmethod
    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        """
        Validate the stored config without making any network calls.
        Returns (is_valid: bool, message: str).
        """

    @abstractmethod
    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        """
        Perform a lightweight reachability / auth probe.
        No real network call is required — stubs return deterministic results.
        Returns (health_status: HealthStatus, message: str).
        """

    @abstractmethod
    def get_status(self, config: ConnectorConfig) -> ConnectorStatus:
        """Build and return the current ConnectorStatus for this connector."""

    @abstractmethod
    def supported_actions(self) -> List[str]:
        """Return the list of action names this connector supports."""

    # ── Convenience ───────────────────────────────────────────────────────────

    def to_summary(self, config: ConnectorConfig) -> ConnectorSummary:
        """Build a ConnectorSummary from the stored config."""
        status = self.get_status(config)
        return ConnectorSummary(
            connector_id=self.connector_id,
            connector_name=self.connector_name,
            description=self.description,
            enabled=config.enabled,
            health=status.health,
            last_check_at=status.last_check_at,
            supported_actions=self.supported_actions(),
        )
