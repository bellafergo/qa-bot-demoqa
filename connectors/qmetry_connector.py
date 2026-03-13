# connectors/qmetry_connector.py
"""
QMetry connector stub / framework placeholder.

QMetry is a test management tool (Jira-native or standalone).
This implementation provides the connector contract and placeholder sync hooks.
The design is intentionally kept thin — extend when the QMetry API details
are finalized.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


class QMetryConnector(BaseConnector):
    connector_id   = "qmetry"
    connector_name = "QMetry"
    description    = "Sync test cases and execution results with QMetry TM for Jira."

    # ── Contract ──────────────────────────────────────────────────────────────

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not config.api_key_present:
            return False, "A QMetry API key is required"
        if not config.base_url:
            return False, "base_url is required (QMetry server URL or Jira base URL)"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        valid, msg = self.validate_config(config)
        if not valid:
            return "unconfigured", msg
        # Stub: real impl would call QMetry /api/authenticate or /rest/atm/1.0/testcase
        return "ok", f"QMetry stub reachable at {config.base_url}"

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
                "base_url":        config.base_url,
                "api_key_present": config.api_key_present,
                "project_key":     config.project_key,
            },
        )

    def supported_actions(self) -> List[str]:
        return [
            "sync_test_cases",
            "push_execution_result",
            "fetch_test_cycles",
            "create_test_cycle",
        ]

    # ── Placeholder sync hooks ────────────────────────────────────────────────

    def sync_test_cases_stub(
        self,
        test_case_ids: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """
        Placeholder: sync Vanya test cases to QMetry.
        Implement by calling QMetry REST API when credentials are available.
        """
        return {
            "synced":        len(test_case_ids or []),
            "test_case_ids": test_case_ids or [],
            "status":        "stub",
            "message":       "QMetry sync hook — awaiting real API credentials",
        }

    def push_execution_result_stub(
        self,
        run_id: str,
        test_case_id: str,
        status: str,
        cycle_name: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Placeholder: push a test execution result to a QMetry test cycle.
        Implement by calling POST /rest/atm/1.0/testrun when ready.
        """
        return {
            "pushed":       True,
            "run_id":       run_id,
            "test_case_id": test_case_id,
            "qmetry_status": _map_status(status),
            "cycle_name":   cycle_name or "Vanya Run",
            "stub":         True,
        }

    def fetch_test_cycles_stub(self) -> List[Dict[str, Any]]:
        """Stub: returns deterministic list of mock QMetry test cycles."""
        return [
            {"id": "CYC-001", "name": "Sprint 42 Regression", "status": "active"},
            {"id": "CYC-002", "name": "Release 2.1 Smoke",    "status": "closed"},
        ]


def _map_status(vanya_status: str) -> str:
    """Map Vanya run status → QMetry execution status."""
    return {
        "pass":  "PASS",
        "fail":  "FAIL",
        "error": "BLOCKED",
    }.get(vanya_status.lower(), "IN_PROGRESS")
