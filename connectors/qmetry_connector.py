# connectors/qmetry_connector.py
"""
QMetry connector — integration framework adapter (QMETRY-01A read-only foundation).

QMETRY-01A is discovery-only. This connector owns:
  - Framework lifecycle (enable/disable, config validation)
  - Health delegation to services/qmetry_integration_service.py

Canonical read-only discovery (projects, test cases, cycles, suites, runs) lives in
qmetry_integration_service.py — not in this connector:
  - list_projects()
  - list_test_cases()
  - list_test_cycles()
  - list_test_suites()
  - list_test_runs()

Write/sync operations (test execution, result upload, cycle/suite mutation) are NOT
implemented in QMETRY-01A. Test result discovery is deferred to QMETRY-01B.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


class QMetryConnector(BaseConnector):
    connector_id   = "qmetry"
    connector_name = "QMetry"
    description    = "Discover QMetry projects, test cases, cycles, suites, and runs (read-only)."

    # ── Contract ──────────────────────────────────────────────────────────────

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not config.api_key_present:
            return False, "A QMetry API key is required"
        if not config.base_url:
            return False, "base_url is required (QMetry server URL or Jira base URL)"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        """Delegate to qmetry_integration_service when enabled and configured."""
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        valid, msg = self.validate_config(config)
        if not valid:
            return "unconfigured", msg
        from services.qmetry_integration_service import validate_qmetry_connection

        status = validate_qmetry_connection()
        if status.connected:
            return "ok", f"QMetry connected — {status.project_count} project(s)"
        if config.base_url:
            return "unreachable", f"QMetry not reachable at {config.base_url}"
        return "unconfigured", "QMetry credentials incomplete"

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
            "discover_projects",
            "discover_test_cases",
            "discover_test_cycles",
            "discover_test_suites",
            "discover_test_runs",
        ]

    # ── Legacy stubs (not part of live discovery path) ────────────────────────
    #
    # These pre-QMETRY-01A placeholders remain for backward-compatible tests only.
    # They do NOT call the QMetry API and are NOT used by discovery routes or UI.
    # Canonical cycle discovery: qmetry_integration_service.list_test_cycles()
    # Write paths will be redesigned in a future sprint — not QMETRY-01A/01B.

    def sync_test_cases_stub(
        self,
        test_case_ids: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """
        DEPRECATED — legacy compatibility stub. Not part of live discovery.

        Do not use for QMetry integration. Real test case discovery is
        qmetry_integration_service.list_test_cases().
        """
        return {
            "synced":        len(test_case_ids or []),
            "test_case_ids": test_case_ids or [],
            "status":        "stub",
            "message":       "Legacy stub — use qmetry_integration_service.list_test_cases()",
        }

    def push_execution_result_stub(
        self,
        run_id: str,
        test_case_id: str,
        status: str,
        cycle_name: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        DEPRECATED — legacy compatibility stub. Not part of live discovery.

        Write operations are not implemented in QMETRY-01A. Do not use.
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
        """
        DEPRECATED — legacy compatibility stub. Not part of live discovery.

        Returns mock data only. Canonical cycle discovery is
        qmetry_integration_service.list_test_cycles().
        """
        return [
            {"id": "CYC-001", "name": "Sprint 42 Regression", "status": "active"},
            {"id": "CYC-002", "name": "Release 2.1 Smoke",    "status": "closed"},
        ]


def _map_status(vanya_status: str) -> str:
    """Map Vanya run status → QMetry execution status (legacy stub helper)."""
    return {
        "pass":  "PASS",
        "fail":  "FAIL",
        "error": "BLOCKED",
    }.get(vanya_status.lower(), "IN_PROGRESS")
