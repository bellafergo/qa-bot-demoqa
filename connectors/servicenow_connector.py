# connectors/servicenow_connector.py
"""
ServiceNow connector — integration framework adapter (SNOW-01A read-only foundation).
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import List, Tuple

from connectors import BaseConnector
from models.connector import ConnectorConfig, ConnectorStatus


class ServiceNowConnector(BaseConnector):
    connector_id = "servicenow"
    connector_name = "ServiceNow"
    description = "Discover ServiceNow incidents, changes, services, and CMDB items (read-only)."

    def validate_config(self, config: ConnectorConfig) -> Tuple[bool, str]:
        if not config.base_url:
            return False, "instance_url (base_url) is required (e.g. https://yourinstance.service-now.com)"
        if not config.workspace:
            return False, "username (workspace) is required"
        if not config.token_present:
            return False, "password (token) is required"
        return True, "Config is valid"

    def health_check(self, config: ConnectorConfig) -> Tuple[str, str]:
        if not config.enabled:
            return "unconfigured", "Connector is disabled"
        valid, msg = self.validate_config(config)
        if not valid:
            return "unconfigured", msg

        from services.servicenow_integration_service import validate_servicenow_connection

        status = validate_servicenow_connection()
        if status.connected:
            return "ok", f"ServiceNow connected at {status.instance_url}"
        if config.base_url:
            return "unreachable", "ServiceNow configured but connection could not be validated"
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
                "instance_url": config.base_url,
                "username": config.workspace,
                "token_present": config.token_present,
                "auth_type": config.auth_type,
            },
        )

    def supported_actions(self) -> List[str]:
        return [
            "list_incidents",
            "list_changes",
            "list_services",
            "list_cmdb_items",
        ]
