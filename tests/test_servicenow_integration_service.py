# tests/test_servicenow_integration_service.py
"""Read-only ServiceNow integration service tests (SNOW-01A)."""
from __future__ import annotations

from datetime import datetime
from unittest.mock import patch

import pytest

from models.connector import ConnectorConfigUpdate
from services import servicenow_integration_service as svc
from services.integration_service import IntegrationService
from services.servicenow_repository_service import ServiceNowAPIError


@pytest.fixture(autouse=True)
def _reset_servicenow_sync():
    svc._LAST_SYNC = None
    yield
    svc._LAST_SYNC = None


@pytest.fixture
def integration_svc(tmp_path, monkeypatch):
    monkeypatch.setattr(
        "services.integration_service._CONFIG_PATH",
        tmp_path / "integrations_config.json",
    )
    return IntegrationService()


def _enable_servicenow(integration_svc: IntegrationService) -> None:
    integration_svc.update_config(
        "servicenow",
        ConnectorConfigUpdate(
            enabled=True,
            base_url="https://acme.service-now.com",
            workspace="integration.user",
            auth_type="basic",
            token="secret-password",
        ),
    )


class TestEmptyConnection:
    def test_validate_returns_disconnected_when_not_configured(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            status = svc.validate_servicenow_connection()
        assert status.connected is False
        assert status.incident_count == 0
        assert status.change_count == 0

    def test_list_incidents_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_incidents()
        assert result.incidents == []
        assert result.total == 0

    def test_list_changes_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_changes()
        assert result.changes == []
        assert result.total == 0

    def test_list_services_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_services()
        assert result.services == []
        assert result.total == 0

    def test_list_cmdb_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_cmdb_items()
        assert result.items == []
        assert result.total == 0


class TestConnectionValidation:
    def test_validate_servicenow_connection_success(self, integration_svc):
        _enable_servicenow(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.validate_connection",
            return_value={"result": []},
        ), patch(
            "services.servicenow_integration_service.count_table",
            side_effect=[42, 7, 5, 120],
        ):
            status = svc.validate_servicenow_connection()

        assert status.connected is True
        assert status.instance_url == "https://acme.service-now.com"
        assert status.incident_count == 42
        assert status.change_count == 7
        assert status.service_count == 5
        assert status.cmdb_count == 120
        assert isinstance(status.last_sync, datetime)

    def test_validate_servicenow_connection_invalid_credentials(self, integration_svc):
        _enable_servicenow(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.validate_connection",
            side_effect=ServiceNowAPIError("invalid", status_code=401, code="invalid_credentials"),
        ):
            status = svc.validate_servicenow_connection()

        assert status.connected is False
        assert status.instance_url == "https://acme.service-now.com"


class TestConnectorHealth:
    def test_health_check_degraded_when_validation_fails(self, integration_svc):
        from connectors.servicenow_connector import ServiceNowConnector

        _enable_servicenow(integration_svc)
        cfg = integration_svc.get_config("servicenow")
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.validate_connection",
            side_effect=ServiceNowAPIError("invalid", status_code=401, code="invalid_credentials"),
        ):
            health, _msg = ServiceNowConnector().health_check(cfg)
        assert health == "degraded"

    def test_config_summary_uses_framework_keys(self, integration_svc):
        from connectors.servicenow_connector import ServiceNowConnector

        _enable_servicenow(integration_svc)
        cfg = integration_svc.get_config("servicenow")
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.validate_connection",
            return_value={"result": []},
        ), patch(
            "services.servicenow_integration_service.count_table",
            side_effect=[0, 0, 0, 0],
        ):
            summary = ServiceNowConnector().get_status(cfg).config_summary

        assert summary == {
            "base_url": "https://acme.service-now.com",
            "workspace": "integration.user",
            "token_present": True,
            "auth_type": "basic",
        }


class TestDiscovery:
    def test_list_incidents_maps_rows(self, integration_svc):
        _enable_servicenow(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.list_table",
            return_value=[
                {
                    "number": "INC0001",
                    "short_description": "Email outage",
                    "state": "2",
                    "priority": "1",
                    "assignment_group": {"display_value": "Service Desk"},
                    "opened_at": "2026-06-01 10:00:00",
                },
            ],
        ):
            result = svc.list_incidents()

        assert result.total == 1
        assert result.incidents[0].number == "INC0001"
        assert result.incidents[0].short_description == "Email outage"
        assert result.incidents[0].assignment_group == "Service Desk"

    def test_list_changes_maps_rows(self, integration_svc):
        _enable_servicenow(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.list_table",
            return_value=[
                {
                    "number": "CHG0001",
                    "short_description": "Database patch",
                    "state": "Implement",
                    "risk": "Moderate",
                    "planned_start_date": "2026-06-15 08:00:00",
                    "planned_end_date": "2026-06-15 12:00:00",
                },
            ],
        ):
            result = svc.list_changes()

        assert result.total == 1
        assert result.changes[0].number == "CHG0001"
        assert result.changes[0].risk == "Moderate"

    def test_list_services_maps_rows(self, integration_svc):
        _enable_servicenow(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.list_table",
            return_value=[
                {
                    "name": "Corporate Email",
                    "business_criticality": "1 - most critical",
                    "operational_status": "1",
                },
            ],
        ):
            result = svc.list_services()

        assert result.total == 1
        assert result.services[0].name == "Corporate Email"

    def test_list_cmdb_items_maps_rows(self, integration_svc):
        _enable_servicenow(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.servicenow_integration_service.list_table",
            return_value=[
                {
                    "name": "app-server-01",
                    "sys_class_name": "cmdb_ci_server",
                    "operational_status": "1",
                },
            ],
        ):
            result = svc.list_cmdb_items()

        assert result.total == 1
        assert result.items[0].name == "app-server-01"
        assert result.items[0].class_name == "cmdb_ci_server"
