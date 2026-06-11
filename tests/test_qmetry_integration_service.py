# tests/test_qmetry_integration_service.py
"""Read-only QMetry integration service tests (QMETRY-01A)."""
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import patch

import pytest

from models.connector import ConnectorConfigUpdate
from models.qmetry_models import QMetryConnectionStatus
from services import qmetry_integration_service as svc
from services.integration_service import IntegrationService


@pytest.fixture(autouse=True)
def _reset_qmetry_sync():
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


def _enable_qmetry(integration_svc: IntegrationService) -> None:
    integration_svc.update_config(
        "qmetry",
        ConnectorConfigUpdate(
            enabled=True,
            base_url="https://jira.example.com",
            project_key="QA",
            api_key="qmetry-secret-key",
        ),
    )


class TestEmptyConnection:
    def test_validate_returns_disconnected_when_not_configured(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            status = svc.validate_qmetry_connection()
        assert status.connected is False
        assert status.project_count == 0
        assert status.test_case_count == 0
        assert status.run_count == 0

    def test_list_projects_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_projects()
        assert result.projects == []
        assert result.total == 0


class TestConnectionValidation:
    def test_validate_qmetry_connection_success(self, integration_svc):
        _enable_qmetry(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.qmetry_integration_service.validate_connection",
            return_value={"projects": []},
        ), patch(
            "services.qmetry_integration_service.fetch_qmetry_projects",
            return_value=[{"id": "1", "name": "QA Platform", "key": "QA"}],
        ), patch(
            "services.qmetry_integration_service.count_test_cases",
            return_value=320,
        ), patch(
            "services.qmetry_integration_service.count_test_runs",
            return_value=48,
        ):
            status = svc.validate_qmetry_connection()

        assert status.connected is True
        assert status.project_count == 1
        assert status.test_case_count == 320
        assert status.run_count == 48
        assert isinstance(status.last_sync, datetime)

    def test_validate_qmetry_connection_auth_failure(self, integration_svc):
        from services.qmetry_repository_service import QMetryAPIError

        _enable_qmetry(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.qmetry_integration_service.validate_connection",
            side_effect=QMetryAPIError("invalid", status_code=401),
        ):
            status = svc.validate_qmetry_connection()

        assert status.connected is False


class TestProjectListing:
    def test_list_projects(self, integration_svc):
        _enable_qmetry(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.qmetry_integration_service.fetch_qmetry_projects",
            return_value=[
                {"id": "100", "name": "QA Platform"},
                {"id": "101", "name": "Web Frontend"},
            ],
        ):
            result = svc.list_projects()

        assert result.total == 2
        assert result.projects[0].project_name == "QA Platform"
        assert result.projects[1].project_id == "101"


class TestTestCaseListing:
    def test_list_test_cases(self, integration_svc):
        _enable_qmetry(integration_svc)
        raw_case = {
            "id": "200",
            "key": "QA-T1",
            "summary": "Login test",
            "priority": {"name": "High"},
            "status": {"name": "Approved"},
        }
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.qmetry_integration_service.search_test_cases",
            return_value=([raw_case], 1),
        ):
            result = svc.list_test_cases()

        assert result.total == 1
        assert result.test_cases[0].test_case_id == "200"
        assert result.test_cases[0].name == "Login test"
        assert result.test_cases[0].priority == "High"


class TestTestCycleListing:
    def test_list_test_cycles(self, integration_svc):
        _enable_qmetry(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.qmetry_integration_service.search_test_cycles",
            return_value=([{"id": "10", "name": "Sprint 42 Regression", "status": "ACTIVE"}], 1),
        ):
            result = svc.list_test_cycles()

        assert result.total == 1
        assert result.test_cycles[0].cycle_name == "Sprint 42 Regression"
        assert result.test_cycles[0].status == "ACTIVE"


class TestTestSuiteListing:
    def test_list_test_suites(self, integration_svc):
        _enable_qmetry(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.qmetry_integration_service.search_test_suites",
            return_value=([{"id": "20", "name": "Regression Suite"}], 1),
        ):
            result = svc.list_test_suites()

        assert result.total == 1
        assert result.test_suites[0].suite_name == "Regression Suite"


class TestTestRunListing:
    def test_list_test_runs(self, integration_svc):
        _enable_qmetry(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.qmetry_integration_service.search_test_runs",
            return_value=([
                {
                    "id": "30",
                    "summary": "Smoke Run",
                    "status": "PASS",
                    "executionDate": "2026-06-01",
                }
            ], 1),
        ):
            result = svc.list_test_runs()

        assert result.total == 1
        assert result.test_runs[0].run_name == "Smoke Run"
        assert result.test_runs[0].execution_date == "2026-06-01"


class TestQMetryApiRoutes:
    def _client(self):
        from fastapi.testclient import TestClient
        from app import app

        return TestClient(app)

    def test_status_route(self):
        fake = QMetryConnectionStatus(
            connected=True,
            project_count=3,
            test_case_count=320,
            run_count=48,
            last_sync=datetime.now(timezone.utc),
        )
        with patch(
            "api.routes.qmetry_integration_routes.validate_qmetry_connection",
            return_value=fake,
        ):
            res = self._client().get("/integrations/qmetry/status")
        assert res.status_code == 200
        data = res.json()
        assert data["connected"] is True
        assert data["test_case_count"] == 320

    def test_projects_route(self):
        from models.qmetry_models import QMetryProject, QMetryProjectsResponse

        with patch(
            "api.routes.qmetry_integration_routes.list_projects",
            return_value=QMetryProjectsResponse(
                projects=[QMetryProject(project_id="1", project_name="QA")],
                total=1,
            ),
        ):
            res = self._client().get("/integrations/qmetry/projects")
        assert res.status_code == 200
        assert res.json()["total"] == 1

    def test_test_cases_route(self):
        from models.qmetry_models import QMetryTestCase, QMetryTestCasesResponse

        with patch(
            "api.routes.qmetry_integration_routes.list_test_cases",
            return_value=QMetryTestCasesResponse(
                test_cases=[QMetryTestCase(test_case_id="1", name="Login", priority="High", status="Approved")],
                total=1,
            ),
        ):
            res = self._client().get("/integrations/qmetry/test-cases")
        assert res.status_code == 200
        assert res.json()["total"] == 1
