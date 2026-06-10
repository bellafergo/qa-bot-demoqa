# tests/test_jira_integration_service.py
"""Read-only Jira integration service tests (JIRA-01A)."""
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import MagicMock, patch

import pytest

from models.connector import ConnectorConfigUpdate
from models.jira_models import JiraConnectionStatus
from services import jira_integration_service as svc
from services.integration_service import IntegrationService


@pytest.fixture(autouse=True)
def _reset_jira_sync():
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


def _enable_jira(integration_svc: IntegrationService) -> None:
    integration_svc.update_config(
        "jira",
        ConnectorConfigUpdate(
            enabled=True,
            base_url="https://acme.atlassian.net",
            workspace="qa@acme.com",
            project_key="QA",
            token="secret-token",
        ),
    )


class TestEmptyConnection:
    def test_validate_returns_disconnected_when_not_configured(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            status = svc.validate_jira_connection()
        assert status.connected is False
        assert status.project_count == 0
        assert status.issue_count == 0

    def test_list_projects_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_projects()
        assert result.projects == []
        assert result.total == 0

    def test_list_issues_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_issues()
        assert result.issues == []
        assert result.total == 0


class TestConnectionValidation:
    def test_validate_jira_connection_success(self, integration_svc):
        _enable_jira(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service.validate_connection",
            return_value={"accountId": "1"},
        ), patch(
            "services.jira_integration_service.fetch_jira_projects",
            return_value=[{"id": "1", "key": "QA", "name": "QA Platform"}],
        ), patch(
            "services.jira_integration_service.count_issues",
            side_effect=[1248, 67],
        ), patch(
            "services.jira_integration_service.list_project_versions",
            return_value=[
                {"id": "10", "name": "2026.1", "released": True, "releaseDate": "2026-01-01"},
                {"id": "11", "name": "2026.2", "released": False},
            ],
        ):
            status = svc.validate_jira_connection()

        assert status.connected is True
        assert status.server_url == "https://acme.atlassian.net"
        assert status.project_count == 1
        assert status.issue_count == 1248
        assert status.epic_count == 67
        assert status.release_count == 1
        assert status.fix_version_count == 1
        assert isinstance(status.last_sync, datetime)

    def test_validate_jira_connection_auth_failure(self, integration_svc):
        from services.jira_repository_service import JiraAPIError

        _enable_jira(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service.validate_connection",
            side_effect=JiraAPIError("invalid", status_code=401),
        ):
            status = svc.validate_jira_connection()

        assert status.connected is False
        assert status.server_url == "https://acme.atlassian.net"


class TestProjectListing:
    def test_list_projects(self, integration_svc):
        _enable_jira(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service.fetch_jira_projects",
            return_value=[
                {"id": "100", "key": "QA", "name": "QA Platform"},
                {"id": "101", "key": "WEB", "name": "Web Frontend"},
            ],
        ):
            result = svc.list_projects()

        assert result.total == 2
        assert result.projects[0].project_key == "QA"
        assert result.projects[1].project_name == "Web Frontend"


class TestIssueListing:
    def test_list_issues(self, integration_svc):
        _enable_jira(integration_svc)
        raw_issue = {
            "id": "200",
            "key": "QA-1",
            "fields": {
                "summary": "Login fails",
                "issuetype": {"name": "Bug"},
                "status": {"name": "Open"},
                "assignee": {"displayName": "Alex"},
                "priority": {"name": "High"},
            },
        }
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service.search_issues",
            return_value=([raw_issue], 1),
        ):
            result = svc.list_issues()

        assert result.total == 1
        assert result.issues[0].issue_key == "QA-1"
        assert result.issues[0].summary == "Login fails"
        assert result.issues[0].assignee == "Alex"


class TestEpicListing:
    def test_list_epics(self, integration_svc):
        _enable_jira(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service.search_issues",
            return_value=(
                [{"id": "300", "key": "QA-10", "fields": {"summary": "Checkout Epic"}}],
                1,
            ),
        ):
            result = svc.list_epics()

        assert result.total == 1
        assert result.epics[0].epic_key == "QA-10"
        assert result.epics[0].epic_name == "Checkout Epic"


class TestReleaseListing:
    def test_list_releases(self, integration_svc):
        _enable_jira(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service._collect_versions",
            return_value=[
                {"id": "10", "name": "2026.1", "released": True, "releaseDate": "2026-03-01"},
                {"id": "11", "name": "2026.2", "released": False},
            ],
        ):
            result = svc.list_releases()

        assert result.total == 1
        assert result.releases[0].release_name == "2026.1"
        assert result.releases[0].released is True


class TestFixVersionListing:
    def test_list_fix_versions(self, integration_svc):
        _enable_jira(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service._collect_versions",
            return_value=[
                {"id": "10", "name": "2026.1", "released": True},
                {"id": "11", "name": "2026.2", "released": False},
            ],
        ):
            result = svc.list_fix_versions()

        assert result.total == 1
        assert result.fix_versions[0].version_name == "2026.2"
        assert result.fix_versions[0].released is False


class TestIssueTypeListing:
    def test_list_issue_types_empty_when_disabled(self, integration_svc):
        with patch.object(svc, "integration_service", integration_svc):
            result = svc.list_issue_types()
        assert result.issue_types == []
        assert result.total == 0

    def test_list_issue_types(self, integration_svc):
        _enable_jira(integration_svc)
        with patch.object(svc, "integration_service", integration_svc), patch(
            "services.jira_integration_service.fetch_jira_projects",
            return_value=[{"id": "100", "key": "QA", "name": "QA"}],
        ), patch(
            "services.jira_integration_service.list_issue_types_for_project",
            return_value=[{"id": "1", "name": "Bug"}, {"id": "2", "name": "Epic"}],
        ):
            result = svc.list_issue_types(project_key="QA")

        assert result.total == 2
        assert result.issue_types[0].issue_type_name == "Bug"


class TestJiraApiRoutes:
    def _client(self):
        from fastapi.testclient import TestClient
        from app import app

        return TestClient(app)

    def test_status_route(self):
        fake = JiraConnectionStatus(
            connected=True,
            server_url="https://acme.atlassian.net",
            project_count=12,
            issue_count=1248,
            epic_count=67,
            release_count=14,
            fix_version_count=31,
            last_sync=datetime.now(timezone.utc),
        )
        with patch(
            "api.routes.jira_integration_routes.validate_jira_connection",
            return_value=fake,
        ):
            res = self._client().get("/integrations/jira/status")
        assert res.status_code == 200
        data = res.json()
        assert data["connected"] is True
        assert data["project_count"] == 12

    def test_projects_route(self):
        from models.jira_models import JiraProject, JiraProjectsResponse

        with patch(
            "api.routes.jira_integration_routes.list_projects",
            return_value=JiraProjectsResponse(
                projects=[JiraProject(project_id="1", project_key="QA", project_name="QA")],
                total=1,
            ),
        ):
            res = self._client().get("/integrations/jira/projects")
        assert res.status_code == 200
        assert res.json()["total"] == 1

    def test_issue_types_route(self):
        from models.jira_models import JiraIssueType, JiraIssueTypesResponse

        with patch(
            "api.routes.jira_integration_routes.list_issue_types",
            return_value=JiraIssueTypesResponse(
                issue_types=[JiraIssueType(issue_type_id="1", issue_type_name="Bug")],
                total=1,
            ),
        ):
            res = self._client().get("/integrations/jira/issue-types")
        assert res.status_code == 200
        assert res.json()["total"] == 1
