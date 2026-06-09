# tests/test_azure_devops_integration.py
"""Azure DevOps Integration — OAuth + PR Analysis unit tests with mocks."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

import pytest
from fastapi.testclient import TestClient

from models.project_knowledge_models import ProjectKnowledge
from services.project_azure_devops_settings_service import (
    build_connection_status,
    disconnect_project_azure_devops,
    get_authorize_url,
    get_project_azure_devops_status,
)


@pytest.fixture()
def client():
    from app import app
    return TestClient(app)


def _mock_project(settings=None):
    p = MagicMock()
    p.id = "demo"
    p.settings = settings or {}
    return p


def _app_azure_settings(**extra):
    base = {
        "enabled": True,
        "provider": "oauth",
        "refresh_token": "rt_secret",
        "organization": "acme",
        "azure_project": "App",
        "repository_id": "repo-guid-1",
        "repository_name": "app-repo",
        "default_branch": "main",
        "connected_at": "2026-01-01T00:00:00+00:00",
        "last_validated_at": "2026-01-01T00:00:00+00:00",
    }
    base.update(extra)
    return {"azure_devops": base}


@patch("services.project_azure_devops_settings_service.is_azure_devops_oauth_configured", return_value=True)
@patch("services.project_azure_devops_settings_service.build_authorize_url")
def test_authorize_url_endpoint(mock_build, _mock_cfg, client):
    mock_build.return_value = "https://login.microsoftonline.com/tenant/oauth2/v2.0/authorize?state=demo"
    res = client.get("/projects/demo/azure-devops/authorize-url")
    assert res.status_code == 200
    body = res.json()
    assert "authorize_url" in body
    assert body["state"] == "demo"
    mock_build.assert_called_once_with(state="demo")


@patch("services.project_azure_devops_settings_service.is_azure_devops_oauth_configured", return_value=True)
@patch("services.project_azure_devops_settings_service.exchange_code_for_tokens")
@patch("services.db.project_repository.project_repo")
def test_oauth_callback_exchanges_and_saves(mock_repo, mock_exchange, _mock_cfg, client):
    mock_exchange.return_value = {
        "access_token": "at_123",
        "refresh_token": "rt_456",
        "expires_in": 3600,
    }
    project = _mock_project({})
    mock_repo.get_project.return_value = project
    mock_repo.update_project.return_value = MagicMock(
        id="demo",
        settings=_app_azure_settings(refresh_token="rt_456", organization="", repository_id=""),
    )

    res = client.get("/azure-devops/callback?code=abc&state=demo", follow_redirects=False)
    assert res.status_code == 302
    assert "integrations" in res.headers.get("location", "")
    mock_exchange.assert_called_once_with("abc")


def test_status_disconnected():
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = _mock_project({})
        status = get_project_azure_devops_status("demo", validate=False)

    assert status.connected is False
    assert status.provider == "none"
    dumped = status.model_dump_json()
    assert "rt_secret" not in dumped
    assert "refresh_token" not in dumped


@patch("services.project_azure_devops_settings_service.is_azure_devops_oauth_configured", return_value=True)
def test_status_connected(_mock_cfg):
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = _mock_project(_app_azure_settings())
        status = get_project_azure_devops_status("demo", validate=False)

    assert status.connected is True
    assert status.organization == "acme"
    assert status.repository_name == "app-repo"


def test_analyze_pull_request_invokes_pr_analysis_v1():
    from services.azure_devops_integration_service import analyze_pull_request

    knowledge = ProjectKnowledge(project_id="demo", risk_score=50.0, risk_level="MEDIUM")

    with patch("services.azure_devops_integration_service._require_client") as req:
        req.return_value = ({}, MagicMock())
        with patch("services.azure_devops_integration_service.get_pull_request_files") as gpf:
            from models.azure_devops_integration_models import AzureDevOpsPRFilesResponse
            gpf.return_value = AzureDevOpsPRFilesResponse(
                pull_request_id=42,
                title="Fix candidates",
                branch="fix/candidates",
                changed_files=["src/CandidateForm.tsx"],
                patches_limited=True,
            )
            with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
                with patch("services.db.catalog_repository.catalog_repo") as cat:
                    cat.all_modules_for_project.return_value = [("TC-1", "Candidates")]
                    report = analyze_pull_request("demo", 42)

    assert report.analysis.project_id == "demo"
    assert report.pull_request.pull_request_id == 42
    assert report.source == "azure_devops_oauth_v1"
    assert "src/CandidateForm.tsx" in report.pull_request.changed_files


def test_pr_analyze_endpoint_registered(client):
    res = client.post("/projects/nonexistent-proj-xyz/azure-devops/pull-requests/1/analyze")
    assert res.status_code == 404


def test_disconnect_clears_azure_settings():
    project = _mock_project(_app_azure_settings())
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = project
        repo.update_project.return_value = MagicMock(id="demo", settings={})
        with patch("services.project_azure_devops_settings_service.azure_devops_token_cache") as cache:
            status = disconnect_project_azure_devops("demo")

    assert status.connected is False
    assert status.provider == "none"
    saved = repo.update_project.call_args[0][1]["settings"]
    assert "azure_devops" not in saved
    cache.invalidate.assert_called_once_with("demo")


def test_disconnect_endpoint(client):
    from models.azure_devops_integration_models import AzureDevOpsConnectionStatus

    with patch("api.routes.azure_devops_project_routes.disconnect_project_azure_devops") as mock_disc:
        mock_disc.return_value = AzureDevOpsConnectionStatus(
            project_id="demo",
            connected=False,
            provider="none",
        )
        res = client.post("/projects/demo/azure-devops/disconnect")
    assert res.status_code == 200
    assert res.json()["connected"] is False


def test_github_still_works_after_azure_routes(client):
    """Regression: GitHub analyze route remains registered."""
    res = client.post("/projects/nonexistent-proj-xyz/github/pull-requests/1/analyze")
    assert res.status_code == 404


@patch("services.project_azure_devops_settings_service.is_azure_devops_oauth_configured", return_value=True)
def test_get_authorize_url_service(_mock_cfg):
    with patch("services.project_azure_devops_settings_service.build_authorize_url") as mock_build:
        mock_build.return_value = "https://login.microsoftonline.com/t/oauth2/v2.0/authorize"
        resp = get_authorize_url("demo")
    assert resp.state == "demo"
    assert resp.authorize_url.startswith("https://")


def test_build_connection_status_masks_secrets():
    status = build_connection_status("demo", az=_app_azure_settings()["azure_devops"])
    dumped = status.model_dump_json()
    assert "rt_secret" not in dumped
