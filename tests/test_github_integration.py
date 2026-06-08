# tests/test_github_integration.py
"""GitHub Integration — SaaS GitHub App unit tests with mocks."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

import pytest
from fastapi.testclient import TestClient

from models.github_integration_models import GitHubConnectAppRequest, GitHubSelectRepositoryRequest
from models.project_knowledge_models import ProjectKnowledge
from services.github_repository_service import GitHubAPIError, GitHubRepositoryClient
from services.project_github_settings_service import (
    connect_project_github_app,
    disconnect_project_github,
    get_project_github_status,
    select_project_repository,
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


def _app_github_settings(**extra):
    base = {
        "enabled": True,
        "provider": "github_app",
        "installation_id": "12345",
        "owner": "acme",
        "repo": "app",
        "default_branch": "main",
        "permissions": {"contents": "read", "pull_requests": "read"},
        "connected_at": "2026-01-01T00:00:00+00:00",
        "last_validated_at": "2026-01-01T00:00:00+00:00",
    }
    base.update(extra)
    return {"github": base}


@patch("services.project_github_settings_service.is_github_app_configured", return_value=True)
@patch("services.project_github_settings_service.get_installation")
@patch("services.project_github_settings_service.validate_installation_permissions")
def test_connect_app_saves_settings_no_token(mock_perms, mock_install, _mock_cfg):
    mock_install.return_value = {"account": {"login": "acme"}, "permissions": {}}
    mock_perms.return_value = {"contents": "read", "pull_requests": "read"}

    project = _mock_project({})
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = project
        repo.update_project.return_value = MagicMock(
            id="demo",
            settings=_app_github_settings(installation_id="12345", owner="", repo=""),
        )
        status = connect_project_github_app(
            "demo",
            GitHubConnectAppRequest(installation_id="12345", connected_by="dev@example.com"),
        )

    assert status.provider == "github_app"
    assert status.installation_id == "12345"
    assert status.connected is False  # repo not selected yet
    assert status.validation_ok is True
    dumped = status.model_dump()
    assert "ghp_" not in str(dumped)
    assert "token" not in dumped
    repo.update_project.assert_called_once()
    saved = repo.update_project.call_args[0][1]["settings"]["github"]
    assert saved.get("github_token") is None
    assert saved.get("provider") == "github_app"


@patch("services.project_github_settings_service.is_github_app_configured", return_value=True)
@patch("services.project_github_settings_service.list_authorized_repositories")
@patch("services.project_github_settings_service.http_config_for_installation")
def test_select_repository_validates_and_saves(mock_http, mock_list, _mock_cfg):
    from models.github_integration_models import GitHubRepositoriesResponse, GitHubRepositorySummary

    mock_list.return_value = GitHubRepositoriesResponse(
        installation_id="12345",
        repositories=[GitHubRepositorySummary(owner="acme", repo="app", full_name="acme/app", default_branch="main")],
    )
    mock_http.return_value = MagicMock(token="inst_tok", api_base="https://api.github.com", timeout_s=30)

    project = _mock_project(_app_github_settings(owner="", repo=""))
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = project
        repo.update_project.return_value = MagicMock(id="demo", settings=_app_github_settings())
        with patch.object(GitHubRepositoryClient, "validate_connection", return_value=({"default_branch": "main"}, 4999)):
            status = select_project_repository(
                "demo",
                GitHubSelectRepositoryRequest(owner="acme", repo="app"),
            )

    assert status.connected is True
    assert status.owner == "acme"
    assert status.repo == "app"


def test_legacy_pat_status_flags_migration():
    gh = {
        "enabled": True,
        "owner": "acme",
        "repo": "app",
        "github_token": "ghp_secret",
        "default_branch": "main",
    }
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = _mock_project({"github": gh})
        status = get_project_github_status("demo", validate=False)

    assert status.needs_migration is True
    assert status.provider == "legacy_pat"
    assert "supersecret" not in status.model_dump_json()
    assert "ghp_secret" not in status.model_dump_json()


def test_github_repository_rate_limit_error():
    http = MagicMock()
    http.token = "tok"
    http.api_base = "https://api.github.com"
    http.timeout_s = 10
    client = GitHubRepositoryClient(http, owner="acme", repo="app")

    res = MagicMock()
    res.status_code = 403
    res.is_success = False
    res.headers = {"X-RateLimit-Remaining": "0", "X-RateLimit-Reset": "999999"}

    with patch("httpx.Client") as mock_client:
        mock_client.return_value.__enter__.return_value.get.return_value = res
        with pytest.raises(GitHubAPIError) as exc:
            client.validate_connection()
    assert exc.value.code == "rate_limit"
    assert "rate limit" in str(exc.value).lower()


def test_analyze_pull_request_invokes_pr_analysis_v1():
    from services.github_integration_service import analyze_pull_request

    knowledge = ProjectKnowledge(project_id="demo", risk_score=50.0, risk_level="MEDIUM")

    with patch("services.github_integration_service._require_client") as req:
        req.return_value = ({}, MagicMock())
        with patch("services.github_integration_service.get_pull_request_files") as gpf:
            from models.github_integration_models import GitHubPRFilesResponse
            gpf.return_value = GitHubPRFilesResponse(
                number=42,
                title="Fix candidates",
                branch="fix/candidates",
                changed_files=["src/CandidateForm.tsx"],
            )
            with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
                with patch("services.db.catalog_repository.catalog_repo") as cat:
                    cat.all_modules_for_project.return_value = [("TC-1", "Candidates")]
                    report = analyze_pull_request("demo", 42)

    assert report.analysis.project_id == "demo"
    assert report.pull_request.number == 42
    assert report.source == "github_app_v1"
    assert "src/CandidateForm.tsx" in report.pull_request.changed_files


def test_pr_analyze_endpoint_registered(client):
    res = client.post("/projects/nonexistent-proj-xyz/github/pull-requests/1/analyze")
    assert res.status_code == 404


def test_disconnect_clears_github_settings():
    project = _mock_project(_app_github_settings())
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = project
        repo.update_project.return_value = MagicMock(id="demo", settings={})
        with patch("services.github_installation_token_cache.installation_token_cache") as cache:
            status = disconnect_project_github("demo")

    assert status.connected is False
    assert status.installation_id == ""
    assert status.provider == "none"
    saved = repo.update_project.call_args[0][1]["settings"]
    assert "github" not in saved
    cache.invalidate.assert_called_once_with("12345")


def test_disconnect_endpoint(client):
    from models.github_integration_models import GitHubConnectionStatus

    with patch("api.routes.github_project_routes.disconnect_project_github") as mock_disc:
        mock_disc.return_value = GitHubConnectionStatus(
            project_id="demo",
            connected=False,
            installation_id="",
            provider="none",
            validation_message="GitHub is not connected for this project.",
        )
        res = client.post("/projects/demo/github/disconnect")
    assert res.status_code == 200
    mock_disc.assert_called_once_with("demo")


@patch("services.project_github_settings_service.is_github_app_configured", return_value=True)
@patch("services.project_github_settings_service.build_app_install_url", return_value="https://github.com/apps/vanya/installations/new?state=demo")
def test_install_url_endpoint(_mock_url, _mock_cfg, client):
    with patch("services.db.project_repository.project_repo") as repo:
        repo.get_project.return_value = _mock_project({})
        res = client.get("/projects/demo/github/install-url")
    assert res.status_code == 200
    data = res.json()
    assert "install_url" in data
    assert "demo" in data["install_url"]


@patch("services.github_app_service.get_installation_access_token", return_value=("inst_tok", "2099-01-01T00:00:00Z"))
def test_installation_token_never_logged_in_cache_get(mock_tok):
    from services.github_installation_token_cache import installation_token_cache

    installation_token_cache.set("99", "inst_tok", "2099-01-01T00:00:00Z")
    tok = installation_token_cache.get("99")
    assert tok == "inst_tok"
    mock_tok.assert_not_called()  # cache hit
