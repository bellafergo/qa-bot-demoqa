# api/routes/github_project_routes.py
"""GitHub Integration — SaaS GitHub App (project-scoped)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException, Query

from models.github_integration_models import (
    GitHubBranchesResponse,
    GitHubConnectAppRequest,
    GitHubConnectionStatus,
    GitHubInstallUrlResponse,
    GitHubPRAnalyzeResponse,
    GitHubPRFilesResponse,
    GitHubPullRequestsResponse,
    GitHubRepositoriesResponse,
    GitHubSelectRepositoryRequest,
)
from services.db.project_errors import ProjectSettingsPersistError
from services.github_integration_service import (
    analyze_pull_request,
    get_pull_request_files,
    list_branches,
    list_pull_requests,
)
from services.github_repository_service import GitHubAPIError
from services.project_github_settings_service import (
    connect_project_github_app,
    get_install_url,
    get_project_github_status,
    list_authorized_repositories,
    select_project_repository,
)

logger = logging.getLogger("vanya.github_project_routes")

router = APIRouter(prefix="/projects", tags=["github-integration"])


def _http_error(exc: Exception) -> HTTPException:
    if isinstance(exc, LookupError):
        return HTTPException(status_code=404, detail=str(exc))
    if isinstance(exc, ProjectSettingsPersistError):
        return HTTPException(status_code=503, detail=str(exc))
    if isinstance(exc, GitHubAPIError):
        return HTTPException(status_code=exc.status_code, detail=str(exc))
    if isinstance(exc, ValueError):
        return HTTPException(status_code=400, detail=str(exc))
    logger.exception("github integration error")
    return HTTPException(status_code=500, detail="GitHub integration failed")


@router.get("/{project_id}/github/install-url", response_model=GitHubInstallUrlResponse)
def github_install_url(project_id: str):
    """Return GitHub App installation URL (user installs app on org/account)."""
    try:
        return get_install_url(project_id)
    except Exception as exc:
        raise _http_error(exc) from None


@router.post("/{project_id}/github/connect-app", response_model=GitHubConnectionStatus)
def github_connect_app(project_id: str, req: GitHubConnectAppRequest):
    """Link a GitHub App installation to a project. No tokens are stored or returned."""
    try:
        return connect_project_github_app(project_id, req)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/github/repositories", response_model=GitHubRepositoriesResponse)
def github_repositories(project_id: str):
    """List repositories authorized for the project's GitHub App installation."""
    try:
        return list_authorized_repositories(project_id)
    except Exception as exc:
        raise _http_error(exc) from None


@router.post("/{project_id}/github/select-repository", response_model=GitHubConnectionStatus)
def github_select_repository(project_id: str, req: GitHubSelectRepositoryRequest):
    """Select and validate the repository for this project."""
    try:
        return select_project_repository(project_id, req)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/github/status", response_model=GitHubConnectionStatus)
def github_status(project_id: str, validate: bool = Query(default=True)):
    """Connection status; optional live validation against GitHub API."""
    try:
        return get_project_github_status(project_id, validate=validate)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/github/branches", response_model=GitHubBranchesResponse)
def github_branches(project_id: str, limit: int = Query(default=30, ge=1, le=100)):
    try:
        return list_branches(project_id, limit=limit)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/github/pull-requests", response_model=GitHubPullRequestsResponse)
def github_pull_requests(project_id: str, limit: int = Query(default=20, ge=1, le=50)):
    try:
        return list_pull_requests(project_id, limit=limit)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/github/pull-requests/{number}/files", response_model=GitHubPRFilesResponse)
def github_pr_files(project_id: str, number: int):
    try:
        return get_pull_request_files(project_id, number)
    except Exception as exc:
        raise _http_error(exc) from None


@router.post("/{project_id}/github/pull-requests/{number}/analyze", response_model=GitHubPRAnalyzeResponse)
def github_pr_analyze(project_id: str, number: int):
    """Fetch PR changed_files from GitHub and run PR Analysis v1."""
    try:
        return analyze_pull_request(project_id, number)
    except Exception as exc:
        raise _http_error(exc) from None
