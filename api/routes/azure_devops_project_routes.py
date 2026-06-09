# api/routes/azure_devops_project_routes.py
"""Azure DevOps Integration — OAuth (project-scoped)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import RedirectResponse

from core.settings import settings
from models.azure_devops_integration_models import (
    AzureDevOpsAuthorizeUrlResponse,
    AzureDevOpsConnectionStatus,
    AzureDevOpsOrganizationsResponse,
    AzureDevOpsPRAnalyzeResponse,
    AzureDevOpsPRFilesResponse,
    AzureDevOpsProjectsResponse,
    AzureDevOpsPullRequestsResponse,
    AzureDevOpsRepositoriesResponse,
    AzureDevOpsSelectTargetRequest,
)
from services.azure_devops_integration_service import (
    analyze_pull_request,
    get_pull_request_files,
    list_pull_requests,
)
from services.azure_devops_repository_service import AzureDevOpsAPIError
from services.db.project_errors import ProjectSettingsPersistError
from services.project_azure_devops_settings_service import (
    complete_oauth_callback,
    disconnect_project_azure_devops,
    get_authorize_url,
    get_project_azure_devops_status,
    list_organizations,
    list_projects,
    list_repositories,
    select_target,
)

logger = logging.getLogger("vanya.azure_devops_project_routes")

router = APIRouter(prefix="/projects", tags=["azure-devops-integration"])


def _http_error(exc: Exception) -> HTTPException:
    if isinstance(exc, LookupError):
        return HTTPException(status_code=404, detail=str(exc))
    if isinstance(exc, ProjectSettingsPersistError):
        return HTTPException(status_code=503, detail=str(exc))
    if isinstance(exc, AzureDevOpsAPIError):
        return HTTPException(status_code=exc.status_code, detail=str(exc))
    if isinstance(exc, ValueError):
        return HTTPException(status_code=400, detail=str(exc))
    logger.exception("azure devops integration error")
    return HTTPException(status_code=500, detail="Azure DevOps integration failed")


def _frontend_integrations_url(*, project_id: str, ok: bool, message: str = "") -> str:
    base = (settings.AZURE_OAUTH_SUCCESS_URL or settings.CORS_ORIGINS[0] if settings.CORS_ORIGINS else "").rstrip("/")
    if not base:
        base = "https://zuperio-vanya.vercel.app"
    q_ok = "1" if ok else "0"
    msg = message.replace(" ", "+")[:200] if message else ""
    return f"{base}/integrations?azure_oauth={q_ok}&project_id={project_id}&msg={msg}"


@router.get("/{project_id}/azure-devops/authorize-url", response_model=AzureDevOpsAuthorizeUrlResponse)
def azure_devops_authorize_url(project_id: str):
    """Return Microsoft OAuth authorize URL (state=project_id)."""
    try:
        return get_authorize_url(project_id)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/azure-devops/callback")
def azure_devops_project_callback(
    project_id: str,
    code: str = Query(default=""),
    state: str = Query(default=""),
    error: str = Query(default=""),
    error_description: str = Query(default=""),
):
    """
    Optional per-project callback alias.

    Production redirect URI should be the fixed ``/azure-devops/callback`` registered in Azure Portal.
    """
    if error:
        return RedirectResponse(
            _frontend_integrations_url(project_id=project_id, ok=False, message=error_description or error),
            status_code=302,
        )
    try:
        pid = (state or project_id or "").strip().lower()
        complete_oauth_callback(code=code, state=pid)
        return RedirectResponse(_frontend_integrations_url(project_id=pid, ok=True), status_code=302)
    except Exception as exc:
        pid = (state or project_id or "").strip().lower()
        return RedirectResponse(
            _frontend_integrations_url(project_id=pid, ok=False, message=str(exc)),
            status_code=302,
        )


@router.get("/{project_id}/azure-devops/status", response_model=AzureDevOpsConnectionStatus)
def azure_devops_status(project_id: str, validate: bool = Query(default=True)):
    try:
        return get_project_azure_devops_status(project_id, validate=validate)
    except Exception as exc:
        raise _http_error(exc) from None


@router.post("/{project_id}/azure-devops/disconnect", response_model=AzureDevOpsConnectionStatus)
def azure_devops_disconnect(project_id: str):
    try:
        return disconnect_project_azure_devops(project_id)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/azure-devops/organizations", response_model=AzureDevOpsOrganizationsResponse)
def azure_devops_organizations(project_id: str):
    try:
        return list_organizations(project_id)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/azure-devops/projects", response_model=AzureDevOpsProjectsResponse)
def azure_devops_projects(project_id: str, organization: str = Query(...)):
    try:
        return list_projects(project_id, organization)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/azure-devops/repositories", response_model=AzureDevOpsRepositoriesResponse)
def azure_devops_repositories(
    project_id: str,
    organization: str = Query(...),
    azure_project: str = Query(...),
):
    try:
        return list_repositories(project_id, organization, azure_project)
    except Exception as exc:
        raise _http_error(exc) from None


@router.post("/{project_id}/azure-devops/select-target", response_model=AzureDevOpsConnectionStatus)
def azure_devops_select_target(project_id: str, req: AzureDevOpsSelectTargetRequest):
    try:
        return select_target(project_id, req)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get("/{project_id}/azure-devops/pull-requests", response_model=AzureDevOpsPullRequestsResponse)
def azure_devops_pull_requests(project_id: str, limit: int = Query(default=20, ge=1, le=50)):
    try:
        return list_pull_requests(project_id, limit=limit)
    except Exception as exc:
        raise _http_error(exc) from None


@router.get(
    "/{project_id}/azure-devops/pull-requests/{pull_request_id}/files",
    response_model=AzureDevOpsPRFilesResponse,
)
def azure_devops_pr_files(project_id: str, pull_request_id: int):
    try:
        return get_pull_request_files(project_id, pull_request_id)
    except Exception as exc:
        raise _http_error(exc) from None


@router.post(
    "/{project_id}/azure-devops/pull-requests/{pull_request_id}/analyze",
    response_model=AzureDevOpsPRAnalyzeResponse,
)
def azure_devops_pr_analyze(project_id: str, pull_request_id: int):
    """Fetch Azure DevOps PR changed_files and run PR Analysis v1.3."""
    try:
        return analyze_pull_request(project_id, pull_request_id)
    except Exception as exc:
        raise _http_error(exc) from None
