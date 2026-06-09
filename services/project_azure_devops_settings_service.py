# services/project_azure_devops_settings_service.py
"""
Project-scoped Azure DevOps OAuth settings.

Persisted per project in ``project.settings.azure_devops``:
  provider, organization, azure_project, repository_id, repository_name,
  refresh_token (masked from API), connected_by, timestamps

Access tokens are cached in memory only.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from models.azure_devops_integration_models import (
    AzureDevOpsAuthorizeUrlResponse,
    AzureDevOpsConnectionStatus,
    AzureDevOpsOrganizationSummary,
    AzureDevOpsOrganizationsResponse,
    AzureDevOpsProjectSummary,
    AzureDevOpsProjectsResponse,
    AzureDevOpsRepositoriesResponse,
    AzureDevOpsRepositorySummary,
    AzureDevOpsSelectTargetRequest,
)
from services.azure_devops_oauth_service import (
    build_authorize_url,
    exchange_code_for_tokens,
    is_azure_devops_oauth_configured,
    refresh_access_token,
)
from services.azure_devops_repository_service import AzureDevOpsAPIError, AzureDevOpsClient
from services.azure_devops_token_cache import azure_devops_token_cache
from services.db.project_errors import ProjectSettingsPersistError
from services.project_settings_service import merge_settings

logger = logging.getLogger("vanya.project_azure_devops_settings")

PROVIDER_OAUTH = "oauth"


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _azure_dict(project: Any) -> Dict[str, Any]:
    st = getattr(project, "settings", None) or {}
    if not isinstance(st, dict):
        return {}
    az = st.get("azure_devops")
    return dict(az) if isinstance(az, dict) else {}


def _provider(az: Dict[str, Any]) -> str:
    p = str(az.get("provider") or "").strip().lower()
    if p == PROVIDER_OAUTH and str(az.get("refresh_token") or "").strip():
        return PROVIDER_OAUTH
    return "none"


def build_connection_status(
    project_id: str,
    *,
    az: Optional[Dict[str, Any]] = None,
    validation_ok: bool = False,
    validation_message: str = "",
) -> AzureDevOpsConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if az is None:
        p = project_repo.get_project(pid)
        az = _azure_dict(p) if p else {}

    provider = _provider(az)
    org = str(az.get("organization") or "").strip()
    azure_project = str(az.get("azure_project") or "").strip()
    repo_id = str(az.get("repository_id") or "").strip()
    repo_name = str(az.get("repository_name") or "").strip()
    enabled = bool(az.get("enabled"))

    connected = (
        enabled
        and provider == PROVIDER_OAUTH
        and bool(org and azure_project and repo_id)
    )

    full_name = ""
    if org and azure_project and repo_name:
        full_name = f"{org}/{azure_project}/{repo_name}"
    elif org and azure_project:
        full_name = f"{org}/{azure_project}"

    repo_url = str(az.get("repo_url") or "").strip()
    if not repo_url and org and azure_project and repo_name:
        base = f"https://dev.azure.com/{org}/{azure_project}/_git/{repo_name}"
        repo_url = base

    return AzureDevOpsConnectionStatus(
        project_id=pid,
        provider=provider if provider == PROVIDER_OAUTH else "none",
        connected=connected,
        enabled=enabled and provider == PROVIDER_OAUTH,
        organization=org,
        azure_project=azure_project,
        repository_id=repo_id,
        repository_name=repo_name,
        full_name=full_name,
        default_branch=str(az.get("default_branch") or "main").strip() or "main",
        repo_url=repo_url,
        connected_by=str(az.get("connected_by") or "").strip(),
        connected_at=str(az.get("connected_at") or ""),
        repo_selected_at=str(az.get("repo_selected_at") or ""),
        last_validated_at=str(az.get("last_validated_at") or ""),
        validation_ok=validation_ok,
        validation_message=validation_message,
        oauth_configured=is_azure_devops_oauth_configured(),
        patches_available=False,
    )


def _assert_field_persisted(
    project_id: str,
    updated: Any,
    *,
    field: str,
    expected: str,
    context: str,
) -> None:
    az = _azure_dict(updated)
    got = str(az.get(field) or "").strip()
    want = str(expected or "").strip()
    if got == want:
        return
    raise ProjectSettingsPersistError(
        f"Azure DevOps {context} for project '{project_id}' was not persisted "
        f"(expected azure_devops.{field}={want!r}, got {got!r})."
    )


def get_authorize_url(project_id: str) -> AzureDevOpsAuthorizeUrlResponse:
    from core.settings import settings

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")
    if not is_azure_devops_oauth_configured():
        raise ValueError("Azure DevOps OAuth is not configured on the server")

    state = pid
    url = build_authorize_url(state=state)
    return AzureDevOpsAuthorizeUrlResponse(
        authorize_url=url,
        state=state,
        redirect_uri=settings.AZURE_REDIRECT_URI,
    )


def complete_oauth_callback(*, code: str, state: str, connected_by: str = "") -> AzureDevOpsConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (state or "").strip().lower()
    if not pid:
        raise ValueError("state (project_id) is required")
    if not (code or "").strip():
        raise ValueError("authorization code is required")

    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    tokens = exchange_code_for_tokens(code)
    access_token = str(tokens.get("access_token") or "")
    refresh_token = str(tokens.get("refresh_token") or "")
    expires_in = int(tokens.get("expires_in") or 3600)

    if not refresh_token:
        raise ValueError("Microsoft did not return refresh_token — ensure offline_access scope")

    azure_devops_token_cache.set(pid, access_token, expires_in=expires_in)

    now = _utc_now_iso()
    existing = _azure_dict(project)
    patch: Dict[str, Any] = {
        "enabled": True,
        "provider": PROVIDER_OAUTH,
        "refresh_token": refresh_token,
        "connected_by": str(connected_by or existing.get("connected_by") or "").strip(),
        "connected_at": existing.get("connected_at") or now,
        "last_validated_at": now,
    }

    merged = merge_settings(project.settings, {"azure_devops": patch})
    updated = project_repo.update_project(pid, {"settings": merged})
    if updated is None:
        raise LookupError(f"project not found: {pid}")

    _assert_field_persisted(pid, updated, field="refresh_token", expected=refresh_token, context="oauth-callback")

    logger.info("azure_devops oauth connected project_id=%s", pid)
    return build_connection_status(
        pid,
        az=_azure_dict(updated),
        validation_ok=True,
        validation_message="OAuth connected. Select organization, project, and repository.",
    )


def _resolve_access_token(project_id: str, az: Dict[str, Any]) -> str:
    pid = (project_id or "").strip().lower()
    cached = azure_devops_token_cache.get(pid)
    if cached:
        return cached

    refresh_token = str(az.get("refresh_token") or "").strip()
    if not refresh_token:
        raise ValueError("Azure DevOps is not connected — complete OAuth first")

    tokens = refresh_access_token(refresh_token)
    access_token = str(tokens.get("access_token") or "")
    expires_in = int(tokens.get("expires_in") or 3600)
    new_refresh = str(tokens.get("refresh_token") or "").strip()

    azure_devops_token_cache.set(pid, access_token, expires_in=expires_in)

    if new_refresh and new_refresh != refresh_token:
        from services.db.project_repository import project_repo

        project = project_repo.get_project(pid)
        if project is not None:
            merged = merge_settings(project.settings, {"azure_devops": {"refresh_token": new_refresh}})
            project_repo.update_project(pid, {"settings": merged})

    return access_token


def resolve_http_for_project(project_id: str) -> Tuple[Dict[str, Any], str]:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    az = _azure_dict(project)
    if _provider(az) != PROVIDER_OAUTH:
        raise ValueError("Azure DevOps OAuth is not connected for this project")

    token = _resolve_access_token(pid, az)
    return az, token


def _client_for_project(project_id: str) -> Tuple[Dict[str, Any], AzureDevOpsClient]:
    az, token = resolve_http_for_project(project_id)
    org = str(az.get("organization") or "").strip()
    azure_project = str(az.get("azure_project") or "").strip()
    repo_id = str(az.get("repository_id") or "").strip()
    repo_name = str(az.get("repository_name") or "").strip()
    if not org:
        raise ValueError("Organization not selected — complete Azure DevOps setup")
    if not azure_project or not repo_id:
        raise ValueError("Repository not selected — use select-target first")

    client = AzureDevOpsClient(
        token,
        organization=org,
        azure_project=azure_project,
        repository_id=repo_id,
        repository_name=repo_name,
    )
    return az, client


def list_organizations(project_id: str) -> AzureDevOpsOrganizationsResponse:
    az, token = resolve_http_for_project(project_id)
    raw = AzureDevOpsClient.list_organizations(token)
    orgs: List[AzureDevOpsOrganizationSummary] = []
    for o in raw:
        orgs.append(AzureDevOpsOrganizationSummary(
            account_id=str(o.get("accountId") or o.get("id") or ""),
            account_name=str(o.get("accountName") or "").strip(),
        ))
    orgs.sort(key=lambda x: x.account_name.lower())
    return AzureDevOpsOrganizationsResponse(organizations=orgs)


def list_projects(project_id: str, organization: str) -> AzureDevOpsProjectsResponse:
    az, token = resolve_http_for_project(project_id)
    org = (organization or az.get("organization") or "").strip()
    if not org:
        raise ValueError("organization is required")

    client = AzureDevOpsClient(token, organization=org, azure_project="", repository_id="stub")
    raw = client.list_projects()
    projects: List[AzureDevOpsProjectSummary] = []
    for p in raw:
        projects.append(AzureDevOpsProjectSummary(
            id=str(p.get("id") or ""),
            name=str(p.get("name") or ""),
            description=str(p.get("description") or ""),
        ))
    projects.sort(key=lambda x: x.name.lower())
    return AzureDevOpsProjectsResponse(organization=org, projects=projects)


def list_repositories(project_id: str, organization: str, azure_project: str) -> AzureDevOpsRepositoriesResponse:
    az, token = resolve_http_for_project(project_id)
    org = (organization or az.get("organization") or "").strip()
    proj = (azure_project or az.get("azure_project") or "").strip()
    if not org or not proj:
        raise ValueError("organization and azure_project are required")

    client = AzureDevOpsClient(token, organization=org, azure_project=proj, repository_id="stub")
    raw = client.list_repositories()
    repos: List[AzureDevOpsRepositorySummary] = []
    for r in raw:
        default_branch = "main"
        proj_obj = r.get("project") if isinstance(r.get("project"), dict) else {}
        repos.append(AzureDevOpsRepositorySummary(
            id=str(r.get("id") or ""),
            name=str(r.get("name") or ""),
            default_branch=str(r.get("defaultBranch") or proj_obj.get("defaultBranch") or "main").replace("refs/heads/", ""),
            remote_url=str(r.get("remoteUrl") or r.get("webUrl") or ""),
        ))
    repos.sort(key=lambda x: x.name.lower())
    return AzureDevOpsRepositoriesResponse(organization=org, azure_project=proj, repositories=repos)


def select_target(project_id: str, req: AzureDevOpsSelectTargetRequest) -> AzureDevOpsConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    az, token = resolve_http_for_project(pid)
    org = (req.organization or "").strip()
    azure_project = (req.azure_project or "").strip()
    repo_id = (req.repository_id or "").strip()
    if not org or not azure_project or not repo_id:
        raise ValueError("organization, azure_project, and repository_id are required")

    repo_name = (req.repository_name or "").strip()
    default_branch = (req.default_branch or "").strip()

    if not repo_name:
        listed = list_repositories(pid, org, azure_project)
        for r in listed.repositories:
            if r.id == repo_id:
                repo_name = r.name
                if not default_branch:
                    default_branch = r.default_branch or "main"
                break

    client = AzureDevOpsClient(
        token,
        organization=org,
        azure_project=azure_project,
        repository_id=repo_id,
        repository_name=repo_name,
    )
    meta, _ = client.validate_connection()
    if not default_branch:
        default_branch = str(meta.get("defaultBranch") or "main").replace("refs/heads/", "")

    now = _utc_now_iso()
    patch = {
        "organization": org,
        "azure_project": azure_project,
        "repository_id": repo_id,
        "repository_name": repo_name or str(meta.get("name") or ""),
        "default_branch": default_branch or "main",
        "repo_url": str(meta.get("webUrl") or meta.get("remoteUrl") or ""),
        "repo_selected_at": now,
        "last_validated_at": now,
        "enabled": True,
        "provider": PROVIDER_OAUTH,
    }
    merged = merge_settings(project.settings, {"azure_devops": patch})
    updated = project_repo.update_project(pid, {"settings": merged})
    if updated is None:
        raise LookupError(f"project not found: {pid}")

    _assert_field_persisted(pid, updated, field="repository_id", expected=repo_id, context="select-target")

    logger.info("azure_devops target selected project_id=%s org=%s repo=%s", pid, org, repo_name)
    return build_connection_status(
        pid,
        az=_azure_dict(updated),
        validation_ok=True,
        validation_message="Repository connected and validated.",
    )


def get_project_azure_devops_status(project_id: str, *, validate: bool = True) -> AzureDevOpsConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    az = _azure_dict(project)
    if _provider(az) != PROVIDER_OAUTH:
        return build_connection_status(
            pid,
            az=az,
            validation_message="Azure DevOps OAuth is not connected for this project.",
        )

    if not validate:
        return build_connection_status(pid, az=az)

    try:
        if not str(az.get("repository_id") or "").strip():
            return build_connection_status(
                pid,
                az=az,
                validation_ok=True,
                validation_message="OAuth connected. Select organization, project, and repository.",
            )
        _, client = _client_for_project(pid)
        client.validate_connection()
        now = _utc_now_iso()
        merged = merge_settings(project.settings, {"azure_devops": {"last_validated_at": now}})
        project_repo.update_project(pid, {"settings": merged})
        az = dict(az)
        az["last_validated_at"] = now
        return build_connection_status(
            pid,
            az=az,
            validation_ok=True,
            validation_message="Connection is valid.",
        )
    except (ValueError, AzureDevOpsAPIError) as e:
        return build_connection_status(pid, az=az, validation_ok=False, validation_message=str(e))


def disconnect_project_azure_devops(project_id: str) -> AzureDevOpsConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")

    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    azure_devops_token_cache.invalidate(pid)

    merged = dict(project.settings or {})
    merged.pop("azure_devops", None)
    updated = project_repo.update_project(pid, {"settings": merged})
    if updated is None:
        raise LookupError(f"project not found: {pid}")

    logger.info("azure_devops disconnected project_id=%s", pid)
    return build_connection_status(
        pid,
        az={},
        validation_message="Azure DevOps is not connected for this project.",
    )
