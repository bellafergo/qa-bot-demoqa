# services/project_github_settings_service.py
"""
Project-scoped GitHub settings — SaaS GitHub App (no PAT persistence).

Persisted per project in ``project.settings.github``:
  provider, installation_id, owner, repo, default_branch, permissions, connected_by, timestamps

Installation access tokens are generated server-side and cached in memory only.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from models.github_integration_models import (
    GitHubConnectAppRequest,
    GitHubConnectionStatus,
    GitHubInstallUrlResponse,
    GitHubRepositoriesResponse,
    GitHubRepositorySummary,
    GitHubSelectRepositoryRequest,
)
from services.github_app_service import (
    build_app_install_url,
    get_installation,
    http_config_for_installation,
    is_github_app_configured,
    list_installation_repositories,
    validate_installation_permissions,
)
from services.github_project_context import GitHubHttpConfig
from services.github_repository_service import GitHubAPIError, GitHubRepositoryClient
from services.db.project_errors import ProjectSettingsPersistError
from services.project_settings_service import merge_settings

logger = logging.getLogger("vanya.project_github_settings")

PROVIDER_APP = "github_app"
PROVIDER_LEGACY = "legacy_pat"


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _github_dict(project: Any) -> Dict[str, Any]:
    st = getattr(project, "settings", None) or {}
    if not isinstance(st, dict):
        return {}
    gh = st.get("github")
    return dict(gh) if isinstance(gh, dict) else {}


def _provider(gh: Dict[str, Any]) -> str:
    p = str(gh.get("provider") or "").strip().lower()
    if p == PROVIDER_APP:
        return PROVIDER_APP
    if gh.get("github_token"):
        return PROVIDER_LEGACY
    return "none"


def _is_legacy_pat(gh: Dict[str, Any]) -> bool:
    return _provider(gh) == PROVIDER_LEGACY or bool(str(gh.get("github_token") or "").strip())


def build_connection_status(
    project_id: str,
    *,
    gh: Optional[Dict[str, Any]] = None,
    validation_ok: bool = False,
    validation_message: str = "",
    rate_limit_remaining: Optional[int] = None,
) -> GitHubConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if gh is None:
        p = project_repo.get_project(pid)
        gh = _github_dict(p) if p else {}

    provider = _provider(gh)
    owner = str(gh.get("owner") or "").strip()
    repo = str(gh.get("repo") or "").strip()
    iid = str(gh.get("installation_id") or gh.get("github_installation_id") or "").strip()
    enabled = bool(gh.get("enabled"))
    legacy = _is_legacy_pat(gh)

    connected = (
        enabled
        and provider == PROVIDER_APP
        and bool(iid and owner and repo)
        and not legacy
    )

    perms = gh.get("permissions") if isinstance(gh.get("permissions"), dict) else {}

    return GitHubConnectionStatus(
        project_id=pid,
        provider=provider if provider in (PROVIDER_APP, PROVIDER_LEGACY) else "none",
        connected=connected,
        enabled=enabled and provider == PROVIDER_APP,
        installation_id=iid,
        owner=owner,
        repo=repo,
        full_name=f"{owner}/{repo}" if owner and repo else "",
        default_branch=str(gh.get("default_branch") or "main").strip() or "main",
        repo_url=str(gh.get("repo_url") or "").strip() or (f"https://github.com/{owner}/{repo}" if owner and repo else ""),
        connected_by=str(gh.get("connected_by") or "").strip(),
        connected_at=str(gh.get("connected_at") or ""),
        repo_selected_at=str(gh.get("repo_selected_at") or ""),
        last_validated_at=str(gh.get("last_validated_at") or ""),
        permissions={str(k): str(v) for k, v in perms.items()},
        validation_ok=validation_ok,
        validation_message=validation_message,
        rate_limit_remaining=rate_limit_remaining,
        needs_migration=legacy,
        app_configured=is_github_app_configured(),
    )


def _assert_github_field_persisted(
    project_id: str,
    updated: Any,
    *,
    field: str,
    expected: str,
    context: str,
) -> None:
    """Raise if Supabase/SQLite read-back lacks a github settings field we just wrote."""
    gh = _github_dict(updated)
    got = str(gh.get(field) or "").strip()
    want = str(expected or "").strip()
    if got == want:
        return
    raise ProjectSettingsPersistError(
        f"GitHub {context} for project '{project_id}' was not persisted "
        f"(expected github.{field}={want!r}, got {got!r}). "
        "If using Supabase, ensure public.projects.settings_json exists and run: "
        "NOTIFY pgrst, 'reload schema';"
    )


def get_install_url(project_id: str) -> GitHubInstallUrlResponse:
    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")
    if not is_github_app_configured():
        raise ValueError("GitHub App is not configured on the server")
    from core.settings import settings

    state = pid
    url = build_app_install_url(state=state)
    hint = (
        f"After installation, complete setup via POST /projects/{pid}/github/connect-app "
        f"with installation_id, or configure GITHUB_APP_SETUP_URL callback."
    )
    if settings.GITHUB_APP_SETUP_URL:
        hint = f"GitHub will redirect to {settings.GITHUB_APP_SETUP_URL}?installation_id=...&state={pid}"
    return GitHubInstallUrlResponse(install_url=url, state=state, setup_callback_hint=hint)


def connect_project_github_app(project_id: str, req: GitHubConnectAppRequest) -> GitHubConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")
    if not is_github_app_configured():
        raise ValueError("GitHub App is not configured on the server")

    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    iid = str(req.installation_id or "").strip()
    if not iid:
        raise ValueError("installation_id is required")

    installation = get_installation(iid)
    perms = validate_installation_permissions(installation)
    account = installation.get("account") if isinstance(installation.get("account"), dict) else {}
    account_login = str(account.get("login") or "")

    now = _utc_now_iso()
    existing = _github_dict(project)
    patch: Dict[str, Any] = {
        "enabled": True,
        "provider": PROVIDER_APP,
        "installation_id": iid,
        "permissions": perms,
        "connected_by": str(req.connected_by or existing.get("connected_by") or account_login or "").strip(),
        "connected_at": existing.get("connected_at") or now,
        "last_validated_at": now,
        # Clear legacy PAT on App connect
        "github_token": None,
    }

    merged = merge_settings(project.settings, {"github": patch})
    updated = project_repo.update_project(pid, {"settings": merged})
    if updated is None:
        raise LookupError(f"project not found: {pid}")

    _assert_github_field_persisted(pid, updated, field="installation_id", expected=iid, context="connect-app")

    logger.info("github_app connected project_id=%s installation_id=%s", pid, iid)
    return build_connection_status(
        pid,
        gh=_github_dict(updated),
        validation_ok=True,
        validation_message="GitHub App installation linked. Select a repository to finish setup.",
    )


def list_authorized_repositories(project_id: str) -> GitHubRepositoriesResponse:
    pid = (project_id or "").strip().lower()
    from services.db.project_repository import project_repo

    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    gh = _github_dict(project)
    if _is_legacy_pat(gh):
        raise ValueError("Legacy PAT configuration must be migrated to GitHub App first.")
    iid = str(gh.get("installation_id") or "").strip()
    if not iid:
        raise ValueError("GitHub App is not connected. Use connect-app first.")

    raw = list_installation_repositories(iid)
    repos: List[GitHubRepositorySummary] = []
    for r in raw:
        owner_obj = r.get("owner") if isinstance(r.get("owner"), dict) else {}
        owner = str(owner_obj.get("login") or "")
        name = str(r.get("name") or "")
        repos.append(GitHubRepositorySummary(
            id=int(r.get("id") or 0),
            owner=owner,
            repo=name,
            full_name=str(r.get("full_name") or f"{owner}/{name}"),
            default_branch=str(r.get("default_branch") or "main"),
            private=bool(r.get("private")),
            html_url=str(r.get("html_url") or ""),
        ))
    repos.sort(key=lambda x: x.full_name.lower())
    return GitHubRepositoriesResponse(installation_id=iid, repositories=repos)


def select_project_repository(project_id: str, req: GitHubSelectRepositoryRequest) -> GitHubConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    gh = _github_dict(project)
    iid = str(gh.get("installation_id") or "").strip()
    if not iid:
        raise ValueError("GitHub App installation is not linked to this project")

    owner = (req.owner or "").strip()
    repo = (req.repo or "").strip()
    if not owner or not repo:
        raise ValueError("owner and repo are required")

    authorized = list_authorized_repositories(pid)
    allowed = {(r.owner.lower(), r.repo.lower()) for r in authorized.repositories}
    if (owner.lower(), repo.lower()) not in allowed:
        raise ValueError(f"Repository {owner}/{repo} is not authorized for this installation")

    default_branch = (req.default_branch or "").strip()
    if not default_branch:
        for r in authorized.repositories:
            if r.owner.lower() == owner.lower() and r.repo.lower() == repo.lower():
                default_branch = r.default_branch or "main"
                break

    http = http_config_for_installation(iid)
    client = GitHubRepositoryClient(http, owner=owner, repo=repo)
    _meta, remaining = client.validate_connection()

    now = _utc_now_iso()
    patch = {
        "owner": owner,
        "repo": repo,
        "default_branch": default_branch or str(_meta.get("default_branch") or "main"),
        "repo_url": f"https://github.com/{owner}/{repo}",
        "repo_selected_at": now,
        "last_validated_at": now,
        "enabled": True,
        "provider": PROVIDER_APP,
    }
    merged = merge_settings(project.settings, {"github": patch})
    updated = project_repo.update_project(pid, {"settings": merged})
    if updated is None:
        raise LookupError(f"project not found: {pid}")

    _assert_github_field_persisted(pid, updated, field="owner", expected=owner, context="select-repository")
    _assert_github_field_persisted(pid, updated, field="repo", expected=repo, context="select-repository")

    logger.info("github_app repo selected project_id=%s repo=%s/%s", pid, owner, repo)
    return build_connection_status(
        pid,
        gh=_github_dict(updated),
        validation_ok=True,
        validation_message="Repository connected and validated.",
        rate_limit_remaining=remaining,
    )


def resolve_http_for_project(project_id: str) -> Tuple[Dict[str, Any], GitHubHttpConfig, str, str]:
    """Return (github_settings, http_config, owner, repo) for API calls."""
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    gh = _github_dict(project)
    if _is_legacy_pat(gh):
        raise ValueError(
            "Legacy PAT configuration detected. Reconnect using GitHub App "
            "(POST /projects/{id}/github/connect-app)."
        )
    if _provider(gh) != PROVIDER_APP:
        raise ValueError("GitHub App is not connected for this project")

    iid = str(gh.get("installation_id") or "").strip()
    owner = str(gh.get("owner") or "").strip()
    repo = str(gh.get("repo") or "").strip()
    if not iid:
        raise ValueError("installation_id is missing — connect GitHub App first")
    if not owner or not repo:
        raise ValueError("Repository not selected — use select-repository first")

    http = http_config_for_installation(iid)
    return gh, http, owner, repo


def _client_for_project(project_id: str) -> Tuple[Dict[str, Any], GitHubRepositoryClient]:
    gh, http, owner, repo = resolve_http_for_project(project_id)
    return gh, GitHubRepositoryClient(http, owner=owner, repo=repo)


def get_project_github_status(project_id: str, *, validate: bool = True) -> GitHubConnectionStatus:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    gh = _github_dict(project)
    if _is_legacy_pat(gh):
        return build_connection_status(
            pid,
            gh=gh,
            validation_ok=False,
            validation_message="Legacy PAT detected. Migrate to GitHub App to continue.",
        )

    if _provider(gh) != PROVIDER_APP:
        return build_connection_status(
            pid,
            gh=gh,
            validation_message="GitHub App is not connected for this project.",
        )

    if not validate:
        return build_connection_status(pid, gh=gh)

    try:
        if not str(gh.get("owner") or "").strip():
            return build_connection_status(
                pid,
                gh=gh,
                validation_ok=True,
                validation_message="Installation linked. Select a repository to complete setup.",
            )
        _, client = _client_for_project(pid)
        _meta, remaining = client.validate_connection()
        now = _utc_now_iso()
        merged = merge_settings(project.settings, {"github": {"last_validated_at": now}})
        project_repo.update_project(pid, {"settings": merged})
        gh = dict(gh)
        gh["last_validated_at"] = now
        return build_connection_status(
            pid,
            gh=gh,
            validation_ok=True,
            validation_message="Connection is valid.",
            rate_limit_remaining=remaining,
        )
    except (ValueError, GitHubAPIError) as e:
        return build_connection_status(pid, gh=gh, validation_ok=False, validation_message=str(e))


def disconnect_project_github(project_id: str) -> GitHubConnectionStatus:
    """
    Remove GitHub App configuration from a project.

    Does not uninstall the GitHub App on GitHub — only clears ``project.settings.github``.
    """
    from services.db.project_repository import project_repo
    from services.github_installation_token_cache import installation_token_cache

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")

    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    gh = _github_dict(project)
    iid = str(gh.get("installation_id") or gh.get("github_installation_id") or "").strip()
    if iid:
        installation_token_cache.invalidate(iid)

    merged = dict(project.settings or {})
    merged.pop("github", None)
    updated = project_repo.update_project(pid, {"settings": merged})
    if updated is None:
        raise LookupError(f"project not found: {pid}")

    logger.info("github_app disconnected project_id=%s", pid)
    return build_connection_status(
        pid,
        gh={},
        validation_message="GitHub is not connected for this project.",
    )
