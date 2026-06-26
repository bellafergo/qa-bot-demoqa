# services/project_knowledge_refresh_utils.py
"""Helpers for System Memory refresh — GitHub repo indexing defaults."""
from __future__ import annotations

from typing import Any, Optional

from models.project_knowledge_models import ProjectKnowledgeRefreshRequest


def project_has_github_repository(project_id: str) -> bool:
    """True when GitHub App is linked and a repository is selected for the project."""
    pid = (project_id or "").strip()
    if not pid:
        return False
    try:
        from services.db.project_repository import project_repo

        project = project_repo.get_project(pid)
        if project is None:
            return False
        settings = getattr(project, "settings", None) or {}
        if not isinstance(settings, dict):
            return False
        gh = settings.get("github") or {}
        if not isinstance(gh, dict):
            return False
        installation_id = str(gh.get("installation_id") or "").strip()
        owner = str(gh.get("owner") or "").strip()
        repo = str(gh.get("repo") or "").strip()
        return bool(installation_id and owner and repo)
    except Exception:
        return False


def resolve_refresh_request(
    project_id: str,
    req: Optional[ProjectKnowledgeRefreshRequest],
    *,
    include_repository_explicit: Optional[bool] = None,
) -> ProjectKnowledgeRefreshRequest:
    """
    Apply refresh defaults.

    When ``include_repository_explicit`` is None and the project has GitHub + repo,
    enable repository indexing automatically.
    """
    opts = req or ProjectKnowledgeRefreshRequest()
    if include_repository_explicit is not None:
        if include_repository_explicit == opts.include_repository:
            return opts
        return opts.model_copy(update={"include_repository": include_repository_explicit})
    if opts.include_repository:
        return opts
    if project_has_github_repository(project_id):
        return opts.model_copy(update={"include_repository": True})
    return opts


def github_app_configuration_diagnostic() -> dict[str, Any]:
    """Return whether GitHub App server config is present and which env vars are missing."""
    try:
        from services.github_app_service import is_github_app_configured
        from core.settings import settings

        missing: list[str] = []
        if not str(getattr(settings, "GITHUB_APP_ID", "") or "").strip():
            missing.append("GITHUB_APP_ID")
        has_key = bool(str(getattr(settings, "GITHUB_APP_PRIVATE_KEY", "") or "").strip())
        has_path = bool(str(getattr(settings, "GITHUB_APP_PRIVATE_KEY_PATH", "") or "").strip())
        if not has_key and not has_path:
            missing.append("GITHUB_APP_PRIVATE_KEY or GITHUB_APP_PRIVATE_KEY_PATH")
        return {
            "configured": is_github_app_configured(),
            "missing_env": missing,
            "used_by": "services/repository_knowledge_service.index_repository_knowledge",
            "validate": (
                "Set GITHUB_APP_ID and GITHUB_APP_PRIVATE_KEY (or GITHUB_APP_PRIVATE_KEY_PATH) "
                "on the API worker, then POST /projects/{id}/knowledge/refresh?include_repository=true."
            ),
        }
    except Exception as exc:
        return {
            "configured": False,
            "missing_env": ["GITHUB_APP_ID", "GITHUB_APP_PRIVATE_KEY"],
            "used_by": "services/repository_knowledge_service.index_repository_knowledge",
            "validate": str(exc),
        }
