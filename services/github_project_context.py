# services/github_project_context.py
"""
Resolve GitHub API credentials and defaults **per catalog project**.

SaaS (preferred): GitHub App installation tokens via ``project.settings.github.installation_id``.
Legacy: per-project PAT or global ``GITHUB_TOKEN`` (deprecated — migrate to GitHub App).

Callers must not read ``os.getenv("GITHUB_OWNER")`` etc.; use the helpers here.
"""
from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Any, Dict, Optional, Tuple

logger = logging.getLogger("vanya.github_project")


@dataclass(frozen=True)
class GitHubHttpConfig:
    """Minimal data needed to call api.github.com (never log ``token``)."""

    token: str
    api_base: str = "https://api.github.com"
    timeout_s: int = 30


@dataclass(frozen=True)
class GitHubProjectFetchContext:
    """Resolved GitHub client + optional binding to a single owner/repo."""

    http: GitHubHttpConfig
    configured_owner: Optional[str] = None
    configured_repo: Optional[str] = None


def _raw_github_dict(project: Any) -> Dict[str, Any]:
    st = getattr(project, "settings", None) or {}
    if not isinstance(st, dict):
        return {}
    gh = st.get("github")
    return gh if isinstance(gh, dict) else {}


def _provider(gh: Dict[str, Any]) -> str:
    p = str(gh.get("provider") or "").strip().lower()
    if p == "github_app":
        return "github_app"
    if gh.get("github_token"):
        return "legacy_pat"
    return "none"


def _http_from_github_dict(gh: Dict[str, Any], *, require_token: bool = True) -> Optional[GitHubHttpConfig]:
    """Legacy PAT resolution from project settings + global fallback."""
    from core.settings import settings

    ptok = str(gh.get("github_token") or "").strip()
    tok = ptok or (settings.GITHUB_TOKEN or "").strip()
    if require_token and not tok:
        return None
    base = str(gh.get("api_base") or settings.GITHUB_API_BASE or "https://api.github.com").strip().rstrip("/")
    try:
        timeout_s = int(gh.get("http_timeout_s") or settings.GITHUB_HTTP_TIMEOUT_S or 30)
    except Exception:
        timeout_s = 30
    return GitHubHttpConfig(token=tok, api_base=base or "https://api.github.com", timeout_s=max(5, timeout_s))


def resolve_legacy_github_http() -> Optional[GitHubHttpConfig]:
    """
    **Legacy / compatibility** — global PAT from environment (via ``core.settings``).

    Deprecated for SaaS — use GitHub App installation tokens per project.
    """
    from core.settings import settings

    tok = (settings.GITHUB_TOKEN or "").strip()
    if not tok:
        return None
    base = (settings.GITHUB_API_BASE or "https://api.github.com").strip().rstrip("/")
    return GitHubHttpConfig(
        token=tok,
        api_base=base or "https://api.github.com",
        timeout_s=max(5, int(settings.GITHUB_HTTP_TIMEOUT_S or 30)),
    )


def _http_for_project_github_dict(gh: Dict[str, Any]) -> Optional[GitHubHttpConfig]:
    """Resolve HTTP config from a project's github settings block."""
    if not gh.get("enabled"):
        return None

    provider = _provider(gh)
    if provider == "github_app":
        iid = str(gh.get("installation_id") or gh.get("github_installation_id") or "").strip()
        if not iid:
            return None
        try:
            from services.github_app_service import http_config_for_installation

            return http_config_for_installation(iid)
        except Exception:
            logger.exception("github: installation token resolution failed")
            return None

    return _http_from_github_dict(gh, require_token=True)


def resolve_github_http_for_repository(owner: str, repo: str) -> Optional[GitHubHttpConfig]:
    """
    Pick HTTP credentials for a GitHub ``owner/repo`` (webhooks, PR comments).

    1) Project with matching owner/repo (GitHub App or legacy PAT).
    2) Else legacy global ``GITHUB_TOKEN`` from settings.
    """
    o = (owner or "").strip().lower()
    r = (repo or "").strip().lower()
    if not o or not r:
        return resolve_legacy_github_http()

    try:
        from services.db.project_repository import project_repo

        for p in project_repo.list_projects():
            gh = _raw_github_dict(p)
            if not gh.get("enabled"):
                continue
            po = str(gh.get("owner") or "").strip().lower()
            pr = str(gh.get("repo") or "").strip().lower()
            if not po or not pr:
                continue
            if po == o and pr == r:
                cfg = _http_for_project_github_dict(gh)
                if cfg:
                    logger.info("github: using project-scoped config project_id=%s repo=%s/%s", p.id, o, r)
                    return cfg
                logger.warning(
                    "github: project %s matches repo but credentials are not configured",
                    p.id,
                )
    except Exception:
        logger.exception("github: resolve_github_http_for_repository failed")

    return resolve_legacy_github_http()


def get_github_context_for_project(project_id: str) -> Tuple[Dict[str, Any], GitHubHttpConfig]:
    """
    Return ``(github_settings_dict, http_config)`` for a project.

    Raises:
        LookupError — project not found
        ValueError — GitHub disabled or incomplete credentials
    """
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")

    p = project_repo.get_project(pid)
    if p is None:
        raise LookupError(f"project not found: {pid}")

    gh = _raw_github_dict(p)
    if not gh.get("enabled"):
        raise ValueError("GitHub is not enabled for this project")

    provider = _provider(gh)
    if provider == "github_app":
        from services.project_github_settings_service import resolve_http_for_project

        gh2, http, _owner, _repo = resolve_http_for_project(pid)
        return gh2, http

    cfg = _http_from_github_dict(gh, require_token=True)
    if cfg is None or not cfg.token:
        raise ValueError(
            "No GitHub credentials available. Connect GitHub App for this project "
            "or migrate from legacy PAT."
        )
    return gh, cfg


def resolve_github_for_pr_fetch(
    project_id: str,
    *,
    pr_owner: str,
    pr_repo: str,
) -> GitHubProjectFetchContext:
    """
    Resolve GitHub HTTP config for ``POST /github/pr/fetch`` scoped to ``project_id``.

    If the project defines ``owner`` and ``repo``, the PR URL must match (prevents
    using one project's credentials against another org's PR).
    """
    gh, http = get_github_context_for_project(project_id)
    co = str(gh.get("owner") or "").strip()
    cr = str(gh.get("repo") or "").strip()
    if co and cr:
        if co.lower() != (pr_owner or "").strip().lower() or cr.lower() != (pr_repo or "").strip().lower():
            raise ValueError(
                f"PR repository {pr_owner}/{pr_repo} does not match this project's GitHub repo ({co}/{cr})"
            )
    return GitHubProjectFetchContext(http=http, configured_owner=co or None, configured_repo=cr or None)
