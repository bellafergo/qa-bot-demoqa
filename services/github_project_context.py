# services/github_project_context.py
"""
Resolve GitHub API credentials and defaults **per catalog project**.

Architecture
------------
- Project-specific repo metadata and optional PAT live under ``project.settings["github"]``.
- Global ``GITHUB_TOKEN`` / ``GITHUB_API_BASE`` (``core.settings``) remain the **legacy /
  default** credential when a project has no PAT, and for webhooks when no project
  matches ``owner/repo``.

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


def _http_from_github_dict(gh: Dict[str, Any], *, require_token: bool = True) -> Optional[GitHubHttpConfig]:
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

    Prefer ``resolve_github_for_pr_fetch`` with a ``project_id`` for new integrations.
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


def resolve_github_http_for_repository(owner: str, repo: str) -> Optional[GitHubHttpConfig]:
    """
    Pick HTTP credentials for a GitHub ``owner/repo`` (webhooks, PR comments).

    1) Project with ``settings.github.enabled`` and matching ``owner`` + ``repo``.
    2) Else legacy global ``GITHUB_TOKEN`` from settings.
    """
    from core.settings import settings

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
                cfg = _http_from_github_dict(gh, require_token=True)
                if cfg:
                    logger.info("github: using project-scoped config project_id=%s repo=%s/%s", p.id, o, r)
                    return cfg
                logger.warning(
                    "github: project %s matches repo but has no token (set github_token or GITHUB_TOKEN)",
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
        ValueError — GitHub disabled or incomplete / no token
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

    cfg = _http_from_github_dict(gh, require_token=True)
    if cfg is None or not cfg.token:
        raise ValueError(
            "No GitHub token available: set project settings.github.github_token "
            "or configure legacy GITHUB_TOKEN on the server"
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
    using one project's token against another org's PR).
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
