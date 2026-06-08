# services/github_app_service.py
"""
GitHub App authentication — JWT app auth + installation access tokens.

No PAT persistence. Installation tokens are cached in memory with TTL.
"""
from __future__ import annotations

import logging
import time
from typing import Any, Dict, List, Optional, Tuple

import httpx
import jwt

from core.settings import settings
from services.github_installation_token_cache import installation_token_cache
from services.github_project_context import GitHubHttpConfig
from services.github_repository_service import GitHubAPIError

logger = logging.getLogger("vanya.github_app")

_REQUIRED_REPO_PERMISSIONS = ("contents", "metadata", "pull_requests")


def is_github_app_configured() -> bool:
    return bool(_app_id() and _private_key())


def _app_id() -> str:
    return str(getattr(settings, "GITHUB_APP_ID", None) or "").strip()


def _private_key() -> str:
    raw = str(getattr(settings, "GITHUB_APP_PRIVATE_KEY", None) or "").strip()
    if raw:
        return raw.replace("\\n", "\n")
    path = str(getattr(settings, "GITHUB_APP_PRIVATE_KEY_PATH", None) or "").strip()
    if path:
        try:
            with open(path, "r", encoding="utf-8") as f:
                return f.read()
        except OSError as e:
            logger.warning("github_app: could not read private key path")
            raise ValueError("GitHub App private key is not readable") from e
    return ""


def _api_base() -> str:
    return (settings.GITHUB_API_BASE or "https://api.github.com").strip().rstrip("/")


def _timeout() -> int:
    return max(5, int(settings.GITHUB_HTTP_TIMEOUT_S or 30))


def build_app_install_url(*, state: str = "") -> str:
    """URL where the user installs the GitHub App on their org/account."""
    slug = str(getattr(settings, "GITHUB_APP_SLUG", None) or "").strip()
    if not slug:
        raise ValueError("GITHUB_APP_SLUG is not configured")
    base = f"https://github.com/apps/{slug}/installations/new"
    st = (state or "").strip()
    return f"{base}?state={st}" if st else base


def create_app_jwt() -> str:
    """Create a short-lived JWT for GitHub App authentication."""
    app_id = _app_id()
    key = _private_key()
    if not app_id or not key:
        raise ValueError("GitHub App is not configured (GITHUB_APP_ID + private key required)")

    now = int(time.time())
    payload = {
        "iat": now - 60,
        "exp": now + 600,
        "iss": app_id,
    }
    return jwt.encode(payload, key, algorithm="RS256")


def _app_headers() -> Dict[str, str]:
    return {
        "Authorization": f"Bearer {create_app_jwt()}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }


def _installation_headers(token: str) -> Dict[str, str]:
    return {
        "Authorization": f"Bearer {token}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }


def _raise_github(res: httpx.Response, *, context: str) -> None:
    sc = res.status_code
    if sc == 401:
        raise GitHubAPIError("GitHub App authentication failed.", status_code=401, code="invalid_app_auth")
    if sc == 404:
        raise GitHubAPIError(f"GitHub resource not found ({context}).", status_code=404, code="not_found")
    if sc == 403:
        if res.headers.get("X-RateLimit-Remaining") == "0":
            raise GitHubAPIError("GitHub API rate limit exceeded.", status_code=429, code="rate_limit")
        raise GitHubAPIError("GitHub API access forbidden.", status_code=403, code="forbidden")
    if sc >= 500:
        raise GitHubAPIError("GitHub is temporarily unavailable.", status_code=502, code="github_unavailable")
    if not res.is_success:
        raise GitHubAPIError(f"GitHub API error ({sc}) for {context}.", status_code=502, code="github_error")


def get_installation_access_token(installation_id: str) -> Tuple[str, str]:
    """
    Return (token, expires_at_iso). Uses in-memory cache when valid.
    """
    iid = str(installation_id or "").strip()
    if not iid:
        raise ValueError("installation_id is required")

    cached = installation_token_cache.get(iid)
    if cached:
        return cached, ""

    url = f"{_api_base()}/app/installations/{iid}/access_tokens"
    try:
        with httpx.Client(timeout=_timeout()) as client:
            res = client.post(url, headers=_app_headers())
    except httpx.TimeoutException as e:
        raise GitHubAPIError("GitHub API request timed out.", status_code=504, code="timeout") from e
    except httpx.RequestError as e:
        raise GitHubAPIError("Could not reach GitHub API.", status_code=502, code="network_error") from e

    _raise_github(res, context=f"installation {iid} access token")
    data = res.json() if res.content else {}
    token = str(data.get("token") or "").strip()
    expires_at = str(data.get("expires_at") or "")
    if not token:
        raise GitHubAPIError("GitHub returned an empty installation token.", status_code=502, code="empty_token")

    installation_token_cache.set(iid, token, expires_at)
    logger.info("github_app: refreshed installation token installation_id=%s", iid)
    return token, expires_at


def http_config_for_installation(installation_id: str) -> GitHubHttpConfig:
    token, _expires = get_installation_access_token(installation_id)
    return GitHubHttpConfig(token=token, api_base=_api_base(), timeout_s=_timeout())


def get_installation(installation_id: str) -> Dict[str, Any]:
    """Fetch installation metadata (permissions, account)."""
    iid = str(installation_id or "").strip()
    url = f"{_api_base()}/app/installations/{iid}"
    with httpx.Client(timeout=_timeout()) as client:
        res = client.get(url, headers=_app_headers())
    _raise_github(res, context=f"installation {iid}")
    data = res.json()
    return data if isinstance(data, dict) else {}


def list_installation_repositories(installation_id: str, *, per_page: int = 100) -> List[Dict[str, Any]]:
    """List repositories accessible to the installation."""
    token, _ = get_installation_access_token(installation_id)
    url = f"{_api_base()}/installation/repositories"
    out: List[Dict[str, Any]] = []
    page = 1
    while page <= 10:
        with httpx.Client(timeout=_timeout()) as client:
            res = client.get(
                url,
                headers=_installation_headers(token),
                params={"per_page": min(100, per_page), "page": page},
            )
        _raise_github(res, context="installation repositories")
        data = res.json() if res.content else {}
        repos = data.get("repositories") if isinstance(data, dict) else []
        if not isinstance(repos, list) or not repos:
            break
        out.extend([r for r in repos if isinstance(r, dict)])
        if len(repos) < min(100, per_page):
            break
        page += 1
    return out


def validate_installation_permissions(installation: Dict[str, Any]) -> Dict[str, str]:
    perms = installation.get("permissions") if isinstance(installation.get("permissions"), dict) else {}
    out = {k: str(v) for k, v in perms.items() if v}
    missing = [p for p in _REQUIRED_REPO_PERMISSIONS if p not in out]
    if missing:
        logger.info("github_app: installation missing permissions: %s", ",".join(missing))
    return out
