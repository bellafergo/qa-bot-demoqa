# services/github_repository_service.py
"""
Low-level GitHub REST API client for project-scoped repository operations.

Never logs tokens. Error messages are sanitized for API responses.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Tuple

import httpx

from services.github_project_context import GitHubHttpConfig

logger = logging.getLogger("vanya.github_repository")


class GitHubAPIError(Exception):
    """Safe GitHub API failure — no credentials in message."""

    def __init__(self, message: str, *, status_code: int = 502, code: str = "github_error"):
        super().__init__(message)
        self.status_code = status_code
        self.code = code


def _headers(http: GitHubHttpConfig) -> Dict[str, str]:
    return {
        "Authorization": f"Bearer {http.token}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }


def _api_base(http: GitHubHttpConfig) -> str:
    return (http.api_base or "https://api.github.com").strip().rstrip("/")


def _raise_for_status(res: httpx.Response, *, context: str) -> None:
    sc = res.status_code
    if sc == 401:
        raise GitHubAPIError(
            "GitHub token is invalid or expired. Update the project token.",
            status_code=401,
            code="invalid_token",
        )
    if sc == 404:
        raise GitHubAPIError(
            f"GitHub resource not found ({context}). Check owner, repo, and token access.",
            status_code=404,
            code="not_found",
        )
    if sc == 403:
        remaining = res.headers.get("X-RateLimit-Remaining")
        if remaining == "0":
            reset = res.headers.get("X-RateLimit-Reset", "")
            raise GitHubAPIError(
                f"GitHub API rate limit exceeded. Retry after reset ({reset}).",
                status_code=429,
                code="rate_limit",
            )
        raise GitHubAPIError(
            "GitHub API access forbidden. Check token scopes for this repository.",
            status_code=403,
            code="forbidden",
        )
    if sc >= 500:
        raise GitHubAPIError(
            "GitHub is temporarily unavailable. Retry later.",
            status_code=502,
            code="github_unavailable",
        )
    if not res.is_success:
        raise GitHubAPIError(
            f"GitHub API error ({sc}) for {context}.",
            status_code=502,
            code="github_error",
        )


class GitHubRepositoryClient:
    """Project-bound GitHub API operations."""

    def __init__(self, http: GitHubHttpConfig, *, owner: str, repo: str):
        self._http = http
        self.owner = (owner or "").strip()
        self.repo = (repo or "").strip()
        if not self.owner or not self.repo:
            raise ValueError("owner and repo are required")

    @property
    def full_name(self) -> str:
        return f"{self.owner}/{self.repo}"

    def _get(self, path: str, *, params: Optional[Dict[str, Any]] = None) -> httpx.Response:
        url = f"{_api_base(self._http)}{path}"
        try:
            with httpx.Client(timeout=min(30, max(5, self._http.timeout_s))) as client:
                res = client.get(url, headers=_headers(self._http), params=params or {})
        except httpx.TimeoutException as e:
            logger.warning("github: timeout GET %s", path)
            raise GitHubAPIError("GitHub API request timed out.", status_code=504, code="timeout") from e
        except httpx.RequestError as e:
            logger.warning("github: network error GET %s: %s", path, type(e).__name__)
            raise GitHubAPIError("Could not reach GitHub API.", status_code=502, code="network_error") from e
        return res

    def validate_connection(self) -> Tuple[Dict[str, Any], Optional[int]]:
        """GET /repos/{owner}/{repo} + rate limit remaining."""
        res = self._get(f"/repos/{self.owner}/{self.repo}")
        _raise_for_status(res, context=self.full_name)
        data = res.json() if res.content else {}
        remaining_raw = res.headers.get("X-RateLimit-Remaining")
        remaining = int(remaining_raw) if remaining_raw and remaining_raw.isdigit() else None
        return data if isinstance(data, dict) else {}, remaining

    def list_branches(self, *, limit: int = 30) -> List[Dict[str, Any]]:
        res = self._get(f"/repos/{self.owner}/{self.repo}/branches", params={"per_page": min(100, limit)})
        _raise_for_status(res, context=f"branches {self.full_name}")
        data = res.json()
        return data if isinstance(data, list) else []

    def get_branch(self, branch: str) -> Dict[str, Any]:
        b = (branch or "").strip()
        res = self._get(f"/repos/{self.owner}/{self.repo}/branches/{b}")
        _raise_for_status(res, context=f"branch {b}")
        data = res.json()
        return data if isinstance(data, dict) else {}

    def list_open_pull_requests(self, *, limit: int = 30) -> List[Dict[str, Any]]:
        res = self._get(
            f"/repos/{self.owner}/{self.repo}/pulls",
            params={"state": "open", "per_page": min(100, limit), "sort": "updated", "direction": "desc"},
        )
        _raise_for_status(res, context=f"pulls {self.full_name}")
        data = res.json()
        return data if isinstance(data, list) else []

    def get_pull_request(self, number: int) -> Dict[str, Any]:
        res = self._get(f"/repos/{self.owner}/{self.repo}/pulls/{int(number)}")
        _raise_for_status(res, context=f"PR #{number}")
        data = res.json()
        return data if isinstance(data, dict) else {}

    def list_pull_request_files(self, number: int) -> List[Dict[str, Any]]:
        out: List[Dict[str, Any]] = []
        page = 1
        while page <= 10:
            res = self._get(
                f"/repos/{self.owner}/{self.repo}/pulls/{int(number)}/files",
                params={"per_page": 100, "page": page},
            )
            _raise_for_status(res, context=f"PR #{number} files")
            batch = res.json()
            if not isinstance(batch, list) or not batch:
                break
            out.extend(batch)
            if len(batch) < 100:
                break
            page += 1
        return out

    def get_branch_head_sha(self, branch: str) -> str:
        data = self.get_branch(branch)
        commit = data.get("commit") if isinstance(data, dict) else {}
        if isinstance(commit, dict):
            sha = str(commit.get("sha") or "").strip()
            if sha:
                return sha
        raise GitHubAPIError(
            f"Could not resolve branch head for {branch!r}.",
            status_code=404,
            code="branch_head_not_found",
        )

    def list_tree_recursive(
        self,
        tree_sha: str,
        *,
        max_paths: int = 15000,
    ) -> Tuple[List[str], bool]:
        """
        Return blob paths from a recursive git tree.

        Tuple: (paths, truncated).
        """
        sha = (tree_sha or "").strip()
        if not sha:
            return [], False
        res = self._get(
            f"/repos/{self.owner}/{self.repo}/git/trees/{sha}",
            params={"recursive": "1"},
        )
        _raise_for_status(res, context=f"tree {sha[:8]}")
        data = res.json() if res.content else {}
        if not isinstance(data, dict):
            return [], False
        truncated = bool(data.get("truncated"))
        tree = data.get("tree")
        if not isinstance(tree, list):
            return [], truncated
        paths: List[str] = []
        for item in tree:
            if not isinstance(item, dict):
                continue
            if str(item.get("type") or "") != "blob":
                continue
            path = str(item.get("path") or "").strip()
            if path:
                paths.append(path)
            if len(paths) >= max_paths:
                truncated = True
                break
        return paths, truncated

    def get_file_text(self, path: str, *, ref: str) -> str:
        """Fetch file content at ref (branch name or commit SHA)."""
        p = (path or "").strip()
        r = (ref or "").strip()
        if not p or not r:
            return ""
        res = self._get(
            f"/repos/{self.owner}/{self.repo}/contents/{p}",
            params={"ref": r},
        )
        if res.status_code == 404:
            return ""
        _raise_for_status(res, context=f"contents {p}")
        data = res.json() if res.content else {}
        if not isinstance(data, dict):
            return ""
        encoding = str(data.get("encoding") or "").lower()
        content = data.get("content")
        if encoding == "base64" and isinstance(content, str):
            import base64

            try:
                raw = base64.b64decode(content, validate=False)
                return raw.decode("utf-8", errors="replace")
            except Exception:
                return ""
        if isinstance(content, str):
            return content
        return ""
