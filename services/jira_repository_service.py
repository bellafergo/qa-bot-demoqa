# services/jira_repository_service.py
"""
Low-level read-only Jira REST API client.

Uses Atlassian Cloud REST API v3. Never logs tokens. No write operations.
"""
from __future__ import annotations

import base64
import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

import httpx

logger = logging.getLogger("vanya.jira_repository")


@dataclass(frozen=True)
class JiraHttpConfig:
    base_url: str
    email: str
    token: str
    project_key: Optional[str] = None


class JiraAPIError(Exception):
    """Safe Jira API failure — no credentials in message."""

    def __init__(self, message: str, *, status_code: int = 502, code: str = "jira_error"):
        super().__init__(message)
        self.status_code = status_code
        self.code = code


def _auth_header(http: JiraHttpConfig) -> str:
    raw = f"{http.email}:{http.token}".encode("utf-8")
    return "Basic " + base64.b64encode(raw).decode("ascii")


def _headers(http: JiraHttpConfig) -> Dict[str, str]:
    return {
        "Authorization": _auth_header(http),
        "Accept": "application/json",
        "Content-Type": "application/json",
    }


def _api_base(http: JiraHttpConfig) -> str:
    return http.base_url.rstrip("/")


def _raise_for_status(res: httpx.Response, *, context: str) -> None:
    sc = res.status_code
    if sc == 401:
        raise JiraAPIError(
            "Jira credentials are invalid or expired. Update the API token.",
            status_code=401,
            code="invalid_token",
        )
    if sc == 403:
        raise JiraAPIError(
            "Jira API access forbidden. Check token permissions.",
            status_code=403,
            code="forbidden",
        )
    if sc == 404:
        raise JiraAPIError(
            f"Jira resource not found ({context}).",
            status_code=404,
            code="not_found",
        )
    if sc >= 500:
        raise JiraAPIError(
            "Jira is temporarily unavailable. Retry later.",
            status_code=502,
            code="jira_unavailable",
        )
    if not res.is_success:
        raise JiraAPIError(
            f"Jira API error ({sc}) for {context}.",
            status_code=502,
            code="jira_error",
        )


def _get(http: JiraHttpConfig, path: str, *, params: Optional[Dict[str, Any]] = None) -> Any:
    url = f"{_api_base(http)}{path}"
    with httpx.Client(timeout=30.0) as client:
        res = client.get(url, headers=_headers(http), params=params or {})
    _raise_for_status(res, context=path)
    if not res.content:
        return {}
    return res.json()


def validate_connection(http: JiraHttpConfig) -> Dict[str, Any]:
    """GET /rest/api/3/myself — read-only connectivity check."""
    return _get(http, "/rest/api/3/myself")


def list_projects(http: JiraHttpConfig, *, max_results: int = 50) -> List[Dict[str, Any]]:
    """GET /rest/api/3/project/search — paginated project discovery."""
    data = _get(
        http,
        "/rest/api/3/project/search",
        params={"maxResults": max_results, "startAt": 0},
    )
    return list(data.get("values") or [])


def search_issues(
    http: JiraHttpConfig,
    *,
    jql: str,
    max_results: int = 50,
    fields: Optional[List[str]] = None,
) -> Tuple[List[Dict[str, Any]], int]:
    """GET /rest/api/3/search/jql — read-only issue search."""
    field_list = fields or [
        "summary",
        "issuetype",
        "status",
        "assignee",
        "priority",
    ]
    data = _get(
        http,
        "/rest/api/3/search",
        params={
            "jql": jql,
            "maxResults": max_results,
            "fields": ",".join(field_list),
        },
    )
    issues = list(data.get("issues") or [])
    total = int(data.get("total") or len(issues))
    return issues, total


def count_issues(http: JiraHttpConfig, *, jql: str = "order by created DESC") -> int:
    """Return total issue count without fetching issue bodies."""
    _, total = search_issues(http, jql=jql, max_results=0)
    return total


def list_issue_types_for_project(http: JiraHttpConfig, project_id: str) -> List[Dict[str, Any]]:
    """GET /rest/api/3/issuetype/project — issue types for a project."""
    data = _get(http, "/rest/api/3/issuetype/project", params={"projectId": project_id})
    return list(data or [])


def list_project_versions(http: JiraHttpConfig, project_key: str) -> List[Dict[str, Any]]:
    """GET /rest/api/3/project/{key}/versions — releases and fix versions."""
    data = _get(http, f"/rest/api/3/project/{project_key}/versions")
    return list(data or [])


def parse_issue(raw: Dict[str, Any]) -> Dict[str, Any]:
    fields = raw.get("fields") or {}
    issue_type = fields.get("issuetype") or {}
    status = fields.get("status") or {}
    assignee = fields.get("assignee") or {}
    priority = fields.get("priority") or {}
    return {
        "issue_id": str(raw.get("id") or ""),
        "issue_key": str(raw.get("key") or ""),
        "summary": str(fields.get("summary") or ""),
        "issue_type": str(issue_type.get("name") or ""),
        "status": str(status.get("name") or ""),
        "assignee": assignee.get("displayName") or assignee.get("emailAddress"),
        "priority": priority.get("name"),
    }


def parse_project(raw: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "project_id": str(raw.get("id") or ""),
        "project_key": str(raw.get("key") or ""),
        "project_name": str(raw.get("name") or ""),
    }


def parse_version(raw: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "version_id": str(raw.get("id") or ""),
        "version_name": str(raw.get("name") or ""),
        "released": bool(raw.get("released")),
        "release_date": raw.get("releaseDate"),
    }
