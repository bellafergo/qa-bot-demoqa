# services/jira_integration_service.py
"""
Read-only Jira integration service (JIRA-01A).

Discovery and intelligence only — no issue creation, updates, transitions, or comments.
Reuses integration_service for connector configuration and in-memory secrets.
"""
from __future__ import annotations

import logging
import os
from datetime import datetime, timezone
from typing import List, Optional

from models.jira_models import (
    JiraConnectionStatus,
    JiraEpic,
    JiraEpicsResponse,
    JiraFixVersion,
    JiraFixVersionsResponse,
    JiraIssue,
    JiraIssueType,
    JiraIssueTypesResponse,
    JiraIssuesResponse,
    JiraProject,
    JiraProjectsResponse,
    JiraRelease,
    JiraReleasesResponse,
)
from services.integration_service import integration_service
from services.jira_repository_service import (
    JiraAPIError,
    JiraHttpConfig,
    count_issues,
    list_issue_types_for_project,
    list_project_versions,
    list_projects as fetch_jira_projects,
    parse_issue,
    parse_project,
    parse_version,
    search_issues,
    validate_connection,
)

logger = logging.getLogger("vanya.jira_integration")

_LAST_SYNC: Optional[datetime] = None


def _touch_sync() -> datetime:
    global _LAST_SYNC
    now = datetime.now(timezone.utc)
    _LAST_SYNC = now
    return now


def _resolve_http_config() -> Optional[JiraHttpConfig]:
    """Resolve Jira credentials from integration config, in-memory secrets, and env."""
    try:
        cfg = integration_service.get_config("jira")
    except KeyError:
        return None

    if not cfg.enabled:
        return None

    base_url = (cfg.base_url or os.getenv("JIRA_BASE_URL") or "").strip().rstrip("/")
    email = (
        cfg.workspace
        or os.getenv("JIRA_EMAIL")
        or os.getenv("JIRA_USER")
        or ""
    ).strip()
    token = (
        integration_service.get_connector_secret("jira", "token")
        or os.getenv("JIRA_API_TOKEN")
        or os.getenv("JIRA_TOKEN")
        or ""
    ).strip()

    has_token = cfg.token_present or bool(token)
    if not base_url or not email or not has_token or not token:
        return None

    return JiraHttpConfig(
        base_url=base_url,
        email=email,
        token=token,
        project_key=(cfg.project_key or "").strip() or None,
    )


def _empty_status(*, server_url: Optional[str] = None) -> JiraConnectionStatus:
    return JiraConnectionStatus(
        connected=False,
        server_url=server_url,
        last_sync=_LAST_SYNC,
    )


def _epic_jql(project_key: Optional[str]) -> str:
    base = 'issuetype = Epic'
    if project_key:
        return f'project = "{project_key}" AND {base}'
    return base


def _issues_jql(project_key: Optional[str]) -> str:
    if project_key:
        return f'project = "{project_key}" ORDER BY updated DESC'
    return "ORDER BY updated DESC"


def validate_jira_connection() -> JiraConnectionStatus:
    """Validate connectivity and return aggregate discovery counts."""
    http = _resolve_http_config()
    if http is None:
        try:
            cfg = integration_service.get_config("jira")
            return _empty_status(server_url=cfg.base_url)
        except KeyError:
            return _empty_status()

    try:
        validate_connection(http)
        projects = fetch_jira_projects(http)
        project_count = len(projects)
        issue_count = count_issues(http, jql=_issues_jql(http.project_key))
        epic_count = count_issues(http, jql=_epic_jql(http.project_key))

        release_count = 0
        fix_version_count = 0
        keys = [http.project_key] if http.project_key else [p.get("key") for p in projects if p.get("key")]
        seen_versions: set[str] = set()
        for key in keys:
            if not key:
                continue
            try:
                versions = list_project_versions(http, key)
            except JiraAPIError:
                continue
            for v in versions:
                vid = str(v.get("id") or "")
                if not vid or vid in seen_versions:
                    continue
                seen_versions.add(vid)
                if v.get("released"):
                    release_count += 1
                else:
                    fix_version_count += 1

        synced = _touch_sync()
        return JiraConnectionStatus(
            connected=True,
            server_url=http.base_url,
            project_count=project_count,
            issue_count=issue_count,
            epic_count=epic_count,
            release_count=release_count,
            fix_version_count=fix_version_count,
            last_sync=synced,
        )
    except JiraAPIError as exc:
        logger.warning("jira validate failed: %s", exc)
        return _empty_status(server_url=http.base_url)


def list_projects() -> JiraProjectsResponse:
    http = _resolve_http_config()
    if http is None:
        return JiraProjectsResponse()

    try:
        raw = fetch_jira_projects(http)
        projects = [JiraProject(**parse_project(p)) for p in raw]
        _touch_sync()
        return JiraProjectsResponse(projects=projects, total=len(projects))
    except JiraAPIError as exc:
        logger.warning("jira list_projects failed: %s", exc)
        return JiraProjectsResponse()


def list_issue_types(*, project_key: Optional[str] = None) -> JiraIssueTypesResponse:
    http = _resolve_http_config()
    if http is None:
        return JiraIssueTypesResponse()

    key = project_key or http.project_key
    try:
        if key:
            projects = fetch_jira_projects(http)
            project_id = next(
                (str(p.get("id")) for p in projects if p.get("key") == key),
                None,
            )
            if not project_id:
                return JiraIssueTypesResponse()
            raw = list_issue_types_for_project(http, project_id)
        else:
            from services.jira_repository_service import _get

            raw = _get(http, "/rest/api/3/issuetype")
        types = [
            JiraIssueType(
                issue_type_id=str(t.get("id") or ""),
                issue_type_name=str(t.get("name") or ""),
            )
            for t in raw
        ]
        _touch_sync()
        return JiraIssueTypesResponse(issue_types=types, total=len(types))
    except JiraAPIError as exc:
        logger.warning("jira list_issue_types failed: %s", exc)
        return JiraIssueTypesResponse()


def list_issues(*, project_key: Optional[str] = None, max_results: int = 50) -> JiraIssuesResponse:
    http = _resolve_http_config()
    if http is None:
        return JiraIssuesResponse()

    jql = _issues_jql(project_key or http.project_key)
    try:
        raw, total = search_issues(http, jql=jql, max_results=max_results)
        issues = [JiraIssue(**parse_issue(i)) for i in raw]
        _touch_sync()
        return JiraIssuesResponse(issues=issues, total=total)
    except JiraAPIError as exc:
        logger.warning("jira list_issues failed: %s", exc)
        return JiraIssuesResponse()


def list_epics(*, project_key: Optional[str] = None, max_results: int = 50) -> JiraEpicsResponse:
    http = _resolve_http_config()
    if http is None:
        return JiraEpicsResponse()

    jql = _epic_jql(project_key or http.project_key)
    try:
        raw, total = search_issues(http, jql=jql, max_results=max_results, fields=["summary"])
        epics = [
            JiraEpic(
                epic_id=str(i.get("id") or ""),
                epic_key=str(i.get("key") or ""),
                epic_name=str((i.get("fields") or {}).get("summary") or ""),
            )
            for i in raw
        ]
        _touch_sync()
        return JiraEpicsResponse(epics=epics, total=total)
    except JiraAPIError as exc:
        logger.warning("jira list_epics failed: %s", exc)
        return JiraEpicsResponse()


def _collect_versions(http: JiraHttpConfig, project_key: Optional[str]) -> List[dict]:
    keys: List[str] = []
    if project_key:
        keys = [project_key]
    else:
        keys = [p.get("key") for p in fetch_jira_projects(http) if p.get("key")]

    merged: List[dict] = []
    seen: set[str] = set()
    for key in keys:
        if not key:
            continue
        try:
            for v in list_project_versions(http, key):
                vid = str(v.get("id") or "")
                if vid and vid not in seen:
                    seen.add(vid)
                    merged.append(v)
        except JiraAPIError:
            continue
    return merged


def list_releases(*, project_key: Optional[str] = None) -> JiraReleasesResponse:
    http = _resolve_http_config()
    if http is None:
        return JiraReleasesResponse()

    try:
        versions = _collect_versions(http, project_key or http.project_key)
        releases = [
            JiraRelease(
                release_id=parsed["version_id"],
                release_name=parsed["version_name"],
                released=parsed["released"],
                release_date=parsed.get("release_date"),
            )
            for v in versions
            if v.get("released")
            for parsed in [parse_version(v)]
        ]
        _touch_sync()
        return JiraReleasesResponse(releases=releases, total=len(releases))
    except JiraAPIError as exc:
        logger.warning("jira list_releases failed: %s", exc)
        return JiraReleasesResponse()


def list_fix_versions(*, project_key: Optional[str] = None) -> JiraFixVersionsResponse:
    http = _resolve_http_config()
    if http is None:
        return JiraFixVersionsResponse()

    try:
        versions = _collect_versions(http, project_key or http.project_key)
        fix_versions = [
            JiraFixVersion(
                version_id=parsed["version_id"],
                version_name=parsed["version_name"],
                released=parsed["released"],
            )
            for v in versions
            if not v.get("released")
            for parsed in [parse_version(v)]
        ]
        _touch_sync()
        return JiraFixVersionsResponse(fix_versions=fix_versions, total=len(fix_versions))
    except JiraAPIError as exc:
        logger.warning("jira list_fix_versions failed: %s", exc)
        return JiraFixVersionsResponse()
