# api/routes/jira_integration_routes.py
"""Read-only Jira discovery routes (JIRA-01A)."""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, Query

from models.jira_models import (
    JiraConnectionStatus,
    JiraEpicsResponse,
    JiraFixVersionsResponse,
    JiraIssuesResponse,
    JiraProjectsResponse,
    JiraReleasesResponse,
)
from services.jira_integration_service import (
    list_epics,
    list_fix_versions,
    list_issues,
    list_projects,
    list_releases,
    validate_jira_connection,
)

logger = logging.getLogger("vanya.jira_integration_routes")

router = APIRouter(prefix="/integrations/jira", tags=["jira-integration"])


@router.get("/status", response_model=JiraConnectionStatus)
def get_jira_status():
    """Validate Jira connectivity and return aggregate discovery counts."""
    return validate_jira_connection()


@router.get("/projects", response_model=JiraProjectsResponse)
def get_jira_projects():
    """List Jira projects (read-only)."""
    return list_projects()


@router.get("/issues", response_model=JiraIssuesResponse)
def get_jira_issues(
    project_key: Optional[str] = Query(None, description="Filter by Jira project key"),
    max_results: int = Query(50, ge=1, le=100, description="Maximum issues to return"),
):
    """List Jira issues (read-only)."""
    return list_issues(project_key=project_key, max_results=max_results)


@router.get("/epics", response_model=JiraEpicsResponse)
def get_jira_epics(
    project_key: Optional[str] = Query(None, description="Filter by Jira project key"),
    max_results: int = Query(50, ge=1, le=100, description="Maximum epics to return"),
):
    """List Jira epics (read-only)."""
    return list_epics(project_key=project_key, max_results=max_results)


@router.get("/releases", response_model=JiraReleasesResponse)
def get_jira_releases(
    project_key: Optional[str] = Query(None, description="Filter by Jira project key"),
):
    """List released Jira versions (read-only)."""
    return list_releases(project_key=project_key)


@router.get("/fix-versions", response_model=JiraFixVersionsResponse)
def get_jira_fix_versions(
    project_key: Optional[str] = Query(None, description="Filter by Jira project key"),
):
    """List unreleased Jira fix versions (read-only)."""
    return list_fix_versions(project_key=project_key)
