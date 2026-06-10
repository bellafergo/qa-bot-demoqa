# api/routes/jira_integration_routes.py
"""
Read-only Jira discovery routes (JIRA-01A).

Route semantics (do not conflate with generic connector routes):
  GET /integrations/jira/status       — Jira discovery aggregate (counts, last_sync, connected)
  GET /integrations/jira/projects     — canonical project listing
  GET /integrations/jira/issues       — issue discovery
  GET /integrations/jira/epics        — epic discovery
  GET /integrations/jira/releases     — released versions
  GET /integrations/jira/fix-versions — unreleased fix versions
  GET /integrations/jira/issue-types  — issue type catalog
  GET /integrations/jira/intelligence — deterministic issue correlation report

Generic connector framework status (health, enabled, config_summary):
  GET  /integrations/jira             — ConnectorStatus via integrations_routes
  POST /integrations/jira/health-check
"""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, Query

from models.jira_issue_intelligence_models import JiraIssueIntelligenceReport
from models.jira_models import (
    JiraConnectionStatus,
    JiraEpicsResponse,
    JiraFixVersionsResponse,
    JiraIssueTypesResponse,
    JiraIssuesResponse,
    JiraProjectsResponse,
    JiraReleasesResponse,
)
from services.jira_integration_service import (
    list_epics,
    list_fix_versions,
    list_issue_types,
    list_issues,
    list_projects,
    list_releases,
    validate_jira_connection,
)
from services.jira_issue_intelligence_service import build_jira_issue_intelligence_report

logger = logging.getLogger("vanya.jira_integration_routes")

router = APIRouter(prefix="/integrations/jira", tags=["jira-integration"])


@router.get("/status", response_model=JiraConnectionStatus)
def get_jira_status():
    """Jira discovery aggregate — connectivity plus project/issue/epic/version counts."""
    return validate_jira_connection()


@router.get("/projects", response_model=JiraProjectsResponse)
def get_jira_projects():
    """List Jira projects (read-only). Canonical project listing path."""
    return list_projects()


@router.get("/issue-types", response_model=JiraIssueTypesResponse)
def get_jira_issue_types(
    project_key: Optional[str] = Query(None, description="Filter by Jira project key"),
):
    """List Jira issue types (read-only)."""
    return list_issue_types(project_key=project_key)


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


@router.get("/intelligence", response_model=JiraIssueIntelligenceReport)
def get_jira_issue_intelligence(
    project_key: Optional[str] = Query(None, description="Filter by Jira project key"),
):
    """Deterministic Jira issue correlation report (read-only intelligence)."""
    return build_jira_issue_intelligence_report(project_key=project_key)
