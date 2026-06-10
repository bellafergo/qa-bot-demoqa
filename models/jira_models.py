# models/jira_models.py
"""Read-only Jira discovery models (JIRA-01A)."""
from __future__ import annotations

from datetime import datetime
from typing import List, Optional

from pydantic import BaseModel, Field


class JiraConnectionStatus(BaseModel):
    connected: bool = False
    server_url: Optional[str] = None
    project_count: int = 0
    issue_count: int = 0
    epic_count: int = 0
    release_count: int = 0
    fix_version_count: int = 0
    last_sync: Optional[datetime] = None


class JiraProject(BaseModel):
    project_id: str
    project_key: str
    project_name: str


class JiraIssue(BaseModel):
    issue_id: str
    issue_key: str
    summary: str
    issue_type: str
    status: str
    assignee: Optional[str] = None
    priority: Optional[str] = None


class JiraEpic(BaseModel):
    epic_id: str
    epic_key: str
    epic_name: str


class JiraRelease(BaseModel):
    release_id: str
    release_name: str
    released: bool = False
    release_date: Optional[str] = None


class JiraFixVersion(BaseModel):
    version_id: str
    version_name: str
    released: bool = False


class JiraIssueType(BaseModel):
    issue_type_id: str
    issue_type_name: str


class JiraProjectsResponse(BaseModel):
    projects: List[JiraProject] = Field(default_factory=list)
    total: int = 0


class JiraIssuesResponse(BaseModel):
    issues: List[JiraIssue] = Field(default_factory=list)
    total: int = 0


class JiraEpicsResponse(BaseModel):
    epics: List[JiraEpic] = Field(default_factory=list)
    total: int = 0


class JiraReleasesResponse(BaseModel):
    releases: List[JiraRelease] = Field(default_factory=list)
    total: int = 0


class JiraFixVersionsResponse(BaseModel):
    fix_versions: List[JiraFixVersion] = Field(default_factory=list)
    total: int = 0


class JiraIssueTypesResponse(BaseModel):
    issue_types: List[JiraIssueType] = Field(default_factory=list)
    total: int = 0
