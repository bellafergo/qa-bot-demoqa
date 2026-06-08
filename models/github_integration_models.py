# models/github_integration_models.py
"""GitHub Integration — SaaS GitHub App contracts."""
from __future__ import annotations

from typing import Dict, List, Literal, Optional

from pydantic import BaseModel, Field

from models.pr_analysis_models import ProjectPRAnalysisReport

GitHubProvider = Literal["github_app", "legacy_pat", "none"]


class GitHubConnectAppRequest(BaseModel):
    installation_id: str
    connected_by: Optional[str] = None


class GitHubSelectRepositoryRequest(BaseModel):
    owner: str
    repo: str
    default_branch: Optional[str] = None


class GitHubInstallUrlResponse(BaseModel):
    install_url: str
    state: str = ""
    setup_callback_hint: str = ""


class GitHubConnectionStatus(BaseModel):
    project_id: str
    provider: GitHubProvider = "none"
    connected: bool = False
    enabled: bool = False
    installation_id: str = ""
    owner: str = ""
    repo: str = ""
    full_name: str = ""
    default_branch: str = ""
    repo_url: str = ""
    connected_by: str = ""
    connected_at: str = ""
    repo_selected_at: str = ""
    last_validated_at: str = ""
    permissions: Dict[str, str] = Field(default_factory=dict)
    validation_ok: bool = False
    validation_message: str = ""
    rate_limit_remaining: Optional[int] = None
    needs_migration: bool = False
    app_configured: bool = False


class GitHubRepositorySummary(BaseModel):
    id: int = 0
    owner: str = ""
    repo: str = ""
    full_name: str = ""
    default_branch: str = "main"
    private: bool = False
    html_url: str = ""


class GitHubRepositoriesResponse(BaseModel):
    installation_id: str = ""
    repositories: List[GitHubRepositorySummary] = Field(default_factory=list)


class GitHubBranchInfo(BaseModel):
    name: str
    protected: bool = False
    last_commit_sha: str = ""
    last_commit_at: str = ""


class GitHubBranchesResponse(BaseModel):
    default_branch: str = ""
    branches: List[GitHubBranchInfo] = Field(default_factory=list)


class GitHubPullRequestSummary(BaseModel):
    number: int
    title: str = ""
    state: str = ""
    branch: str = ""
    base_branch: str = ""
    author: str = ""
    html_url: str = ""
    updated_at: str = ""
    draft: bool = False
    changed_files_count: Optional[int] = None


class GitHubPullRequestsResponse(BaseModel):
    pull_requests: List[GitHubPullRequestSummary] = Field(default_factory=list)


class GitHubPRFileEntry(BaseModel):
    filename: str
    status: str = ""
    additions: int = 0
    deletions: int = 0


class GitHubPRFilesResponse(BaseModel):
    number: int
    title: str = ""
    branch: str = ""
    changed_files: List[str] = Field(default_factory=list)
    files: List[GitHubPRFileEntry] = Field(default_factory=list)


class GitHubPRAnalyzeResponse(BaseModel):
    pull_request: GitHubPRFilesResponse
    analysis: ProjectPRAnalysisReport
    source: Literal["github_app_v1"] = "github_app_v1"
