# models/azure_devops_integration_models.py
"""Azure DevOps Integration — OAuth + REST API contracts."""
from __future__ import annotations

from typing import Dict, List, Literal, Optional

from pydantic import BaseModel, Field

from models.pr_analysis_models import ProjectPRAnalysisReport

AzureDevOpsProvider = Literal["oauth", "none"]


class AzureDevOpsAuthorizeUrlResponse(BaseModel):
    authorize_url: str
    state: str = ""
    redirect_uri: str = ""


class AzureDevOpsConnectionStatus(BaseModel):
    project_id: str
    provider: AzureDevOpsProvider = "none"
    connected: bool = False
    enabled: bool = False
    organization: str = ""
    azure_project: str = ""
    repository_id: str = ""
    repository_name: str = ""
    full_name: str = ""
    default_branch: str = ""
    repo_url: str = ""
    connected_by: str = ""
    connected_at: str = ""
    repo_selected_at: str = ""
    last_validated_at: str = ""
    validation_ok: bool = False
    validation_message: str = ""
    oauth_configured: bool = False
    patches_available: bool = False


class AzureDevOpsOrganizationSummary(BaseModel):
    account_id: str = ""
    account_name: str = ""


class AzureDevOpsOrganizationsResponse(BaseModel):
    organizations: List[AzureDevOpsOrganizationSummary] = Field(default_factory=list)


class AzureDevOpsProjectSummary(BaseModel):
    id: str = ""
    name: str = ""
    description: str = ""


class AzureDevOpsProjectsResponse(BaseModel):
    organization: str = ""
    projects: List[AzureDevOpsProjectSummary] = Field(default_factory=list)


class AzureDevOpsRepositorySummary(BaseModel):
    id: str = ""
    name: str = ""
    default_branch: str = "main"
    remote_url: str = ""


class AzureDevOpsRepositoriesResponse(BaseModel):
    organization: str = ""
    azure_project: str = ""
    repositories: List[AzureDevOpsRepositorySummary] = Field(default_factory=list)


class AzureDevOpsSelectTargetRequest(BaseModel):
    organization: str
    azure_project: str
    repository_id: str
    repository_name: Optional[str] = None
    default_branch: Optional[str] = None


class AzureDevOpsPullRequestSummary(BaseModel):
    pull_request_id: int
    title: str = ""
    state: str = ""
    branch: str = ""
    base_branch: str = ""
    author: str = ""
    html_url: str = ""
    updated_at: str = ""
    draft: bool = False
    changed_files_count: Optional[int] = None


class AzureDevOpsPullRequestsResponse(BaseModel):
    pull_requests: List[AzureDevOpsPullRequestSummary] = Field(default_factory=list)


class AzureDevOpsPRFileEntry(BaseModel):
    filename: str
    status: str = ""
    additions: int = 0
    deletions: int = 0
    patch: Optional[str] = None


class AzureDevOpsPRFilesResponse(BaseModel):
    pull_request_id: int
    title: str = ""
    branch: str = ""
    changed_files: List[str] = Field(default_factory=list)
    files: List[AzureDevOpsPRFileEntry] = Field(default_factory=list)
    patches_limited: bool = False


class AzureDevOpsPRAnalyzeResponse(BaseModel):
    pull_request: AzureDevOpsPRFilesResponse
    analysis: ProjectPRAnalysisReport
    source: Literal["azure_devops_oauth_v1"] = "azure_devops_oauth_v1"
