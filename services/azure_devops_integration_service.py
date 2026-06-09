# services/azure_devops_integration_service.py
"""
Azure DevOps Integration orchestration — PRs, files, PR Analysis v1.3 via shared engine.
"""
from __future__ import annotations

import logging
from typing import List

from models.azure_devops_integration_models import (
    AzureDevOpsPRAnalyzeResponse,
    AzureDevOpsPRFileEntry,
    AzureDevOpsPRFilesResponse,
    AzureDevOpsPullRequestSummary,
    AzureDevOpsPullRequestsResponse,
)
from models.pr_analysis_models import ProjectPRAnalysisRequest
from services.azure_devops_repository_service import AzureDevOpsAPIError
from services.pr_analysis_project_debug import log_project_id_lookup
from services.project_azure_devops_settings_service import _client_for_project
from services.pr_analysis_service import pr_analysis_service

logger = logging.getLogger("vanya.azure_devops_integration")


def _require_client(project_id: str):
    return _client_for_project(project_id)


def list_pull_requests(project_id: str, *, limit: int = 20) -> AzureDevOpsPullRequestsResponse:
    _, client = _require_client(project_id)
    raw = client.list_open_pull_requests(limit=limit)
    prs: List[AzureDevOpsPullRequestSummary] = []
    for pr in raw:
        if not isinstance(pr, dict):
            continue
        summary = client.pull_request_summary_from_raw(pr)
        prs.append(AzureDevOpsPullRequestSummary(**summary))
    return AzureDevOpsPullRequestsResponse(pull_requests=prs)


def get_pull_request_files(project_id: str, pull_request_id: int) -> AzureDevOpsPRFilesResponse:
    _, client = _require_client(project_id)
    pr = client.get_pull_request(pull_request_id)
    raw_files, patches_limited = client.pull_request_file_entries(pull_request_id)

    files: List[AzureDevOpsPRFileEntry] = []
    changed: List[str] = []
    for f in raw_files:
        fn = str(f.get("filename") or "")
        if fn:
            changed.append(fn)
        files.append(AzureDevOpsPRFileEntry(
            filename=fn,
            status=str(f.get("status") or ""),
            additions=int(f.get("additions") or 0),
            deletions=int(f.get("deletions") or 0),
            patch=f.get("patch"),
        ))

    if not changed:
        logger.info(
            "azure_devops: PR %s has no changed files project_id=%s",
            pull_request_id,
            project_id,
        )

    branch = client._ref_name(str(pr.get("sourceRefName") or ""))
    return AzureDevOpsPRFilesResponse(
        pull_request_id=int(pull_request_id),
        title=str(pr.get("title") or ""),
        branch=branch,
        changed_files=changed,
        files=files,
        patches_limited=patches_limited,
    )


def analyze_pull_request(project_id: str, pull_request_id: int) -> AzureDevOpsPRAnalyzeResponse:
    log_project_id_lookup(project_id=project_id)
    pr_files = get_pull_request_files(project_id, pull_request_id)
    if not pr_files.changed_files:
        raise ValueError(f"Azure DevOps PR {pull_request_id} has no changed files to analyze")

    file_patches = {
        f.filename: f.patch
        for f in pr_files.files
        if f.filename and f.patch
    }
    if pr_files.patches_limited and not file_patches:
        logger.info(
            "azure_devops: analyzing PR %s without unified patches (API limitation) project_id=%s",
            pull_request_id,
            project_id,
        )

    analysis = pr_analysis_service.analyze_for_project(
        project_id,
        ProjectPRAnalysisRequest(
            changed_files=pr_files.changed_files,
            pr_id=str(pull_request_id),
            title=pr_files.title,
            branch=pr_files.branch,
            file_patches=file_patches,
        ),
    )
    return AzureDevOpsPRAnalyzeResponse(pull_request=pr_files, analysis=analysis)
