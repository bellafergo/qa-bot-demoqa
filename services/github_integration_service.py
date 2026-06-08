# services/github_integration_service.py
"""
GitHub Integration v1 orchestration — branches, PRs, files, PR Analysis v1.
"""
from __future__ import annotations

import logging
from typing import List

from models.github_integration_models import (
    GitHubBranchInfo,
    GitHubBranchesResponse,
    GitHubPRAnalyzeResponse,
    GitHubPRFileEntry,
    GitHubPRFilesResponse,
    GitHubPullRequestSummary,
    GitHubPullRequestsResponse,
)
from models.pr_analysis_models import ProjectPRAnalysisRequest
from services.github_repository_service import GitHubAPIError
from services.pr_analysis_project_debug import log_project_id_lookup
from services.project_github_settings_service import _client_for_project, _github_dict
from services.pr_analysis_service import pr_analysis_service

logger = logging.getLogger("vanya.github_integration")


def _require_client(project_id: str):
    from services.db.project_repository import project_repo

    project = project_repo.get_project((project_id or "").strip().lower())
    if project is None:
        raise LookupError(f"project not found: {project_id}")
    gh = _github_dict(project)
    if not gh.get("enabled"):
        raise ValueError("GitHub is not enabled for this project. Connect a repository first.")
    _gh, client = _client_for_project(project_id)
    return gh, client


def list_branches(project_id: str, *, limit: int = 30) -> GitHubBranchesResponse:
    gh, client = _require_client(project_id)
    try:
        raw = client.list_branches(limit=limit)
    except GitHubAPIError:
        raise

    branches: List[GitHubBranchInfo] = []
    for b in raw:
        if not isinstance(b, dict):
            continue
        name = str(b.get("name") or "")
        commit = b.get("commit") if isinstance(b.get("commit"), dict) else {}
        branches.append(GitHubBranchInfo(
            name=name,
            protected=bool(b.get("protected")),
            last_commit_sha=str(commit.get("sha") or "")[:12],
        ))

    default_branch = str(gh.get("default_branch") or "main")
    # Enrich default branch with last commit if present in list
    try:
        detail = client.get_branch(default_branch)
        commit = detail.get("commit") if isinstance(detail.get("commit"), dict) else {}
        for br in branches:
            if br.name == default_branch:
                br.last_commit_sha = str(commit.get("sha") or "")[:12]
                br.last_commit_at = str((commit.get("commit") or {}).get("committer", {}).get("date") or "")
                break
    except GitHubAPIError:
        logger.debug("github: default branch detail unavailable", exc_info=True)

    return GitHubBranchesResponse(default_branch=default_branch, branches=branches)


def list_pull_requests(project_id: str, *, limit: int = 20) -> GitHubPullRequestsResponse:
    _, client = _require_client(project_id)
    raw = client.list_open_pull_requests(limit=limit)
    prs: List[GitHubPullRequestSummary] = []
    for pr in raw:
        if not isinstance(pr, dict):
            continue
        user = pr.get("user") if isinstance(pr.get("user"), dict) else {}
        head = pr.get("head") if isinstance(pr.get("head"), dict) else {}
        base = pr.get("base") if isinstance(pr.get("base"), dict) else {}
        prs.append(GitHubPullRequestSummary(
            number=int(pr.get("number") or 0),
            title=str(pr.get("title") or ""),
            state=str(pr.get("state") or ""),
            branch=str(head.get("ref") or ""),
            base_branch=str(base.get("ref") or ""),
            author=str(user.get("login") or ""),
            html_url=str(pr.get("html_url") or ""),
            updated_at=str(pr.get("updated_at") or ""),
            draft=bool(pr.get("draft")),
        ))
    return GitHubPullRequestsResponse(pull_requests=prs)


def get_pull_request_files(project_id: str, number: int) -> GitHubPRFilesResponse:
    _, client = _require_client(project_id)
    pr = client.get_pull_request(number)
    files_raw = client.list_pull_request_files(number)

    files: List[GitHubPRFileEntry] = []
    changed: List[str] = []
    for f in files_raw:
        if not isinstance(f, dict):
            continue
        fn = str(f.get("filename") or "")
        if fn:
            changed.append(fn)
        files.append(GitHubPRFileEntry(
            filename=fn,
            status=str(f.get("status") or ""),
            additions=int(f.get("additions") or 0),
            deletions=int(f.get("deletions") or 0),
        ))

    if not changed:
        logger.info("github: PR #%s has no changed files project_id=%s", number, project_id)

    head = pr.get("head") if isinstance(pr.get("head"), dict) else {}
    return GitHubPRFilesResponse(
        number=int(number),
        title=str(pr.get("title") or ""),
        branch=str(head.get("ref") or ""),
        changed_files=changed,
        files=files,
    )


def analyze_pull_request(project_id: str, number: int) -> GitHubPRAnalyzeResponse:
    log_project_id_lookup(project_id=project_id)
    pr_files = get_pull_request_files(project_id, number)
    if not pr_files.changed_files:
        raise ValueError(f"PR #{number} has no changed files to analyze")

    analysis = pr_analysis_service.analyze_for_project(
        project_id,
        ProjectPRAnalysisRequest(
            changed_files=pr_files.changed_files,
            pr_id=str(number),
            title=pr_files.title,
            branch=pr_files.branch,
        ),
    )
    return GitHubPRAnalyzeResponse(pull_request=pr_files, analysis=analysis)
