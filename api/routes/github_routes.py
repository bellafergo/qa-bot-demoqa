# api/routes/github_routes.py
"""
GitHub integration — PR fetch for PR Impact Analysis.

POST /github/pr/fetch  — fetch PR metadata + changed files from GitHub API by URL

Credentials resolve in this order:
- When ``project_id`` is sent: ``project.settings["github"]`` (enabled + optional per-project PAT)
  with fallback to global ``GITHUB_TOKEN`` from ``core.settings`` when no project PAT is set.
- Legacy: omit ``project_id`` and rely on server ``GITHUB_TOKEN`` only (deprecated).
"""
from __future__ import annotations

import logging
import re
from typing import List, Optional

import httpx
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

from services.github_project_context import resolve_github_for_pr_fetch, resolve_legacy_github_http

logger = logging.getLogger("vanya.github")

router = APIRouter(prefix="/github", tags=["github"])

_PR_URL_RE = re.compile(
    r"https?://github\.com/(?P<owner>[^/]+)/(?P<repo>[^/]+)/pull/(?P<number>\d+)",
    re.IGNORECASE,
)


# ── Request / Response ────────────────────────────────────────────────────────

class PRFetchRequest(BaseModel):
    url: str
    project_id: Optional[str] = None


_DIFF_MAX_CHARS = 15_000


class PRFetchResult(BaseModel):
    title: str
    description: str
    branch: str
    pr_id: str
    changed_files: List[str]
    diff: str = ""


# ── Helpers ───────────────────────────────────────────────────────────────────

def _parse_pr_url(url: str):
    """Return (owner, repo, pull_number) or raise 400."""
    m = _PR_URL_RE.match((url or "").strip())
    if not m:
        raise HTTPException(
            status_code=400,
            detail=(
                "Invalid GitHub PR URL. "
                "Expected format: https://github.com/owner/repo/pull/123"
            ),
        )
    return m.group("owner"), m.group("repo"), int(m.group("number"))


# ── Endpoint ──────────────────────────────────────────────────────────────────

@router.post("/pr/fetch", response_model=PRFetchResult)
def fetch_pr(body: PRFetchRequest):
    """
    Fetch PR title, description, branch and changed files from GitHub API.

    Prefer ``project_id`` so credentials and default repo binding come from the catalog project.
    Legacy callers may omit ``project_id`` only when ``GITHUB_TOKEN`` is configured on the server.
    """
    owner, repo, number = _parse_pr_url(body.url)

    if body.project_id and str(body.project_id).strip():
        try:
            gh_ctx = resolve_github_for_pr_fetch(
                str(body.project_id).strip(),
                pr_owner=owner,
                pr_repo=repo,
            )
            http = gh_ctx.http
        except LookupError:
            raise HTTPException(status_code=404, detail="Project not found") from None
        except ValueError as e:
            raise HTTPException(status_code=400, detail=str(e)) from None
    else:
        http = resolve_legacy_github_http()
        if not http:
            raise HTTPException(
                status_code=503,
                detail=(
                    "GitHub is not configured: open Project settings and enable GitHub for this workspace, "
                    "or set GITHUB_TOKEN on the server (legacy)."
                ),
            )

    api_base = (http.api_base or "https://api.github.com").strip().rstrip("/")
    headers = {
        "Authorization": f"Bearer {http.token}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }

    try:
        with httpx.Client(timeout=min(30, max(5, http.timeout_s))) as client:
            # ── PR metadata ──────────────────────────────────────────────────
            pr_res = client.get(
                f"{api_base}/repos/{owner}/{repo}/pulls/{number}",
                headers=headers,
            )
            if pr_res.status_code == 404:
                raise HTTPException(
                    status_code=404,
                    detail=f"PR #{number} not found in {owner}/{repo}. "
                    "Check the URL and that the token has access.",
                )
            if pr_res.status_code == 401:
                raise HTTPException(
                    status_code=401,
                    detail="GitHub token is invalid or expired. Update the project GitHub token or GITHUB_TOKEN.",
                )
            if not pr_res.is_success:
                raise HTTPException(
                    status_code=502,
                    detail=f"GitHub API returned {pr_res.status_code}.",
                )
            pr = pr_res.json()

            # ── Diff (raw patch, truncated to _DIFF_MAX_CHARS) ───────────────
            diff_text = ""
            diff_res = client.get(
                f"{api_base}/repos/{owner}/{repo}/pulls/{number}",
                headers={**headers, "Accept": "application/vnd.github.v3.diff"},
            )
            if diff_res.is_success:
                diff_text = diff_res.text[:_DIFF_MAX_CHARS]

            # ── Changed files (paginated until exhausted, 100/page) ──────────
            changed_files: List[str] = []
            page = 1
            while True:
                files_res = client.get(
                    f"{api_base}/repos/{owner}/{repo}/pulls/{number}/files",
                    headers=headers,
                    params={"per_page": 100, "page": page},
                )
                if not files_res.is_success:
                    break
                batch = [
                    f["filename"]
                    for f in files_res.json()
                    if isinstance(f, dict) and "filename" in f
                ]
                changed_files.extend(batch)
                if len(batch) < 100:
                    break
                page += 1

    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("github: fetch_pr failed for %s", body.url)
        raise HTTPException(
            status_code=502,
            detail=f"Failed to reach GitHub API: {type(exc).__name__}: {exc}",
        )

    return PRFetchResult(
        title=pr.get("title") or "",
        description=pr.get("body") or "",
        branch=(pr.get("head") or {}).get("ref") or "",
        pr_id=str(pr.get("number") or number),
        changed_files=changed_files,
        diff=diff_text,
    )
