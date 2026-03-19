# api/routes/github_routes.py
"""
GitHub integration — minimal PR fetch for PR Impact Analysis.

POST /github/pr/fetch  — fetch PR metadata + changed files from GitHub API by URL
"""
from __future__ import annotations

import logging
import re
from typing import List

import httpx
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

from core.settings import settings

logger = logging.getLogger("vanya.github")

router = APIRouter(prefix="/github", tags=["github"])

_PR_URL_RE = re.compile(
    r"https?://github\.com/(?P<owner>[^/]+)/(?P<repo>[^/]+)/pull/(?P<number>\d+)",
    re.IGNORECASE,
)
_GITHUB_API = "https://api.github.com"


# ── Request / Response ────────────────────────────────────────────────────────

class PRFetchRequest(BaseModel):
    url: str


class PRFetchResult(BaseModel):
    title: str
    description: str
    branch: str
    pr_id: str
    changed_files: List[str]


# ── Helpers ───────────────────────────────────────────────────────────────────

def _require_token() -> str:
    token = settings.GITHUB_TOKEN
    if not token:
        raise HTTPException(
            status_code=503,
            detail="GITHUB_TOKEN is not configured on server",
        )
    return token


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

    Requires the GITHUB_TOKEN environment variable (classic or fine-grained PAT
    with at least `repo:read` scope for private repos; no scope needed for public).
    """
    token  = _require_token()
    owner, repo, number = _parse_pr_url(body.url)

    headers = {
        "Authorization": f"Bearer {token}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }

    try:
        with httpx.Client(timeout=15) as client:
            # ── PR metadata ──────────────────────────────────────────────────
            pr_res = client.get(
                f"{_GITHUB_API}/repos/{owner}/{repo}/pulls/{number}",
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
                    detail="GitHub token is invalid or expired. Regenerate GITHUB_TOKEN.",
                )
            if not pr_res.is_success:
                raise HTTPException(
                    status_code=502,
                    detail=f"GitHub API returned {pr_res.status_code}.",
                )
            pr = pr_res.json()

            # ── Changed files (paginated until exhausted, 100/page) ──────────
            changed_files: List[str] = []
            page = 1
            while True:
                files_res = client.get(
                    f"{_GITHUB_API}/repos/{owner}/{repo}/pulls/{number}/files",
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
        title         = pr.get("title") or "",
        description   = pr.get("body")  or "",
        branch        = (pr.get("head") or {}).get("ref") or "",
        pr_id         = str(pr.get("number") or number),
        changed_files = changed_files,
    )
