# services/pr_agent.py
from __future__ import annotations

import os
import hmac
import hashlib
import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

import requests

logger = logging.getLogger("vanya.pr_agent")

GITHUB_TOKEN = (os.getenv("GITHUB_TOKEN") or "").strip()
GITHUB_WEBHOOK_SECRET = (os.getenv("GITHUB_WEBHOOK_SECRET") or "").strip()

# ============================================================
# Models
# ============================================================

@dataclass
class PRContext:
    provider: str  # "github"
    owner: str
    repo: str
    pr_number: int
    sha: str
    title: str
    html_url: str


@dataclass
class SuiteSelection:
    tags: List[str]
    reason: str


# ============================================================
# Webhook verification (GitHub)
# ============================================================

def verify_github_signature(raw_body: bytes, signature_header: str) -> bool:
    """
    signature_header example: "sha256=..."
    """
    if not GITHUB_WEBHOOK_SECRET:
        logger.warning("GITHUB_WEBHOOK_SECRET missing; signature verification skipped")
        return True

    if not signature_header or "sha256=" not in signature_header:
        return False

    expected = "sha256=" + hmac.new(
        GITHUB_WEBHOOK_SECRET.encode("utf-8"),
        raw_body,
        hashlib.sha256,
    ).hexdigest()

    return hmac.compare_digest(expected, signature_header)


# ============================================================
# Parse PR payload (GitHub)
# ============================================================

def parse_github_pull_request_event(payload: Dict[str, Any]) -> Optional[PRContext]:
    pr = payload.get("pull_request") or {}
    repo = payload.get("repository") or {}

    html_url = pr.get("html_url") or ""
    number = pr.get("number")
    title = pr.get("title") or ""
    sha = (pr.get("head") or {}).get("sha") or ""
    full_name = (repo.get("full_name") or "")  # "owner/repo"

    if not number or not sha or "/" not in full_name:
        return None

    owner, name = full_name.split("/", 1)
    return PRContext(
        provider="github",
        owner=owner,
        repo=name,
        pr_number=int(number),
        sha=str(sha),
        title=str(title),
        html_url=str(html_url),
    )


# ============================================================
# GitHub API
# ============================================================

def _gh_headers() -> Dict[str, str]:
    if not GITHUB_TOKEN:
        raise RuntimeError("GITHUB_TOKEN missing")
    return {
        "Authorization": f"Bearer {GITHUB_TOKEN}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }


def list_changed_files(ctx: PRContext, *, max_pages: int = 3) -> List[Dict[str, Any]]:
    """
    Returns GitHub PR files list entries (filename, status, patch, etc.)
    """
    out: List[Dict[str, Any]] = []
    url = f"https://api.github.com/repos/{ctx.owner}/{ctx.repo}/pulls/{ctx.pr_number}/files?per_page=100"

    for _ in range(max_pages):
        r = requests.get(url, headers=_gh_headers(), timeout=30)
        r.raise_for_status()
        items = r.json() or []
        out.extend(items)

        # pagination (Link header)
        link = r.headers.get("Link") or ""
        next_url = _parse_next_link(link)
        if not next_url:
            break
        url = next_url

    return out


def _parse_next_link(link_header: str) -> Optional[str]:
    # Very small parser for: <url>; rel="next"
    if not link_header:
        return None
    parts = [p.strip() for p in link_header.split(",")]
    for p in parts:
        if 'rel="next"' in p:
            start = p.find("<")
            end = p.find(">")
            if start >= 0 and end > start:
                return p[start + 1 : end]
    return None


def post_pr_comment(ctx: PRContext, body: str) -> None:
    url = f"https://api.github.com/repos/{ctx.owner}/{ctx.repo}/issues/{ctx.pr_number}/comments"
    r = requests.post(url, headers=_gh_headers(), json={"body": body}, timeout=30)
    r.raise_for_status()


# ============================================================
# Suite selection (rules-based MVP)
# ============================================================

def select_suites(changed_files: List[Dict[str, Any]]) -> SuiteSelection:
    """
    MVP: mapa simple de archivos -> suites.
    En 2da iteración: LLM + Codeowners + route mapping.
    """
    files = [str(f.get("filename") or "") for f in changed_files]
    files_l = " ".join(files).lower()

    tags: List[str] = []
    reasons: List[str] = []

    def add(tag: str, why: str):
        if tag not in tags:
            tags.append(tag)
        reasons.append(why)

    # UI / Frontend
    if any(x in files_l for x in ["frontend", "src/", "pages/", "components/", ".tsx", ".jsx", ".css", ".scss"]):
        add("ui_smoke", "Cambios en frontend/UI")
        add("visual_regression", "Cambios en UI pueden afectar layout")

    # Auth / login
    if any(x in files_l for x in ["auth", "login", "session", "oauth", "token"]):
        add("login", "Cambios relacionados a autenticación")

    # Checkout / payments
    if any(x in files_l for x in ["checkout", "cart", "payment", "payments", "order", "orders", "promo", "coupon"]):
        add("checkout", "Cambios en flujo de compra/pago/promos")

    # API changes
    if any(x in files_l for x in ["api/", "routes/", "controllers", "graphql", "openapi", "swagger"]):
        add("api_smoke", "Cambios en API/rutas")

    # Default minimum
    if not tags:
        tags = ["smoke"]
        reasons = ["No se detectó área específica; correr smoke mínimo"]

    return SuiteSelection(tags=tags, reason="; ".join(reasons))


def format_comment(ctx: PRContext, suites: SuiteSelection, run_links: Optional[List[str]] = None) -> str:
    run_links = run_links or []
    links_txt = "\n".join([f"- {u}" for u in run_links]) if run_links else "_(pendiente)_"

    return (
        f"🛡️ **Vanya PR Agent**\n\n"
        f"**PR:** #{ctx.pr_number} — {ctx.title}\n"
        f"**Commit:** `{ctx.sha[:7]}`\n\n"
        f"### Suites recomendadas\n"
        f"- **Tags:** `{', '.join(suites.tags)}`\n"
        f"- **Razón:** {suites.reason}\n\n"
        f"### Ejecuciones\n"
        f"{links_txt}\n"
    )
