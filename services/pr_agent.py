# services/pr_agent.py
from __future__ import annotations

import os
import hmac
import hashlib
import json
import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

import requests

from services.pr_runs import trigger_runs  # <-- NUEVO

logger = logging.getLogger("vanya.pr_agent")

# ============================================================
# ENV
# ============================================================

GITHUB_TOKEN = (os.getenv("GITHUB_TOKEN") or "").strip()
GITHUB_WEBHOOK_SECRET = (os.getenv("GITHUB_WEBHOOK_SECRET") or "").strip()
GITHUB_API_BASE = (os.getenv("GITHUB_API_BASE") or "https://api.github.com").strip().rstrip("/")
DEFAULT_TIMEOUT_S = int((os.getenv("GITHUB_HTTP_TIMEOUT_S") or "30").strip() or "30")

ALLOW_UNSIGNED_WEBHOOKS = (os.getenv("ALLOW_UNSIGNED_WEBHOOKS") or "0").strip() == "1"

# LLM (opcional, lo dejamos por si ya lo traes)
LLM_ENABLED = (os.getenv("LLM_ENABLED") or "1").strip() != "0"
LLM_PROVIDER = (os.getenv("LLM_PROVIDER") or "openai").strip().lower()
LLM_MODEL = (os.getenv("LLM_MODEL") or "gpt-4o-mini").strip()
OPENAI_API_KEY = (os.getenv("OPENAI_API_KEY") or "").strip()
OPENAI_API_BASE = (os.getenv("OPENAI_API_BASE") or "https://api.openai.com/v1").strip().rstrip("/")
OPENAI_TIMEOUT_S = int((os.getenv("OPENAI_TIMEOUT_S") or "40").strip() or "40")

MAX_PATCH_CHARS = int((os.getenv("PR_AGENT_MAX_PATCH_CHARS") or "1400").strip() or "1400")
MAX_FILES_FOR_LLM = int((os.getenv("PR_AGENT_MAX_FILES_FOR_LLM") or "40").strip() or "40")
MAX_TOTAL_CONTEXT_CHARS = int((os.getenv("PR_AGENT_MAX_TOTAL_CONTEXT_CHARS") or "24000").strip() or "24000")


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
    api_url: str
    is_draft: bool = False


@dataclass
class SuiteSelection:
    tags: List[str]
    reason: str


@dataclass
class LLMAnalysis:
    summary: str
    risks: List[Dict[str, Any]]
    recommended_tags: List[str]
    reasoning: str


# ============================================================
# Webhook verification (GitHub)
# ============================================================

def verify_github_signature(raw_body: bytes, signature_header: str) -> bool:
    if ALLOW_UNSIGNED_WEBHOOKS:
        logger.warning("ALLOW_UNSIGNED_WEBHOOKS=1; signature verification skipped")
        return True

    if not GITHUB_WEBHOOK_SECRET:
        logger.warning("GITHUB_WEBHOOK_SECRET missing; signature verification skipped (NOT recommended for prod)")
        return True

    if not signature_header or not signature_header.startswith("sha256="):
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

    number = pr.get("number")
    title = pr.get("title") or ""
    html_url = pr.get("html_url") or ""
    api_url = pr.get("url") or ""
    sha = (pr.get("head") or {}).get("sha") or ""
    full_name = repo.get("full_name") or ""
    is_draft = bool(pr.get("draft") is True)

    if not number or not sha or "/" not in full_name:
        return None

    owner, name = full_name.split("/", 1)
    return PRContext(
        provider="github",
        owner=str(owner),
        repo=str(name),
        pr_number=int(number),
        sha=str(sha),
        title=str(title),
        html_url=str(html_url),
        api_url=str(api_url),
        is_draft=is_draft,
    )


# ============================================================
# GitHub API client
# ============================================================

def _has_token() -> bool:
    return bool(GITHUB_TOKEN)

def _gh_headers() -> Dict[str, str]:
    if not _has_token():
        raise RuntimeError("GITHUB_TOKEN missing")
    return {
        "Authorization": f"Bearer {GITHUB_TOKEN}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }

def _safe_body(r: requests.Response) -> str:
    try:
        return (r.text or "")[:800]
    except Exception:
        return "<unreadable>"

def _request(method: str, url: str, *, headers: Optional[Dict[str, str]] = None, json_body: Any = None) -> requests.Response:
    try:
        r = requests.request(method, url, headers=headers, json=json_body, timeout=DEFAULT_TIMEOUT_S)
        if r.status_code in (401, 403):
            logger.warning("GitHub API %s %s -> %s. body=%s", method, url, r.status_code, _safe_body(r))
        r.raise_for_status()
        return r
    except Exception:
        logger.exception("GitHub API error: %s %s", method, url)
        raise

def _parse_next_link(link_header: str) -> Optional[str]:
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

def list_changed_files(ctx: PRContext, *, max_pages: int = 3) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    url = f"{GITHUB_API_BASE}/repos/{ctx.owner}/{ctx.repo}/pulls/{ctx.pr_number}/files?per_page=100"

    for _ in range(max_pages):
        r = _request("GET", url, headers=_gh_headers())
        items = r.json() or []
        if isinstance(items, list):
            out.extend(items)

        link = r.headers.get("Link") or ""
        next_url = _parse_next_link(link)
        if not next_url:
            break
        url = next_url

    return out

def create_pr_comment(ctx: PRContext, body: str) -> int:
    """
    Create comment and return comment_id so we can UPDATE it later.
    """
    url = f"{GITHUB_API_BASE}/repos/{ctx.owner}/{ctx.repo}/issues/{ctx.pr_number}/comments"
    r = _request("POST", url, headers=_gh_headers(), json_body={"body": body})
    data = r.json() or {}
    cid = int(data.get("id") or 0)
    if not cid:
        raise RuntimeError("GitHub did not return comment id")
    return cid

def update_pr_comment(ctx: PRContext, comment_id: int, body: str) -> None:
    url = f"{GITHUB_API_BASE}/repos/{ctx.owner}/{ctx.repo}/issues/comments/{comment_id}"
    _request("PATCH", url, headers=_gh_headers(), json_body={"body": body})


# ============================================================
# Rules-based suite selection (fallback)
# ============================================================

def select_suites_rules(changed_files: List[Dict[str, Any]]) -> SuiteSelection:
    files = [str(f.get("filename") or "") for f in (changed_files or [])]
    files_l = " ".join(files).lower()

    tags: List[str] = []
    reasons: List[str] = []

    def add(tag: str, why: str):
        if tag not in tags:
            tags.append(tag)
        if why not in reasons:
            reasons.append(why)

    if any(x in files_l for x in ["frontend", "src/", "pages/", "components/", ".tsx", ".jsx", ".css", ".scss"]):
        add("ui_smoke", "Cambios en frontend/UI")

    if any(x in files_l for x in ["auth", "login", "session", "oauth", "token"]):
        add("login", "Cambios relacionados a autenticación")

    if any(x in files_l for x in ["checkout", "cart", "payment", "payments", "order", "orders", "promo", "coupon"]):
        add("checkout", "Cambios en flujo de compra/pago/promos")

    if any(x in files_l for x in ["api/", "routes/", "controllers", "graphql", "openapi", "swagger"]):
        add("api_smoke", "Cambios en API/rutas")

    if not tags:
        tags = ["smoke"]
        reasons = ["No se detectó área específica; correr smoke mínimo"]

    return SuiteSelection(tags=tags, reason="; ".join(reasons))


# ============================================================
# Comment formatting (LIVE)
# ============================================================

def format_comment_running(ctx: PRContext, suites: SuiteSelection) -> str:
    return (
        f"🛡️ **Vanya PR Agent**\n\n"
        f"**PR:** #{ctx.pr_number} — {ctx.title}\n"
        f"**Commit:** `{ctx.sha[:7]}`\n"
        f"**Link:** {ctx.html_url}\n\n"
        f"### Suites seleccionadas\n"
        f"- **Tags:** `{', '.join(suites.tags)}`\n"
        f"- **Razón:** {suites.reason}\n\n"
        f"### Estado\n"
        f"🟡 Ejecutando suites... _(en progreso)_\n\n"
        f"> Cuando termine, actualizo este comentario con ✅/❌ y links a evidencia.\n"
    )

def format_comment_results(ctx: PRContext, suites: SuiteSelection, run_summaries: List[Any]) -> str:
    lines: List[str] = []
    ok_all = True

    for rs in (run_summaries or []):
        # rs es RunSummary de pr_runs.py
        status = getattr(rs, "status", "unknown")
        ok = bool(getattr(rs, "ok", False))
        ok_all = ok_all and ok

        tag = getattr(rs, "tag", "suite")
        evidence_id = getattr(rs, "evidence_id", "")
        reason = getattr(rs, "reason", "") or ""
        run_url = getattr(rs, "run_url", "") or ""
        report_url = getattr(rs, "report_url", None)
        evidence_url = getattr(rs, "evidence_url", None)

        icon = "✅" if ok else "❌"
        row = f"- {icon} **{tag}** — `{status}`"
        if evidence_id:
            row += f" — `evid: {evidence_id}`"
        lines.append(row)

        if run_url:
            lines.append(f"  - Run: {run_url}")
        if report_url:
            lines.append(f"  - Report: {report_url}")
        if evidence_url:
            lines.append(f"  - Evidence: {evidence_url}")
        if reason and not ok:
            lines.append(f"  - Reason: {reason}")

    overall = "✅ PASSED" if ok_all else "❌ FAILED"

    return (
        f"🛡️ **Vanya PR Agent**\n\n"
        f"**PR:** #{ctx.pr_number} — {ctx.title}\n"
        f"**Commit:** `{ctx.sha[:7]}`\n"
        f"**Link:** {ctx.html_url}\n\n"
        f"### Suites seleccionadas\n"
        f"- **Tags:** `{', '.join(suites.tags)}`\n"
        f"- **Razón:** {suites.reason}\n\n"
        f"### Resultado global\n"
        f"{overall}\n\n"
        f"### Ejecuciones\n"
        f"{chr(10).join(lines) if lines else '- _(sin ejecuciones)_'}\n\n"
        f"> Siguiente paso: convertir esto en **Status Check** para bloquear merge si falla.\n"
    )


# ============================================================
# Orchestrator (called by webhooks.py)
# ============================================================

def handle_pull_request_event(payload: Dict[str, Any]) -> Dict[str, Any]:
    """
    - Parse ctx
    - If draft: ignore
    - If token: list changed files
    - Select suites (rules)
    - Create "running..." comment
    - Trigger runs
    - Update comment with results (live)
    """
    ctx = parse_github_pull_request_event(payload)
    if not ctx:
        return {"ok": False, "error": "unsupported_payload"}

    action = (payload.get("action") or "").strip().lower()
    if action and action not in {"opened", "synchronize", "reopened", "ready_for_review"}:
        return {"ok": True, "ignored": True, "action": action, "pr": ctx.html_url}

    if ctx.is_draft:
        return {"ok": True, "ignored": True, "reason": "draft_pr", "pr": ctx.html_url}

    if not _has_token():
        return {"ok": True, "commented": False, "reason": "missing_token", "pr": ctx.html_url}

    # 1) files + suites
    files = list_changed_files(ctx)
    suites = select_suites_rules(files)

    # 2) create live comment
    running_body = format_comment_running(ctx, suites)
    comment_id = create_pr_comment(ctx, running_body)

    # 3) trigger runs
    runs = trigger_runs(suites.tags)

    # 4) update same comment
    results_body = format_comment_results(ctx, suites, runs)
    update_pr_comment(ctx, comment_id, results_body)

    # Return for webhook response
    return {
        "ok": True,
        "commented": True,
        "comment_id": comment_id,
        "tags": suites.tags,
        "reason": suites.reason,
        "pr": ctx.html_url,
        "runs": [
            {
                "tag": r.tag,
                "ok": r.ok,
                "status": r.status,
                "evidence_id": r.evidence_id,
                "run_url": r.run_url,
                "report_url": r.report_url,
                "evidence_url": r.evidence_url,
                "duration_ms": r.duration_ms,
                "reason": r.reason,
            }
            for r in runs
        ],
    }
