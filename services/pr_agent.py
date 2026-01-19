# services/pr_agent.py
from __future__ import annotations

import hashlib
import hmac
import logging
import os
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

import requests

logger = logging.getLogger("vanya.pr_agent")

# ============================================================
# ENV
# ============================================================

GITHUB_TOKEN = (os.getenv("GITHUB_TOKEN") or "").strip()
GITHUB_WEBHOOK_SECRET = (os.getenv("GITHUB_WEBHOOK_SECRET") or "").strip()

GITHUB_API_BASE = (os.getenv("GITHUB_API_BASE") or "https://api.github.com").strip().rstrip("/")
DEFAULT_TIMEOUT_S = int((os.getenv("GITHUB_HTTP_TIMEOUT_S") or "30").strip() or "30")

# Si quieres “solo comentar” sin ejecutar runs:
PR_AGENT_EXECUTE_RUNS = (os.getenv("PR_AGENT_EXECUTE_RUNS") or "true").strip().lower() in ("1", "true", "yes", "y")

# Evitar spam: solo reaccionar a estos actions
ALLOWED_PR_ACTIONS = {"opened", "synchronize", "reopened", "ready_for_review"}

# Marker que pr_runs usará para inyectar el bloque live
RUNS_MARKER = "<!-- VANYA_RUNS -->"


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
    api_url = pr.get("url") or ""  # API URL del PR
    sha = (pr.get("head") or {}).get("sha") or ""
    full_name = repo.get("full_name") or ""  # "owner/repo"
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
        return (r.text or "")[:600]
    except Exception:
        return "<unreadable>"


def _request(
    method: str,
    url: str,
    *,
    headers: Optional[Dict[str, str]] = None,
    json: Any = None,
) -> requests.Response:
    """
    Wrapper con logging útil (auth / rate limit).
    """
    try:
        r = requests.request(method, url, headers=headers, json=json, timeout=DEFAULT_TIMEOUT_S)
        if r.status_code in (401, 403):
            logger.warning(
                "GitHub API %s %s -> %s (check token perms / rate limit). body=%s",
                method,
                url,
                r.status_code,
                _safe_body(r),
            )
        r.raise_for_status()
        return r
    except requests.HTTPError:
        logger.exception("GitHub API error: %s %s", method, url)
        raise
    except Exception:
        logger.exception("GitHub API request failed: %s %s", method, url)
        raise


def _parse_next_link(link_header: str) -> Optional[str]:
    """
    Very small parser for: <url>; rel="next"
    """
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
    """
    Returns GitHub PR files list entries (filename, status, patch, etc.)
    Requires token.
    """
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


def post_pr_comment(ctx: PRContext, body: str) -> int:
    """
    Crea comentario en PR y devuelve comment_id.
    """
    url = f"{GITHUB_API_BASE}/repos/{ctx.owner}/{ctx.repo}/issues/{ctx.pr_number}/comments"
    r = _request("POST", url, headers=_gh_headers(), json={"body": body})
    data = r.json() or {}
    return int(data.get("id") or 0)


def update_pr_comment(ctx: PRContext, comment_id: int, body: str) -> None:
    """
    Edita comentario existente (para “live updates”).
    """
    if not comment_id:
        return
    url = f"{GITHUB_API_BASE}/repos/{ctx.owner}/{ctx.repo}/issues/comments/{comment_id}"
    _request("PATCH", url, headers=_gh_headers(), json={"body": body})


# ============================================================
# Suite selection (rules-based MVP)
# ============================================================

def select_suites(changed_files: List[Dict[str, Any]]) -> SuiteSelection:
    files = [str(f.get("filename") or "") for f in (changed_files or [])]
    files_l = " ".join(files).lower()

    tags: List[str] = []
    reasons: List[str] = []

    def add(tag: str, why: str):
        if tag not in tags:
            tags.append(tag)
        if why not in reasons:
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


# ============================================================
# Comment rendering (LIVE)
# ============================================================

def format_comment_live(
    ctx: PRContext,
    suites: SuiteSelection,
    *,
    state_line: str = "⏳ Preparando ejecuciones…",
) -> str:
    """
    Comentario base. El bloque de ejecuciones “live” lo inserta/actualiza pr_runs
    usando el marcador RUNS_MARKER.
    """
    draft_txt = " (DRAFT)" if ctx.is_draft else ""
    tags_txt = ", ".join(suites.tags) if suites.tags else "smoke"

    return (
        f"🛡️ **Vanya PR Agent**\n\n"
        f"**PR:** #{ctx.pr_number}{draft_txt} — {ctx.title}\n"
        f"**Commit:** `{ctx.sha[:7]}`\n"
        f"**Link:** {ctx.html_url}\n\n"
        f"### Suites recomendadas\n"
        f"- **Tags:** `{tags_txt}`\n"
        f"- **Razón:** {suites.reason}\n\n"
        f"### Estado\n"
        f"{state_line}\n\n"
        f"{RUNS_MARKER}\n"
    )


# ============================================================
# Orchestrator (called by webhooks.py)
# ============================================================

def handle_pull_request_event(payload: Dict[str, Any]) -> Dict[str, Any]:
    """
    - Parse ctx
    - Ignora drafts
    - Filtra actions para no spamear
    - Baja changed files, selecciona suites
    - Comenta PR (comment_id)
    - (Opcional) dispara runs en background y hace updates live
    """
    ctx = parse_github_pull_request_event(payload)
    if not ctx:
        return {"ok": False, "error": "unsupported_payload"}

    # Evita drafts
    if ctx.is_draft:
        return {"ok": True, "ignored": True, "reason": "draft_pr", "pr": ctx.html_url}

    action = (payload.get("action") or "").strip().lower()
    if action and action not in ALLOWED_PR_ACTIONS:
        return {"ok": True, "ignored": True, "action": action}

    if not _has_token():
        return {"ok": True, "commented": False, "reason": "missing_token", "pr": ctx.html_url}

    # 1) Analiza files y sugiere suites
    files = list_changed_files(ctx)
    suites = select_suites(files)

    # 2) Comentario inicial (marker para live runs)
    state = (
        "⏳ Ejecutando suites detectadas…"
        if (PR_AGENT_EXECUTE_RUNS and suites.tags)
        else "📝 Recomendación generada (runs desactivados)."
    )
    initial_body = format_comment_live(ctx, suites, state_line=state)
    comment_id = post_pr_comment(ctx, initial_body)

    # 3) Disparar runs en background (si aplica)
    run_ids: List[str] = []
    if PR_AGENT_EXECUTE_RUNS and suites.tags:
        try:
            from services.pr_runs import trigger_runs_for_tags  # lazy import
            run_ids = trigger_runs_for_tags(
                ctx,
                suites.tags,
                comment_id=comment_id,
                comment_body_template=initial_body,
            )
        except Exception:
            logger.exception("Failed to trigger runs")
            # Aviso en comentario
            try:
                fail_body = format_comment_live(
                    ctx,
                    suites,
                    state_line="⚠️ No se pudieron disparar runs (revisar logs/runner).",
                )
                update_pr_comment(ctx, comment_id, fail_body)
            except Exception:
                pass

    return {
        "ok": True,
        "commented": True,
        "comment_id": comment_id,
        "tags": suites.tags,
        "reason": suites.reason,
        "run_ids": run_ids,
        "pr": ctx.html_url,
    }
