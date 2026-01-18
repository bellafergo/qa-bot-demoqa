# services/pr_agent.py
from __future__ import annotations

import os
import hmac
import hashlib
import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

import requests

logger = logging.getLogger("vanya.pr_agent")

GITHUB_TOKEN = (os.getenv("GITHUB_TOKEN") or "").strip()
GITHUB_WEBHOOK_SECRET = (os.getenv("GITHUB_WEBHOOK_SECRET") or "").strip()

GITHUB_API_BASE = (os.getenv("GITHUB_API_BASE") or "https://api.github.com").strip().rstrip("/")
DEFAULT_TIMEOUT_S = int((os.getenv("GITHUB_HTTP_TIMEOUT_S") or "30").strip() or "30")


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
    """
    Nota: Para comentar en PR necesitas token.
    (Si no hay token, no llames a endpoints que requieren auth.)
    """
    if not _has_token():
        raise RuntimeError("GITHUB_TOKEN missing")
    return {
        "Authorization": f"Bearer {GITHUB_TOKEN}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }


def _request(method: str, url: str, *, headers: Optional[Dict[str, str]] = None, json: Any = None) -> requests.Response:
    """
    Wrapper con manejo de errores más claro.
    """
    try:
        r = requests.request(method, url, headers=headers, json=json, timeout=DEFAULT_TIMEOUT_S)
        # Si hay rate limit o forbidden, loggea para debug
        if r.status_code in (401, 403):
            logger.warning("GitHub API %s %s -> %s (check token perms / rate limit). body=%s",
                           method, url, r.status_code, _safe_body(r))
        r.raise_for_status()
        return r
    except requests.HTTPError as e:
        logger.exception("GitHub API error: %s %s", method, url)
        raise
    except Exception:
        logger.exception("GitHub API request failed: %s %s", method, url)
        raise


def _safe_body(r: requests.Response) -> str:
    try:
        return (r.text or "")[:600]
    except Exception:
        return "<unreadable>"


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


def post_pr_comment(ctx: PRContext, body: str) -> None:
    """
    Comments on PR using issues comments endpoint.
    Requires token.
    """
    url = f"{GITHUB_API_BASE}/repos/{ctx.owner}/{ctx.repo}/issues/{ctx.pr_number}/comments"
    _request("POST", url, headers=_gh_headers(), json={"body": body})


# ============================================================
# Suite selection (rules-based MVP)
# ============================================================

def select_suites(changed_files: List[Dict[str, Any]]) -> SuiteSelection:
    """
    MVP: mapa simple de archivos -> suites.
    En 2da iteración: LLM + Codeowners + route mapping.
    """
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


def format_comment(ctx: PRContext, suites: SuiteSelection, run_links: Optional[List[str]] = None) -> str:
    run_links = run_links or []
    links_txt = "\n".join([f"- {u}" for u in run_links]) if run_links else "_(pendiente)_"

    draft_txt = " (DRAFT)" if ctx.is_draft else ""
    return (
        f"🛡️ **Vanya PR Agent**\n\n"
        f"**PR:** #{ctx.pr_number}{draft_txt} — {ctx.title}\n"
        f"**Commit:** `{ctx.sha[:7]}`\n"
        f"**Link:** {ctx.html_url}\n\n"
        f"### Suites recomendadas\n"
        f"- **Tags:** `{', '.join(suites.tags)}`\n"
        f"- **Razón:** {suites.reason}\n\n"
        f"### Ejecuciones\n"
        f"{links_txt}\n\n"
        f"> _Siguiente paso: conectar estas tags a tu runner para disparar suites automáticamente._\n"
    )


# ============================================================
# Orchestrator (lo que llamará tu webhooks.py)
# ============================================================

def handle_pull_request_event(payload: Dict[str, Any]) -> Dict[str, Any]:
    """
    - Parse ctx
    - Si draft: ignora
    - Si hay token: baja changed files, selecciona suites, comenta PR
    - Si NO hay token: solo devuelve recomendación (sin comentar)
    """
    ctx = parse_github_pull_request_event(payload)
    if not ctx:
        return {"ok": False, "error": "unsupported_payload"}

    if ctx.is_draft:
        return {"ok": True, "ignored": True, "reason": "draft_pr", "pr": ctx.html_url}

    action = (payload.get("action") or "").strip().lower()
    # Solo cuando se crea/actualiza (evita spam)
    if action and action not in {"opened", "synchronize", "reopened", "ready_for_review"}:
        return {"ok": True, "ignored": True, "action": action}

    if not _has_token():
        # sin token, no podemos consultar files ni comentar
        return {
            "ok": True,
            "commented": False,
            "reason": "missing_token",
            "pr": ctx.html_url,
        }

    files = list_changed_files(ctx)
    suites = select_suites(files)
    comment = format_comment(ctx, suites, run_links=[])

    post_pr_comment(ctx, comment)

    return {"ok": True, "commented": True, "tags": suites.tags, "reason": suites.reason, "pr": ctx.html_url}

<!-- test pr agent --> 