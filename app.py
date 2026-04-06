# app.py
from __future__ import annotations

import logging
import os
import uuid
from pathlib import Path
from typing import List, Optional

from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse, JSONResponse
import html

from core.settings import settings
from core.auth_middleware import VanyaAuthMiddleware
from core.rate_limit_middleware import RateLimitMiddleware
from core.vanya_auth import (
    auth_enforcement_enabled,
    docs_allowed_in_this_deployment,
    log_auth_startup_warnings,
)
from db import init_db
from services.run_store import get_run
from services.run_history_service import run_history_service

from api.routes.health import router as health_router
from api.routes.meta import router as meta_router
from api.routes.execute import router as execute_router
from api.routes.threads import router as threads_router
from api.routes.chat import router as chat_router
from api.routes.webhooks import router as webhooks_router  # /webhooks/github vive aquí
from api.routes.documents import router as documents_router
from api.routes.test_catalog_routes import router as test_catalog_router
from api.routes.test_catalog_routes import runs_router as test_runs_router
from api.routes.test_orchestrator_routes import router as orchestrator_router
from api.routes.dashboard_routes import router as dashboard_router
from api.routes.pr_analysis_routes import router as pr_analysis_router
from api.routes.test_generation_routes import router as test_generation_router
from api.routes.test_data_routes import router as test_data_router
from api.routes.rca_routes import router as rca_router
from api.routes.business_risk_routes import router as business_risk_router
from api.routes.coverage_routes import router as coverage_router
from api.routes.risk_selection_routes import router as risk_selection_router
from api.routes.api_testing_routes import router as api_testing_router
from api.routes.execution_routes import router as execution_router
from api.routes.failure_intelligence_routes import router as failure_intelligence_router
from api.routes.integrations_routes import router as integrations_router
from api.routes.app_explorer_routes import router as app_explorer_router
from api.routes.drafts_routes import router as drafts_router
from api.routes.github_routes import router as github_router
from api.routes.analytics_routes import router as analytics_router
from api.routes.planner_routes import router as planner_router
from api.routes.evidence_routes import router as evidence_router
from api.routes.project_routes import router as project_router

logger = logging.getLogger("vanya")

# ============================================================
# APP
# ============================================================
app = FastAPI(title="Vanya QA Bot", version="1.0.0")


# ============================================================
# HELPERS
# ============================================================
def _normalize_origins(origins: Optional[List[str]]) -> List[str]:
    out: List[str] = []
    for o in (origins or []):
        if not o:
            continue
        s = str(o).strip()
        if not s:
            continue
        s = s[:-1] if s.endswith("/") else s
        out.append(s)

    seen = set()
    uniq: List[str] = []
    for o in out:
        if o in seen:
            continue
        seen.add(o)
        uniq.append(o)
    return uniq


def _safe_mount_static(app_: FastAPI, url_path: str, directory: str, name: str) -> None:
    try:
        p = Path(directory)
        if p.exists() and p.is_dir():
            app_.mount(url_path, StaticFiles(directory=str(p)), name=name)
            logger.info("Static mounted: %s -> %s", url_path, p)
        else:
            logger.warning("Static NOT mounted (missing dir): %s -> %s", url_path, p)
    except Exception:
        logger.exception("Static mount failed: %s -> %s", url_path, directory)


def _is_prod() -> bool:
    if (os.getenv("DEBUG_ERRORS") or "").strip().lower() in ("1", "true", "yes"):
        return False

    env = (os.getenv("ENV") or os.getenv("ENVIRONMENT") or "").lower().strip()
    if env in ("prod", "production"):
        return True

    if (os.getenv("RENDER") or "").strip().lower() in ("1", "true", "yes"):
        return True

    return False


def _apply_cors_headers_if_needed(request: Request, response: JSONResponse, allowed_origins: List[str]) -> None:
    origin = (request.headers.get("origin") or "").strip()
    if origin and origin in allowed_origins:
        response.headers["Access-Control-Allow-Origin"] = origin
        response.headers["Access-Control-Allow-Credentials"] = "true"
        response.headers["Vary"] = "Origin"


# ============================================================
# CORS
# ============================================================
# Origins and optional regex are fully driven by environment variables.
# Set CORS_ORIGINS="https://your-app.vercel.app,http://localhost:5173" on Render.
# Set CORS_ORIGIN_REGEX="^https://.*-zuperio-vanya\\.vercel\\.app$" to allow preview deploys.
cors_origins = _normalize_origins(settings.CORS_ORIGINS)
cors_origin_regex = settings.CORS_ORIGIN_REGEX or None

# Starlette: last add_middleware = outermost (runs first on the request). Request path must be:
#   CORS → VanyaAuth (sets request.state) → RateLimit (uses state for per-user buckets) → routes
# so that 401/403 from auth still pass back through CORS for browser Access-Control-Allow-Origin.
# Add order (inner → outer): RateLimit, VanyaAuth, CORS.
app.add_middleware(RateLimitMiddleware)
app.add_middleware(VanyaAuthMiddleware)
app.add_middleware(
    CORSMiddleware,
    allow_origins=cors_origins,
    allow_origin_regex=cors_origin_regex,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# OpenAPI UI — only in non-prod-style deployments (see core.vanya_auth.docs_allowed_in_this_deployment).
if not docs_allowed_in_this_deployment():
    app.openapi_url = None
    app.docs_url = None
    app.redoc_url = None


# ============================================================
# REQUEST ID (correlation) — trazabilidad end-to-end
# ============================================================
@app.middleware("http")
async def request_id_middleware(request: Request, call_next):
    rid = request.headers.get("x-request-id") or uuid.uuid4().hex[:12]
    request.state.request_id = rid
    # Optional multi-client partitioning metadata (no auth/RBAC yet).
    request.state.client_id = request.headers.get("x-client-id")
    request.state.workspace_id = request.headers.get("x-workspace-id")
    response = await call_next(request)
    response.headers["x-request-id"] = rid
    return response


# ============================================================
# STATIC (Evidence/Reports)
# ============================================================
try:
    Path(str(settings.EVIDENCE_DIR)).mkdir(parents=True, exist_ok=True)
    Path("evidence/reports").mkdir(parents=True, exist_ok=True)
    logger.info("Runtime directories ensured: evidence/, evidence/reports/")
except Exception:
    logger.exception("Failed to create runtime evidence directories")

_safe_mount_static(app, "/evidence", str(settings.EVIDENCE_DIR), "evidence")
_safe_mount_static(app, "/reports", "evidence/reports", "reports")


# ============================================================
# ERROR HANDLER
# ============================================================
@app.exception_handler(Exception)
async def unhandled_exception_handler(request: Request, exc: Exception):
    logger.exception("Unhandled error")

    if _is_prod():
        response = JSONResponse(status_code=500, content={"detail": "Internal Server Error"})
    else:
        response = JSONResponse(status_code=500, content={"detail": f"{type(exc).__name__}: {str(exc)}"})

    _apply_cors_headers_if_needed(request, response, cors_origins)
    return response


# ============================================================
# STARTUP
# ============================================================
@app.on_event("startup")
def on_startup():
    env_level = (os.getenv("LOG_LEVEL") or "INFO").upper().strip()
    level = getattr(logging, env_level, logging.INFO)
    logging.basicConfig(level=level)

    # Legacy SQLAlchemy DB (threads / messages)
    if getattr(settings, "HAS_DB", False):
        init_db()
        logger.info("DB enabled: init_db() executed")
    else:
        logger.info("DB disabled: DATABASE_URL not set")

    logger.info("CORS allow_origins = %s", cors_origins)
    log_auth_startup_warnings()
    if auth_enforcement_enabled():
        logger.info("Vanya auth enforcement: ENABLED (Supabase JWT + optional service token)")
    else:
        logger.warning(
            "Vanya auth enforcement: DISABLED — set SUPABASE_URL and VANYA_AUTH_ENABLED=1 for production (JWKS / ES256)"
        )

    if (os.getenv("RATE_LIMIT_ENABLED") or "").strip().lower() in ("1", "true", "yes", "on"):
        logger.info("Rate limiting: ENABLED (in-memory; not shared across workers)")
    else:
        logger.info("Rate limiting: disabled (set RATE_LIMIT_ENABLED=1 to enable)")

    # Initialize catalog / runs / jobs SQLite DB
    try:
        from services.db.init_db import init_catalog_db
        init_catalog_db()
    except Exception:
        logger.exception("catalog db: init failed (non-fatal)")

    # Load demo seed test cases — skipped if catalog table is already populated
    try:
        from services.test_catalog_service import load_seed_catalog
        load_seed_catalog()
    except Exception:
        logger.exception("test_catalog: seed load failed (non-fatal)")

    # Start orchestrator background worker
    try:
        from services.catalog_orchestrator import ensure_worker_started
        ensure_worker_started()
    except Exception:
        logger.exception("orchestrator: worker start failed (non-fatal)")


# ============================================================
# ENDPOINTS
# ============================================================
@app.get("/runs/{evidence_id}", response_class=HTMLResponse)
def get_run_evidence(evidence_id: str, request: Request, format: str = "html"):
    # 1) short-term in-memory store (chat / execute / async runs)
    run = get_run(evidence_id)

    # 2) fallback: official SQLite history (catalog / orchestrator runs)
    if not run:
        canonical = run_history_service.get_run(evidence_id)
        if canonical:
            # Adapt CanonicalRun → flat dict expected by the HTML renderer and
            # run_from_legacy_store.  model_dump() converts nested Pydantic models
            # (RunMeta, RunArtifacts) to plain dicts automatically.
            d = canonical.model_dump()
            arts: dict = d.pop("artifacts", None) or {}

            # Flatten artifact URLs to top level (run_from_legacy_store reads here)
            # and also into meta (HTML renderer reads meta.get("evidence_url")).
            meta_d: dict = d.get("meta") or {}
            for url_key in ("evidence_url", "report_url"):
                val = arts.get(url_key)
                if val:
                    d[url_key] = val
                    meta_d[url_key] = val
            d["meta"] = meta_d

            # Flatten screenshot_b64 to top level (HTML renderer + run_from_legacy_store)
            if arts.get("screenshot_b64") and not d.get("screenshot_b64"):
                d["screenshot_b64"] = arts["screenshot_b64"]

            # Map error_summary → reason (HTML renderer reads run.get("reason"))
            if not d.get("reason") and d.get("error_summary"):
                d["reason"] = d["error_summary"]

            # Ensure evidence_id alias is present
            d.setdefault("evidence_id", d.get("run_id") or evidence_id)

            run = d

    if not run:
        raise HTTPException(status_code=404, detail="Run not found")

    # Si explícitamente piden JSON — devuelve CanonicalRun + extras legacy
    accept = (request.headers.get("accept") or "").lower()
    if format == "json" or "application/json" in accept:
        from services.run_mapper import run_from_legacy_store
        canonical = run_from_legacy_store(run)
        # Merge: canonical fields take priority; execution-specific extras
        # (failure_analysis, resolution_log, reason, outcome, expected, logs, steps)
        # are preserved from the original dict for backward compatibility.
        merged = {**run, **canonical.model_dump()}
        # Preserve evidence_id as alias — frontend uses it for display.
        # run_id already holds the same value, but keep both for compat.
        merged["evidence_id"] = run.get("evidence_id") or merged.get("run_id")
        return JSONResponse(merged)

    # Helpers
    def _img_data_uri(b64: str) -> str:
        b64 = (b64 or "").strip()
        if not b64:
            return ""
        # tu take_screenshot_robust casi siempre regresa PNG base64
        return f"data:image/png;base64,{b64}"

    steps = run.get("steps") or []
    logs = run.get("logs") or []
    meta = run.get("meta") or {}
    reason = run.get("reason") or ""
    status = run.get("status") or "unknown"
    duration_ms = run.get("duration_ms")

    # Si en algún punto guardas URLs ya subidas (Cloudinary), muéstralas:
    evidence_url = None
    report_url = None
    # intenta encontrarlas en varios lugares típicos
    if isinstance(meta, dict):
        evidence_url = meta.get("evidence_url") or meta.get("screenshot_url") or meta.get("evidenceUrl")
        report_url = meta.get("report_url") or meta.get("report_pdf_url") or meta.get("reportUrl")

    # screenshot principal: usa screenshot_b64 si existe, si no el último step
    main_b64 = (run.get("screenshot_b64") or "").strip()
    if not main_b64 and steps and isinstance(steps, list):
        last = steps[-1] if isinstance(steps[-1], dict) else {}
        main_b64 = (last.get("screenshot_b64") or "").strip()

    main_img = _img_data_uri(main_b64)

    # HTML
    def esc(x):
        return html.escape(str(x or ""))

    items_html = []
    if isinstance(steps, list):
        for i, st in enumerate(steps, start=1):
            if not isinstance(st, dict):
                continue
            name = esc(st.get("name") or f"step_{i}")
            b64 = (st.get("screenshot_b64") or "").strip()
            if not b64:
                continue
            img = _img_data_uri(b64)
            items_html.append(
                f"""
                <div style="margin:16px 0; padding:12px; border:1px solid #eee; border-radius:10px;">
                  <div style="font-weight:600; margin-bottom:8px;">{name}</div>
                  <img src="{img}" style="max-width:100%; border-radius:10px; border:1px solid #ddd;" />
                </div>
                """
            )

    logs_html = ""
    if isinstance(logs, list) and logs:
        safe_logs = "\n".join([str(x) for x in logs][-400:])  # evita páginas enormes
        logs_html = f"""
        <h3>Logs</h3>
        <pre style="white-space:pre-wrap; background:#0b0f19; color:#d6deeb; padding:12px; border-radius:10px; overflow:auto;">
{esc(safe_logs)}
        </pre>
        """

    links_html = []
    if evidence_url:
        links_html.append(f'<a href="{esc(evidence_url)}" target="_blank">Evidence URL</a>')
    if report_url:
        links_html.append(f'<a href="{esc(report_url)}" target="_blank">Report URL</a>')
    links_html.append(f'<a href="/runs/{esc(evidence_id)}?format=json" target="_blank">View JSON</a>')

    links_line = " | ".join(links_html)

    html_out = f"""
    <html>
      <head>
        <meta charset="utf-8" />
        <title>Run {esc(evidence_id)} — Evidence</title>
      </head>
      <body style="font-family: ui-sans-serif, system-ui, -apple-system; margin:24px; max-width:1100px;">
        <h2>Run Evidence: {esc(evidence_id)}</h2>

        <div style="margin:10px 0; padding:12px; border:1px solid #eee; border-radius:10px;">
          <div><b>Status:</b> {esc(status)} </div>
          <div><b>Duration:</b> {esc(duration_ms)} ms</div>
          <div><b>Reason:</b> {esc(reason)}</div>
          <div style="margin-top:8px;">{links_line}</div>
        </div>

        {"<h3>Última captura</h3><img src='"+main_img+"' style='max-width:100%; border-radius:10px; border:1px solid #ddd;' />" if main_img else "<p>No screenshot_b64 disponible.</p>"}

        <h3>Steps</h3>
        {''.join(items_html) if items_html else "<p>No steps con screenshots disponibles.</p>"}

        {logs_html}
      </body>
    </html>
    """
    return HTMLResponse(content=html_out, status_code=200)


# ============================================================
# ROUTERS
# ============================================================
app.include_router(health_router, tags=["health"])
app.include_router(meta_router, tags=["meta"])
app.include_router(threads_router, tags=["threads"])
app.include_router(chat_router, tags=["chat"])
app.include_router(webhooks_router, tags=["webhooks"])  # /webhooks/github
app.include_router(execute_router, tags=["execute"])
app.include_router(documents_router, tags=["documents"])
app.include_router(project_router)        # GET/POST/PATCH/DELETE /projects
app.include_router(test_catalog_router)   # GET/POST /tests, POST /tests/{id}/run, POST /tests/run-suite
app.include_router(test_runs_router)      # GET /test-runs, GET /test-runs/{run_id}
app.include_router(orchestrator_router)   # POST /orchestrator/jobs/single|suite, GET /orchestrator/jobs
app.include_router(dashboard_router)      # GET /dashboard/summary, /recent-runs, /by-module, …
app.include_router(pr_analysis_router)    # POST /pr-analysis/analyze, /analyze-and-enqueue, …
app.include_router(test_generation_router)  # POST /test-generation/generate, /approve, …
app.include_router(test_data_router)        # POST /test-data/generate, GET /test-data/entity-types
app.include_router(rca_router)              # POST /rca/analyze, GET /rca/analyze/{run_id}
app.include_router(business_risk_router)    # POST /business-risk/analyze
app.include_router(coverage_router)         # GET /coverage/summary, /coverage/module/{module}
app.include_router(risk_selection_router)   # POST /risk-selection/select-tests, /select-and-run
app.include_router(api_testing_router)      # POST /api-testing/parse-spec, /generate-tests, /approve, /run
app.include_router(execution_router)        # GET /execution/health|status, POST /execution/run-batch|retry-failed
app.include_router(failure_intelligence_router)  # GET /failure-intelligence/summary|clusters|flaky-tests|regressions
app.include_router(integrations_router)          # GET|POST /integrations, /integrations/{id}/health-check|enable|disable|config|actions
app.include_router(app_explorer_router)          # GET /app-explorer/health, POST /app-explorer/explore
app.include_router(drafts_router)                # POST /drafts/generate, POST /drafts/approve
app.include_router(github_router)               # POST /github/pr/fetch
app.include_router(analytics_router)           # GET /analytics/runs/dashboard
app.include_router(planner_router)             # POST /plan_from_text, POST /execute_text
app.include_router(evidence_router)           # GET /evidences
