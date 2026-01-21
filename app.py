# app.py
from __future__ import annotations

import logging
import os
from pathlib import Path
from typing import List, Optional

from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse, JSONResponse
import html

from core.settings import settings
from db import init_db
from services.run_store import get_run

from api.routes.health import router as health_router
from api.routes.meta import router as meta_router
from api.routes.execute import router as execute_router
from api.routes.threads import router as threads_router
from api.routes.chat import router as chat_router
from api.routes.webhooks import router as webhooks_router  # /webhooks/github vive aquí

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
DEFAULT_FRONTEND_ORIGINS = [
    "https://valtre-vanya.vercel.app",
    "http://localhost:5173",
    "http://localhost:3000",
]

cors_origins = _normalize_origins(DEFAULT_FRONTEND_ORIGINS)

app.add_middleware(
    CORSMiddleware,
    allow_origins=cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


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

    if getattr(settings, "HAS_DB", False):
        init_db()
        logger.info("DB enabled: init_db() executed")
    else:
        logger.info("DB disabled: DATABASE_URL not set")

    logger.info("CORS allow_origins = %s", cors_origins)


# ============================================================
# ENDPOINTS
# ============================================================
@app.get("/runs/{evidence_id}", response_class=HTMLResponse)
def get_run_evidence(evidence_id: str, request: Request, format: str = "html"):
    run = run_store.get_run(evidence_id)
    if not run:
        raise HTTPException(status_code=404, detail="Run not found")

    # Si explícitamente piden JSON
    accept = (request.headers.get("accept") or "").lower()
    if format == "json" or "application/json" in accept:
        return JSONResponse(run)

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
