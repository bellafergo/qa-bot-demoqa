# app.py
from __future__ import annotations

import logging
import os
from pathlib import Path
from typing import List, Optional

from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi.staticfiles import StaticFiles

from core.settings import settings
from db import init_db
from services.run_store import get_run

from api.routes.health import router as health_router
from api.routes.meta import router as meta_router
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
@app.get("/runs/{evidence_id}")
def read_run(evidence_id: str):
    r = get_run(evidence_id)
    if not r:
        raise HTTPException(status_code=404, detail="Run not found")
    return {"ok": True, "run": r}


# ============================================================
# ROUTERS
# ============================================================
app.include_router(health_router, tags=["health"])
app.include_router(meta_router, tags=["meta"])
app.include_router(threads_router, tags=["threads"])
app.include_router(chat_router, tags=["chat"])
app.include_router(webhooks_router, tags=["webhooks"])  # /webhooks/github
