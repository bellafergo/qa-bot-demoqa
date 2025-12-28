# app.py
import logging
import os
import uuid
from pathlib import Path
from typing import List, Optional

from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse, Response
from fastapi.staticfiles import StaticFiles
from starlette.middleware.base import BaseHTTPMiddleware

from core.settings import settings

# Routers
from api.routes.health import router as health_router
from api.routes.meta import router as meta_router
from api.routes.threads import router as threads_router
from api.routes.chat import router as chat_router

# DB init
from db import init_db

logger = logging.getLogger("vanya")


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


def _safe_mount_static(app: FastAPI, url_path: str, directory: str, name: str) -> None:
    try:
        p = Path(directory)
        if p.exists() and p.is_dir():
            app.mount(url_path, StaticFiles(directory=str(p)), name=name)
            logger.info(f"Static mounted: {url_path} -> {p}")
        else:
            logger.warning(f"Static NOT mounted (missing dir): {url_path} -> {p}")
    except Exception:
        logger.exception(f"Static mount failed: {url_path} -> {directory}")


def _is_prod() -> bool:
    # Si DEBUG_ERRORS=1 muestra detalle incluso en Render
    if (os.getenv("DEBUG_ERRORS") or "").strip().lower() in ("1", "true", "yes"):
        return False
    env = (os.getenv("ENV") or os.getenv("ENVIRONMENT") or "").strip().lower()
    return env in ("prod", "production")


def _origin_allowed(origin: str) -> bool:
    origin = (origin or "").strip()
    if not origin:
        return False

    allowed = _normalize_origins(getattr(settings, "CORS_ORIGINS", None) or [])
    if origin in allowed:
        return True

    # Regex opcional (para previews de Vercel, etc.)
    regex = (getattr(settings, "CORS_ORIGIN_REGEX", None) or "").strip()
    if regex:
        try:
            import re
            return re.match(regex, origin) is not None
        except Exception:
            logger.exception("Invalid CORS_ORIGIN_REGEX in settings/env")
            return False

    return False


# ============================================================
# CORS MIDDLEWARE (PRODUCT-GRADE)
# - Agrega headers SIEMPRE (200/400/500) si el Origin está permitido
# - Maneja OPTIONS (preflight) incluso si algún router truena
# ============================================================
class DynamicCORSMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request: Request, call_next):
        origin = (request.headers.get("origin") or "").strip()

        # Preflight
        if request.method == "OPTIONS":
            resp = Response(status_code=204)
            if _origin_allowed(origin):
                resp.headers["Access-Control-Allow-Origin"] = origin
                resp.headers["Access-Control-Allow-Credentials"] = "true"
                resp.headers["Access-Control-Allow-Methods"] = "GET,POST,PUT,PATCH,DELETE,OPTIONS"
                req_headers = request.headers.get("access-control-request-headers")
                resp.headers["Access-Control-Allow-Headers"] = req_headers or "Content-Type, Authorization"
                resp.headers["Vary"] = "Origin"
            return resp

        try:
            resp = await call_next(request)
        except Exception as exc:
            # Si hay excepción, generamos respuesta consistente (y con CORS)
            request_id = str(uuid.uuid4())
            logger.exception(f"Unhandled error request_id={request_id}")
            if _is_prod():
                resp = JSONResponse(status_code=500, content={"detail": "Internal Server Error", "request_id": request_id})
            else:
                resp = JSONResponse(
                    status_code=500,
                    content={"detail": f"{type(exc).__name__}: {str(exc)}", "request_id": request_id},
                )

        # Headers CORS para responses normales y de error
        if _origin_allowed(origin):
            resp.headers["Access-Control-Allow-Origin"] = origin
            resp.headers["Access-Control-Allow-Credentials"] = "true"
            resp.headers["Vary"] = "Origin"

        return resp


# ============================================================
# APP
# ============================================================
app = FastAPI(title="Vanya QA Bot", version="1.0.0")
app.add_middleware(DynamicCORSMiddleware)


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

    logger.info(f"CORS_ORIGINS={getattr(settings, 'CORS_ORIGINS', None)}")
    logger.info(f"CORS_ORIGIN_REGEX={getattr(settings, 'CORS_ORIGIN_REGEX', None)}")


# ============================================================
# ROUTERS
# ============================================================
app.include_router(health_router, tags=["health"])
app.include_router(meta_router, tags=["meta"])
app.include_router(threads_router, tags=["threads"])
app.include_router(chat_router, tags=["chat"])