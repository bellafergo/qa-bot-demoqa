# app.py
import logging
import os
from pathlib import Path

from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi.staticfiles import StaticFiles

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
# APP
# ============================================================
app = FastAPI(
    title="Vanya QA Bot",
    version="1.0.0",
)


# ============================================================
# CORS (IMPORTANT)
# ============================================================
def _normalize_origins(origins):
    """
    Normaliza orígenes (quita / final), filtra vacíos y elimina duplicados.
    """
    out = []
    for o in (origins or []):
        if not o:
            continue
        s = str(o).strip()
        if not s:
            continue
        s = s[:-1] if s.endswith("/") else s
        out.append(s)
    # dedupe preservando orden
    seen = set()
    uniq = []
    for o in out:
        if o in seen:
            continue
        seen.add(o)
        uniq.append(o)
    return uniq


# Asegura que SIEMPRE se permita el origin del frontend
DEFAULT_FRONTEND_ORIGINS = [
    "https://valtre-vanya.vercel.app",
    "http://localhost:5173",
    "http://localhost:3000",
]

cors_origins = _normalize_origins(getattr(settings, "CORS_ORIGINS", None) or [])
cors_origins = _normalize_origins(cors_origins + DEFAULT_FRONTEND_ORIGINS)

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
def _safe_mount_static(url_path: str, directory: str, name: str):
    """
    Monta StaticFiles solo si el directorio existe.
    Evita que el app truene si en Render no está la carpeta.
    """
    try:
        p = Path(directory)
        if p.exists() and p.is_dir():
            app.mount(url_path, StaticFiles(directory=str(p)), name=name)
            logger.info(f"Static mounted: {url_path} -> {p}")
        else:
            logger.warning(f"Static NOT mounted (missing dir): {url_path} -> {p}")
    except Exception as e:
        logger.exception(f"Static mount failed: {url_path} -> {directory} ({e})")


# Evidence dir (configurable)
_safe_mount_static("/evidence", str(settings.EVIDENCE_DIR), "evidence")

# Reports dentro de evidence/reports (mantengo tu ruta)
_safe_mount_static("/reports", "evidence/reports", "reports")


# ============================================================
# ERROR HANDLER
# ============================================================
def _is_prod() -> bool:
    """
    Detecta ambiente prod. Ajusta si tu settings maneja otra bandera.
    """
    env = (os.getenv("ENV") or os.getenv("ENVIRONMENT") or "").lower().strip()
    if env in ("prod", "production"):
        return True
    # Render normalmente usa RENDER=true
    if os.getenv("RENDER", "").strip().lower() in ("1", "true", "yes"):
        return True
    return False


@app.exception_handler(Exception)
async def unhandled_exception_handler(request: Request, exc: Exception):
    """
    Importante: En PROD no regresamos el stack/exception al cliente para no filtrar.
    Pero SI loggeamos todo.
    """
    logger.exception("Unhandled error")

    if _is_prod():
        return JSONResponse(
            status_code=500,
            content={"detail": "Internal Server Error"},
        )

    # En dev sí ayudamos con detalle
    return JSONResponse(
        status_code=500,
        content={"detail": f"{type(exc).__name__}: {str(exc)}"},
    )


# ============================================================
# STARTUP
# ============================================================
@app.on_event("startup")
def on_startup():
    # Ajusta logging si quieres ver logs en Render
    logging.basicConfig(level=getattr(logging, (settings.LOG_LEVEL or "INFO").upper(), logging.INFO))

    # ✅ Solo inicializa DB si existe DATABASE_URL
    if settings.HAS_DB:
        init_db()
        logger.info("DB enabled: init_db() executed")
    else:
        logger.info("DB disabled: DATABASE_URL not set")

    logger.info(f"CORS allow_origins = {cors_origins}")


# ============================================================
# ROUTERS
# ============================================================
app.include_router(health_router, tags=["health"])
app.include_router(meta_router, tags=["meta"])
app.include_router(threads_router, tags=["threads"])
app.include_router(chat_router, tags=["chat"])