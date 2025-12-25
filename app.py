# app.py
import logging

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
# STATIC (Evidence)
# ============================================================
# Si no lo usas, puedes borrar este bloque.
app.mount("/evidence", StaticFiles(directory=str(settings.EVIDENCE_DIR)), name="evidence")


# ============================================================
# MIDDLEWARES
# ============================================================
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.CORS_ORIGINS,  # o settings.CORS_ALLOW_ORIGINS (equivalente)
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# ============================================================
# ERROR HANDLER
# ============================================================
@app.exception_handler(Exception)
async def unhandled_exception_handler(request: Request, exc: Exception):
    logger.error("Unhandled error", exc_info=True)
    return JSONResponse(
        status_code=500,
        content={"detail": f"{type(exc).__name__}: {str(exc)}"},
    )


# ============================================================
# STARTUP
# ============================================================
@app.on_event("startup")
def on_startup():
    # ✅ Solo inicializa DB si existe DATABASE_URL
    if settings.HAS_DB:
        init_db()
        logger.info("DB enabled: init_db() executed")
    else:
        logger.info("DB disabled: DATABASE_URL not set")


# ============================================================
# ROUTERS
# ============================================================
app.include_router(health_router, tags=["health"])
app.include_router(meta_router, tags=["meta"])
app.include_router(threads_router, tags=["threads"])
app.include_router(chat_router, tags=["chat"])