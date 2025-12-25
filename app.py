# app.py
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from core.settings import settings

# Routers
from api.routes.health import router as health_router
from api.routes.meta import router as meta_router
from api.routes.threads import router as threads_router
from api.routes.chat import router as chat_router

# DB init (si aplica)
from db import init_db

# ============================================================
# APP
# ============================================================

app = FastAPI(
    title="Vanya QA Bot",
    version="1.0.0",
)

# ============================================================
# MIDDLEWARES
# ============================================================

app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.CORS_ALLOW_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ============================================================
# STARTUP
# ============================================================

@app.on_event("startup")
def on_startup():
    init_db()

# ============================================================
# ROUTERS
# ============================================================

app.include_router(health_router, tags=["health"])
app.include_router(meta_router, tags=["meta"])
app.include_router(threads_router, tags=["threads"])
app.include_router(chat_router, tags=["chat"])