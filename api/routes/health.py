# api/routes/health.py
from fastapi import APIRouter
from fastapi.responses import JSONResponse

router = APIRouter()

@router.get("/health")
def health_get():
    return {"ok": True}

@router.head("/health")
def health_head():
    # HEAD no debe regresar body grande
    return JSONResponse(status_code=200, content=None)