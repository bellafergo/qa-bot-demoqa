# api/routes/execute.py
import logging
from typing import Any, Dict, List, Optional

from fastapi import APIRouter
from pydantic import BaseModel

from runner import execute_test, execute_heb_full_purchase
from services.run_store import save_run

logger = logging.getLogger("vanya")
router = APIRouter()


class ExecuteStepsRequest(BaseModel):
    steps: List[Dict[str, Any]]
    base_url: Optional[str] = None
    headless: bool = True
    viewport: Optional[Dict[str, int]] = None
    timeout_s: Optional[int] = None
    expected: Optional[str] = None
    meta: Optional[Dict[str, Any]] = None  # opcional, para tags/pr/etc


class ExecuteHebRequest(BaseModel):
    headless: bool = True
    viewport: Optional[Dict[str, int]] = None
    timeout_s: Optional[int] = None
    expected: Optional[str] = None
    only_add_to_cart: bool = True

    # ✅ NUEVO: productos (si no mandan nada, usamos los default)
    products: Optional[List[str]] = None

    meta: Optional[Dict[str, Any]] = None


def _wrap_and_store(run: Dict[str, Any], meta: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
    """
    Normaliza payload para UI + guarda en run_store (memoria).
    """
    if not isinstance(run, dict):
        run = {"ok": False, "status": "error", "reason": "Runner returned non-dict"}

    if meta and isinstance(meta, dict):
        run_meta = run.get("meta")
        if not isinstance(run_meta, dict):
            run_meta = {}
        merged = dict(run_meta)
        merged.update(meta)
        run["meta"] = merged

    try:
        save_run(run)
    except Exception as e:
        logger.warning("save_run failed (best-effort): %s: %s", type(e).__name__, e)

    return {"ok": True, "run": run, "evidence_id": run.get("evidence_id")}


@router.post("/execute_steps")
def execute_steps(req: ExecuteStepsRequest) -> Dict[str, Any]:
    run = execute_test(
        steps=req.steps,
        base_url=req.base_url,
        headless=req.headless,
        viewport=req.viewport,
        timeout_s=req.timeout_s,
        expected=req.expected,
    )
    return _wrap_and_store(run, meta=req.meta)


@router.post("/execute_heb")
def execute_heb(req: ExecuteHebRequest) -> Dict[str, Any]:
    # ✅ default seguro si no mandan products
    products = req.products or ["tomate", "coca cola"]

    run = execute_heb_full_purchase(
        headless=req.headless,
        viewport=req.viewport,
        timeout_s=req.timeout_s,
        expected=req.expected,
        only_add_to_cart=req.only_add_to_cart,

        # ✅ IMPORTANTE: tu runner HEB debe aceptar products=
        products=products,
    )
    return _wrap_and_store(run, meta=req.meta)
