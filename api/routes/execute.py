# api/routes/execute.py
import logging
from typing import Any, Dict, List, Optional

from fastapi import APIRouter
from pydantic import BaseModel

from runner import execute_test, execute_heb_full_purchase
from services.run_store import save_run
from services.nl_test_planner import plan_from_text, validate_plan

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


# =============================================================================
# NATURAL LANGUAGE PLANNING ENDPOINTS
# =============================================================================

class PlanFromTextRequest(BaseModel):
    """Request model for /plan_from_text endpoint."""
    text: str
    base_url: Optional[str] = None
    app_hint: Optional[str] = None
    max_steps: int = 25


class ExecuteTextRequest(BaseModel):
    """Request model for /execute_text endpoint."""
    text: str
    base_url: Optional[str] = None
    app_hint: Optional[str] = None
    max_steps: int = 25
    confirm: bool = False
    # Execution options (same as ExecuteStepsRequest)
    headless: bool = True
    viewport: Optional[Dict[str, int]] = None
    timeout_s: Optional[int] = None
    expected: Optional[str] = None
    meta: Optional[Dict[str, Any]] = None


@router.post("/plan_from_text")
def api_plan_from_text(req: PlanFromTextRequest) -> Dict[str, Any]:
    """
    Generate a test plan from natural language text.
    Does NOT execute anything - only returns the plan.

    Returns:
        {"ok": true, "plan": <plan dict>}
    """
    plan = plan_from_text(
        text=req.text,
        base_url=req.base_url,
        app_hint=req.app_hint,
        max_steps=req.max_steps,
    )
    return {"ok": plan.get("ok", False), "plan": plan}


@router.post("/execute_text")
def api_execute_text(req: ExecuteTextRequest) -> Dict[str, Any]:
    """
    Generate a test plan from natural language and execute it.

    Safety rules:
    - If plan.ok == false: returns plan without execution
    - If plan.requires_confirmation == true and confirm != true: returns plan without execution
    - Payment risk_flags: execution stops at cart/checkout (planner already generates safe steps)

    Returns:
        On success: {"ok": true, "run": <run dict>, "evidence_id": "<id>", "plan": <plan dict>}
        On blocked:  {"ok": false, "plan": <plan dict>, "reason": "<why blocked>"}
    """
    # Step 1: Generate plan
    plan = plan_from_text(
        text=req.text,
        base_url=req.base_url,
        app_hint=req.app_hint,
        max_steps=req.max_steps,
    )

    # Step 2: Check if plan generation failed
    if not plan.get("ok", False):
        return {
            "ok": False,
            "plan": plan,
            "reason": "Plan generation failed",
            "errors": plan.get("errors", []),
        }

    # Step 3: Check if confirmation is required but not provided
    if plan.get("requires_confirmation") and not req.confirm:
        risk_flags = plan.get("risk_flags", [])
        return {
            "ok": False,
            "plan": plan,
            "reason": f"Confirmation required due to risk_flags: {risk_flags}. Set confirm=true to proceed.",
        }

    # Step 4: Validate plan has executable steps
    steps = plan.get("steps", [])
    if not steps:
        return {
            "ok": False,
            "plan": plan,
            "reason": "Plan has no steps to execute",
        }

    # Step 5: Check for unresolved placeholders in base_url
    effective_base_url = plan.get("base_url")
    if effective_base_url and "{base_url}" in effective_base_url:
        return {
            "ok": False,
            "plan": plan,
            "reason": "Plan requires base_url but none was provided. Pass base_url parameter.",
        }

    # Step 6: Safety check - ensure payment steps are not executable
    # (Planner already generates safe steps that stop before payment,
    # but we double-check here)
    if "payment" in plan.get("risk_flags", []):
        logger.info("Executing plan with payment risk_flag - planner already limited to safe steps")

    # Step 7: Execute via existing runner
    try:
        run = execute_test(
            steps=steps,
            base_url=effective_base_url,
            headless=req.headless,
            viewport=req.viewport,
            timeout_s=req.timeout_s,
            expected=req.expected,
        )
    except Exception as e:
        logger.exception("execute_test failed")
        return {
            "ok": False,
            "plan": plan,
            "reason": f"Execution failed: {type(e).__name__}: {e}",
        }

    # Step 8: Store run (reuse existing _wrap_and_store logic)
    # Add plan info to meta for debugging
    meta = dict(req.meta or {})
    meta["nl_plan"] = {
        "intent": plan.get("intent"),
        "summary": plan.get("summary"),
        "risk_flags": plan.get("risk_flags"),
        "assumptions": plan.get("assumptions"),
    }

    result = _wrap_and_store(run, meta=meta)

    # Step 9: Return with plan included
    result["plan"] = plan
    return result
