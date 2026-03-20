# api/routes/planner_routes.py
"""
Natural-language test planner endpoints.

POST /plan_from_text   — convert text → test plan (steps + assertions)
POST /execute_text     — convert text → plan and execute immediately
"""
from __future__ import annotations

import logging
from typing import Any, Dict, Optional

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

from core.step_validator import validate_steps

logger = logging.getLogger("vanya.planner")

router = APIRouter(tags=["planner"])


# ── Request / response models ─────────────────────────────────────────────────

class PlanFromTextRequest(BaseModel):
    text:        str
    base_url:    Optional[str]  = None
    app_hint:    Optional[str]  = None
    max_steps:   int            = 25
    allow_risky: bool           = False


class ExecuteTextRequest(BaseModel):
    text:        str
    base_url:    Optional[str]  = None
    app_hint:    Optional[str]  = None
    max_steps:   int            = 25
    confirm:     bool           = False
    headless:    bool           = True
    allow_risky: bool           = False


# ── Step expansion ───────────────────────────────────────────────────────────

def _expand_abstract_steps(steps: list[dict]) -> list[dict]:
    """
    Expand abstract planner steps into concrete runner-compatible steps.

    Currently handles:
      {"action": "login", "email": "...", "password": "..."}
        → fill #user-name, fill #password, click #login-button  (SauceDemo / generic)

    All other steps are passed through unchanged.
    """
    expanded: list[dict] = []
    for step in steps:
        if step.get("action") == "login" and not any(
            k in step for k in ("selector", "target", "loc")
        ):
            email    = step.get("email")    or step.get("username") or "{EMAIL}"
            password = step.get("password") or "{PASSWORD}"
            expanded += [
                {"action": "fill", "selector": "#user-name", "value": email},
                {"action": "fill", "selector": "#password",  "value": password},
                {"action": "click", "selector": "#login-button"},
            ]
        else:
            expanded.append(step)
    return expanded


# ── Endpoints ─────────────────────────────────────────────────────────────────

@router.post("/plan_from_text")
def plan_from_text_endpoint(req: PlanFromTextRequest) -> Dict[str, Any]:
    """
    Convert natural-language text to a structured test plan.
    Returns the plan dict produced by nl_test_planner.plan_from_text.
    """
    from services.nl_test_planner import plan_from_text
    try:
        result = plan_from_text(
            text     = req.text,
            base_url = req.base_url,
            app_hint = req.app_hint,
            max_steps= req.max_steps,
        )
        return result
    except Exception as exc:
        logger.exception("plan_from_text failed")
        raise HTTPException(status_code=500, detail=str(exc))


@router.post("/execute_text")
def execute_text_endpoint(req: ExecuteTextRequest) -> Dict[str, Any]:
    """
    Convert natural-language text to a plan and execute it immediately.
    Returns { plan, run } where run is the CanonicalRun-like result dict.
    """
    from services.nl_test_planner import plan_from_text
    from runner import execute_test

    # 1 — plan
    try:
        plan = plan_from_text(
            text     = req.text,
            base_url = req.base_url,
            app_hint = req.app_hint,
            max_steps= req.max_steps,
        )
    except Exception as exc:
        logger.exception("execute_text: planning failed")
        raise HTTPException(status_code=500, detail=f"Planning error: {exc}")

    # Honour requires_confirmation guard
    if plan.get("requires_confirmation") and not req.confirm:
        return {
            "plan":   plan,
            "run":    None,
            "reason": plan.get("risk_flags", ["Confirmation required before execution"]),
        }

    steps = plan.get("steps") or []
    if not steps:
        return {"plan": plan, "run": None, "reason": "No executable steps generated"}

    # Expand abstract steps (e.g. login → fill/click) before handing to runner
    steps = _expand_abstract_steps(steps)

    validation = validate_steps(steps)
    if not validation.valid:
        err_msgs = [f"Step {e.step_index}: {e.message}" for e in validation.errors]
        raise HTTPException(
            status_code=400,
            detail={
                "reason": "Invalid steps",
                "errors": [e.model_dump() for e in validation.errors],
                "summary": "; ".join(err_msgs[:5]),
            },
        )

    # 2 — execute
    try:
        run = execute_test(
            steps    = steps,
            base_url = req.base_url or plan.get("base_url"),
            headless = req.headless,
        )
    except Exception as exc:
        logger.exception("execute_text: runner failed")
        return {"plan": plan, "run": None, "reason": f"Runner error: {exc}"}

    return {"plan": plan, "run": run}
