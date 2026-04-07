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
from fastapi import Request
from pydantic import BaseModel

from core.step_compiler import (
    CompileError,
    augment_steps_with_prompt_assertions,
    compile_to_runner_steps,
    ensure_has_assert,
)
from core.step_validator import validate_steps
from services.run_mapper import normalize_run

logger = logging.getLogger("vanya.planner")

router = APIRouter(tags=["planner"])


def _merge_runner_result_with_canonical(
    raw_run: Any,
    *,
    correlation_id: Optional[str],
) -> Any:
    """
    Enrich run (runner-like dict from runners/generic_steps.execute_test) so it
    is much closer to CanonicalRun, without breaking existing frontend usage.

    Strategy:
    - Keep the original keys (reason/ok/outcome/runner meta/evidence bundle)
    - Add CanonicalRun fields (run_id, steps_count, error_summary, artifacts, ...)
    - Preserve the runner's original `meta` under the merged `meta` dict to
      avoid removing potentially useful legacy keys.
    """
    if not isinstance(raw_run, dict):
        return raw_run

    payload: Dict[str, Any] = dict(raw_run)

    raw_meta: Dict[str, Any] = dict(payload.get("meta") or {})
    if correlation_id and not raw_meta.get("correlation_id"):
        raw_meta["correlation_id"] = correlation_id
    # Let run_mapper produce meta.trigger_source="api" instead of default "chat".
    raw_meta.setdefault("trigger_source", "api")
    payload["meta"] = raw_meta
    payload.setdefault("source", "api")

    status_raw = str(payload.get("status") or "").strip().lower()
    if status_raw in ("failed", "error"):
        # run_mapper uses `error_message` to populate CanonicalRun.error_summary
        payload.setdefault("error_message", payload.get("reason") or "Run failed")

    try:
        canonical = normalize_run(payload)  # -> CanonicalRun
        canonical_dict = canonical.model_dump()

        # Merge canonical meta with runner meta so we don't delete legacy keys.
        merged_meta = dict(canonical_dict.get("meta") or {})
        merged_meta.update(raw_meta)
        canonical_dict["meta"] = merged_meta

        # Combine: keep legacy runner keys + add canonical keys
        return {**raw_run, **canonical_dict}
    except Exception:
        # Never break endpoint contract; best-effort enrichment only.
        return raw_run


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


# Step expansion: use step_compiler (see core/step_compiler.py)


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
def execute_text_endpoint(req: ExecuteTextRequest, request: Request) -> Dict[str, Any]:
    """
    Convert natural-language text to a plan and execute it immediately.
    Returns { plan, run } where run is the CanonicalRun-like result dict.
    """
    from services.nl_test_planner import plan_from_text
    from runner import execute_test

    correlation_id = getattr(request.state, "request_id", None)

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
            "correlation_id": correlation_id,
            "error_type": "confirmation_required",
        }

    steps = plan.get("steps") or []
    if not steps:
        return {
            "plan": plan,
            "run": None,
            "reason": "No executable steps generated",
            "correlation_id": correlation_id,
            "error_type": "no_steps",
        }

    base_url = req.base_url or plan.get("base_url")

    # Compile planner DSL → runner steps
    try:
        steps = compile_to_runner_steps(
            plan_steps=steps,
            base_url=base_url,
            context={"base_url": base_url, "app_hint": req.app_hint},
        )
        steps = augment_steps_with_prompt_assertions(req.text, steps, base_url or "")
        steps = ensure_has_assert(steps, base_url or "")
    except CompileError as e:
        logger.warning("execute_text: compile failed — %s", e)
        compile_dict = e.to_dict() if hasattr(e, "to_dict") else {}
        raise HTTPException(
            status_code=400,
            detail={
                "reason": "Step compilation failed",
                "message": str(e),
                "step_index": e.step_index,
                "action": e.action,
                "error_type": getattr(e, "error_type", None) or "compile_error",
                "details": getattr(e, "details", None) if getattr(e, "details", None) else compile_dict.get("details"),
                "error_summary": str(e),
                "correlation_id": correlation_id,
            },
        )

    if not steps:
        return {
            "plan": plan,
            "run": None,
            "reason": "Compilation produced no steps",
            "correlation_id": correlation_id,
            "error_type": "no_compiled_steps",
        }

    validation = validate_steps(steps)
    if not validation.valid:
        err_msgs = [
            f"{e.error_type} (step {e.step_index}{f', field={e.field}' if e.field else ''}): {e.message}"
            for e in validation.errors
        ]
        raise HTTPException(
            status_code=400,
            detail={
                "reason": "Invalid steps",
                "error_type": "validation_error",
                "errors": [e.model_dump() for e in validation.errors],
                "summary": "; ".join(err_msgs[:5]),
                "error_summary": err_msgs[0] if err_msgs else "Invalid steps",
                "correlation_id": correlation_id,
            },
        )

    # 2 — execute
    try:
        run = execute_test(
            steps    = steps,
            base_url = base_url,
            headless = req.headless,
            correlation_id=correlation_id,
        )
    except Exception as exc:
        logger.exception("execute_text: runner failed")
        return {
            "plan": plan,
            "run": None,
            "reason": f"Runner error: {type(exc).__name__}: {exc}",
            "correlation_id": correlation_id,
            "error_type": "runner_exception",
            "error_summary": f"{type(exc).__name__}: {exc}",
        }

    run = _merge_runner_result_with_canonical(
        run,
        correlation_id=correlation_id,
    )
    # Persist via unified pipeline so /tests/from-run can resolve by evidence_id/run_id
    try:
        from services.run_access import persist_run_payload

        payload = dict(run) if isinstance(run, dict) else {}
        if payload.get("evidence_id"):
            persist_run_payload(payload)
    except Exception:
        logger.warning("execute_text: persist_run_payload failed (non-fatal)", exc_info=True)

    return {"plan": plan, "run": run}
