# core/step_validator.py
"""
Step validation layer — validates step dicts BEFORE execution.

validate_steps() accepts a list of raw step dicts (catalog or runner format)
and returns a StepValidationResult with structured errors and warnings.

Contrato canónico:
  VALID_ACTIONS = RUNNER_ACTIONS de core.schemas (acciones que generic_steps ejecuta).
  Acciones DSL alto (login, search, add_to_cart, etc.) deben compilarse antes de validar.

Design principles
-----------------
- Pure function, no I/O, no Playwright dependency.
- Safe to call in tests, CI pipelines, and before enqueueing a job.
- Errors block execution; warnings are advisory only.
- Backward compatible: does not alter input steps.
"""
from __future__ import annotations

from typing import Any, Dict, List, Optional
from pydantic import BaseModel

from core.schemas import RUNNER_ACTIONS

# Contrato único: solo acciones que el runner ejecuta realmente
VALID_ACTIONS: frozenset = RUNNER_ACTIONS

# Actions that require a selector or target.primary
_SELECTOR_REQUIRED: frozenset = frozenset({
    "fill",
    "click",
    "press",
    "hover",
    "select",
    "check",
    "uncheck",
    "assert_visible",
    "assert_not_visible",
    "assert_text_contains",
})

# Actions that require url or value
_URL_REQUIRED: frozenset = frozenset({"goto"})

# Actions that require key
_KEY_REQUIRED: frozenset = frozenset({"press"})

# Actions that require ms
_MS_SUGGESTED: frozenset = frozenset({"wait_ms"})


# ── Result models ──────────────────────────────────────────────────────────────

class StepValidationError(BaseModel):
    """One structural problem found in a step."""
    step_index:  int
    action:      Optional[str]
    error_type:  str       # "missing_selector" | "invalid_action" | "malformed_target" | "missing_required_field"
    message:     str
    field:       Optional[str] = None


class StepValidationResult(BaseModel):
    """Aggregate result of validate_steps()."""
    valid:           bool
    errors:          List[StepValidationError]
    warnings:        List[StepValidationError]
    validated_count: int


# ── Public API ─────────────────────────────────────────────────────────────────

def validate_steps(steps: List[Dict[str, Any]]) -> StepValidationResult:
    """
    Validate a list of step dicts before execution.

    Returns StepValidationResult.  result.valid == True means no errors
    (warnings may still be present).

    Does NOT mutate the input steps.
    """
    errors:   List[StepValidationError] = []
    warnings: List[StepValidationError] = []

    for i, step in enumerate(steps):
        if not isinstance(step, dict):
            errors.append(StepValidationError(
                step_index=i, action=None,
                error_type="malformed_target",
                message=f"Step at index {i} is not a dict (got {type(step).__name__})",
            ))
            continue

        action = str(step.get("action") or "").strip()

        # ── 1. Missing action ──────────────────────────────────────────────
        if not action:
            errors.append(StepValidationError(
                step_index=i, action=None,
                error_type="missing_required_field",
                message="Step has no 'action' field.",
                field="action",
            ))
            continue   # can't check further without knowing the action

        # ── 2. Unknown action ──────────────────────────────────────────────
        if action not in VALID_ACTIONS:
            errors.append(StepValidationError(
                step_index=i, action=action,
                error_type="invalid_action",
                message=(
                    f"Unknown action '{action}'. "
                    f"Valid actions: {sorted(VALID_ACTIONS)}"
                ),
                field="action",
            ))
            # still validate other fields so we surface all problems at once

        # ── 3. URL / value required ────────────────────────────────────────
        if action in _URL_REQUIRED:
            if not (step.get("url") or step.get("value")):
                errors.append(StepValidationError(
                    step_index=i, action=action,
                    error_type="missing_required_field",
                    message=f"Action '{action}' requires 'url' or 'value'.",
                    field="url",
                ))

        # ── 4. Selector / target required ─────────────────────────────────
        if action in _SELECTOR_REQUIRED:
            has_selector = bool(step.get("selector"))
            target = step.get("target")
            has_target = (
                isinstance(target, dict) and bool(target.get("primary"))
            )
            if not has_selector and not has_target:
                errors.append(StepValidationError(
                    step_index=i, action=action,
                    error_type="missing_selector",
                    message=(
                        f"Action '{action}' requires 'selector' or "
                        f"a 'target' dict with 'primary'."
                    ),
                    field="selector",
                ))

        # ── 5. Malformed target ────────────────────────────────────────────
        target = step.get("target")
        if target is not None:
            if not isinstance(target, dict):
                errors.append(StepValidationError(
                    step_index=i, action=action,
                    error_type="malformed_target",
                    message="'target' must be a dict with a 'primary' key.",
                    field="target",
                ))
            elif action in _SELECTOR_REQUIRED and not target.get("primary"):
                warnings.append(StepValidationError(
                    step_index=i, action=action,
                    error_type="malformed_target",
                    message="'target' dict is present but has no 'primary' selector.",
                    field="target.primary",
                ))

        # ── 6. Key required ────────────────────────────────────────────────
        if action in _KEY_REQUIRED and not step.get("key"):
            errors.append(StepValidationError(
                step_index=i, action=action,
                error_type="missing_required_field",
                message=f"Action '{action}' requires a 'key' field.",
                field="key",
            ))

        # ── 7. wait_ms advisory ────────────────────────────────────────────
        if action in _MS_SUGGESTED and step.get("ms") is None:
            warnings.append(StepValidationError(
                step_index=i, action=action,
                error_type="missing_required_field",
                message="Action 'wait_ms' should have an 'ms' field (will default to 0).",
                field="ms",
            ))

    return StepValidationResult(
        valid=len(errors) == 0,
        errors=errors,
        warnings=warnings,
        validated_count=len(steps),
    )
