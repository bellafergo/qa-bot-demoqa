# core/step_validator.py
"""
Step validation layer — validates step dicts BEFORE execution.

validate_steps() accepts a list of raw step dicts (catalog or runner format)
and returns a StepValidationResult with structured errors and warnings.

Contrato canónico (multi-runner):
  - runner_kind "web" (default): acciones en RUNNER_ACTIONS (Playwright / generic_steps).
  - runner_kind "desktop": acciones en core.runner_contract.DESKTOP_RUNNER_ACTIONS (pywinauto).
  - runner_kind "api": api_request, wait_ms.

  Acciones DSL alto (login, search, …) deben compilarse antes de validar en modo web.

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
from core.runner_contract import (
    API_RUNNER_ACTIONS,
    DESKTOP_RUNNER_ACTIONS,
    mismatch_hint,
    normalize_desktop_action,
    runner_kind_for_test_type,
)

# Default: web Playwright runner
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
})

# Actions that require url or value
_URL_REQUIRED: frozenset = frozenset({"goto"})

# Actions that require key
_KEY_REQUIRED: frozenset = frozenset({"press"})

# Actions that require ms
_MS_SUGGESTED: frozenset = frozenset({"wait_ms"})

# Actions that require a text/expected value (not a selector)
_TEXT_REQUIRED: frozenset = frozenset({
    "assert_text_contains",
    "assert_text_not_contains",
})

# Desktop: steps that need a control/window target (string)
_DESKTOP_TARGET_ACTIONS: frozenset = frozenset({
    "click",
    "input_text",
    "type_keys",
    "select",
    "read_text",
    "assert_text_contains",
    "assert_exists",
    "wait_for",
})

_DESKTOP_TITLE_ACTIONS: frozenset = frozenset(
    {"launch_app", "attach_window", "focus_window"}
)


# ── Result models ──────────────────────────────────────────────────────────────

class StepValidationError(BaseModel):
    """One structural problem found in a step."""
    step_index:  int
    action:      Optional[str]
    error_type:  str       # "missing_selector" | "invalid_action" | "malformed_target" | "missing_required_field"
    message:     str
    field:       Optional[str] = None
    hint:        Optional[str] = None


class StepValidationResult(BaseModel):
    """Aggregate result of validate_steps()."""
    valid:           bool
    errors:          List[StepValidationError]
    warnings:        List[StepValidationError]
    validated_count: int


# ── Public API ─────────────────────────────────────────────────────────────────

def validate_steps(
    steps: List[Dict[str, Any]],
    *,
    runner_kind: Optional[str] = None,
    test_type: Optional[str] = None,
) -> StepValidationResult:
    """
    Validate a list of step dicts before execution.

    *runner_kind*: "web" | "desktop" | "api". If omitted, derived from *test_type*
    (ui → web, desktop → desktop, api → api).

    Returns StepValidationResult.  result.valid == True means no errors
    (warnings may still be present).

    Does NOT mutate the input steps.
    """
    rk = (runner_kind or "").strip().lower() if runner_kind else ""
    if not rk and test_type is not None:
        rk = runner_kind_for_test_type(test_type)
    if not rk:
        rk = "web"

    if rk == "desktop":
        return _validate_desktop_steps(steps)
    if rk == "api":
        return _validate_api_steps(steps)
    return _validate_web_steps(steps)


def _validate_web_steps(steps: List[Dict[str, Any]]) -> StepValidationResult:
    errors:   List[StepValidationError] = []
    warnings: List[StepValidationError] = []

    for i, step in enumerate(steps):
        if not isinstance(step, dict):
            errors.append(StepValidationError(
                step_index=i, action=None,
                error_type="malformed_target",
                message=f"Step at index {i} is not a dict (got {type(step).__name__})",
                hint="Provide each step as a dict with at least an 'action' key.",
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
                hint="Provide an 'action' key (e.g. goto/click/fill/assert_visible).",
            ))
            continue   # can't check further without knowing the action

        # ── 2. Unknown action ──────────────────────────────────────────────
        if action not in VALID_ACTIONS:
            valid_sorted = sorted(VALID_ACTIONS)
            sample = valid_sorted[:12]
            mh = mismatch_hint(action, "web")
            errors.append(StepValidationError(
                step_index=i, action=action,
                error_type="invalid_action",
                message=(
                    f"Unknown action '{action}' for web runner. "
                    f"Valid actions sample: {sample}. Total valid actions: {len(valid_sorted)}."
                ),
                field="action",
                hint=mh or "Use a supported runner action from core.schemas.RUNNER_ACTIONS.",
            ))
            # still validate other fields so we surface all problems at once

        # ── 3. URL / value required ────────────────────────────────────────
        if action in _URL_REQUIRED:
            if not (step.get("url") or step.get("value")):
                errors.append(StepValidationError(
                    step_index=i, action=action,
                    error_type="missing_required_field",
                    message=f"Action '{action}' requires 'url' or 'value' (got none).",
                    field="url",
                    hint="Set step['url'] (preferred) or step['value'] to a full http(s) URL.",
                ))

        # ── 4. Selector / target required ─────────────────────────────────
        if action in _SELECTOR_REQUIRED:
            selector_raw = step.get("selector")
            has_selector = bool(selector_raw)
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
                    hint="Add 'selector' or set 'target': {'primary': '<selector>'}.",
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
                    hint="Set target as a dict like {'primary': '#login-button'}.",
                ))
            elif action in _SELECTOR_REQUIRED and not target.get("primary"):
                warnings.append(StepValidationError(
                    step_index=i, action=action,
                    error_type="malformed_target",
                    message="'target' dict is present but has no 'primary' selector.",
                    field="target.primary",
                    hint="Provide target.primary inside the 'target' dict.",
                ))

        # ── 6. Text required ──────────────────────────────────────────────
        if action in _TEXT_REQUIRED and not step.get("text"):
            errors.append(StepValidationError(
                step_index=i, action=action,
                error_type="missing_required_field",
                message=f"Action '{action}' requires 'text'.",
                field="text",
                hint="Add step['text'] with the string to assert (e.g. 'Welcome').",
            ))

        # ── 7. Key required ────────────────────────────────────────────────
        if action in _KEY_REQUIRED and not step.get("key"):
            errors.append(StepValidationError(
                step_index=i, action=action,
                error_type="missing_required_field",
                message=f"Action '{action}' requires a 'key' field.",
                field="key",
                hint="For press steps, set step['key'] (e.g. 'Enter').",
            ))

        # ── 7. wait_ms advisory ────────────────────────────────────────────
        if action in _MS_SUGGESTED and step.get("ms") is None:
            warnings.append(StepValidationError(
                step_index=i, action=action,
                error_type="missing_required_field",
                message="Action 'wait_ms' should have an 'ms' field (will default to 0).",
                field="ms",
                hint="Add step['ms'] as an integer delay in milliseconds.",
            ))

    return StepValidationResult(
        valid=len(errors) == 0,
        errors=errors,
        warnings=warnings,
        validated_count=len(steps),
    )


def _has_desktop_target(step: Dict[str, Any]) -> bool:
    for key in (
        "target", "selector", "loc", "title", "window_title",
        "value", "path", "keys",
    ):
        v = step.get(key)
        if v is not None and str(v).strip():
            return True
    return False


def _validate_desktop_steps(steps: List[Dict[str, Any]]) -> StepValidationResult:
    errors: List[StepValidationError] = []
    warnings: List[StepValidationError] = []

    for i, step in enumerate(steps):
        if not isinstance(step, dict):
            errors.append(StepValidationError(
                step_index=i, action=None,
                error_type="malformed_target",
                message=f"Step at index {i} is not a dict (got {type(step).__name__})",
                hint="Provide each step as a dict with at least an 'action' key.",
            ))
            continue

        raw_action = str(step.get("action") or "").strip()
        if not raw_action:
            errors.append(StepValidationError(
                step_index=i, action=None,
                error_type="missing_required_field",
                message="Step has no 'action' field.",
                field="action",
                hint="Desktop actions: attach_window, click, input_text, assert_exists, …",
            ))
            continue

        action = normalize_desktop_action(raw_action)
        if action not in DESKTOP_RUNNER_ACTIONS:
            sample = sorted(DESKTOP_RUNNER_ACTIONS)[:12]
            mh = mismatch_hint(raw_action, "desktop")
            errors.append(StepValidationError(
                step_index=i, action=raw_action,
                error_type="invalid_action",
                message=(
                    f"Unknown or web-only action {raw_action!r} for desktop runner "
                    f"(normalized: {action!r}). Sample desktop actions: {sample}."
                ),
                field="action",
                hint=mh or "See core.runner_contract.DESKTOP_RUNNER_ACTIONS.",
            ))

        if action in _DESKTOP_TITLE_ACTIONS:
            if not _has_desktop_target(step):
                errors.append(StepValidationError(
                    step_index=i, action=raw_action,
                    error_type="missing_required_field",
                    message=(
                        f"Action '{action}' needs a window/app target "
                        "(target, selector, title, or value with title/path)."
                    ),
                    field="target",
                    hint="Example: {\"action\": \"attach_window\", \"target\": \"GCC POS\"}",
                ))

        if action in _DESKTOP_TARGET_ACTIONS:
            if not _has_desktop_target(step):
                errors.append(StepValidationError(
                    step_index=i, action=raw_action,
                    error_type="missing_selector",
                    message=(
                        f"Action '{action}' needs a control target "
                        "(target, selector, or loc — caption or automation id)."
                    ),
                    field="target",
                ))

        if action == "assert_text_contains" and not (
            step.get("value") or step.get("text") or step.get("expected")
        ):
            warnings.append(StepValidationError(
                step_index=i, action=raw_action,
                error_type="missing_required_field",
                message="assert_text_contains should include value, text, or expected.",
                field="value",
            ))

        if action in _MS_SUGGESTED and step.get("ms") is None:
            warnings.append(StepValidationError(
                step_index=i, action=action,
                error_type="missing_required_field",
                message="Action 'wait_ms' should have an 'ms' field (will default in runner).",
                field="ms",
            ))

    return StepValidationResult(
        valid=len(errors) == 0,
        errors=errors,
        warnings=warnings,
        validated_count=len(steps),
    )


def _validate_api_steps(steps: List[Dict[str, Any]]) -> StepValidationResult:
    errors: List[StepValidationError] = []
    warnings: List[StepValidationError] = []

    for i, step in enumerate(steps):
        if not isinstance(step, dict):
            errors.append(StepValidationError(
                step_index=i, action=None,
                error_type="malformed_target",
                message=f"Step at index {i} is not a dict.",
            ))
            continue

        action = str(step.get("action") or "").strip().lower()
        if not action:
            errors.append(StepValidationError(
                step_index=i, action=None,
                error_type="missing_required_field",
                message="Step has no 'action' field.",
                field="action",
            ))
            continue

        if action not in API_RUNNER_ACTIONS:
            errors.append(StepValidationError(
                step_index=i, action=action,
                error_type="invalid_action",
                message=f"Action {action!r} is not valid for API runner.",
                field="action",
                hint=mismatch_hint(action, "api") or "Allowed: api_request, wait_ms.",
            ))

        if action == "api_request" and not step.get("endpoint"):
            warnings.append(StepValidationError(
                step_index=i, action=action,
                error_type="missing_required_field",
                message="api_request should set 'endpoint' (e.g. '/users').",
                field="endpoint",
            ))

    return StepValidationResult(
        valid=len(errors) == 0,
        errors=errors,
        warnings=warnings,
        validated_count=len(steps),
    )
