# services/test_auto_fixer.py
"""
Deterministic auto-fix engine for test steps and assertions.

No LLM. No external dependencies. Pure rule-based corrections.

Each rule either fixes a step/assertion in-place or removes it (invalid action),
and logs a human-readable message to the `changes` list.

Rules applied (in order):
  1. Normalize action aliases (input→fill, navigate→goto, etc.)
  2. Remove steps with unknown / unsupported actions
  3. Prepend a goto if the first step is not one
  4. Add default selector to click/fill steps that are missing one
  5. Add default value to fill steps that are missing one
  6. Add default type to assertions that are missing one
  7. Add default selector to assertions that require one but lack it
  8. Add a default assertion if assertions list is empty
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional

from core.runner_contract import (
    DESKTOP_RUNNER_ACTIONS,
    normalize_desktop_action,
    runner_kind_for_test_type,
)

# ── Unstable / dynamic selector patterns (React / Radix / RHF, nth-child, hash IDs) ──
_RE_RADIX_COLON_ID = re.compile(r"#:[A-Za-z0-9]{4,}:-")
_RE_NTH_CHILD_TAIL = re.compile(r":nth-child\(\d+\)$")
_RE_HASH_SUFFIX_ID = re.compile(r"#[a-z]+-[a-f0-9]{6,}", re.IGNORECASE)


def is_unstable_selector(selector: Any) -> bool:
    """
    True if the selector string matches known fragile patterns (dynamic React/Radix IDs,
    nth-child terminal, hashed suffix IDs). Non-strings / empty → False.
    """
    if selector is None or not isinstance(selector, str):
        return False
    s = selector.strip()
    if not s:
        return False
    if _RE_RADIX_COLON_ID.search(s):
        return True
    if _RE_NTH_CHILD_TAIL.search(s):
        return True
    if _RE_HASH_SUFFIX_ID.search(s):
        return True
    return False


def _step_selector_string(step: dict) -> Optional[str]:
    sel = step.get("selector")
    if isinstance(sel, str) and sel.strip():
        return sel.strip()
    t = step.get("target")
    if isinstance(t, str) and t.strip():
        return t.strip()
    return None


def _assertion_selector_string(a: dict) -> Optional[str]:
    for key in ("selector", "target"):
        v = a.get(key)
        if isinstance(v, str) and v.strip():
            return v.strip()
    return None


def _unstable_selector_warning_message(sel: str) -> str:
    """Human message; {sel!r} avoids format injection if selector contains braces."""
    return (
        f"Selector posiblemente inestable: {sel!r}. "
        "Reemplázalo por data-testid, aria-label o texto visible."
    )


# ── Canonical allowed actions (sourced from runners/generic_steps.py) ────────

_ALLOWED_ACTIONS = frozenset({
    "goto",
    "fill",
    "click",
    "press",
    "wait_ms",
    "assert_visible",
    "assert_not_visible",
    "assert_url_contains",
    "assert_text_contains",
})

# Aliases that map to a canonical action (mirrors _ACTION_ALIASES in catalog service)
_ACTION_ALIASES: Dict[str, str] = {
    "input":               "fill",
    "type":                "fill",
    "enter":               "fill",
    "navigate":            "goto",
    "open":                "goto",
    "wait":                "wait_ms",
    "sleep":               "wait_ms",
    "assert_text":         "assert_text_contains",
    "assert_visible_text": "assert_text_contains",
    "text_visible":        "assert_text_contains",
    "check_text":          "assert_text_contains",
    "verify_text":         "assert_text_contains",
    "assert_url":          "assert_url_contains",
    "url_contains":        "assert_url_contains",
    "visible":             "assert_visible",
    "element_visible":     "assert_visible",
    "not_visible":         "assert_not_visible",
    "element_not_visible": "assert_not_visible",
}

# Assertion types that must have a selector
_SELECTOR_REQUIRED = frozenset({
    "assert_visible",
    "assert_not_visible",
    "assert_text_contains",
})

# Default fallback values
_DEFAULT_SELECTOR  = "[data-test='auto']"
_DEFAULT_GOTO_URL  = "https://example.com"
_DEFAULT_FILL_VAL  = "test"
_DEFAULT_BODY_SEL  = "body"


def auto_fix_test(
    steps:      List[Dict[str, Any]],
    assertions: List[Dict[str, Any]],
    *,
    runner_kind: Optional[str] = None,
    test_type: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Apply deterministic fixes to *steps* and *assertions*.

    *runner_kind* / *test_type*: when desktop or api, web-only rules (default goto,
    CSS testid defaults) are not applied; steps are normalized to the desktop/API DSL.

    Returns:
        {
            "steps":      fixed_steps,
            "assertions": fixed_assertions,
            "changes": [
                {"type": "fix" | "warning", "message": "..."}
            ]
        }

    Never raises.  Bad inputs (None, non-list) are treated as empty lists.
    Original lists are not mutated.
    """
    steps      = _safe_list(steps)
    assertions = _safe_list(assertions)
    changes: List[Dict[str, str]] = []

    rk = (runner_kind or "").strip().lower() if runner_kind else ""
    if not rk and test_type is not None:
        rk = runner_kind_for_test_type(test_type)
    if not rk:
        rk = "web"

    if rk == "desktop":
        fixed_steps = _fix_steps_desktop(steps, changes)
        fixed_assertions = _fix_assertions_desktop(assertions, changes)
    elif rk == "api":
        fixed_steps = _fix_steps_api(steps, changes)
        fixed_assertions = _fix_assertions_api(assertions, changes)
    else:
        fixed_steps = _fix_steps(steps, changes)
        fixed_assertions = _fix_assertions(assertions, changes)

    return {
        "steps":      fixed_steps,
        "assertions": fixed_assertions,
        "changes":    changes,
    }


# ── Step fixers ───────────────────────────────────────────────────────────────

def _fix_steps(
    steps:   List[Dict[str, Any]],
    changes: List[Dict[str, str]],
) -> List[Dict[str, Any]]:
    result: List[Dict[str, Any]] = []

    for i, raw in enumerate(steps):
        if not isinstance(raw, dict):
            changes.append({"type": "warning", "message": f"step {i + 1}: not a dict — removed"})
            continue

        step = dict(raw)  # shallow copy, do not mutate original

        _usel = _step_selector_string(step)
        if _usel and is_unstable_selector(_usel):
            changes.append({"type": "warning", "message": _unstable_selector_warning_message(_usel)})

        action_raw = (step.get("action") or "").strip().lower()

        # Rule 1: normalize aliases
        canonical = _ACTION_ALIASES.get(action_raw, action_raw)
        if canonical != action_raw:
            changes.append({
                "type":    "fix",
                "message": f"step {i + 1}: normalized action '{action_raw}' → '{canonical}'",
            })
        step["action"] = canonical

        # Rule 2: remove unknown actions
        if canonical not in _ALLOWED_ACTIONS:
            changes.append({
                "type":    "warning",
                "message": f"step {i + 1}: unknown action '{canonical}' — removed",
            })
            continue

        # Rule 4: missing selector on click / fill
        if canonical in ("click", "fill"):
            if not _has_selector(step):
                step["selector"] = _DEFAULT_SELECTOR
                changes.append({
                    "type":    "fix",
                    "message": (
                        f"step {i + 1}: '{canonical}' missing selector "
                        f"— set to '{_DEFAULT_SELECTOR}'"
                    ),
                })

        # Rule 5: missing value on fill
        if canonical == "fill":
            if not step.get("value"):
                step["value"] = _DEFAULT_FILL_VAL
                changes.append({
                    "type":    "fix",
                    "message": f"step {i + 1}: 'fill' missing value — set to '{_DEFAULT_FILL_VAL}'",
                })

        result.append(step)

    # Rule 3: prepend goto if first step is not goto
    if not result or result[0].get("action") != "goto":
        result.insert(0, {"action": "goto", "url": _DEFAULT_GOTO_URL})
        changes.append({
            "type":    "fix",
            "message": f"prepended default goto '{_DEFAULT_GOTO_URL}' (no goto at start)",
        })

    return result


def _fix_steps_desktop(
    steps:   List[Dict[str, Any]],
    changes: List[Dict[str, str]],
) -> List[Dict[str, Any]]:
    """Normalize desktop aliases; drop only truly unknown actions. No default goto."""
    result: List[Dict[str, Any]] = []

    for i, raw in enumerate(steps):
        if not isinstance(raw, dict):
            changes.append({"type": "warning", "message": f"step {i + 1}: not a dict — removed"})
            continue

        step = dict(raw)
        action_raw = (step.get("action") or "").strip().lower()
        canonical = normalize_desktop_action(action_raw)
        if canonical != action_raw and action_raw:
            changes.append({
                "type":    "fix",
                "message": f"step {i + 1}: normalized desktop action '{action_raw}' → '{canonical}'",
            })
        step["action"] = canonical

        if canonical not in DESKTOP_RUNNER_ACTIONS:
            changes.append({
                "type":    "warning",
                "message": (
                    f"step {i + 1}: action {canonical!r} is not in desktop DSL — "
                    "removed (fix test_type or use a desktop action)"
                ),
            })
            continue

        result.append(step)

    return result


_DESKTOP_ASSERT_ALIASES: Dict[str, str] = {
    "assert_text": "assert_text_contains",
    "check_text": "assert_text_contains",
    "verify_text": "assert_text_contains",
    "visible": "assert_exists",
    "assert_visible": "assert_exists",
    "element_visible": "assert_exists",
    "assert_control_visible": "assert_exists",
}


def _fix_assertions_desktop(
    assertions: List[Dict[str, Any]],
    changes:    List[Dict[str, str]],
) -> List[Dict[str, Any]]:
    """
    Keep assertions compatible with _build_desktop_steps (types: assert_text_contains, assert_exists).
    Does not inject web-style assert_visible + body selector defaults.
    """
    result: List[Dict[str, Any]] = []

    for i, raw in enumerate(assertions):
        if not isinstance(raw, dict):
            changes.append({"type": "warning", "message": f"assertion {i + 1}: not a dict — removed"})
            continue

        a = dict(raw)
        raw_type = (a.get("type") or a.get("action") or "").strip().lower()
        canonical = _DESKTOP_ASSERT_ALIASES.get(raw_type, raw_type)
        if canonical != raw_type and raw_type:
            if "type" in a:
                a["type"] = canonical
            else:
                a["action"] = canonical
            changes.append({
                "type":    "fix",
                "message": f"assertion {i + 1}: normalized type '{raw_type}' → '{canonical}'",
            })

        eff = (a.get("type") or a.get("action") or "").strip().lower()
        if eff == "assert_not_visible":
            changes.append({
                "type":    "warning",
                "message": f"assertion {i + 1}: assert_not_visible not supported on desktop runner — removed",
            })
            continue

        if eff in ("assert_url_contains", "assert_url"):
            changes.append({
                "type":    "warning",
                "message": f"assertion {i + 1}: URL assertions are web-only — removed",
            })
            continue

        if not eff:
            changes.append({
                "type":    "warning",
                "message": f"assertion {i + 1}: missing type — removed (desktop: add assert_text_contains or assert_exists)",
            })
            continue

        if eff == "assert_text_contains" and not (a.get("value") or a.get("text")):
            changes.append({
                "type":    "warning",
                "message": f"assertion {i + 1}: assert_text_contains without value — removed",
            })
            continue

        if eff == "assert_exists" and not (a.get("target") or a.get("selector")):
            changes.append({
                "type":    "warning",
                "message": f"assertion {i + 1}: assert_exists without target — removed",
            })
            continue

        result.append(a)

    return result


def _fix_steps_api(
    steps:   List[Dict[str, Any]],
    changes: List[Dict[str, str]],
) -> List[Dict[str, Any]]:
    from core.runner_contract import API_RUNNER_ACTIONS

    result: List[Dict[str, Any]] = []
    for i, raw in enumerate(steps):
        if not isinstance(raw, dict):
            changes.append({"type": "warning", "message": f"step {i + 1}: not a dict — removed"})
            continue
        step = dict(raw)
        action = (step.get("action") or "").strip().lower()
        if action not in API_RUNNER_ACTIONS:
            changes.append({
                "type":    "warning",
                "message": f"step {i + 1}: {action!r} is not an API runner action — removed",
            })
            continue
        result.append(step)
    return result


def _fix_assertions_api(
    assertions: List[Dict[str, Any]],
    changes:    List[Dict[str, str]],
) -> List[Dict[str, Any]]:
    """API assertions are evaluated by api_runner; do not coerce to web assert_visible."""
    result: List[Dict[str, Any]] = []
    for i, raw in enumerate(assertions):
        if not isinstance(raw, dict):
            changes.append({"type": "warning", "message": f"assertion {i + 1}: not a dict — removed"})
            continue
        a = dict(raw)
        if not (a.get("type") or a.get("action")):
            changes.append({
                "type":    "warning",
                "message": f"assertion {i + 1}: missing type — removed",
            })
            continue
        result.append(a)
    return result


# ── Assertion fixers ──────────────────────────────────────────────────────────

def _fix_assertions(
    assertions: List[Dict[str, Any]],
    changes:    List[Dict[str, str]],
) -> List[Dict[str, Any]]:
    result: List[Dict[str, Any]] = []

    for i, raw in enumerate(assertions):
        if not isinstance(raw, dict):
            changes.append({"type": "warning", "message": f"assertion {i + 1}: not a dict — removed"})
            continue

        a = dict(raw)  # shallow copy

        _asel = _assertion_selector_string(a)
        if _asel and is_unstable_selector(_asel):
            changes.append({"type": "warning", "message": _unstable_selector_warning_message(_asel)})

        # Normalize type/action alias (mirrors Rule 1 for steps)
        raw_type = (a.get("type") or a.get("action") or "").strip().lower()
        canonical_type = _ACTION_ALIASES.get(raw_type, raw_type)
        if canonical_type != raw_type and raw_type:
            if "type" in a:
                a["type"] = canonical_type
            else:
                a["action"] = canonical_type
            changes.append({
                "type":    "fix",
                "message": f"assertion {i + 1}: normalized type '{raw_type}' → '{canonical_type}'",
            })

        # Rule 6: missing type
        if not (a.get("type") or a.get("action")):
            a["type"] = "assert_visible"
            changes.append({
                "type":    "fix",
                "message": f"assertion {i + 1}: missing type — set to 'assert_visible'",
            })

        # Resolve effective type (support both "type" and "action" fields)
        eff_type = (a.get("type") or a.get("action") or "").lower()

        # Rule 7: missing selector for types that require one
        if eff_type in _SELECTOR_REQUIRED:
            if not (a.get("selector") or a.get("target")):
                a["selector"] = _DEFAULT_BODY_SEL
                changes.append({
                    "type":    "fix",
                    "message": (
                        f"assertion {i + 1}: '{eff_type}' missing selector "
                        f"— set to '{_DEFAULT_BODY_SEL}'"
                    ),
                })

        result.append(a)

    # Rule 8: empty assertions
    if not result:
        result.append({"type": "assert_visible", "selector": _DEFAULT_BODY_SEL})
        changes.append({
            "type":    "fix",
            "message": "assertions was empty — added default assert_visible body",
        })

    return result


# ── Helpers ───────────────────────────────────────────────────────────────────

def _safe_list(value: Any) -> List[Dict[str, Any]]:
    """Return *value* if it is a list, otherwise an empty list."""
    return value if isinstance(value, list) else []


def _has_selector(step: dict) -> bool:
    """True if the step has any non-empty selector-like field."""
    return bool(step.get("selector") or step.get("target"))
