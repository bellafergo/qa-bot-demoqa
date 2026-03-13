# runners/desktop_target.py
"""
Desktop Element Target Resolution.

Resolves a desktop control from a target specification using multiple
fallback strategies — analogous to selector_healer.py for web automation.

Strategy priority order (most to least reliable):
  1. automation_id  — pywinauto child_window(auto_id=...)
  2. control_name   — pywinauto child_window(title=...)
  3. class_name     — pywinauto child_window(class_name=...)
  4. text_label     — pywinauto child_window(title_re=<partial match>)
  5. control_type   — pywinauto child_window(control_type=...)

Target spec formats:
  str   → treated as control_name (title match)
  dict  → {
      "primary":  "<control name>",
      "fallbacks": [
          {"type": "automation_id", "value": "loginBtn"},
          {"type": "class_name",    "value": "Button"},
      ],
      "window_title": "POS Main Window",   # optional window constraint
  }

Returns:
  (control, used_strategy, resolved_id, meta)

Raises:
  Exception({"reason": "desktop_target_not_found", "classification": str, ...})
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger("vanya.desktop_target")


# ── Supported resolution strategies ──────────────────────────────────────────

_SUPPORTED_STRATEGIES = frozenset({
    "automation_id",
    "control_name",
    "class_name",
    "text_label",
    "control_type",
})

# ── Failure classification ─────────────────────────────────────────────────────

def _classify_failure(errors: List[str], primary: str, fallbacks: List[Any]) -> str:
    combined = " ".join(errors).lower()
    if "timeout" in combined or "timed out" in combined:
        return "transient"
    if not primary and not fallbacks:
        return "invalid_target"
    if errors and any(e.startswith("primary") for e in errors):
        if len(errors) == 1:
            return "primary_not_found"
        return "all_fallbacks_failed"
    return "primary_not_found"


# ── Per-strategy resolution ───────────────────────────────────────────────────

def _try_strategy(
    window: Any,
    strategy: str,
    value: Any,
    timeout_ms: int,
) -> Any:
    """
    Attempt to locate a control using the given strategy.
    Returns the control on success.
    Raises on failure.

    For mock windows (MockControl), tries attribute access and identity returns.
    For real pywinauto windows, calls the appropriate child_window method.
    """
    timeout_s = max(1, timeout_ms // 1000)
    value_str = str(value or "").strip()
    if not value_str:
        raise ValueError(f"{strategy} fallback: empty value")

    # Mock-aware path: if window is a MockControl (from MockDesktopBackend),
    # just return the window itself (mock is always "found").
    from runners.desktop_adapter import MockControl
    if isinstance(window, MockControl):
        return window

    # Real pywinauto path
    if strategy == "automation_id":
        ctrl = window.child_window(auto_id=value_str)
    elif strategy == "control_name":
        ctrl = window.child_window(title=value_str)
    elif strategy == "class_name":
        ctrl = window.child_window(class_name=value_str)
    elif strategy == "text_label":
        ctrl = window.child_window(title_re=f".*{value_str}.*")
    elif strategy == "control_type":
        ctrl = window.child_window(control_type=value_str)
    else:
        raise ValueError(f"unsupported strategy: {strategy}")

    ctrl.wait("exists visible", timeout=timeout_s)
    return ctrl


# ── Public API ────────────────────────────────────────────────────────────────

def resolve_desktop_target(
    window: Any,
    target: Any,
    *,
    timeout_ms: int = 5000,
) -> Tuple[Any, str, str, Dict[str, Any]]:
    """
    Resolve a desktop control using target spec and fallback strategies.

    Parameters
    ----------
    window     : pywinauto window handle or MockControl
    target     : str or dict (see module docstring)
    timeout_ms : per-attempt timeout

    Returns
    -------
    (control, used_strategy, resolved_id, meta)
      used_strategy  : "primary" | strategy string used
      resolved_id    : human-readable ID of the resolved control
      meta           : {"fallback_index": int|None, "attempts": int, "errors": list}

    Raises
    ------
    Exception with dict payload on total failure.
    """
    if not target:
        raise ValueError("desktop target is empty or None")

    # Normalise target to dict form
    if isinstance(target, str):
        primary_val = target.strip()
        primary_strategy = "control_name"
        fallbacks: List[Dict[str, Any]] = []
    elif isinstance(target, dict):
        primary_val = str(target.get("primary") or "").strip()
        primary_strategy = str(target.get("primary_strategy") or "control_name").strip()
        fallbacks = list(target.get("fallbacks") or [])
    else:
        raise ValueError(f"target must be str or dict, got {type(target).__name__}")

    errors: List[str] = []
    attempts = 0

    # ── 1. Primary ────────────────────────────────────────────────────────
    if primary_val:
        attempts += 1
        try:
            ctrl = _try_strategy(window, primary_strategy, primary_val, timeout_ms)
            return ctrl, "primary", primary_val, {
                "fallback_index": None,
                "fallback_strategy": None,
                "attempts": attempts,
                "errors": [],
            }
        except Exception as e:
            errors.append(f"primary failed: {type(e).__name__}: {str(e)[:200]}")

    # ── 2. Fallbacks ──────────────────────────────────────────────────────
    for idx, fb in enumerate(fallbacks):
        fb_strategy = str(fb.get("type") or "").strip().lower()
        if fb_strategy not in _SUPPORTED_STRATEGIES:
            continue
        fb_val = fb.get("value")
        attempts += 1
        try:
            ctrl = _try_strategy(window, fb_strategy, fb_val, timeout_ms)
            return ctrl, fb_strategy, f"{fb_strategy}={fb_val}", {
                "fallback_index": idx,
                "fallback_strategy": fb_strategy,
                "attempts": attempts,
                "errors": errors,
            }
        except Exception as e:
            errors.append(f"{fb_strategy}[{idx}] failed: {type(e).__name__}: {str(e)[:200]}")

    # ── 3. Total failure ──────────────────────────────────────────────────
    classification = _classify_failure(errors, primary_val, fallbacks)
    raise Exception({
        "reason":         "desktop_target_not_found",
        "classification": classification,
        "errors":         errors,
        "target":         target,
    })
