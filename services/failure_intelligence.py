# services/failure_intelligence.py
"""
Run-level failure intelligence for Vanya.

    classify_failure(run: dict) -> dict

Operates on the complete run result dict (as returned by execute_test or
assembled by workers/jobs.py) and produces a single structured classification.

Distinction from services/failure_classifier.py:
  - failure_classifier  — per-step level, called inside the runner on Exception + signals
  - failure_intelligence — run level, called after all steps have executed on the full run dict

Output schema:
  {
    "failure_type": "selector_not_found" | "assertion_failed" |
                    "navigation_failed"  | "timeout" | "unknown",
    "layer":        "resolver" | "assertion" | "navigation" | "runner" | "unknown",
    "target":       <str | None>,   # e.g. "button(login)" or "assert_visible(#login-button)"
    "confidence":   "high" | "medium" | "low",
  }

Signals read from the run dict:
  run["reason"]          — primary failure reason string (set by runner on step failure)
  run["error_message"]   — error traceback string (set by jobs.py exception handler)
  run["steps"]           — per-step report: [{action, selector, status, error, target, ...}]
  run["failure_context"] — {action, original_selector, primary, error, page_ctx, ...}

Pure module — no I/O, no side effects, no runner/healer imports.
"""
from __future__ import annotations

from typing import Any, Dict, Optional


def classify_failure(run: dict) -> dict:
    """
    Classify a failed run dict into a structured failure analysis.

    Returns {failure_type, layer, target, confidence}.
    """
    if not isinstance(run, dict):
        return _out("unknown", "unknown", None, "low")

    # ── Extract signals ───────────────────────────────────────────────────────
    reason_str = _s(run.get("reason") or run.get("error_message") or "")
    reason_low = reason_str.lower()

    all_steps = run.get("steps") or []
    failed_steps = [
        s for s in all_steps
        if isinstance(s, dict) and s.get("status") in ("failed", "error")
    ]
    first_failed = failed_steps[0] if failed_steps else None

    fc = run.get("failure_context") or {}
    if not isinstance(fc, dict):
        fc = {}

    failed_action = _s(
        (first_failed or {}).get("action") or fc.get("action") or ""
    ).lower()

    # Prefer step dict for target formatting; fall back to failure_context
    target_source = first_failed or (fc if fc.get("action") else None)
    target_str = _format_target(target_source)

    # ── 1. TIMEOUT ───────────────────────────────────────────────────────────
    if "timeout" in reason_low or "timed out" in reason_low or "timeouterror" in reason_low:
        layer = "navigation" if failed_action == "goto" else "runner"
        return _out("timeout", layer, target_str, "high")

    # ── 2. NAVIGATION FAILED ─────────────────────────────────────────────────
    _nav_signals = failed_action == "goto" or any(k in reason_low for k in [
        "net::err",
        "failed to navigate",
        "failed to load",
        "navigation",
        "goto requiere",
        "page.goto",
    ])
    if _nav_signals:
        return _out("navigation_failed", "navigation", target_str, "high")

    # ── 3. SELECTOR NOT FOUND ────────────────────────────────────────────────
    _locator_signals = any(k in reason_low for k in [
        "waiting for locator",
        "waiting for selector",
        "locator resolved to",
        "no node found",
        "element is not attached",
        "element is not visible",
        "not found",
        "cannot find",
        "strict mode violation",
    ])
    if _locator_signals:
        return _out("selector_not_found", "resolver", target_str, "high")

    # ── 4. ASSERTION FAILED ──────────────────────────────────────────────────
    _assert_action_failed = any(
        _s(s.get("action") or "").startswith("assert_")
        for s in failed_steps
    ) or failed_action.startswith("assert_")

    _assert_msg_signals = any(k in reason_low for k in [
        "assertionerror",
        "texto no encontrado",
        "expected contiene",
        "not visible",
        "url does not contain",
        "assertion",
        "assert",
    ])
    if _assert_action_failed or _assert_msg_signals:
        confidence = "high" if _assert_action_failed else "medium"
        return _out("assertion_failed", "assertion", target_str, confidence)

    # ── 5. UNKNOWN ───────────────────────────────────────────────────────────
    return _out("unknown", "unknown", target_str, "low")


# ── Helpers ────────────────────────────────────────────────────────────────────

def _out(
    failure_type: str,
    layer: str,
    target: Optional[str],
    confidence: str,
) -> Dict[str, Any]:
    return {
        "failure_type": failure_type,
        "layer":        layer,
        "target":       target,
        "confidence":   confidence,
    }


def _s(v: Any) -> str:
    try:
        return str(v) if v is not None else ""
    except Exception:
        return ""


def _format_target(step: Optional[Any]) -> Optional[str]:
    """
    Derive a human-readable target string from a step or failure_context dict.
    Prefers semantic target {kind, name}  →  "kind(name)"  (e.g. "button(login)").
    Falls back to "action(selector)"  or action alone.
    """
    if not step or not isinstance(step, dict):
        return None
    t = step.get("target")
    if isinstance(t, dict) and t.get("kind") and t.get("name"):
        return f"{t['kind']}({t['name']})"
    action   = _s(step.get("action") or "")
    selector = _s(
        step.get("selector") or step.get("original_selector") or step.get("primary") or ""
    )
    if action and selector:
        return f"{action}({selector})"
    return action or selector or None
