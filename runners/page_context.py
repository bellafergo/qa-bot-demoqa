# runners/page_context.py
"""
Lightweight DOM/page context capture for execution observability.

All functions fail gracefully — never raise, always return partial data.
No heavy DOM crawling. Safe to use at any point during a Playwright run.
"""
from __future__ import annotations

from typing import Any, Dict, Optional

# Maximum characters of visible element text to store
_MAX_ELEM_TEXT = 120
# Stable attributes worth capturing for debugging (ordered by usefulness)
_ELEMENT_ATTRS = ("id", "name", "data-testid", "data-test", "role", "aria-label", "placeholder")


def capture_page_context(page) -> Dict[str, Any]:
    """
    Capture lightweight current-page state: URL and title.
    Returns an empty/partial dict on any error.
    """
    ctx: Dict[str, Any] = {}
    try:
        ctx["url"] = page.url or ""
    except Exception:
        ctx["url"] = ""
    try:
        ctx["title"] = (page.title() or "").strip()
    except Exception:
        ctx["title"] = ""
    return ctx


def capture_element_context(locator) -> Dict[str, Any]:
    """
    Capture lightweight element attributes from a resolved Playwright locator.
    Only reads a fixed set of stable, debug-useful attributes.
    Truncates visible text to _MAX_ELEM_TEXT characters.
    Returns an empty/partial dict on any error — never raises.
    """
    ctx: Dict[str, Any] = {}

    for attr in _ELEMENT_ATTRS:
        try:
            val = locator.get_attribute(attr, timeout=500)
            if val:
                ctx[attr] = val
        except Exception:
            pass

    try:
        text = (locator.inner_text(timeout=500) or "").strip()
        if text:
            ctx["text"] = text[:_MAX_ELEM_TEXT]
    except Exception:
        pass

    return ctx


def build_failure_context(
    *,
    step_index: int,
    action: str,
    step: Dict[str, Any],
    error_str: str,
    page_ctx: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Build a structured failure evidence object.
    Pure function — does not touch the page directly.
    """
    target = step.get("target")
    primary = target.get("primary") if isinstance(target, dict) else None

    return {
        "step_index": step_index,
        "action": action,
        "original_selector": step.get("selector"),
        "primary": primary,
        "error": error_str,
        "page": page_ctx or {},
    }
