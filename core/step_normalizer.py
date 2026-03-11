# core/step_normalizer.py
"""
Pure helpers for upgrading flat selector-based steps to target-object steps.
No heavy dependencies — safe to import in tests without Playwright/reportlab.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List

# ============================================================
# UI actions that carry a selector/target (non-UI steps pass through)
# ============================================================
_UI_ACTIONS = frozenset({
    "fill", "click", "press",
    "assert_visible", "assert_not_visible", "assert_text_contains",
})

# Compiled patterns for conservative fallback inference
_RE_ID = re.compile(r'^#([\w-]+)$')
_RE_NAME = re.compile(r'^\[name=["\']([^"\']+)["\']\]$')
_RE_DATA_TESTID = re.compile(r'^\[data-testid=["\']([^"\']+)["\']\]$')
_RE_DATA_TEST = re.compile(r'^\[data-test=["\']([^"\']+)["\']\]$')


def _infer_fallbacks(selector: str) -> List[Dict[str, Any]]:
    """
    Generate conservative fallbacks for obvious, well-known selector patterns.
    Never guesses. Returns [] when no safe fallback can be inferred.

    Patterns handled:
      #id               → css [id='value']
      [name='v']        → name  v  (forward-compat; healer ignores unknown types)
      [data-testid='v'] → testid v + css [data-test='v']
      [data-test='v']   → testid v + css [data-testid='v']
    """
    m = _RE_ID.match(selector)
    if m:
        return [{"type": "css", "value": f"[id='{m.group(1)}']"}]

    m = _RE_NAME.match(selector)
    if m:
        return [{"type": "name", "value": m.group(1)}]

    m = _RE_DATA_TESTID.match(selector)
    if m:
        return [
            {"type": "testid", "value": m.group(1)},
            {"type": "css", "value": f"[data-test='{m.group(1)}']"},
        ]

    m = _RE_DATA_TEST.match(selector)
    if m:
        return [
            {"type": "testid", "value": m.group(1)},
            {"type": "css", "value": f"[data-testid='{m.group(1)}']"},
        ]

    return []


def normalize_steps_to_target(steps: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """
    Upgrades flat selector-based steps to target-object steps.

    Rules:
    - Steps that already have a valid target dict are left unchanged.
    - Non-UI steps (goto, wait_ms, assert_url_contains, …) pass through unchanged.
    - The original "selector" key is preserved for backward compatibility.
    - Conservative fallbacks are added when the selector matches a known pattern.
    """
    out: List[Dict[str, Any]] = []
    for step in steps:
        action = str(step.get("action") or "").strip()

        if action not in _UI_ACTIONS:
            out.append(step)
            continue

        # already has a valid target dict — leave untouched
        if isinstance(step.get("target"), dict) and step["target"].get("primary"):
            out.append(step)
            continue

        selector = str(step.get("selector") or "").strip()
        if not selector:
            out.append(step)
            continue

        upgraded = dict(step)
        upgraded["target"] = {
            "primary": selector,
            "fallbacks": _infer_fallbacks(selector),
            "timeout_ms": step.get("timeout_ms", 3000),
            "state": "visible",
        }
        out.append(upgraded)
    return out
