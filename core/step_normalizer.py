# core/step_normalizer.py
"""
Pure helpers for upgrading flat selector-based steps to target-object steps.
No heavy dependencies — safe to import in tests without Playwright/reportlab.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional

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


# ── Additional fallback inference patterns (Block 16) ─────────────────────────

_RE_CLASS = re.compile(r'^\.([\w-]+)$')
_RE_ARIA_LABEL = re.compile(r'^\[aria-label=["\']([^"\']+)["\']\]$')
_RE_PLACEHOLDER = re.compile(r'^\[placeholder=["\']([^"\']+)["\']\]$')


def _infer_fallbacks_extended(selector: str) -> List[Dict[str, Any]]:
    """
    Extended fallback inference covering more selector patterns.
    Adds class, aria-label, and placeholder patterns on top of _infer_fallbacks.
    """
    # First try the original conservative patterns
    base = _infer_fallbacks(selector)
    if base:
        return base

    # Single class selector → partial-text heuristic (css only)
    m = _RE_CLASS.match(selector)
    if m:
        cls = m.group(1)
        return [{"type": "css", "value": f"[class*='{cls}']"}]

    # [aria-label='...'] → label fallback
    m = _RE_ARIA_LABEL.match(selector)
    if m:
        val = m.group(1)
        return [
            {"type": "label", "value": val},
            {"type": "text",  "value": val},
        ]

    # [placeholder='...'] → placeholder fallback
    m = _RE_PLACEHOLDER.match(selector)
    if m:
        val = m.group(1)
        return [
            {"type": "placeholder", "value": val},
            {"type": "label",       "value": val},
        ]

    return []


def normalize_steps_full(
    steps: List[Dict[str, Any]],
    default_timeout_ms: int = 10_000,
) -> List["NormalizedStep"]:  # type: ignore[name-defined]  # noqa: F821
    """
    Upgraded normalizer that returns typed NormalizedStep objects.

    Differences from normalize_steps_to_target():
    - Returns List[NormalizedStep] instead of List[dict].
    - Uses extended fallback inference (_infer_fallbacks_extended).
    - Applies default_timeout_ms to every step.
    - Passes through all extra fields via metadata.

    Fully backward-compatible: normalize_steps_to_target is unchanged.
    """
    from core.schemas import NormalizedStep, TargetSpec, TargetFallback

    out: List[NormalizedStep] = []

    for step in steps:
        action = str(step.get("action") or "").strip()

        # -- non-UI passthrough steps --
        if action not in _UI_ACTIONS:
            ns = NormalizedStep(
                action=action,
                url=step.get("url") or step.get("value"),
                value=step.get("value"),
                text=step.get("text"),
                key=step.get("key"),
                ms=step.get("ms"),
                expected=step.get("expected"),
                timeout_ms=int(step.get("timeout_ms") or default_timeout_ms),
            )
            out.append(ns)
            continue

        # -- already has structured target --
        raw_target = step.get("target")
        if isinstance(raw_target, dict) and raw_target.get("primary"):
            fbs_raw = raw_target.get("fallbacks") or []
            fbs = [TargetFallback(**fb) if isinstance(fb, dict) else fb for fb in fbs_raw]
            ts = TargetSpec(
                primary=raw_target["primary"],
                fallbacks=fbs,
                timeout_ms=int(raw_target.get("timeout_ms") or default_timeout_ms),
                state=raw_target.get("state", "visible"),
                intent=raw_target.get("intent"),
            )
            ns = NormalizedStep(
                action=action,
                target=ts,
                selector=step.get("selector"),
                fallbacks=fbs,
                value=step.get("value"),
                text=step.get("text"),
                key=step.get("key"),
                ms=step.get("ms"),
                expected=step.get("expected"),
                timeout_ms=ts.timeout_ms,
            )
            out.append(ns)
            continue

        # -- flat selector → build target from it --
        selector = str(step.get("selector") or "").strip()
        if selector:
            raw_fbs = _infer_fallbacks_extended(selector)
            fbs = [TargetFallback(**fb) for fb in raw_fbs]
            ts = TargetSpec(
                primary=selector,
                fallbacks=fbs,
                timeout_ms=default_timeout_ms,
                state="visible",
            )
            ns = NormalizedStep(
                action=action,
                target=ts,
                selector=selector,
                fallbacks=fbs,
                value=step.get("value"),
                text=step.get("text"),
                key=step.get("key"),
                ms=step.get("ms"),
                expected=step.get("expected"),
                timeout_ms=default_timeout_ms,
            )
        else:
            # no selector, no target — pass through as-is (validator will flag it)
            ns = NormalizedStep(
                action=action,
                value=step.get("value"),
                text=step.get("text"),
                key=step.get("key"),
                ms=step.get("ms"),
                expected=step.get("expected"),
                timeout_ms=default_timeout_ms,
            )

        out.append(ns)

    return out


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
