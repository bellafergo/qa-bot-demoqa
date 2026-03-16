# core/semantic_step_builder.py
"""
Maps semantic step intents {kind, name} to structured target dicts compatible
with services/selector_healer.resolve_locator() and runners/generic_steps.py.

This is the common contract between planner and runner:
  Planner  → generates steps with kind + name  (semantic intent)
  Builder  → resolves to primary + fallbacks   (this module)
  Runner   → consumes target dict via selector_healer

Supported kinds:  "input", "button", "text"
Supported names (MVP): "username", "password", "login", "error"

Resolution priority:
  1. site_override   — site-specific primary selector (e.g. SauceDemo #user-name)
  2. well_known      — generic form patterns via selector_resolver
  3. minimal_fallback — [name=X] with no fallbacks (confidence=low → caller warned)
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

from core.selector_resolver import build_well_known_form_target

logger = logging.getLogger("vanya.semantic_step_builder")

# ── Action implied by each kind ───────────────────────────────────────────────
_KIND_ACTION: Dict[str, str] = {
    "input":  "fill",
    "button": "click",
    "text":   "assert_visible",
}

# ── Site-specific overrides ───────────────────────────────────────────────────
# Key: (domain_fragment, kind, name)
# Value: partial target dict (primary + fallbacks) — confidence always "high"
_SITE_OVERRIDES: Dict[tuple, Dict[str, Any]] = {
    # SauceDemo uses non-standard id "user-name" (with hyphen)
    ("saucedemo.com", "input", "username"): {
        "primary": "#user-name",
        "fallbacks": [
            {"type": "css",         "value": "input[name='user-name']"},
            {"type": "css",         "value": "input[data-test='username']"},
            {"type": "name",        "value": "user-name"},
            {"type": "placeholder", "value": "Username"},
            {"type": "label",       "value": "Username"},
        ],
    },
    ("saucedemo.com", "input", "password"): {
        "primary": "#password",
        "fallbacks": [
            {"type": "css",         "value": "input[name='password']"},
            {"type": "css",         "value": "input[type='password']"},
            {"type": "placeholder", "value": "Password"},
            {"type": "label",       "value": "Password"},
        ],
    },
    ("saucedemo.com", "button", "login"): {
        "primary": "#login-button",
        "fallbacks": [
            {"type": "css",  "value": "button[type='submit']"},
            {"type": "css",  "value": "input[type='submit']"},
            {"type": "text", "value": "Login"},
            {"type": "role", "value": {"role": "button", "name": "Login"}},
        ],
    },
    ("saucedemo.com", "text", "error"): {
        "primary": "[data-test='error']",
        "fallbacks": [
            {"type": "css", "value": ".error-message-container"},
            {"type": "css", "value": "[data-testid='error']"},
        ],
    },
}


def build_semantic_target(
    kind: str,
    name: str,
    context_url: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Build a structured target dict from semantic intent {kind, name}.

    Returns a dict with:
        primary      : best-guess CSS / Playwright selector
        fallbacks    : list of {type, value} tried in order by selector_healer
        kind         : original kind  ("input" | "button" | "text")
        name         : original name  ("username" | "password" | "login" | …)
        confidence   : "high" | "medium" | "low"
        resolved_by  : source of resolution (for debugging)

    Backward compat: callers that only read "primary" still work.
    """
    kind = (kind or "").strip().lower()
    name = (name or "").strip().lower()
    domain = (context_url or "").lower()

    # 1. Site-specific override — highest confidence
    for (site, ok, on), spec in _SITE_OVERRIDES.items():
        if site in domain and ok == kind and on == name:
            return {
                **spec,
                "kind": kind,
                "name": name,
                "confidence": "high",
                "resolved_by": "semantic_step_builder.site_override",
            }

    # 2. Generic well-known form target (selector_resolver)
    action = _KIND_ACTION.get(kind, "fill")
    wk = build_well_known_form_target(action, name)
    if wk:
        return {
            **wk,
            "kind": kind,
            "name": name,
            "confidence": "medium",
            "resolved_by": "semantic_step_builder.well_known",
        }

    # 3. Low-confidence minimal fallback — runner will attempt DOM resolution
    logger.warning(
        "[SEMANTIC] No static mapping for kind=%r name=%r url=%r — "
        "using minimal fallback [name=%s] (confidence=low). "
        "Runner will attempt DOM resolution at execution time.",
        kind, name, context_url, name,
    )
    return {
        "primary": f"[name='{name}']",
        "fallbacks": [],
        "kind": kind,
        "name": name,
        "confidence": "low",
        "resolved_by": "semantic_step_builder.minimal_fallback",
    }
