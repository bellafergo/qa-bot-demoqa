# core/runner_contract.py
"""
Multi-runner contract: how catalog `test_type` maps to execution and which
step actions are legal per runner.

- Web  → Playwright (`runners.generic_steps.execute_test`)
- Desktop → pywinauto (`runners.desktop_runner.run_desktop_test`)
- API  → httpx (`services.api_runner.run_api_test`)

`test_type` on TestCase remains the persisted field: ui | desktop | api.
`runner_kind` is the resolved execution lane: web | desktop | api.
"""
from __future__ import annotations

from typing import Optional

from core.schemas import RUNNER_ACTIONS

# ── Web (Playwright) — canonical actions executed by generic_steps ────────────

WEB_RUNNER_ACTIONS: frozenset = RUNNER_ACTIONS

# ── Desktop (pywinauto) — canonical names after alias normalization ──────────
# Keep in sync with runners/desktop_runner.py dispatch table.

_DESKTOP_ALIASES: dict[str, str] = {
    # Synonyms / legacy / user-facing names → canonical
    "input": "input_text",
    "fill": "input_text",
    "type": "input_text",
    "type_text": "input_text",
    "click_control": "click",
    "assert_text": "assert_text_contains",
    "check_text": "assert_text_contains",
    "verify_text": "assert_text_contains",
    "exists": "assert_exists",
    "visible": "assert_exists",
    "assert_control_visible": "assert_exists",
    "assert_visible": "assert_exists",  # web assertion name → desktop assert_exists
    "wait": "wait_for",
    "wait_for_window": "attach_window",
    "focus": "focus_window",
    "attach": "attach_window",
    "launch": "launch_app",
    "start": "launch_app",
    "open": "launch_app",
    "keys": "type_keys",
    "sendkeys": "send_keys",
    "send_keys": "type_keys",
    "sleep": "wait_ms",
    "select_menu": "select",
    "menu_select": "select",
}

DESKTOP_RUNNER_ACTIONS: frozenset = frozenset(
    {
        "launch_app",
        "attach_window",
        "focus_window",
        "wait_for",
        "click",
        "input_text",
        "type_keys",
        "select",
        "read_text",
        "assert_text_contains",
        "assert_exists",
        "screenshot",
        "wait_ms",
    }
)

# ── API runner ────────────────────────────────────────────────────────────────

API_RUNNER_ACTIONS: frozenset = frozenset({"api_request", "wait_ms"})

# Actions that clearly belong to another runner (for error hints)

WEB_ONLY_HINTS: frozenset = frozenset(
    {
        "goto",
        "fill",
        "press",
        "assert_visible",
        "assert_not_visible",
        "assert_url_contains",
    }
)

DESKTOP_ONLY_HINTS: frozenset = frozenset(
    {
        "launch_app",
        "attach_window",
        "focus_window",
        "input_text",
        "type_keys",
        "wait_for",
        "assert_exists",
        "read_text",
        "select",
    }
)

# After normalize_desktop_action — web-only actions that must never run on desktop lane
DESKTOP_INVALID_RAW: frozenset = frozenset(
    {"goto", "fill", "press", "hover", "check", "uncheck", "assert_url_contains"}
)


def normalize_desktop_action(raw: str) -> str:
    a = (raw or "").strip().lower()
    return _DESKTOP_ALIASES.get(a, a)


def runner_kind_for_test_type(test_type: Optional[str]) -> str:
    t = (test_type or "ui").strip().lower()
    if t == "desktop":
        return "desktop"
    if t == "api":
        return "api"
    return "web"


def effective_actions_for_runner_kind(runner_kind: str) -> frozenset:
    k = (runner_kind or "web").strip().lower()
    if k == "desktop":
        return DESKTOP_RUNNER_ACTIONS
    if k == "api":
        return API_RUNNER_ACTIONS
    return WEB_RUNNER_ACTIONS


def mismatch_hint(action: str, runner_kind: str) -> str:
    """Short hint when an action is wrong for the selected runner."""
    a = (action or "").strip().lower()
    rk = (runner_kind or "web").strip().lower()
    if rk == "web" and a in DESKTOP_ONLY_HINTS:
        return (
            f"Action {action!r} is for the desktop runner. "
            "Set test_type to 'desktop' or replace with web actions (goto, fill, click, …)."
        )
    if rk == "desktop" and (a in DESKTOP_INVALID_RAW or a in WEB_ONLY_HINTS):
        return (
            f"Action {action!r} is web-only. "
            "Set test_type to 'ui' or use desktop actions (attach_window, click, input_text, …)."
        )
    if rk == "api" and a not in API_RUNNER_ACTIONS:
        return (
            f"API tests only support {sorted(API_RUNNER_ACTIONS)}. "
            f"Got {action!r}."
        )
    return ""
