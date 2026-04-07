# services/explorer_auth_bootstrap.py
"""
Run compiled login steps on an existing Playwright page (explorer session).

Reuses the same step shape as generic_steps (goto, fill, click, wait_ms,
assert_*). Intended only for short bootstrap sequences from
compile_to_runner_steps([{"action": "login"}], ...).

Security: sanitize_error_for_client strips project variable values from messages.
"""
from __future__ import annotations

import logging
import re
from typing import Any, Dict, List, Optional, Tuple

from playwright.sync_api import TimeoutError as PlaywrightTimeoutError

from runners.common import (
    _as_int,
    _domain_from_url,
    _normalize_action,
    _pick_timeout_ms,
    _safe_str,
    _selector_from_step,
    _url_from_step,
)
from services.selector_healer import resolve_locator

logger = logging.getLogger("vanya.explorer_auth")

_DEFAULT_STEP_TIMEOUT_MS = 18_000


def sanitize_error_for_client(message: str, context: Optional[Dict[str, Any]] = None) -> str:
    """Remove secret values that might appear in Playwright/assertion errors."""
    m = str(message or "")
    ctx = context or {}
    pv = ctx.get("project_variables")
    if isinstance(pv, dict):
        for _k, v in pv.items():
            vs = str(v).strip() if v is not None else ""
            if len(vs) >= 4:
                m = m.replace(vs, "***")
    creds = ctx.get("credentials")
    if isinstance(creds, dict):
        for _k, v in creds.items():
            vs = str(v).strip() if v is not None else ""
            if len(vs) >= 4:
                m = m.replace(vs, "***")
    # crude email scrub
    m = re.sub(
        r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b",
        "***@***",
        m,
    )
    return m.strip() or "Authentication step failed."


def _build_target(step: Dict[str, Any], selector: str) -> Dict[str, Any]:
    action = _normalize_action(step)
    raw_target = step.get("target")
    if isinstance(raw_target, dict):
        t = dict(raw_target)
        if not t.get("primary") and selector:
            t["primary"] = selector
        if not t.get("intent"):
            t["intent"] = f"{action}:{selector}" if selector else action
        return t
    primary = selector.strip() if selector else ""
    return {"intent": f"{action}:{primary}", "primary": primary}


def _normalize_ws(s: str) -> str:
    return " ".join(s.split())


def run_explorer_login_steps(
    page,
    steps: List[Dict[str, Any]],
    *,
    base_url: Optional[str],
    context: Optional[Dict[str, Any]] = None,
) -> Tuple[bool, str, str]:
    """
    Returns (ok, message_for_client, error_code).
    error_code: "" if ok, else one of
      timeout, assertion, navigation, missing_selector, compile_skipped, unknown
    """
    from runners.generic_steps import (  # noqa: WPS433 — reuse HTML5 helpers
        _collect_html5_validation_messages,
        _fail_if_locator_invalid,
        _is_submit_click,
    )

    ctx = context or {}
    inferred_base = base_url
    for st in steps:
        if _normalize_action(st) == "goto":
            u = _url_from_step(st, base_url)
            if u:
                inferred_base = base_url or u
            break

    for i, step in enumerate(steps):
        action = _normalize_action(step)
        timeout_ms = _pick_timeout_ms(step, _DEFAULT_STEP_TIMEOUT_MS)
        try:
            if action == "goto":
                url = _url_from_step(step, inferred_base)
                if not url:
                    return False, "Login navigation failed: missing URL.", "navigation"
                page.goto(url, wait_until="domcontentloaded", timeout=timeout_ms)
                try:
                    page.wait_for_load_state("networkidle", timeout=min(8000, timeout_ms))
                except Exception:
                    try:
                        page.wait_for_timeout(400)
                    except Exception:
                        pass
                continue

            if action == "wait_ms":
                ms = _as_int(step.get("ms"), 500)
                page.wait_for_timeout(ms)
                continue

            sel = _selector_from_step(step)

            if not sel and action not in ("assert_text_contains", "assert_url_contains"):
                return (
                    False,
                    f"Login step {i + 1} ({action}) requires a selector.",
                    "missing_selector",
                )

            domain = ""
            try:
                if page.url:
                    domain = _domain_from_url(page.url)
            except Exception:
                domain = ""

            if action == "fill":
                val = _safe_str(step.get("value"))
                target = _build_target(step, sel)
                locator, _used, _rs, _meta = resolve_locator(
                    page, target, domain=domain or None, timeout_ms=timeout_ms
                )
                locator.wait_for(state="visible", timeout=timeout_ms)
                locator.fill(val, timeout=timeout_ms)
                try:
                    locator.evaluate("el => { try { el.blur(); } catch(e) {} }")
                except Exception:
                    pass
                try:
                    page.wait_for_timeout(120)
                except Exception:
                    pass
                _fail_if_locator_invalid(locator, "after fill")
                continue

            if action == "click":
                target = _build_target(step, sel)
                locator, _used, _rs, _meta = resolve_locator(
                    page, target, domain=domain or None, timeout_ms=timeout_ms
                )
                intent = _safe_str(target.get("intent") or "")
                locator.wait_for(state="visible", timeout=timeout_ms)
                locator.click(timeout=timeout_ms)
                if _is_submit_click(sel, intent):
                    try:
                        page.wait_for_load_state("load", timeout=8000)
                    except Exception:
                        pass
                    try:
                        page.wait_for_timeout(200)
                    except Exception:
                        pass
                    vsubmit = _collect_html5_validation_messages(page)
                    if vsubmit:
                        msg = "; ".join(vsubmit[:4])
                        return (
                            False,
                            sanitize_error_for_client(
                                f"Form validation after submit: {msg}", ctx,
                            ),
                            "assertion",
                        )
                continue

            if action == "assert_visible":
                target = _build_target(step, sel)
                locator, _used, _rs, _meta = resolve_locator(
                    page, target, domain=domain or None, timeout_ms=timeout_ms
                )
                locator.wait_for(state="visible", timeout=timeout_ms)
                continue

            if action == "assert_url_contains":
                needle = _safe_str(step.get("value") or step.get("text") or step.get("contains") or "").strip()
                if not needle:
                    return False, "Login validation misconfigured (URL assert).", "assertion"
                current = page.url or ""
                if needle not in current:
                    return (
                        False,
                        f"After login, the URL did not contain the expected fragment. "
                        f"(Expected a path or query segment configured in the project.)",
                        "assertion",
                    )
                continue

            if action == "assert_text_contains":
                expected_text = _safe_str(step.get("expected") or step.get("text") or "").strip()
                if not expected_text:
                    return False, "Login validation misconfigured (text assert).", "assertion"
                needle = _normalize_ws(expected_text)
                target_sel = _selector_from_step(step) or "body"
                loc = page.locator(target_sel)
                loc.wait_for(state="visible", timeout=timeout_ms)
                content = ""
                try:
                    content = (loc.inner_text(timeout=timeout_ms) or "").strip()
                except Exception:
                    content = ""
                if not content:
                    try:
                        content = (loc.text_content(timeout=timeout_ms) or "").strip()
                    except Exception:
                        content = ""
                if needle not in _normalize_ws(content):
                    try:
                        body_text = str(page.evaluate("() => document.body.innerText") or "")
                        if needle not in _normalize_ws(body_text):
                            return (
                                False,
                                "After login, the expected success text was not found on the page. "
                                "Check success_text in the project login profile.",
                                "assertion",
                            )
                    except Exception:
                        return (
                            False,
                            "After login, the expected success text was not found on the page.",
                            "assertion",
                        )
                continue

            return False, f"Unsupported login step action: {action}", "unknown"

        except PlaywrightTimeoutError as e:
            msg = sanitize_error_for_client(
                f"Timeout during login step {i + 1} ({action}).", ctx,
            )
            logger.info("explorer auth timeout step=%s action=%s", i, action)
            return False, msg, "timeout"
        except Exception as e:
            raw = str(e)
            if "locator_not_found" in raw.lower() or "timeout" in raw.lower():
                msg = sanitize_error_for_client(
                    f"Could not find or interact with an element during login (step {i + 1}: {action}). "
                    f"Verify selectors in the project login profile.",
                    ctx,
                )
                return False, msg, "missing_selector"
            msg = sanitize_error_for_client(raw, ctx)
            return False, msg, "assertion"

    return True, "", ""
