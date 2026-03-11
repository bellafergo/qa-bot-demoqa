# tests/test_page_context.py
"""
Unit tests for the lightweight page/DOM context capture helpers and
the enriched runner result contract.

Run with:
  python -m pytest tests/test_page_context.py -v
Or standalone:
  python tests/test_page_context.py
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from runners.page_context import (
    build_failure_context,
    capture_element_context,
    capture_page_context,
)


# ============================================================
# Mock helpers — no Playwright required
# ============================================================

class _MockPage:
    """Minimal page mock with controllable url/title."""
    def __init__(self, url="https://example.com/login", title="Login Page"):
        self.url = url
        self._title = title

    def title(self):
        return self._title


class _BrokenPage:
    """Simulates a page whose url/title raise."""
    @property
    def url(self):
        raise RuntimeError("page crashed")

    def title(self):
        raise RuntimeError("page crashed")


class _MockLocator:
    """Minimal locator mock."""
    def __init__(self, attrs=None, text=""):
        self._attrs = attrs or {}
        self._text = text

    def get_attribute(self, name, timeout=None):
        if name not in self._attrs:
            return None
        return self._attrs[name]

    def inner_text(self, timeout=None):
        return self._text


class _BrokenLocator:
    """Simulates a locator whose methods always raise."""
    def get_attribute(self, name, timeout=None):
        raise RuntimeError("locator broken")

    def inner_text(self, timeout=None):
        raise RuntimeError("locator broken")


# ============================================================
# capture_page_context
# ============================================================

def test_page_context_happy_path():
    ctx = capture_page_context(_MockPage("https://example.com/inventory", "Products"))
    assert ctx["url"] == "https://example.com/inventory"
    assert ctx["title"] == "Products"


def test_page_context_empty_url():
    page = _MockPage(url="", title="Empty")
    ctx = capture_page_context(page)
    assert ctx["url"] == ""
    assert ctx["title"] == "Empty"


def test_page_context_fails_gracefully():
    """A broken page must return a partial dict, never raise."""
    ctx = capture_page_context(_BrokenPage())
    assert isinstance(ctx, dict)
    assert "url" in ctx   # key present even if empty


def test_page_context_no_full_dom():
    """Result must not contain raw HTML or dom key."""
    ctx = capture_page_context(_MockPage())
    assert "html" not in ctx
    assert "dom" not in ctx
    assert "body" not in ctx


# ============================================================
# capture_element_context
# ============================================================

def test_element_context_standard_attrs():
    loc = _MockLocator(attrs={"id": "user-name", "name": "username", "placeholder": "Username"})
    ctx = capture_element_context(loc)
    assert ctx["id"] == "user-name"
    assert ctx["name"] == "username"
    assert ctx["placeholder"] == "Username"


def test_element_context_data_attrs():
    loc = _MockLocator(attrs={"data-testid": "login-btn", "data-test": "login"})
    ctx = capture_element_context(loc)
    assert ctx["data-testid"] == "login-btn"
    assert ctx["data-test"] == "login"


def test_element_context_visible_text_captured():
    loc = _MockLocator(text="Sign in")
    ctx = capture_element_context(loc)
    assert ctx.get("text") == "Sign in"


def test_element_context_text_truncated():
    long_text = "A" * 200
    loc = _MockLocator(text=long_text)
    ctx = capture_element_context(loc)
    assert len(ctx.get("text", "")) <= 120


def test_element_context_missing_attrs_omitted():
    """Attributes not present on the element should not appear in ctx."""
    loc = _MockLocator(attrs={"id": "submit"})
    ctx = capture_element_context(loc)
    assert "id" in ctx
    assert "aria-label" not in ctx
    assert "role" not in ctx


def test_element_context_fails_gracefully():
    """A broken locator must return an empty dict, never raise."""
    ctx = capture_element_context(_BrokenLocator())
    assert isinstance(ctx, dict)


def test_element_context_no_full_dom():
    """Result must not contain innerHTML or outerHTML."""
    loc = _MockLocator(attrs={"id": "foo"}, text="bar")
    ctx = capture_element_context(loc)
    assert "innerHTML" not in ctx
    assert "outerHTML" not in ctx
    assert "html" not in ctx


# ============================================================
# build_failure_context
# ============================================================

def test_failure_context_basic_shape():
    step = {"action": "click", "selector": "#login-button", "target": {"primary": "#login-button"}}
    page_ctx = {"url": "https://example.com", "title": "Login"}
    fc = build_failure_context(step_index=3, action="click", step=step, error_str="Timeout", page_ctx=page_ctx)

    assert fc["step_index"] == 3
    assert fc["action"] == "click"
    assert fc["original_selector"] == "#login-button"
    assert fc["primary"] == "#login-button"
    assert fc["error"] == "Timeout"
    assert fc["page"] == page_ctx


def test_failure_context_no_target():
    step = {"action": "fill", "selector": "#user-name"}
    fc = build_failure_context(step_index=1, action="fill", step=step, error_str="Element not found")
    assert fc["primary"] is None
    assert fc["original_selector"] == "#user-name"


def test_failure_context_no_page_ctx():
    step = {"action": "click", "selector": "#btn"}
    fc = build_failure_context(step_index=0, action="click", step=step, error_str="err")
    assert isinstance(fc["page"], dict)   # always a dict, never None


# ============================================================
# runner result backward compatibility
# ============================================================

def test_runner_result_has_new_fields():
    """
    Verify that the three new fields are present in the result dict.
    Uses a real execute_test call with invalid steps so no Playwright needed.
    """
    from runners.generic_steps import execute_test
    result = execute_test(steps=[])  # empty steps → immediate return

    assert "resolution_log" in result
    assert "failure_context" in result
    assert "page_context" in result


def test_runner_result_preserves_existing_fields():
    from runners.generic_steps import execute_test
    result = execute_test(steps=[])

    required = {"ok", "status", "expected", "outcome", "reason",
                "evidence_id", "steps", "logs", "screenshot_b64",
                "duration_ms", "meta"}
    missing = required - set(result.keys())
    assert not missing, f"Missing legacy fields: {missing}"


def test_runner_result_resolution_log_is_list():
    from runners.generic_steps import execute_test
    result = execute_test(steps=[])
    assert isinstance(result["resolution_log"], list)


def test_runner_result_failure_context_none_on_empty():
    from runners.generic_steps import execute_test
    result = execute_test(steps=[])
    # empty steps → failure is "steps vacío", failure_context is None
    # (the early-return path doesn't run the step loop)
    assert result["failure_context"] is None


# ============================================================
# Standalone runner
# ============================================================

if __name__ == "__main__":
    tests = [
        test_page_context_happy_path,
        test_page_context_empty_url,
        test_page_context_fails_gracefully,
        test_page_context_no_full_dom,
        test_element_context_standard_attrs,
        test_element_context_data_attrs,
        test_element_context_visible_text_captured,
        test_element_context_text_truncated,
        test_element_context_missing_attrs_omitted,
        test_element_context_fails_gracefully,
        test_element_context_no_full_dom,
        test_failure_context_basic_shape,
        test_failure_context_no_target,
        test_failure_context_no_page_ctx,
        test_runner_result_has_new_fields,
        test_runner_result_preserves_existing_fields,
        test_runner_result_resolution_log_is_list,
        test_runner_result_failure_context_none_on_empty,
    ]

    passed = failed = 0
    for fn in tests:
        try:
            fn()
            print(f"  [PASS] {fn.__name__}")
            passed += 1
        except Exception as e:
            print(f"  [FAIL] {fn.__name__}: {e}")
            failed += 1

    print(f"\n{passed} passed, {failed} failed")
    sys.exit(0 if failed == 0 else 1)
