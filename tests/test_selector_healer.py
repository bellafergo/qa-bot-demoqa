# tests/test_selector_healer.py
"""
Unit tests for services/selector_healer.py — resolve_locator() and helpers.

Run with:
  python -m pytest tests/test_selector_healer.py -v
Or standalone:
  python tests/test_selector_healer.py
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from unittest.mock import MagicMock

from services.selector_healer import (
    _classify_resolution_failure,
    resolve_locator,
)


# ============================================================
# Mock helpers
# ============================================================

def _ok_loc():
    """Locator whose wait_for always succeeds."""
    loc = MagicMock()
    loc.wait_for.return_value = None
    loc.first = MagicMock()
    loc.first.wait_for.return_value = None
    return loc


def _fail_loc():
    """Locator whose wait_for always raises."""
    loc = MagicMock()
    loc.wait_for.side_effect = Exception("element not found")
    loc.first = MagicMock()
    loc.first.wait_for.side_effect = Exception("element not found")
    return loc


def _page_all_fail():
    """Mock page where every locator strategy fails."""
    page = MagicMock()
    page.locator.return_value = _fail_loc()
    page.get_by_test_id.return_value = _fail_loc()
    page.get_by_text.return_value = _fail_loc()
    page.get_by_role.return_value = _fail_loc()
    page.get_by_label.return_value = _fail_loc()
    page.get_by_placeholder.return_value = _fail_loc()
    return page


def _page_css_finds(selector: str):
    """Mock page where only the given CSS selector succeeds."""
    page = _page_all_fail()
    ok = _ok_loc()
    page.locator.side_effect = lambda sel: ok if sel == selector else _fail_loc()
    return page


def _page_testid_finds(testid: str):
    page = _page_all_fail()
    page.locator.return_value = _fail_loc()
    ok = _ok_loc()
    page.get_by_test_id.side_effect = lambda val: ok if val == testid else _fail_loc()
    return page


def _page_role_finds(role: str, name=None):
    page = _page_all_fail()
    ok = _ok_loc()
    page.get_by_role.side_effect = lambda r, **kw: ok if r == role else _fail_loc()
    return page


def _page_label_finds(label: str):
    page = _page_all_fail()
    ok = _ok_loc()
    page.get_by_label.side_effect = lambda val: ok if val == label else _fail_loc()
    return page


def _page_text_finds(text: str):
    page = _page_all_fail()
    ok = _ok_loc()
    page.get_by_text.side_effect = lambda val, **kw: ok if val == text else _fail_loc()
    return page


def _page_placeholder_finds(ph: str):
    page = _page_all_fail()
    ok = _ok_loc()
    page.get_by_placeholder.side_effect = lambda val: ok if val == ph else _fail_loc()
    return page


# ============================================================
# Primary success
# ============================================================

def test_primary_success():
    page = _page_css_finds("#login-button")
    loc, used, resolved, meta = resolve_locator(page, {"primary": "#login-button"})
    assert used == "primary"
    assert resolved == "#login-button"


def test_primary_success_meta_shape():
    page = _page_css_finds("#foo")
    _, _, _, meta = resolve_locator(page, {"primary": "#foo"})
    assert meta["fallback_index"] is None
    assert meta["fallback_type"] is None
    assert meta["attempts"] == 1
    assert meta["errors"] == []


# ============================================================
# Fallback success after primary failure
# ============================================================

def test_css_fallback_after_primary_fail():
    page = _page_all_fail()
    fb_loc = _ok_loc()
    page.locator.side_effect = lambda sel: fb_loc if sel == ".alt" else _fail_loc()
    target = {"primary": "#missing", "fallbacks": [{"type": "css", "value": ".alt"}]}
    _, used, resolved, meta = resolve_locator(page, target)
    assert used == "css"
    assert resolved == ".alt"
    assert meta["fallback_index"] == 0
    assert meta["fallback_type"] == "css"
    assert meta["attempts"] == 2  # 1 primary + 1 fallback


def test_testid_fallback():
    page = _page_testid_finds("login-btn")
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "testid", "value": "login-btn"}],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert used == "testid"
    assert resolved == "testid=login-btn"
    assert meta["fallback_type"] == "testid"


def test_name_fallback():
    page = _page_css_finds("[name='username']")
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "name", "value": "username"}],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert used == "name"
    assert resolved == "[name='username']"


def test_role_fallback():
    page = _page_role_finds("button")
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "role", "value": {"role": "button", "name": "Sign in"}}],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert used == "role"
    assert "role=button" in resolved


def test_label_fallback():
    page = _page_label_finds("Email address")
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "label", "value": "Email address"}],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert used == "label"
    assert resolved == "label=Email address"


def test_text_fallback():
    page = _page_text_finds("Sign in")
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "text", "value": "Sign in"}],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert used == "text"
    assert resolved == "text=Sign in"


def test_placeholder_fallback():
    page = _page_placeholder_finds("Enter email")
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "placeholder", "value": "Enter email"}],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert used == "placeholder"
    assert resolved == "placeholder=Enter email"


# ============================================================
# Fallback order respected
# ============================================================

def test_fallback_order_first_wins():
    """If the first fallback succeeds, the second is never tried."""
    page = _page_all_fail()
    first_loc = _ok_loc()
    call_count = [0]

    def locator_se(sel):
        call_count[0] += 1
        if sel == ".first":
            return first_loc
        return _fail_loc()

    page.locator.side_effect = locator_se
    target = {
        "primary": "#missing",
        "fallbacks": [
            {"type": "css", "value": ".first"},
            {"type": "css", "value": ".second"},
        ],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert resolved == ".first"
    assert meta["fallback_index"] == 0
    # ".second" selector was never passed to page.locator
    called_selectors = [c.args[0] for c in page.locator.call_args_list]
    assert ".second" not in called_selectors


def test_fallback_order_second_wins_when_first_fails():
    page = _page_all_fail()
    second_loc = _ok_loc()

    def locator_se(sel):
        if sel == ".second":
            return second_loc
        return _fail_loc()

    page.locator.side_effect = locator_se
    target = {
        "primary": "#missing",
        "fallbacks": [
            {"type": "css", "value": ".first"},
            {"type": "css", "value": ".second"},
        ],
    }
    _, used, resolved, meta = resolve_locator(page, target)
    assert resolved == ".second"
    assert meta["fallback_index"] == 1
    assert meta["attempts"] == 3  # primary + .first + .second


# ============================================================
# Unsupported fallback types
# ============================================================

def test_unsupported_type_ignored_safely():
    """An unknown fallback type must be skipped without raising."""
    page = _page_css_finds("#real")
    target = {
        "primary": "#real",
        "fallbacks": [{"type": "xpath", "value": "//button"}],  # not supported
    }
    _, used, _, _ = resolve_locator(page, target)
    assert used == "primary"  # primary succeeded before unsupported fallback needed


def test_unsupported_type_falls_through_to_supported():
    """Unsupported type is skipped; next supported type is tried."""
    page = _page_testid_finds("btn")
    target = {
        "primary": "#missing",
        "fallbacks": [
            {"type": "xpath", "value": "//button"},     # unsupported — skip
            {"type": "testid", "value": "btn"},          # supported — should win
        ],
    }
    _, used, _, meta = resolve_locator(page, target)
    assert used == "testid"
    assert meta["fallback_index"] == 1  # xpath was at index 0, testid at 1


def test_only_unsupported_types_classification():
    """If only unsupported fallbacks, classification is unsupported_fallback_type."""
    page = _page_all_fail()
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "xpath", "value": "//div"}],
    }
    try:
        resolve_locator(page, target)
        assert False, "should have raised"
    except Exception as e:
        payload = e.args[0]
        assert payload["classification"] == "unsupported_fallback_type"


# ============================================================
# Total failure
# ============================================================

def test_all_fallbacks_fail_raises():
    page = _page_all_fail()
    target = {
        "primary": "#missing",
        "fallbacks": [{"type": "css", "value": ".also-missing"}],
    }
    try:
        resolve_locator(page, target)
        assert False, "should have raised"
    except Exception as e:
        payload = e.args[0]
        assert payload["reason"] == "locator_not_found"
        assert "errors" in payload
        assert len(payload["errors"]) >= 2  # primary + fallback


def test_all_fallbacks_fail_classification():
    page = _page_all_fail()
    target = {
        "primary": "#p",
        "fallbacks": [{"type": "css", "value": ".f"}],
    }
    try:
        resolve_locator(page, target)
    except Exception as e:
        assert e.args[0]["classification"] == "all_fallbacks_failed"


def test_primary_not_found_classification():
    page = _page_all_fail()
    target = {"primary": "#missing"}
    try:
        resolve_locator(page, target)
    except Exception as e:
        assert e.args[0]["classification"] == "primary_not_found"


def test_no_primary_no_fallback_raises_value_error():
    page = MagicMock()
    try:
        resolve_locator(page, {"primary": "", "fallbacks": []})
        assert False, "should have raised"
    except ValueError:
        pass  # expected


def test_non_dict_target_raises_value_error():
    page = MagicMock()
    try:
        resolve_locator(page, "not-a-dict")
        assert False, "should have raised"
    except ValueError:
        pass


# ============================================================
# _classify_resolution_failure unit tests
# ============================================================

def test_classify_invalid_target():
    result = _classify_resolution_failure([], "", [], 0)
    assert result == "invalid_target"


def test_classify_primary_not_found():
    errors = ["primary failed: Exception: not found"]
    result = _classify_resolution_failure(errors, "#foo", [], 0)
    assert result == "primary_not_found"


def test_classify_all_fallbacks_failed():
    errors = ["primary failed: Exception: nope", "css[0] failed: Exception: nope"]
    result = _classify_resolution_failure(errors, "#foo", [{"type": "css"}], 0)
    assert result == "all_fallbacks_failed"


def test_classify_unsupported_only():
    errors = ["primary failed: Exception: nope"]
    result = _classify_resolution_failure(errors, "#foo", [{"type": "xpath"}], 1)
    assert result == "unsupported_fallback_type"


def test_classify_transient():
    errors = ["primary failed: TimeoutError: Timeout 3000ms exceeded"]
    result = _classify_resolution_failure(errors, "#foo", [], 0)
    assert result == "transient"


# ============================================================
# Backward compat — 4-tuple unpacking
# ============================================================

def test_return_is_four_tuple():
    page = _page_css_finds("#btn")
    result = resolve_locator(page, {"primary": "#btn"})
    assert len(result) == 4


def test_four_tuple_unpack():
    page = _page_css_finds("#btn")
    loc, used, resolved, meta = resolve_locator(page, {"primary": "#btn"})
    assert used == "primary"
    assert isinstance(meta, dict)


# ============================================================
# Standalone runner
# ============================================================

if __name__ == "__main__":
    tests = [
        test_primary_success,
        test_primary_success_meta_shape,
        test_css_fallback_after_primary_fail,
        test_testid_fallback,
        test_name_fallback,
        test_role_fallback,
        test_label_fallback,
        test_text_fallback,
        test_placeholder_fallback,
        test_fallback_order_first_wins,
        test_fallback_order_second_wins_when_first_fails,
        test_unsupported_type_ignored_safely,
        test_unsupported_type_falls_through_to_supported,
        test_only_unsupported_types_classification,
        test_all_fallbacks_fail_raises,
        test_all_fallbacks_fail_classification,
        test_primary_not_found_classification,
        test_no_primary_no_fallback_raises_value_error,
        test_non_dict_target_raises_value_error,
        test_classify_invalid_target,
        test_classify_primary_not_found,
        test_classify_all_fallbacks_failed,
        test_classify_unsupported_only,
        test_classify_transient,
        test_return_is_four_tuple,
        test_four_tuple_unpack,
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
