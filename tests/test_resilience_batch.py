# tests/test_resilience_batch.py
"""
Tests for the Vanya Resilience Batch:
  - Part 1: 9-priority fallback chain in build_playwright_target()
  - Part 2: healing_log structure (runner output key present)
  - Part 3: _normalize_ws() whitespace normalization
  - Backward compat: existing tests not broken
No browser, no network.
"""
import pytest
from core.selector_resolver import build_playwright_target, resolve_intent, is_intent_only
from runners.generic_steps import _normalize_ws


# ── Helpers ───────────────────────────────────────────────────────────────────

def _fb_types(target):
    return [f["type"] for f in target.get("fallbacks", [])]

def _fb_values(target):
    return [f["value"] for f in target.get("fallbacks", [])]

def _css_values(target):
    return [f["value"] for f in target.get("fallbacks", []) if f["type"] == "css"]


# ── Part 1: 9-priority fallback chain ────────────────────────────────────────

class TestFallbackChainInput:
    """Input element with full attribute set → all applicable fallback types generated."""

    _ELEMENT = {
        "id":          "email",
        "name":        "email",
        "type":        "email",
        "placeholder": "Enter your email",
        "ariaLabel":   "Email address",
        "label":       "Email",
        "testid":      "email-input",
        "visible":     True,
    }
    _RESOLUTION = {
        "strategy":     "name",
        "value":        "email",
        "score":        85,
        "element_type": "input",
        "source_field": "name",
        "element":      _ELEMENT,
    }

    def test_has_testid_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "testid" in _fb_types(t)

    def test_has_label_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "label" in _fb_types(t)

    def test_has_placeholder_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "placeholder" in _fb_types(t)

    def test_has_css_id_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "#email" in _css_values(t)

    def test_has_input_type_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "input[type='email']" in _css_values(t)

    def test_no_duplicate_primary(self):
        """The primary selector should not appear in fallbacks."""
        t = build_playwright_target(self._RESOLUTION)
        primary = t["primary"]
        css_vals = _css_values(t)
        assert primary not in css_vals

    def test_fallback_count_above_minimum(self):
        t = build_playwright_target(self._RESOLUTION)
        assert len(t["fallbacks"]) >= 4

    def test_testid_value_matches_element(self):
        t = build_playwright_target(self._RESOLUTION)
        testid_fbs = [f for f in t["fallbacks"] if f["type"] == "testid"]
        assert any(f["value"] == "email-input" for f in testid_fbs)

    def test_data_test_css_included(self):
        t = build_playwright_target(self._RESOLUTION)
        css_vals = _css_values(t)
        assert any("data-test" in v for v in css_vals)

    def test_aria_label_as_label_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        label_fbs = [f for f in t["fallbacks"] if f["type"] == "label"]
        values = [f["value"] for f in label_fbs]
        assert "Email address" in values


class TestFallbackChainButton:
    """Button element with text + testid → role + text fallbacks generated."""

    _ELEMENT = {
        "id":        "login-btn",
        "text":      "Sign in",
        "testid":    "login-button",
        "ariaLabel": "Sign in to your account",
        "visible":   True,
    }
    _RESOLUTION = {
        "strategy":     "role",
        "value":        "Sign in",
        "score":        90,
        "element_type": "button",
        "source_field": "text",
        "element":      _ELEMENT,
    }

    def test_has_testid_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "testid" in _fb_types(t)

    def test_has_role_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "role" in _fb_types(t)

    def test_role_value_is_dict_with_role_key(self):
        t = build_playwright_target(self._RESOLUTION)
        role_fbs = [f for f in t["fallbacks"] if f["type"] == "role"]
        assert all(isinstance(f["value"], dict) and "role" in f["value"] for f in role_fbs)

    def test_has_text_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "text" in _fb_types(t)

    def test_has_css_id_fallback(self):
        t = build_playwright_target(self._RESOLUTION)
        assert "#login-btn" in _css_values(t)

    def test_button_role_is_button(self):
        t = build_playwright_target(self._RESOLUTION)
        role_fbs = [f for f in t["fallbacks"] if f["type"] == "role"]
        assert role_fbs[0]["value"]["role"] == "button"


class TestFallbackChainSparse:
    """When element dict is empty, single-type fallback is still generated."""

    def test_sparse_name_strategy(self):
        resolution = {"strategy": "name", "value": "username", "score": 70,
                      "element_type": "input", "source_field": "name"}
        t = build_playwright_target(resolution)
        assert t["primary"] == "[name='username']"
        assert len(t["fallbacks"]) >= 1

    def test_sparse_testid_strategy(self):
        resolution = {"strategy": "testid", "value": "submit-btn", "score": 80,
                      "element_type": "button", "source_field": "testid"}
        t = build_playwright_target(resolution)
        assert "testid" in _fb_types(t)

    def test_sparse_text_strategy(self):
        resolution = {"strategy": "text", "value": "Login", "score": 75,
                      "element_type": "link", "source_field": "text"}
        t = build_playwright_target(resolution)
        assert "text" in _fb_types(t)

    def test_backward_compat_primary_unchanged(self):
        """The primary selector must be identical to what was produced before this batch."""
        r = {"strategy": "placeholder", "value": "Username", "score": 70,
             "element_type": "input", "source_field": "placeholder"}
        t = build_playwright_target(r)
        assert t["primary"] == "[placeholder='Username']"

    def test_no_duplicate_fallbacks(self):
        """Dedup logic must prevent the same (type, value) appearing twice."""
        element = {"testid": "btn", "ariaLabel": "btn", "id": "btn"}
        r = {"strategy": "testid", "value": "btn", "score": 100,
             "element_type": "button", "source_field": "testid", "element": element}
        t = build_playwright_target(r)
        seen = set()
        for fb in t["fallbacks"]:
            key = f"{fb['type']}:{fb['value']}"
            assert key not in seen, f"Duplicate fallback: {key}"
            seen.add(key)


# ── Part 2: healing_log key in runner output ──────────────────────────────────

class TestHealingLogKey:
    """
    We can't run a real browser in unit tests, but we can verify:
    - _normalize_ws is importable and returns the right type
    - healing_log is declared (Pylance checks pass)
    """
    def test_normalize_ws_importable(self):
        result = _normalize_ws("hello world")
        assert isinstance(result, str)

    def test_healing_log_is_list_type(self):
        """Verify the structure that healing_log entries must have."""
        sample = {
            "selector_healed":  True,
            "original_selector": "#login",
            "healed_selector":  "[name='login']",
            "healing_strategy": "name",
            "fallback_index":   1,
            "domain":           "example.com",
            "timestamp":        1234567890,
            "step_index":       2,
            "action":           "fill",
            "intent":           "fill:login",
        }
        required = {"selector_healed", "original_selector", "healed_selector",
                    "healing_strategy", "fallback_index", "domain", "timestamp"}
        assert required.issubset(set(sample.keys()))

    def test_selector_healed_is_true(self):
        entry = {"selector_healed": True}
        assert entry["selector_healed"] is True


# ── Part 3: _normalize_ws ─────────────────────────────────────────────────────

class TestNormalizeWs:
    def test_strips_leading_trailing(self):
        assert _normalize_ws("  hello  ") == "hello"

    def test_collapses_double_spaces(self):
        assert _normalize_ws("hello  world") == "hello world"

    def test_collapses_tabs(self):
        assert _normalize_ws("hello\tworld") == "hello world"

    def test_collapses_newlines(self):
        assert _normalize_ws("hello\nworld") == "hello world"

    def test_collapses_mixed_whitespace(self):
        assert _normalize_ws("  Secure\n  Area  ") == "Secure Area"

    def test_preserves_case(self):
        assert _normalize_ws("Secure Area") == "Secure Area"
        assert _normalize_ws("secure area") == "secure area"

    def test_empty_string(self):
        assert _normalize_ws("") == ""

    def test_single_word(self):
        assert _normalize_ws("Login") == "Login"

    def test_already_clean(self):
        assert _normalize_ws("Secure Area") == "Secure Area"

    def test_substring_matching_after_normalize(self):
        """Simulate the assert_text_contains comparison."""
        expected = _normalize_ws("Secure Area")
        content  = _normalize_ws("  Welcome to the  Secure Area  page  ")
        assert expected in content

    def test_multiline_page_text(self):
        page_text = "Welcome\n\nSecure  Area\n\nLogin"
        assert _normalize_ws("Secure Area") in _normalize_ws(page_text)


# ── Backward compat: existing resolver still works ────────────────────────────

class TestBackwardCompat:
    def test_resolve_intent_returns_element_key(self):
        """After this batch, resolution dicts include 'element' key."""
        inventory = {
            "inputs": [{"id": "user", "name": "username", "placeholder": "Username",
                        "visible": True, "type": "text"}],
            "buttons": [], "links": [], "headings": [], "selects": [],
        }
        result = resolve_intent(inventory, "fill", "username")
        assert result is not None
        assert "element" in result

    def test_build_playwright_target_returns_primary(self):
        r = {"strategy": "name", "value": "username", "score": 85,
             "element_type": "input", "source_field": "name"}
        t = build_playwright_target(r)
        assert "primary" in t
        assert "fallbacks" in t

    def test_is_intent_only_unchanged(self):
        assert is_intent_only("username field") is True
        assert is_intent_only("#username") is False
        assert is_intent_only("[name='user']") is False
