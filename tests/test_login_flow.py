# tests/test_login_flow.py
"""
Tests for login-flow reliability improvements:
  - Part 1: login field selector priorities in build_well_known_form_target()
  - Part 2: _is_submit_click() helper (nav wait guard)
  - Backward compat: existing tests not broken
No browser, no network.
"""
import pytest
from core.selector_resolver import build_well_known_form_target
from runners.generic_steps import _is_submit_click


# ── helpers ───────────────────────────────────────────────────────────────────

def _css_values(target):
    return [f["value"] for f in target.get("fallbacks", []) if f.get("type") == "css"]

def _fb_types(target):
    return [f["type"] for f in target.get("fallbacks", [])]


# ── Part 1a: username field selector priority ─────────────────────────────────

class TestUsernameSelectors:
    def test_primary_is_hash_username(self):
        t = build_well_known_form_target("fill", "username field")
        assert t["primary"] == "#username"

    def test_has_name_username(self):
        t = build_well_known_form_target("fill", "username field")
        assert "input[name='username']" in _css_values(t)

    def test_has_name_user(self):
        t = build_well_known_form_target("fill", "username field")
        assert "input[name='user']" in _css_values(t)

    def test_has_type_text(self):
        t = build_well_known_form_target("fill", "username field")
        assert "input[type='text']" in _css_values(t)

    def test_has_placeholder_substring_css(self):
        """input[placeholder*='user'] catches placeholders like 'Your username'."""
        t = build_well_known_form_target("fill", "username field")
        assert "input[placeholder*='user']" in _css_values(t)

    def test_name_username_before_type_text(self):
        """More specific name selector should come before generic type fallback."""
        vals = _css_values(build_well_known_form_target("fill", "username field"))
        assert vals.index("input[name='username']") < vals.index("input[type='text']")

    def test_has_placeholder_label_fallbacks(self):
        t = build_well_known_form_target("fill", "username field")
        types = _fb_types(t)
        assert "placeholder" in types
        assert "label" in types


# ── Part 1b: password field selector priority ─────────────────────────────────

class TestPasswordSelectors:
    def test_primary_is_hash_password(self):
        t = build_well_known_form_target("fill", "password field")
        assert t["primary"] == "#password"

    def test_has_name_password(self):
        t = build_well_known_form_target("fill", "password field")
        assert "input[name='password']" in _css_values(t)

    def test_has_type_password(self):
        t = build_well_known_form_target("fill", "password field")
        assert "input[type='password']" in _css_values(t)


# ── Part 1c: login button selector priority ───────────────────────────────────

class TestLoginButtonSelectors:
    def test_primary_is_submit_button(self):
        t = build_well_known_form_target("click", "login button")
        assert t["primary"] == "button[type='submit']"

    def test_has_has_text_login_css(self):
        """button:has-text() CSS fallback is present for Login."""
        t = build_well_known_form_target("click", "login button")
        assert "button:has-text('Login')" in _css_values(t)

    def test_has_has_text_sign_in_css(self):
        t = build_well_known_form_target("click", "login button")
        assert "button:has-text('Sign in')" in _css_values(t)

    def test_has_input_submit_fallback(self):
        t = build_well_known_form_target("click", "login button")
        assert "input[type='submit']" in _css_values(t)

    def test_has_text_type_fallbacks(self):
        t = build_well_known_form_target("click", "login button")
        text_vals = [f["value"] for f in t["fallbacks"] if f.get("type") == "text"]
        assert "Login" in text_vals
        assert "Sign in" in text_vals

    def test_has_role_fallback(self):
        t = build_well_known_form_target("click", "login button")
        assert "role" in _fb_types(t)

    def test_has_text_css_before_input_submit(self):
        """button:has-text() is more specific; should appear before input[type='submit']."""
        vals = _css_values(build_well_known_form_target("click", "login button"))
        assert vals.index("button:has-text('Login')") < vals.index("input[type='submit']")

    def test_sign_in_button_resolves(self):
        t = build_well_known_form_target("click", "sign in button")
        assert t is not None
        assert t["primary"] == "button[type='submit']"


# ── Part 2: _is_submit_click() navigation wait guard ─────────────────────────

class TestIsSubmitClick:
    def test_submit_in_sel(self):
        assert _is_submit_click("button[type='submit']", "") is True

    def test_login_in_sel(self):
        assert _is_submit_click("login button", "") is True

    def test_login_in_intent(self):
        assert _is_submit_click("", "click:login button") is True

    def test_sign_in_intent(self):
        assert _is_submit_click("", "sign in button") is True

    def test_signin_intent(self):
        assert _is_submit_click("", "signin") is True

    def test_entrar_intent(self):
        assert _is_submit_click("", "entrar") is True

    def test_ingresar_intent(self):
        assert _is_submit_click("", "ingresar") is True

    def test_acceder_intent(self):
        assert _is_submit_click("", "acceder") is True

    def test_regular_click_returns_false(self):
        assert _is_submit_click("nav-link", "click:nav-link") is False

    def test_username_click_returns_false(self):
        assert _is_submit_click("#username", "fill:username") is False

    def test_empty_returns_false(self):
        assert _is_submit_click("", "") is False

    def test_case_insensitive(self):
        assert _is_submit_click("Login", "") is True
        assert _is_submit_click("SUBMIT", "") is True
        assert _is_submit_click("Sign In", "") is True

    def test_combined_sel_and_intent(self):
        """Both sel and intent are concatenated before checking."""
        assert _is_submit_click("btn-primary", "click:login") is True
