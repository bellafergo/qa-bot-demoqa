# tests/test_form_selectors.py
"""
Unit tests for build_well_known_form_target() in core/selector_resolver.py.
Validates that common form field intents produce correct multi-fallback targets.
No browser, no network.
"""
import pytest
from core.selector_resolver import build_well_known_form_target


# ── helpers ───────────────────────────────────────────────────────────────────

def _css_values(target):
    """Extract 'value' from all css-type fallbacks."""
    return [f["value"] for f in target.get("fallbacks", []) if f.get("type") == "css"]

def _fallback_types(target):
    return [f["type"] for f in target.get("fallbacks", [])]


# ── username / email field ────────────────────────────────────────────────────

class TestUsernameField:
    def test_username_field_returns_target(self):
        t = build_well_known_form_target("fill", "username field")
        assert t is not None

    def test_username_primary_is_id(self):
        t = build_well_known_form_target("fill", "username field")
        assert t["primary"] == "#username"

    def test_username_has_name_fallback(self):
        t = build_well_known_form_target("fill", "username field")
        assert "input[name='username']" in _css_values(t)

    def test_username_has_type_text_fallback(self):
        t = build_well_known_form_target("fill", "username")
        assert "input[type='text']" in _css_values(t)

    def test_username_has_placeholder_fallback(self):
        t = build_well_known_form_target("fill", "username")
        types = _fallback_types(t)
        assert "placeholder" in types

    def test_username_has_label_fallback(self):
        t = build_well_known_form_target("fill", "username")
        types = _fallback_types(t)
        assert "label" in types

    def test_email_field_resolves_to_username_target(self):
        t = build_well_known_form_target("fill", "email address")
        assert t is not None
        assert "input[name='email']" in _css_values(t)

    def test_usuario_spanish_resolves(self):
        t = build_well_known_form_target("fill", "campo usuario")
        assert t is not None

    def test_user_short_form_resolves(self):
        t = build_well_known_form_target("fill", "user field")
        assert t is not None

    def test_press_action_also_resolves_username(self):
        t = build_well_known_form_target("press", "username field")
        assert t is not None


# ── password field ────────────────────────────────────────────────────────────

class TestPasswordField:
    def test_password_field_returns_target(self):
        t = build_well_known_form_target("fill", "password field")
        assert t is not None

    def test_password_primary_is_id(self):
        t = build_well_known_form_target("fill", "password field")
        assert t["primary"] == "#password"

    def test_password_has_type_password_fallback(self):
        t = build_well_known_form_target("fill", "password")
        assert "input[type='password']" in _css_values(t)

    def test_password_has_name_fallback(self):
        t = build_well_known_form_target("fill", "password field")
        assert "input[name='password']" in _css_values(t)

    def test_password_has_placeholder_fallback(self):
        t = build_well_known_form_target("fill", "password")
        assert "placeholder" in _fallback_types(t)

    def test_password_has_multiple_fallbacks(self):
        t = build_well_known_form_target("fill", "password")
        assert len(t["fallbacks"]) >= 3


# ── login / submit button ─────────────────────────────────────────────────────

class TestLoginButton:
    def test_login_button_returns_target(self):
        t = build_well_known_form_target("click", "login button")
        assert t is not None

    def test_login_button_primary_is_submit(self):
        t = build_well_known_form_target("click", "login button")
        assert t["primary"] == "button[type='submit']"

    def test_login_button_has_input_submit_fallback(self):
        t = build_well_known_form_target("click", "login button")
        assert "input[type='submit']" in _css_values(t)

    def test_login_button_has_text_fallbacks(self):
        t = build_well_known_form_target("click", "login button")
        text_values = [f["value"] for f in t["fallbacks"] if f.get("type") == "text"]
        assert "Login" in text_values
        assert "Sign in" in text_values

    def test_login_button_has_role_fallback(self):
        t = build_well_known_form_target("click", "login button")
        assert "role" in _fallback_types(t)

    def test_sign_in_button_resolves(self):
        t = build_well_known_form_target("click", "sign in button")
        assert t is not None
        assert t["primary"] == "button[type='submit']"

    def test_submit_button_resolves(self):
        t = build_well_known_form_target("click", "submit button")
        assert t is not None


# ── unknown / no-match ────────────────────────────────────────────────────────

class TestNoMatch:
    def test_unknown_intent_returns_none(self):
        assert build_well_known_form_target("fill", "search query") is None

    def test_empty_intent_returns_none(self):
        assert build_well_known_form_target("fill", "") is None

    def test_click_username_returns_none(self):
        # click on "username" is not a button — should not match login button pattern
        assert build_well_known_form_target("click", "username") is None

    def test_fill_login_button_returns_none(self):
        # "login" for fill — not a password or username keyword
        # "login" is not in username keyword set, and fill won't match button pattern
        assert build_well_known_form_target("fill", "login button") is None

    def test_noise_only_returns_none(self):
        assert build_well_known_form_target("fill", "field input box") is None

    def test_assert_action_returns_none(self):
        # assert_visible is not a supported action for well-known form targets
        assert build_well_known_form_target("assert_visible", "username") is None


# ── target structure ──────────────────────────────────────────────────────────

class TestTargetStructure:
    def test_target_has_primary_key(self):
        t = build_well_known_form_target("fill", "username")
        assert "primary" in t

    def test_target_has_fallbacks_list(self):
        t = build_well_known_form_target("fill", "username")
        assert isinstance(t["fallbacks"], list)

    def test_all_fallbacks_have_type_and_value(self):
        t = build_well_known_form_target("fill", "password")
        for fb in t["fallbacks"]:
            assert "type" in fb
            assert "value" in fb

    def test_role_fallback_value_is_dict(self):
        t = build_well_known_form_target("click", "login button")
        role_fbs = [f for f in t["fallbacks"] if f.get("type") == "role"]
        assert len(role_fbs) > 0
        for rfb in role_fbs:
            assert isinstance(rfb["value"], dict)
            assert "role" in rfb["value"]
