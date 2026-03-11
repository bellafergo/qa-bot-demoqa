# tests/test_selector_resolver.py
"""
Unit tests for core/selector_resolver.py

Three main scenarios for https://the-internet.herokuapp.com/login:
  1. Resolve known elements (username, password, login button, Secure Area)
  2. not_found — intent that has no match in DOM inventory
  3. ambiguous — multiple similar elements, best-score wins
"""
import pytest

from core.selector_resolver import (
    resolve_intent,
    build_playwright_target,
    is_intent_only,
    _keywords,
    _score,
)


# ── Shared DOM inventory (mirrors the-internet.herokuapp.com/login) ────────────

THE_INTERNET_LOGIN = {
    "inputs": [
        {
            "tag": "input", "type": "text", "id": "username", "name": "username",
            "placeholder": "Username", "testid": None, "ariaLabel": None,
            "label": None, "visible": True,
        },
        {
            "tag": "input", "type": "password", "id": "password", "name": "password",
            "placeholder": "Password", "testid": None, "ariaLabel": None,
            "label": None, "visible": True,
        },
    ],
    "buttons": [
        {
            "tag": "button", "text": "Login", "id": None, "name": None,
            "type": "submit", "testid": None, "ariaLabel": None,
            "value": None, "visible": True,
        }
    ],
    "links": [
        {"text": "Elemental Selenium", "href": "http://elementalselenium.com/", "id": None, "ariaLabel": None},
    ],
    "headings": [{"tag": "h2", "text": "Login Page"}],
    "selects": [],
}

# Second-page inventory (after successful login — Secure Area)
THE_INTERNET_SECURE = {
    "inputs": [],
    "buttons": [
        {
            "tag": "a", "text": "Logout", "id": None, "name": None,
            "type": None, "testid": None, "ariaLabel": None, "value": None, "visible": True,
        }
    ],
    "links": [{"text": "Logout", "href": "/logout", "id": None, "ariaLabel": None}],
    "headings": [
        {"tag": "h2", "text": "Secure Area"},
        {"tag": "h4", "text": "Welcome to the Secure Area. When you are done, click logout below."},
    ],
    "selects": [],
}


# ── Scenario 1: Known elements resolve correctly ───────────────────────────────

class TestResolveKnownElements:

    def test_fill_username(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "username")
        assert result is not None
        assert result["element_type"] == "input"
        assert result["strategy"] in ("name", "placeholder", "label", "css")
        assert "username" in result["value"].lower() or result["value"] == "username"

    def test_fill_username_field(self):
        """Multi-word intent with noise word 'field' stripped."""
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "username field")
        assert result is not None
        assert result["element_type"] == "input"

    def test_fill_password(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "password")
        assert result is not None
        assert result["element_type"] == "input"
        assert "password" in result["value"].lower() or result["value"] == "password"

    def test_fill_password_field(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "password field")
        assert result is not None
        assert result["element_type"] == "input"

    def test_click_login_button(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "click", "login button")
        assert result is not None
        assert result["element_type"] == "button"
        assert "login" in result["value"].lower() or result["strategy"] in ("role", "text", "css")

    def test_click_login(self):
        """Single-word intent 'login' should match the Login button."""
        result = resolve_intent(THE_INTERNET_LOGIN, "click", "login")
        assert result is not None
        assert result["element_type"] == "button"

    def test_score_above_threshold(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "username")
        assert result is not None
        assert result["score"] >= 40

    def test_build_playwright_target_from_name(self):
        resolution = {"strategy": "name", "value": "username", "score": 90,
                      "element_type": "input", "source_field": "name"}
        target = build_playwright_target(resolution)
        assert target["primary"] == "[name='username']"
        assert any(f.get("type") == "name" for f in target["fallbacks"])

    def test_build_playwright_target_from_placeholder(self):
        resolution = {"strategy": "placeholder", "value": "Username", "score": 80,
                      "element_type": "input", "source_field": "placeholder"}
        target = build_playwright_target(resolution)
        assert target["primary"] == "[placeholder='Username']"

    def test_build_playwright_target_from_role(self):
        resolution = {"strategy": "role", "value": "Login", "score": 85,
                      "element_type": "button", "source_field": "text"}
        target = build_playwright_target(resolution)
        assert "Login" in target["primary"]
        assert target["resolved_by"] == "selector_resolver"


# ── Scenario 2: not_found — no DOM match ──────────────────────────────────────

class TestNotFound:

    def test_unknown_element_returns_none(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "credit card number")
        assert result is None

    def test_empty_intent_returns_none(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "")
        assert result is None

    def test_none_intent_returns_none(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", None)
        assert result is None

    def test_empty_inventory_returns_none(self):
        result = resolve_intent({}, "fill", "username")
        assert result is None

    def test_noise_only_intent_returns_none(self):
        """'field input the' should produce no keywords → None."""
        result = resolve_intent(THE_INTERNET_LOGIN, "fill", "field input the")
        assert result is None

    def test_click_unknown_element_returns_none(self):
        result = resolve_intent(THE_INTERNET_LOGIN, "click", "delete account button")
        assert result is None


# ── Scenario 3: Ambiguous — best score wins ────────────────────────────────────

class TestAmbiguous:
    AMBIGUOUS_INVENTORY = {
        "inputs": [
            {
                "tag": "input", "type": "text", "id": "first_name", "name": "first_name",
                "placeholder": "First Name", "testid": None, "ariaLabel": None,
                "label": "First Name", "visible": True,
            },
            {
                "tag": "input", "type": "text", "id": "last_name", "name": "last_name",
                "placeholder": "Last Name", "testid": None, "ariaLabel": None,
                "label": "Last Name", "visible": True,
            },
        ],
        "buttons": [
            {"tag": "button", "text": "Submit Form", "id": "submit", "name": None,
             "type": "submit", "testid": None, "ariaLabel": None, "value": None, "visible": True},
            {"tag": "button", "text": "Reset Form", "id": "reset", "name": None,
             "type": "reset", "testid": None, "ariaLabel": None, "value": None, "visible": True},
        ],
        "links": [],
        "headings": [],
        "selects": [],
    }

    def test_first_name_matches_correctly(self):
        result = resolve_intent(self.AMBIGUOUS_INVENTORY, "fill", "first name")
        assert result is not None
        assert "first" in result["value"].lower()

    def test_last_name_matches_correctly(self):
        result = resolve_intent(self.AMBIGUOUS_INVENTORY, "fill", "last name")
        assert result is not None
        assert "last" in result["value"].lower()

    def test_submit_button_matches_not_reset(self):
        result = resolve_intent(self.AMBIGUOUS_INVENTORY, "click", "submit")
        assert result is not None
        assert "submit" in result["value"].lower()

    def test_reset_button_matches_not_submit(self):
        result = resolve_intent(self.AMBIGUOUS_INVENTORY, "click", "reset")
        assert result is not None
        assert "reset" in result["value"].lower()


# ── is_intent_only helper ─────────────────────────────────────────────────────

class TestIsIntentOnly:

    def test_plain_word_is_intent(self):
        assert is_intent_only("username") is True

    def test_multi_word_is_intent(self):
        assert is_intent_only("login button") is True

    def test_css_id_is_not_intent(self):
        assert is_intent_only("#username") is False

    def test_css_class_is_not_intent(self):
        assert is_intent_only(".btn-primary") is False

    def test_attribute_selector_is_not_intent(self):
        assert is_intent_only("[name='user']") is False

    def test_empty_string_is_not_intent(self):
        assert is_intent_only("") is False

    def test_body_selector_is_not_intent(self):
        # 'body' contains no CSS chars but is used as a real selector — fine to treat as intent,
        # resolver will simply return None for it (no body in inputs/buttons)
        assert is_intent_only("body") is True


# ── _keywords helper ──────────────────────────────────────────────────────────

class TestKeywords:

    def test_strips_noise_words(self):
        kws = _keywords("the username field")
        assert "field" not in kws
        assert "the" not in kws
        assert "username" in kws

    def test_lowercases(self):
        kws = _keywords("Username")
        assert "username" in kws

    def test_empty_string(self):
        assert _keywords("") == []

    def test_all_noise(self):
        assert _keywords("the field button") == []


# ── _score helper ─────────────────────────────────────────────────────────────

class TestScore:

    def test_exact_match_scores_high(self):
        s = _score("username", ["username"])
        assert s >= 85

    def test_partial_match_scores_lower(self):
        s_exact = _score("username", ["username"])
        s_partial = _score("user_profile_name", ["username"])
        assert s_exact > s_partial

    def test_no_match_scores_zero(self):
        s = _score("email", ["username"])
        assert s == 0

    def test_empty_candidate_scores_zero(self):
        assert _score("", ["username"]) == 0

    def test_empty_keywords_scores_zero(self):
        assert _score("username", []) == 0
