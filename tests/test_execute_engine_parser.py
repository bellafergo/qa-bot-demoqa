# tests/test_execute_engine_parser.py
"""
Unit tests for the _parse_steps_from_prompt() parser inside services/execute_engine.py.
No browser, no OpenAI, no Redis — pure regex/logic coverage.
"""
import pytest
from services.execute_engine import _parse_steps_from_prompt

BASE = "https://the-internet.herokuapp.com/login"
SAUCE = "https://www.saucedemo.com"


# ── helpers ───────────────────────────────────────────────────────────────────

def _actions(steps):
    return [s["action"] for s in steps]

def _first_of(steps, action):
    return next((s for s in steps if s["action"] == action), None)


# ── quoted-text visibility assertions ────────────────────────────────────────

class TestQuotedTextVisibility:
    """
    Prompts like 'Verify that "X" is visible' must produce assert_text_contains
    with selector="body", NOT assert_visible with a URL fragment as selector.
    """

    def test_secure_area_visible(self):
        steps = _parse_steps_from_prompt('Verify that "Secure Area" is visible', BASE)
        assert steps is not None
        assert "assert_text_contains" in _actions(steps)
        assert "assert_visible" not in _actions(steps)

    def test_secure_area_text_and_selector(self):
        steps = _parse_steps_from_prompt('Verify that "Secure Area" is visible', BASE)
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert step["selector"] == "body"
        assert step["text"] == "Secure Area"

    def test_invalid_password_message(self):
        steps = _parse_steps_from_prompt('Verify that "Your password is invalid!" is visible', BASE)
        assert steps is not None
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert step["selector"] == "body"
        assert step["text"] == "Your password is invalid!"

    def test_no_herokuapp_selector_leakage(self):
        """Regression: .herokuapp must never appear as a selector."""
        steps = _parse_steps_from_prompt('Verify that "Secure Area" is visible', BASE)
        assert steps is not None
        selectors = [s.get("selector", "") for s in steps]
        assert not any("herokuapp" in sel for sel in selectors)

    def test_single_quotes_also_work(self):
        steps = _parse_steps_from_prompt("Verify that 'Login Page' is visible", BASE)
        assert steps is not None
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert step["text"] == "Login Page"

    def test_check_that_variant(self):
        steps = _parse_steps_from_prompt('Check that "Welcome" is visible', BASE)
        assert steps is not None
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert step["text"] == "Welcome"

    def test_assert_that_variant(self):
        steps = _parse_steps_from_prompt('Assert that "Error" is displayed', BASE)
        assert steps is not None
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert step["text"] == "Error"

    def test_verify_without_that(self):
        steps = _parse_steps_from_prompt('Verify "Dashboard" is visible', BASE)
        assert steps is not None
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert step["text"] == "Dashboard"

    def test_wait_ms_precedes_assertion(self):
        steps = _parse_steps_from_prompt('Verify that "Secure Area" is visible', BASE)
        assert steps is not None
        actions = _actions(steps)
        wi = actions.index("wait_ms") if "wait_ms" in actions else -1
        ai = actions.index("assert_text_contains") if "assert_text_contains" in actions else -1
        assert wi < ai, "wait_ms should come before assert_text_contains"


# ── existing parsing not broken ───────────────────────────────────────────────

class TestExistingParsingUnchanged:
    def test_fill_still_works(self):
        steps = _parse_steps_from_prompt(
            'Fill username field with tomsmith\nFill password field with SuperSecretPassword!',
            BASE,
        )
        assert steps is not None
        assert "fill" in _actions(steps)

    def test_click_still_works(self):
        steps = _parse_steps_from_prompt("Click login button", BASE)
        assert steps is not None
        assert "click" in _actions(steps)

    def test_goto_always_first(self):
        steps = _parse_steps_from_prompt('Verify that "Secure Area" is visible', BASE)
        assert steps is not None
        assert steps[0]["action"] == "goto"
        assert steps[0]["url"] == BASE

    def test_real_css_selector_still_uses_assert_visible(self):
        # A prompt with an explicit CSS selector should still produce assert_visible
        steps = _parse_steps_from_prompt("verify that #flash is visible", BASE)
        assert steps is not None
        # Should NOT have assert_text_contains (no quotes around text)
        assert "assert_text_contains" not in _actions(steps)

    def test_none_returned_for_empty_prompt(self):
        assert _parse_steps_from_prompt("", BASE) is None
