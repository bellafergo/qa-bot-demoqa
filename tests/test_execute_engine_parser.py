# tests/test_execute_engine_parser.py
"""
Unit tests for the parse_steps_from_prompt() parser (now in core.step_compiler).
No browser, no OpenAI, no Redis — pure regex/logic coverage.
"""
import pytest
from core.step_compiler import parse_steps_from_prompt as _parse_steps_from_prompt

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

    def test_no_assert_visible_before_natural_language_fill(self):
        """Natural-language fill must NOT get a pre-check assert_visible before fill."""
        steps = _parse_steps_from_prompt(
            "Fill username field with tomsmith", BASE
        )
        assert steps is not None
        actions = _actions(steps)
        assert "fill" in actions
        fill_idx = actions.index("fill")
        # No assert_visible should appear at an index lower than fill
        assert not any(
            actions[j] == "assert_visible" for j in range(fill_idx)
        ), "assert_visible must not precede natural-language fill"

    def test_no_assert_visible_before_natural_language_click(self):
        """Natural-language click must NOT get a pre-check assert_visible before click."""
        steps = _parse_steps_from_prompt("Click login button", BASE)
        assert steps is not None
        actions = _actions(steps)
        assert "click" in actions
        click_idx = actions.index("click")
        assert not any(
            actions[j] == "assert_visible" for j in range(click_idx)
        ), "assert_visible must not precede natural-language click"

    def test_css_click_still_gets_assert_visible_before_click(self):
        """Explicit CSS selectors should still get the pre-check assert_visible."""
        steps = _parse_steps_from_prompt("Click #login-button", BASE)
        assert steps is not None
        actions = _actions(steps)
        assert "click" in actions
        click_idx = actions.index("click")
        assert any(
            actions[j] == "assert_visible" for j in range(click_idx)
        ), "assert_visible should precede CSS click"

    def test_none_returned_for_empty_prompt(self):
        assert _parse_steps_from_prompt("", BASE) is None


# ── multi-step login flow (regression for early-return bug) ──────────────────

_LOGIN_PROMPT = """\
Open https://the-internet.herokuapp.com/login
Fill username field with tomsmith
Fill password field with SuperSecretPassword!
Click login button
Verify that "Secure Area" is visible"""

_WRONG_PASS_PROMPT = """\
Open https://the-internet.herokuapp.com/login
Fill username field with tomsmith
Fill password field with wrongpassword
Click login button
Verify that "Your password is invalid!" is visible"""


class TestMultiStepLoginFlow:
    """
    The fill and click steps must NOT be dropped when 'visible' appears
    in the same multi-step prompt.  Regression for the early-return bug
    where the 'if visible in low' block returned before parsing fill/click.
    """

    def test_fill_steps_present(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        assert steps is not None
        fills = [s for s in steps if s["action"] == "fill"]
        assert len(fills) == 2, f"expected 2 fill steps, got: {fills}"

    def test_fill_username_value(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        fills = [s for s in steps if s["action"] == "fill"]
        usernames = [s for s in fills if "username" in str(s.get("selector", "")).lower()]
        assert any(s["value"] == "tomsmith" for s in usernames)

    def test_fill_password_value(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        fills = [s for s in steps if s["action"] == "fill"]
        passwords = [s for s in fills if "password" in str(s.get("selector", "")).lower()]
        assert any(s["value"] == "SuperSecretPassword!" for s in passwords)

    def test_click_step_present(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        assert steps is not None
        assert "click" in _actions(steps)

    def test_click_targets_login_button(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        click = _first_of(steps, "click")
        assert click is not None
        assert "login" in str(click.get("selector", "")).lower()

    def test_assert_text_contains_secure_area(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert step["text"] == "Secure Area"
        assert step["selector"] == "body"

    def test_step_order_fill_before_click_before_assert(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        actions = _actions(steps)
        assert "fill" in actions
        assert "click" in actions
        assert "assert_text_contains" in actions
        first_fill = actions.index("fill")
        first_click = actions.index("click")
        first_assert = actions.index("assert_text_contains")
        assert first_fill < first_click < first_assert

    def test_wrong_password_prompt_preserves_fill_click(self):
        steps = _parse_steps_from_prompt(_WRONG_PASS_PROMPT, BASE)
        assert steps is not None
        actions = _actions(steps)
        assert "fill" in actions
        assert "click" in actions
        assert "assert_text_contains" in actions

    def test_wrong_password_assert_text(self):
        steps = _parse_steps_from_prompt(_WRONG_PASS_PROMPT, BASE)
        step = _first_of(steps, "assert_text_contains")
        assert step is not None
        assert "invalid" in step["text"].lower()

    def test_goto_is_first_step(self):
        steps = _parse_steps_from_prompt(_LOGIN_PROMPT, BASE)
        assert steps[0]["action"] == "goto"

    def test_single_visible_assertion_still_early_exits(self):
        """
        Single-line visibility prompt (no fill/click) must still use the
        early-return path — no change to backward-compatible behavior.
        """
        steps = _parse_steps_from_prompt('Verify that "Secure Area" is visible', BASE)
        actions = _actions(steps)
        assert "assert_text_contains" in actions
        assert "fill" not in actions
        assert "click" not in actions
