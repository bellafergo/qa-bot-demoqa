# tests/test_auto_fixer.py
"""Unit tests for the deterministic auto-fix engine (services/test_auto_fixer.py)."""
import pytest
from services.test_auto_fixer import auto_fix_test, is_unstable_selector


# ── helpers ──────────────────────────────────────────────────────────────────

def fix(steps=None, assertions=None):
    return auto_fix_test(steps or [], assertions or [])


def types(changes):
    return [c["type"] for c in changes]


# ── Rule 1: normalize aliases ─────────────────────────────────────────────────

def test_alias_input_to_fill():
    r = fix([{"action": "input", "selector": "#x", "value": "hi"}])
    assert r["steps"][1]["action"] == "fill"        # [0] is the prepended goto
    assert "fix" in types(r["changes"])


def test_alias_navigate_to_goto():
    r = fix([{"action": "navigate", "url": "https://example.com"}])
    assert r["steps"][0]["action"] == "goto"
    assert any("navigate" in c["message"] for c in r["changes"])


def test_alias_assert_text_to_assert_text_contains():
    r = fix(
        [{"action": "goto", "url": "https://example.com"}],
        [{"type": "assert_text", "selector": "h1", "expected": "Hello"}],
    )
    assert r["assertions"][0]["type"] == "assert_text_contains"


# ── Rule 2: remove unknown actions ────────────────────────────────────────────

def test_unknown_action_removed():
    r = fix([{"action": "goto", "url": "https://x.com"}, {"action": "hover", "selector": "#btn"}])
    actions = [s["action"] for s in r["steps"]]
    assert "hover" not in actions
    assert any("warning" == c["type"] for c in r["changes"])


# ── Rule 3: prepend goto if missing ──────────────────────────────────────────

def test_prepend_goto_when_missing():
    r = fix([{"action": "click", "selector": "#btn"}])
    assert r["steps"][0]["action"] == "goto"
    assert any("prepended" in c["message"] for c in r["changes"])


def test_no_extra_goto_when_first_step_is_goto():
    steps = [{"action": "goto", "url": "https://example.com"}, {"action": "click", "selector": "#x"}]
    r = fix(steps)
    assert r["steps"][0]["url"] == "https://example.com"
    assert sum(1 for s in r["steps"] if s["action"] == "goto") == 1
    # no prepend change
    assert not any("prepended" in c["message"] for c in r["changes"])


# ── Rule 4: add selector to click/fill when missing ──────────────────────────

def test_click_missing_selector_gets_default():
    r = fix([{"action": "goto", "url": "https://x.com"}, {"action": "click"}])
    click_step = next(s for s in r["steps"] if s["action"] == "click")
    assert "selector" in click_step
    assert click_step["selector"] == "[data-test='auto']"


def test_fill_missing_selector_gets_default():
    r = fix([{"action": "goto", "url": "https://x.com"}, {"action": "fill", "value": "hi"}])
    fill_step = next(s for s in r["steps"] if s["action"] == "fill")
    assert fill_step["selector"] == "[data-test='auto']"


# ── Rule 5: add value to fill when missing ───────────────────────────────────

def test_fill_missing_value_gets_default():
    r = fix([{"action": "goto", "url": "https://x.com"}, {"action": "fill", "selector": "#q"}])
    fill_step = next(s for s in r["steps"] if s["action"] == "fill")
    assert fill_step["value"] == "test"


# ── Rule 6: add type to assertions missing it ────────────────────────────────

def test_assertion_missing_type_gets_assert_visible():
    r = fix(
        [{"action": "goto", "url": "https://x.com"}],
        [{"selector": "#hero"}],
    )
    assert r["assertions"][0]["type"] == "assert_visible"
    assert any("missing type" in c["message"] for c in r["changes"])


# ── Rule 7: add selector to assertions that require one ──────────────────────

def test_assert_visible_missing_selector_gets_body():
    r = fix(
        [{"action": "goto", "url": "https://x.com"}],
        [{"type": "assert_visible"}],
    )
    assert r["assertions"][0]["selector"] == "body"


def test_assert_url_contains_does_not_need_selector():
    r = fix(
        [{"action": "goto", "url": "https://x.com"}],
        [{"type": "assert_url_contains", "expected": "/login"}],
    )
    # no selector added (assert_url_contains is not in _SELECTOR_REQUIRED)
    assert "selector" not in r["assertions"][0]


# ── Rule 8: default assertion when list is empty ─────────────────────────────

def test_empty_assertions_gets_default():
    r = fix([{"action": "goto", "url": "https://x.com"}], [])
    assert len(r["assertions"]) == 1
    assert r["assertions"][0]["type"] == "assert_visible"
    assert r["assertions"][0]["selector"] == "body"
    assert any("empty" in c["message"] for c in r["changes"])


# ── Safety: non-list inputs ───────────────────────────────────────────────────

def test_none_inputs_do_not_raise():
    r = auto_fix_test(None, None)
    assert isinstance(r["steps"], list)
    assert isinstance(r["assertions"], list)


def test_non_dict_steps_removed_with_warning():
    r = fix(["not a dict", {"action": "goto", "url": "https://x.com"}])
    assert r["steps"][0]["action"] == "goto"
    assert any("not a dict" in c["message"] for c in r["changes"])


# ── No mutation of originals ──────────────────────────────────────────────────

def test_original_lists_not_mutated():
    original_steps = [{"action": "click"}]
    original_assertions = []
    auto_fix_test(original_steps, original_assertions)
    assert original_steps == [{"action": "click"}]
    assert original_assertions == []


# ── Unstable selector detection + warnings ────────────────────────────────────

def test_is_unstable_selector_radix_style():
    assert is_unstable_selector("#:Rhjdjtskq:-form-item")
    assert not is_unstable_selector("#stable-id")


def test_is_unstable_selector_nth_child_tail():
    assert is_unstable_selector("div.items > li:nth-child(3)")
    assert not is_unstable_selector(":nth-child(2) > span")


def test_is_unstable_selector_hash_suffix():
    assert is_unstable_selector("#root-abc123def456")
    assert not is_unstable_selector("#ab-12345")


def test_is_unstable_selector_non_string():
    assert not is_unstable_selector(None)
    assert not is_unstable_selector("")
    assert not is_unstable_selector(42)


def test_autofix_emits_warning_for_radix_selector():
    steps = [
        {"action": "goto", "url": "https://example.com"},
        {"action": "click", "selector": "#:Rhjdjtskq:-form-item"},
    ]
    r = fix(steps, [])
    assert any(
        c["type"] == "warning" and "inestable" in c["message"] and "Rhjdjtskq" in c["message"]
        for c in r["changes"]
    )
    assert r["steps"][-1]["selector"] == "#:Rhjdjtskq:-form-item"


def test_autofix_warning_on_assertion_selector():
    r = fix(
        [{"action": "goto", "url": "https://example.com"}],
        [{"type": "assert_visible", "selector": "#:Abcd1234:-root"}],
    )
    assert any("inestable" in c["message"] for c in r["changes"])
