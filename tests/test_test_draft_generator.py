# tests/test_test_draft_generator.py
"""
Tests for services/test_draft_generator.py

All tests are pure — no browser, no I/O.

Run: .venv/bin/python -m pytest tests/test_test_draft_generator.py -v
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.test_draft_generator import (
    exploration_fallback_draft,
    generate_test_drafts,
    _build_draft_steps,
    _placeholder,
    _find_form,
    _find_form_and_missing_field,
    _input_selector_map,
    _button_selector_map,
)


# ── Fixtures ───────────────────────────────────────────────────────────────────

_SAUCEDEMO_INV = {
    "url":   "https://www.saucedemo.com",
    "title": "Swag Labs",
    "inputs": [
        {"name": "user-name", "selector": "#user-name"},
        {"name": "password",  "selector": "#password"},
    ],
    "buttons": [
        {"name": "Login", "selector": "#login-button"},
    ],
    "links": [
        {"text": "Forgot password", "selector": 'a[href="/forgot-password"]'},
    ],
    "forms": [
        {
            "name":    "login_form",
            "fields":  ["user-name", "password"],
            "buttons": ["Login"],
        }
    ],
}

_SEARCH_INV = {
    "url":     "https://shop.example.com",
    "title":   "Shop",
    "inputs":  [{"name": "q", "selector": "#q"}],
    "buttons": [{"name": "Search", "selector": "#search-btn"}],
    "links":   [{"text": "Home", "selector": 'a[href="/"]'}],
    "forms":   [],
}

_SAUCE_SUGGESTIONS = [
    {"test_name": "login_form_valid_submission",  "reason": "form_detected",           "priority": "high"},
    {"test_name": "login_form_missing_user_name", "reason": "required_field_detected", "priority": "high"},
    {"test_name": "login_form_missing_password",  "reason": "required_field_detected", "priority": "high"},
    {"test_name": "navigation_smoke",             "reason": "links_detected",          "priority": "low"},
]

URL = "https://www.saucedemo.com"


def _sug(test_name, reason="form_detected", priority="high"):
    return {"test_name": test_name, "reason": reason, "priority": priority}


# ══════════════════════════════════════════════════════════════
# 1. Output contract
# ══════════════════════════════════════════════════════════════

class TestContract:

    def test_returns_list(self):
        assert isinstance(generate_test_drafts({}, []), list)

    def test_empty_inputs_return_empty(self):
        assert generate_test_drafts({}, []) == []

    def test_none_inventory_safe(self):
        assert generate_test_drafts(None, []) == []

    def test_none_suggestions_safe(self):
        assert generate_test_drafts({}, None) == []

    def test_draft_has_required_keys(self):
        drafts = generate_test_drafts(_SAUCEDEMO_INV, _SAUCE_SUGGESTIONS[:1])
        d = drafts[0]
        for key in ("test_name", "status", "priority", "reason", "steps"):
            assert key in d, f"missing key: {key}"

    def test_status_is_always_draft(self):
        drafts = generate_test_drafts(_SAUCEDEMO_INV, _SAUCE_SUGGESTIONS)
        assert all(d["status"] == "draft" for d in drafts)

    def test_steps_is_list(self):
        drafts = generate_test_drafts(_SAUCEDEMO_INV, _SAUCE_SUGGESTIONS[:1])
        assert isinstance(drafts[0]["steps"], list)

    def test_non_dict_suggestions_skipped(self):
        mixed = [None, "bad", 42, _SAUCE_SUGGESTIONS[0]]
        drafts = generate_test_drafts(_SAUCEDEMO_INV, mixed)
        assert len(drafts) == 1


# ══════════════════════════════════════════════════════════════
# 2. _placeholder heuristics
# ══════════════════════════════════════════════════════════════

class TestPlaceholder:

    def test_password_field(self):
        assert _placeholder("password") == "<password>"

    def test_username_field(self):
        assert _placeholder("username") == "<username>"

    def test_user_name_hyphen(self):
        assert _placeholder("user-name") == "<username>"

    def test_email_field(self):
        assert _placeholder("email") == "<email>"

    def test_unknown_field_wraps_name(self):
        ph = _placeholder("zip_code")
        assert ph.startswith("<") and ph.endswith(">")

    def test_search_field(self):
        assert _placeholder("search") == "<search_query>"

    def test_empty_field_returns_value_placeholder(self):
        assert _placeholder("") == "<value>"

    def test_placeholder_never_empty(self):
        for name in ("x", "y123", "widget_field", ""):
            ph = _placeholder(name)
            assert ph and ph.startswith("<") and ph.endswith(">")


# ══════════════════════════════════════════════════════════════
# 3. form_detected drafts
# ══════════════════════════════════════════════════════════════

class TestFormDetectedDraft:

    def _draft(self):
        sug = _sug("login_form_valid_submission", reason="form_detected")
        drafts = generate_test_drafts(_SAUCEDEMO_INV, [sug])
        return drafts[0]

    def test_draft_generated(self):
        assert self._draft() is not None

    def test_test_name_preserved(self):
        assert self._draft()["test_name"] == "login_form_valid_submission"

    def test_priority_preserved(self):
        assert self._draft()["priority"] == "high"

    def test_reason_preserved(self):
        assert self._draft()["reason"] == "form_detected"

    def test_first_step_is_goto(self):
        assert self._draft()["steps"][0] == {"action": "goto", "url": URL}

    def test_fill_steps_present(self):
        actions = [s["action"] for s in self._draft()["steps"]]
        assert "fill" in actions

    def test_fill_uses_real_selectors(self):
        fills = [s for s in self._draft()["steps"] if s["action"] == "fill"]
        sels = {s["selector"] for s in fills}
        assert "#user-name" in sels
        assert "#password"  in sels

    def test_fill_values_are_placeholders(self):
        fills = [s for s in self._draft()["steps"] if s["action"] == "fill"]
        for f in fills:
            assert f["value"].startswith("<") and f["value"].endswith(">")

    def test_click_uses_login_button_selector(self):
        click = next(s for s in self._draft()["steps"] if s["action"] == "click")
        assert click["selector"] == "#login-button"

    def test_last_step_assert_visible_body(self):
        last = self._draft()["steps"][-1]
        assert last == {"action": "assert_visible", "selector": "body"}

    def test_unknown_form_returns_no_draft(self):
        sug = _sug("ghost_form_valid_submission", reason="form_detected")
        assert generate_test_drafts(_SAUCEDEMO_INV, [sug]) == []


# ══════════════════════════════════════════════════════════════
# 4. required_field_detected drafts
# ══════════════════════════════════════════════════════════════

class TestMissingFieldDraft:

    def _draft_missing(self, field_slug):
        sug = _sug(f"login_form_missing_{field_slug}", reason="required_field_detected")
        drafts = generate_test_drafts(_SAUCEDEMO_INV, [sug])
        return drafts[0] if drafts else None

    def test_missing_password_draft_generated(self):
        assert self._draft_missing("password") is not None

    def test_missing_user_name_draft_generated(self):
        assert self._draft_missing("user_name") is not None

    def test_missing_password_field_not_filled(self):
        draft = self._draft_missing("password")
        fills = [s for s in draft["steps"] if s["action"] == "fill"]
        filled_sels = {s["selector"] for s in fills}
        assert "#password" not in filled_sels

    def test_other_fields_filled_when_password_missing(self):
        draft = self._draft_missing("password")
        fills = [s for s in draft["steps"] if s["action"] == "fill"]
        filled_sels = {s["selector"] for s in fills}
        assert "#user-name" in filled_sels

    def test_missing_user_name_not_filled(self):
        draft = self._draft_missing("user_name")
        fills = [s for s in draft["steps"] if s["action"] == "fill"]
        filled_sels = {s["selector"] for s in fills}
        assert "#user-name" not in filled_sels

    def test_last_step_asserts_missing_field_visible(self):
        draft = self._draft_missing("password")
        last = draft["steps"][-1]
        assert last["action"]   == "assert_visible"
        assert last["selector"] == "#password"

    def test_fill_values_are_placeholders(self):
        draft = self._draft_missing("password")
        fills = [s for s in draft["steps"] if s["action"] == "fill"]
        for f in fills:
            assert f["value"].startswith("<") and f["value"].endswith(">")

    def test_unknown_field_returns_no_draft(self):
        sug = _sug("login_form_missing_nonexistent", reason="required_field_detected")
        assert generate_test_drafts(_SAUCEDEMO_INV, [sug]) == []


# ══════════════════════════════════════════════════════════════
# 5. search_button_detected drafts
# ══════════════════════════════════════════════════════════════

class TestSearchButtonDraft:

    def _draft(self):
        sug = _sug("search_empty_search", reason="search_button_detected")
        drafts = generate_test_drafts(_SEARCH_INV, [sug])
        return drafts[0] if drafts else None

    def test_draft_generated(self):
        assert self._draft() is not None

    def test_first_step_goto(self):
        assert self._draft()["steps"][0]["action"] == "goto"

    def test_click_search_button(self):
        click = next(s for s in self._draft()["steps"] if s["action"] == "click")
        assert click["selector"] == "#search-btn"

    def test_no_fill_steps(self):
        assert not any(s["action"] == "fill" for s in self._draft()["steps"])

    def test_last_step_assert_visible_body(self):
        assert self._draft()["steps"][-1] == {"action": "assert_visible", "selector": "body"}

    def test_unknown_search_button_returns_no_draft(self):
        sug = _sug("ghost_empty_search", reason="search_button_detected")
        assert generate_test_drafts(_SEARCH_INV, [sug]) == []


# ══════════════════════════════════════════════════════════════
# 6. links_detected (navigation smoke) drafts
# ══════════════════════════════════════════════════════════════

class TestNavigationSmokeDraft:

    def _draft(self):
        sug = _sug("navigation_smoke", reason="links_detected", priority="low")
        drafts = generate_test_drafts(_SAUCEDEMO_INV, [sug])
        return drafts[0]

    def test_draft_generated(self):
        assert self._draft() is not None

    def test_priority_low(self):
        assert self._draft()["priority"] == "low"

    def test_exactly_two_steps(self):
        assert len(self._draft()["steps"]) == 2

    def test_goto_url(self):
        assert self._draft()["steps"][0] == {"action": "goto", "url": URL}

    def test_assert_body_visible(self):
        assert self._draft()["steps"][1] == {"action": "assert_visible", "selector": "body"}


# ══════════════════════════════════════════════════════════════
# 7. Deduplication
# ══════════════════════════════════════════════════════════════

class TestDeduplication:

    def test_no_duplicate_test_names(self):
        drafts = generate_test_drafts(_SAUCEDEMO_INV, _SAUCE_SUGGESTIONS)
        names = [d["test_name"] for d in drafts]
        assert len(names) == len(set(names))

    def test_duplicate_suggestion_deduped(self):
        sug = _sug("login_form_valid_submission", reason="form_detected")
        drafts = generate_test_drafts(_SAUCEDEMO_INV, [sug, sug, sug])
        assert len(drafts) == 1

    def test_mixed_suggestions_all_unique(self):
        drafts = generate_test_drafts(_SAUCEDEMO_INV, _SAUCE_SUGGESTIONS)
        names = [d["test_name"] for d in drafts]
        assert len(names) == len(set(names))


# ══════════════════════════════════════════════════════════════
# 8. Full inventory round-trip (SauceDemo)
# ══════════════════════════════════════════════════════════════

class TestFullRoundTrip:

    def setup_method(self):
        self.drafts = generate_test_drafts(_SAUCEDEMO_INV, _SAUCE_SUGGESTIONS)
        self.by_name = {d["test_name"]: d for d in self.drafts}

    def test_generates_four_drafts(self):
        assert len(self.drafts) == 4

    def test_valid_submission_present(self):
        assert "login_form_valid_submission" in self.by_name

    def test_missing_user_name_present(self):
        assert "login_form_missing_user_name" in self.by_name

    def test_missing_password_present(self):
        assert "login_form_missing_password" in self.by_name

    def test_navigation_smoke_present(self):
        assert "navigation_smoke" in self.by_name

    def test_all_statuses_are_draft(self):
        assert all(d["status"] == "draft" for d in self.drafts)

    def test_all_steps_lists_non_empty(self):
        assert all(len(d["steps"]) > 0 for d in self.drafts)

    def test_all_fill_values_are_placeholders(self):
        for draft in self.drafts:
            for step in draft["steps"]:
                if step["action"] == "fill":
                    v = step["value"]
                    assert v.startswith("<") and v.endswith(">"), \
                        f"{draft['test_name']}: fill value {v!r} is not a placeholder"

    def test_no_real_passwords_in_drafts(self):
        """Drafts must never contain real credential strings."""
        real_creds = {"standard_user", "secret_sauce", "TestPass123!"}
        for draft in self.drafts:
            for step in draft["steps"]:
                assert step.get("value") not in real_creds


# ══════════════════════════════════════════════════════════════
# 9. Fallback selectors when inventory is sparse
# ══════════════════════════════════════════════════════════════

class TestFallbackSelectors:

    _SPARSE_INV = {
        "url":     "https://example.com",
        "title":   "Test",
        "inputs":  [],   # no selector info
        "buttons": [],
        "links":   [],
        "forms": [
            {"name": "signup_form", "fields": ["email"], "buttons": ["Register"]}
        ],
    }

    def test_draft_still_generated_with_no_selectors(self):
        sug = _sug("signup_form_valid_submission", reason="form_detected")
        drafts = generate_test_drafts(self._SPARSE_INV, [sug])
        assert len(drafts) == 1

    def test_fallback_fill_selector_is_reasonable(self):
        sug = _sug("signup_form_valid_submission", reason="form_detected")
        draft = generate_test_drafts(self._SPARSE_INV, [sug])[0]
        fills = [s for s in draft["steps"] if s["action"] == "fill"]
        assert len(fills) == 1
        # fallback is [name="email"] style or similar
        assert fills[0]["selector"] != ""

    def test_fallback_button_selector_is_reasonable(self):
        sug = _sug("signup_form_valid_submission", reason="form_detected")
        draft = generate_test_drafts(self._SPARSE_INV, [sug])[0]
        clicks = [s for s in draft["steps"] if s["action"] == "click"]
        assert len(clicks) == 1
        assert clicks[0]["selector"] != ""


# ══════════════════════════════════════════════════════════════
# 10. Modern heuristic drafts
# ══════════════════════════════════════════════════════════════

class TestExplorationFallbackDraft:

    def test_returns_none_for_empty_url(self):
        assert exploration_fallback_draft("") is None
        assert exploration_fallback_draft("   ") is None

    def test_minimal_steps(self):
        d = exploration_fallback_draft("https://app.example/")
        assert d["test_name"] == "exploration_landing_smoke"
        assert d["reason"] == "exploration_fallback"
        assert d["steps"][0] == {"action": "goto", "url": "https://app.example/"}
        assert d["steps"][-1] == {"action": "assert_visible", "selector": "body"}


class TestStandaloneAndButtonProbeDrafts:

    def test_standalone_inputs_draft(self):
        inv = {
            "url": "https://spa.test/",
            "inputs": [{"name": "search", "selector": "#q"}],
            "buttons": [], "links": [], "forms": [],
        }
        sug = {"test_name": "standalone_inputs_smoke", "reason": "standalone_inputs_detected", "priority": "medium"}
        drafts = generate_test_drafts(inv, [sug])
        assert len(drafts) == 1
        actions = [s["action"] for s in drafts[0]["steps"]]
        assert actions == ["goto", "assert_visible", "fill", "assert_visible"]

    def test_nav_probe_draft(self):
        inv = {
            "url": "https://spa.test/",
            "buttons": [{"name": "Settings", "selector": "#settings"}],
            "inputs": [], "links": [], "forms": [],
        }
        sug = {"test_name": "nav_probe_settings", "reason": "spa_nav_button_detected", "priority": "low"}
        drafts = generate_test_drafts(inv, [sug])
        assert len(drafts) == 1
        click = next(s for s in drafts[0]["steps"] if s["action"] == "click")
        assert click["selector"] == "#settings"
