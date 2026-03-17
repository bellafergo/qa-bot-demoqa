# tests/test_suggested_tests.py
"""
Tests for services/suggested_tests.py

Run: .venv/bin/python -m pytest tests/test_suggested_tests.py -v
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.suggested_tests import suggest_tests_from_inventory


# ── Builders ───────────────────────────────────────────────────────────────────

def _inv(forms=None, buttons=None, links=None, inputs=None, title="", url=""):
    return {
        "url":     url,
        "title":   title,
        "inputs":  inputs  or [],
        "buttons": buttons or [],
        "links":   links   or [],
        "forms":   forms   or [],
    }


def _form(name, fields, buttons):
    return {"name": name, "fields": fields, "buttons": buttons}


def _btn(name, selector="button"):
    return {"name": name, "selector": selector}


def _link(text, selector="a"):
    return {"text": text, "selector": selector}


# ══════════════════════════════════════════════════════════════
# 1. Output contract
# ══════════════════════════════════════════════════════════════

class TestContract:

    def test_returns_list(self):
        assert isinstance(suggest_tests_from_inventory({}), list)

    def test_empty_inventory_returns_empty(self):
        assert suggest_tests_from_inventory(_inv()) == []

    def test_none_input_returns_empty(self):
        assert suggest_tests_from_inventory(None) == []

    def test_non_dict_input_returns_empty(self):
        assert suggest_tests_from_inventory("bad") == []
        assert suggest_tests_from_inventory(42)    == []

    def test_suggestion_has_required_keys(self):
        inv = _inv(forms=[_form("login_form", ["username"], ["Login"])])
        s = suggest_tests_from_inventory(inv)[0]
        for key in ("test_name", "reason", "priority"):
            assert key in s, f"missing key: {key}"

    def test_test_name_is_snake_case(self):
        inv = _inv(forms=[_form("Login Form", ["User Name"], ["Submit"])])
        result = suggest_tests_from_inventory(inv)
        for s in result:
            assert s["test_name"] == s["test_name"].lower()
            assert " " not in s["test_name"]
            assert "-" not in s["test_name"]


# ══════════════════════════════════════════════════════════════
# 2. Rule 1 — form valid submission
# ══════════════════════════════════════════════════════════════

class TestFormValidSubmission:

    def test_login_form_generates_valid_submission(self):
        inv = _inv(forms=[_form("login_form", ["username", "password"], ["Login"])])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "login_form_valid_submission" in names

    def test_valid_submission_reason(self):
        inv = _inv(forms=[_form("login_form", ["username"], ["Submit"])])
        s = next(s for s in suggest_tests_from_inventory(inv) if "valid_submission" in s["test_name"])
        assert s["reason"] == "form_detected"

    def test_valid_submission_priority_high(self):
        inv = _inv(forms=[_form("login_form", ["username"], ["Submit"])])
        s = next(s for s in suggest_tests_from_inventory(inv) if "valid_submission" in s["test_name"])
        assert s["priority"] == "high"

    def test_form_without_button_skipped(self):
        inv = _inv(forms=[_form("partial_form", ["username"], [])])
        result = suggest_tests_from_inventory(inv)
        assert not any("valid_submission" in s["test_name"] for s in result)

    def test_form_without_fields_skipped(self):
        inv = _inv(forms=[_form("empty_form", [], ["Submit"])])
        result = suggest_tests_from_inventory(inv)
        assert not any("valid_submission" in s["test_name"] for s in result)

    def test_multiple_forms_each_gets_valid_submission(self):
        inv = _inv(forms=[
            _form("login_form",    ["username"], ["Login"]),
            _form("register_form", ["email"],    ["Register"]),
        ])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "login_form_valid_submission"    in names
        assert "register_form_valid_submission" in names

    def test_form_name_slugified(self):
        inv = _inv(forms=[_form("Login Form", ["username"], ["Submit"])])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "login_form_valid_submission" in names


# ══════════════════════════════════════════════════════════════
# 3. Rule 2 — missing field per input
# ══════════════════════════════════════════════════════════════

class TestMissingFields:

    def test_missing_username_generated(self):
        inv = _inv(forms=[_form("login_form", ["username", "password"], ["Login"])])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "login_form_missing_username" in names

    def test_missing_password_generated(self):
        inv = _inv(forms=[_form("login_form", ["username", "password"], ["Login"])])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "login_form_missing_password" in names

    def test_missing_field_reason(self):
        inv = _inv(forms=[_form("login_form", ["username"], ["Submit"])])
        s = next(s for s in suggest_tests_from_inventory(inv) if "missing" in s["test_name"])
        assert s["reason"] == "required_field_detected"

    def test_missing_field_priority_high(self):
        inv = _inv(forms=[_form("login_form", ["username"], ["Submit"])])
        s = next(s for s in suggest_tests_from_inventory(inv) if "missing" in s["test_name"])
        assert s["priority"] == "high"

    def test_missing_field_count_matches_fields(self):
        inv = _inv(forms=[_form("f", ["a", "b", "c"], ["Submit"])])
        missing = [s for s in suggest_tests_from_inventory(inv) if "missing" in s["test_name"]]
        assert len(missing) == 3

    def test_missing_field_not_generated_for_form_without_button(self):
        inv = _inv(forms=[_form("f", ["username"], [])])
        result = suggest_tests_from_inventory(inv)
        assert not any("missing" in s["test_name"] for s in result)

    def test_field_name_slugified_in_test_name(self):
        inv = _inv(forms=[_form("login_form", ["user-name"], ["Submit"])])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "login_form_missing_user_name" in names


# ══════════════════════════════════════════════════════════════
# 4. Rule 3 — search button
# ══════════════════════════════════════════════════════════════

class TestSearchButton:

    def test_search_button_generates_empty_search(self):
        inv = _inv(buttons=[_btn("Search")])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "search_empty_search" in names

    def test_search_button_reason(self):
        inv = _inv(buttons=[_btn("Search")])
        s = next(s for s in suggest_tests_from_inventory(inv) if "search" in s["test_name"])
        assert s["reason"] == "search_button_detected"

    def test_search_button_priority_medium(self):
        inv = _inv(buttons=[_btn("Search")])
        s = next(s for s in suggest_tests_from_inventory(inv) if "search" in s["test_name"])
        assert s["priority"] == "medium"

    def test_non_search_button_excluded(self):
        inv = _inv(buttons=[_btn("Login"), _btn("Cancel")])
        result = suggest_tests_from_inventory(inv)
        assert not any("empty_search" in s["test_name"] for s in result)

    def test_search_keyword_case_insensitive(self):
        inv = _inv(buttons=[_btn("SEARCH")])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert any("empty_search" in n for n in names)

    def test_buscar_keyword_detected(self):
        inv = _inv(buttons=[_btn("Buscar")])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert any("empty_search" in n for n in names)

    def test_product_search_button_slug(self):
        inv = _inv(buttons=[_btn("Product Search")])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "product_search_empty_search" in names


# ══════════════════════════════════════════════════════════════
# 5. Rule 4 — navigation smoke
# ══════════════════════════════════════════════════════════════

class TestNavigationSmoke:

    def test_links_generate_navigation_smoke(self):
        inv = _inv(links=[_link("Home", 'a[href="/home"]')])
        names = [s["test_name"] for s in suggest_tests_from_inventory(inv)]
        assert "navigation_smoke" in names

    def test_navigation_smoke_reason(self):
        inv = _inv(links=[_link("Home")])
        s = next(s for s in suggest_tests_from_inventory(inv) if s["test_name"] == "navigation_smoke")
        assert s["reason"] == "links_detected"

    def test_navigation_smoke_priority_low(self):
        inv = _inv(links=[_link("Home")])
        s = next(s for s in suggest_tests_from_inventory(inv) if s["test_name"] == "navigation_smoke")
        assert s["priority"] == "low"

    def test_no_links_no_navigation_smoke(self):
        inv = _inv(links=[])
        result = suggest_tests_from_inventory(inv)
        assert not any(s["test_name"] == "navigation_smoke" for s in result)

    def test_multiple_links_still_one_navigation_smoke(self):
        inv = _inv(links=[_link("Home"), _link("About"), _link("Contact")])
        nav_tests = [s for s in suggest_tests_from_inventory(inv) if s["test_name"] == "navigation_smoke"]
        assert len(nav_tests) == 1


# ══════════════════════════════════════════════════════════════
# 6. Deduplication
# ══════════════════════════════════════════════════════════════

class TestDeduplication:

    def test_no_duplicate_test_names(self):
        inv = _inv(
            forms=[_form("login_form", ["username", "password"], ["Login"])],
            buttons=[_btn("Search")],
            links=[_link("Home")],
        )
        result = suggest_tests_from_inventory(inv)
        names = [s["test_name"] for s in result]
        assert len(names) == len(set(names))

    def test_two_forms_with_same_name_field_deduplicated(self):
        """Two forms with identical name produce only one valid_submission."""
        inv = _inv(forms=[
            _form("login_form", ["username"], ["Submit"]),
            _form("login_form", ["username"], ["Submit"]),   # duplicate
        ])
        valid = [s for s in suggest_tests_from_inventory(inv) if "valid_submission" in s["test_name"]]
        assert len(valid) == 1

    def test_navigation_smoke_not_duplicated_with_links_and_forms(self):
        inv = _inv(
            forms=[_form("f", ["a"], ["Submit"])],
            links=[_link("Home"), _link("About")],
        )
        nav = [s for s in suggest_tests_from_inventory(inv) if s["test_name"] == "navigation_smoke"]
        assert len(nav) == 1


# ══════════════════════════════════════════════════════════════
# 7. Priority ordering
# ══════════════════════════════════════════════════════════════

class TestPriorityOrdering:

    def test_high_before_medium_before_low(self):
        inv = _inv(
            forms=[_form("login_form", ["username", "password"], ["Login"])],
            buttons=[_btn("Search")],
            links=[_link("Home")],
        )
        result = suggest_tests_from_inventory(inv)
        priorities = [s["priority"] for s in result]
        order = {"high": 0, "medium": 1, "low": 2}
        assert priorities == sorted(priorities, key=lambda p: order[p])

    def test_form_tests_all_high(self):
        inv = _inv(forms=[_form("f", ["a", "b"], ["Submit"])])
        result = suggest_tests_from_inventory(inv)
        assert all(s["priority"] == "high" for s in result)

    def test_navigation_smoke_last_when_mixed(self):
        inv = _inv(
            forms=[_form("f", ["a"], ["Submit"])],
            links=[_link("Home")],
        )
        result = suggest_tests_from_inventory(inv)
        assert result[-1]["test_name"] == "navigation_smoke"


# ══════════════════════════════════════════════════════════════
# 8. Full-page integration (SauceDemo-like inventory)
# ══════════════════════════════════════════════════════════════

class TestFullInventory:

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
            {"text": "Forgot password", "selector": 'a.forgot-password'},
        ],
        "forms": [
            {
                "name":    "login_form",
                "fields":  ["user-name", "password"],
                "buttons": ["Login"],
            }
        ],
    }

    def test_generates_valid_submission(self):
        names = [s["test_name"] for s in suggest_tests_from_inventory(self._SAUCEDEMO_INV)]
        assert "login_form_valid_submission" in names

    def test_generates_missing_user_name(self):
        names = [s["test_name"] for s in suggest_tests_from_inventory(self._SAUCEDEMO_INV)]
        assert "login_form_missing_user_name" in names

    def test_generates_missing_password(self):
        names = [s["test_name"] for s in suggest_tests_from_inventory(self._SAUCEDEMO_INV)]
        assert "login_form_missing_password" in names

    def test_generates_navigation_smoke(self):
        names = [s["test_name"] for s in suggest_tests_from_inventory(self._SAUCEDEMO_INV)]
        assert "navigation_smoke" in names

    def test_no_search_suggestion_without_search_button(self):
        result = suggest_tests_from_inventory(self._SAUCEDEMO_INV)
        assert not any("empty_search" in s["test_name"] for s in result)

    def test_total_suggestion_count(self):
        # valid_submission + missing_user_name + missing_password + navigation_smoke = 4
        result = suggest_tests_from_inventory(self._SAUCEDEMO_INV)
        assert len(result) == 4
