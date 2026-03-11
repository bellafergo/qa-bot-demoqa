# tests/test_dom_analyzer.py
"""
Unit tests for core/dom_analyzer.py
Tests DOM inventory extraction and summary without a real browser (mocked page).
"""
import pytest
from unittest.mock import MagicMock

from core.dom_analyzer import extract_dom_inventory, summarize_inventory, _empty


# ── Helpers ────────────────────────────────────────────────────────────────────

def _mock_page(inventory: dict):
    """Return a mock Playwright page that returns a fixed inventory from evaluate()."""
    page = MagicMock()
    page.evaluate.return_value = inventory
    return page


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
    "links": [],
    "headings": [
        {"tag": "h2", "text": "Login Page"},
    ],
    "selects": [],
}


# ── extract_dom_inventory ──────────────────────────────────────────────────────

class TestExtractDomInventory:
    def test_returns_inventory_from_page(self):
        page = _mock_page(THE_INTERNET_LOGIN)
        result = extract_dom_inventory(page)
        assert result == THE_INTERNET_LOGIN

    def test_returns_empty_on_non_dict_result(self):
        page = MagicMock()
        page.evaluate.return_value = "broken"
        result = extract_dom_inventory(page)
        assert result == _empty()

    def test_returns_empty_on_page_exception(self):
        page = MagicMock()
        page.evaluate.side_effect = RuntimeError("page crashed")
        result = extract_dom_inventory(page)
        assert result == _empty()

    def test_inventory_has_expected_keys(self):
        page = _mock_page(THE_INTERNET_LOGIN)
        result = extract_dom_inventory(page)
        for key in ("inputs", "buttons", "links", "headings", "selects"):
            assert key in result

    def test_inputs_are_list(self):
        page = _mock_page(THE_INTERNET_LOGIN)
        result = extract_dom_inventory(page)
        assert isinstance(result["inputs"], list)
        assert len(result["inputs"]) == 2

    def test_buttons_are_list(self):
        page = _mock_page(THE_INTERNET_LOGIN)
        result = extract_dom_inventory(page)
        assert isinstance(result["buttons"], list)
        assert len(result["buttons"]) == 1

    def test_headings_are_list(self):
        page = _mock_page(THE_INTERNET_LOGIN)
        result = extract_dom_inventory(page)
        assert result["headings"][0]["text"] == "Login Page"

    def test_empty_inventory_all_lists(self):
        inv = _empty()
        for key in ("inputs", "buttons", "links", "headings", "selects"):
            assert isinstance(inv[key], list)
            assert len(inv[key]) == 0


# ── summarize_inventory ────────────────────────────────────────────────────────

class TestSummarizeInventory:
    def test_summary_contains_headings(self):
        summary = summarize_inventory(THE_INTERNET_LOGIN)
        assert "Login Page" in summary

    def test_summary_contains_inputs(self):
        summary = summarize_inventory(THE_INTERNET_LOGIN)
        assert "username" in summary.lower() or "Username" in summary

    def test_summary_contains_buttons(self):
        summary = summarize_inventory(THE_INTERNET_LOGIN)
        assert "Login" in summary

    def test_summary_on_empty_inventory(self):
        summary = summarize_inventory(_empty())
        assert "no DOM elements" in summary

    def test_summary_is_string(self):
        summary = summarize_inventory(THE_INTERNET_LOGIN)
        assert isinstance(summary, str)
        assert len(summary) > 0
