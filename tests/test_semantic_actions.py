# tests/test_semantic_actions.py
"""
Tests for extract_action_intent() in core/semantic_intent_extractor.py
and its integration with _parse_steps_from_prompt in execute_engine.py.

Run: .venv/bin/python -m pytest tests/test_semantic_actions.py -v
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

_GENERIC_URL = "https://example.com"


# ══════════════════════════════════════════════════════════════
# 1. extract_action_intent — unit tests
# ══════════════════════════════════════════════════════════════

class TestExtractActionIntent:

    def _ea(self, prompt):
        from core.semantic_intent_extractor import extract_action_intent
        return extract_action_intent(prompt)

    # ── None / empty ──────────────────────────────────────────

    def test_none_for_none(self):
        assert self._ea(None) is None

    def test_none_for_empty_string(self):
        assert self._ea("") is None

    def test_none_for_unrecognized(self):
        assert self._ea("do something random on the page") is None

    def test_none_for_css_selector_prompt(self):
        # CSS selector prompts are not handled by this extractor
        assert self._ea("fill #user-name with tomsmith") is None

    # ── CLICK ─────────────────────────────────────────────────

    def test_click_haz_click_en_login(self):
        r = self._ea("haz click en login")
        assert r == {"action": "click", "target": {"kind": "button", "name": "login"}}

    def test_click_click_en_login(self):
        r = self._ea("click en login")
        assert r == {"action": "click", "target": {"kind": "button", "name": "login"}}

    def test_click_da_click_en_login(self):
        r = self._ea("da click en login")
        assert r == {"action": "click", "target": {"kind": "button", "name": "login"}}

    def test_click_login_button(self):
        r = self._ea("click login button")
        assert r == {"action": "click", "target": {"kind": "button", "name": "login"}}

    def test_click_submit(self):
        r = self._ea("click submit")
        assert r == {"action": "click", "target": {"kind": "button", "name": "submit"}}

    def test_click_search_button(self):
        r = self._ea("click search button")
        assert r == {"action": "click", "target": {"kind": "button", "name": "search"}}

    def test_click_spanish_ingresar(self):
        r = self._ea("haz click en ingresar")
        assert r == {"action": "click", "target": {"kind": "button", "name": "login"}}

    def test_click_unknown_returns_none(self):
        assert self._ea("click unknown_xyz_button") is None

    def test_click_result_has_action_and_target(self):
        r = self._ea("click login button")
        assert r is not None
        assert r["action"] == "click"
        assert "target" in r
        assert r["target"]["kind"] == "button"
        assert isinstance(r["target"]["name"], str)

    # ── FILL — value-first ("escribe X en field") ─────────────

    def test_fill_escribe_admin_en_username(self):
        r = self._ea("escribe admin en username")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "username"},
            "value": "admin",
        }

    def test_fill_ingresa_secret_sauce_en_password(self):
        r = self._ea("ingresa secret_sauce en password")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "password"},
            "value": "secret_sauce",
        }

    def test_fill_teclea_value_en_email(self):
        r = self._ea("teclea user@test.com en email")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "email"},
            "value": "user@test.com",
        }

    def test_fill_value_first_spanish_alias(self):
        r = self._ea("escribe admin en usuario")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "username"},
            "value": "admin",
        }

    def test_fill_value_first_unknown_field_returns_none(self):
        assert self._ea("escribe algo en xyz_unknown_field") is None

    # ── FILL — field-first ("llena field con X") ──────────────

    def test_fill_llena_username_con_admin(self):
        r = self._ea("llena username con admin")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "username"},
            "value": "admin",
        }

    def test_fill_llena_password_con_secret(self):
        r = self._ea("llena password con secret_sauce")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "password"},
            "value": "secret_sauce",
        }

    def test_fill_fill_username_with(self):
        r = self._ea("fill username with tomsmith")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "username"},
            "value": "tomsmith",
        }

    def test_fill_field_first_with_suffix(self):
        r = self._ea("llena username field con admin")
        assert r == {
            "action": "fill",
            "target": {"kind": "input", "name": "username"},
            "value": "admin",
        }

    def test_fill_result_has_value(self):
        r = self._ea("escribe admin en username")
        assert r is not None
        assert "value" in r
        assert r["value"] == "admin"

    # ── ASSERT TEXT ───────────────────────────────────────────

    def test_assert_verifica_que_aparezca(self):
        r = self._ea("verifica que aparezca Checkout")
        assert r == {"action": "assert_text_contains", "selector": "body", "text": "Checkout"}

    def test_assert_valida_que_se_vea(self):
        r = self._ea("valida que se vea Checkout")
        assert r == {"action": "assert_text_contains", "selector": "body", "text": "Checkout"}

    def test_assert_asegura_que_aparezca(self):
        r = self._ea("asegura que aparezca Checkout")
        assert r == {"action": "assert_text_contains", "selector": "body", "text": "Checkout"}

    def test_assert_text_with_spaces(self):
        r = self._ea("verifica que aparezca Swag Labs")
        assert r is not None
        assert r["action"] == "assert_text_contains"
        assert r["text"] == "Swag Labs"

    def test_assert_text_with_el_texto(self):
        r = self._ea("verifica que aparezca el texto Checkout")
        assert r is not None
        assert r["action"] == "assert_text_contains"
        assert r["text"] == "Checkout"

    def test_assert_text_result_has_selector_body(self):
        r = self._ea("valida que se vea Welcome")
        assert r is not None
        assert r["selector"] == "body"


# ══════════════════════════════════════════════════════════════
# 2. Integration: _parse_steps_from_prompt builds enriched steps
# ══════════════════════════════════════════════════════════════

class TestSemanticActionIntegration:

    def _parse(self, prompt, url=_GENERIC_URL):
        from services.execute_engine import _parse_steps_from_prompt
        return _parse_steps_from_prompt(prompt, url) or []

    # ── CLICK ─────────────────────────────────────────────────

    def test_click_login_produces_click_step(self):
        steps = self._parse("click login button")
        click_steps = [s for s in steps if s.get("action") == "click"]
        assert click_steps, "Expected at least one click step"

    def test_click_login_step_has_target(self):
        steps = self._parse("click login button")
        click_steps = [s for s in steps if s.get("action") == "click"]
        assert any("target" in s for s in click_steps)

    def test_click_login_target_kind_button(self):
        steps = self._parse("click login button")
        click = next((s for s in steps if s.get("action") == "click" and "target" in s), None)
        assert click is not None
        assert click["target"]["kind"] == "button"
        assert click["target"]["name"] == "login"

    def test_click_login_selector_present(self):
        steps = self._parse("click login button")
        click = next((s for s in steps if s.get("action") == "click" and "target" in s), None)
        assert click is not None
        assert isinstance(click["selector"], str) and click["selector"]

    def test_click_haz_click_en_login(self):
        steps = self._parse("haz click en login")
        click_steps = [s for s in steps if s.get("action") == "click" and "target" in s]
        assert click_steps

    # ── FILL ──────────────────────────────────────────────────

    def test_fill_username_produces_fill_step(self):
        steps = self._parse("llena username con admin")
        fill_steps = [s for s in steps if s.get("action") == "fill"]
        assert fill_steps, "Expected at least one fill step"

    def test_fill_username_step_has_target(self):
        steps = self._parse("llena username con admin")
        fill_steps = [s for s in steps if s.get("action") == "fill"]
        assert any("target" in s for s in fill_steps)

    def test_fill_username_target_kind_input(self):
        steps = self._parse("llena username con admin")
        fill = next((s for s in steps if s.get("action") == "fill" and "target" in s), None)
        assert fill is not None
        assert fill["target"]["kind"] == "input"
        assert fill["target"]["name"] == "username"

    def test_fill_username_value_preserved(self):
        steps = self._parse("llena username con admin")
        fill = next((s for s in steps if s.get("action") == "fill" and "target" in s), None)
        assert fill is not None
        assert fill["value"] == "admin"

    def test_fill_password_step_has_target(self):
        steps = self._parse("ingresa secret_sauce en password")
        fill_steps = [s for s in steps if s.get("action") == "fill" and "target" in s]
        assert fill_steps

    def test_fill_password_value_preserved(self):
        steps = self._parse("ingresa secret_sauce en password")
        fill = next((s for s in steps if s.get("action") == "fill" and "target" in s), None)
        assert fill is not None
        assert fill["value"] == "secret_sauce"

    def test_fill_selector_preserved_alongside_target(self):
        steps = self._parse("llena username con admin")
        fill = next((s for s in steps if s.get("action") == "fill" and "target" in s), None)
        assert fill is not None
        assert isinstance(fill["selector"], str) and fill["selector"]

    # ── ASSERT TEXT ───────────────────────────────────────────

    def test_assert_text_verifica_que_aparezca(self):
        steps = self._parse("verifica que aparezca Checkout")
        at = [s for s in steps if s.get("action") == "assert_text_contains"]
        assert at, "Expected assert_text_contains step"

    def test_assert_text_text_value_correct(self):
        steps = self._parse("verifica que aparezca Checkout")
        at = next((s for s in steps if s.get("action") == "assert_text_contains"), None)
        assert at is not None
        assert at["text"] == "Checkout"

    def test_assert_text_selector_is_body(self):
        steps = self._parse("valida que se vea Checkout")
        at = next((s for s in steps if s.get("action") == "assert_text_contains"), None)
        assert at is not None
        assert at["selector"] == "body"

    def test_assert_text_asegura(self):
        steps = self._parse("asegura que aparezca Checkout")
        at = [s for s in steps if s.get("action") == "assert_text_contains"]
        assert at

    # ── CSS prompts bypass semantic extractor (regression) ────

    def test_css_fill_not_intercepted_by_semantic(self):
        steps = self._parse("fill #user-name with tomsmith")
        fill_steps = [s for s in steps if s.get("action") == "fill"]
        assert fill_steps, "CSS fill must still produce fill steps"
        # CSS-based fill should use the literal selector
        assert any(s.get("selector") == "#user-name" for s in fill_steps)
