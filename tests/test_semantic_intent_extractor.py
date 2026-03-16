# tests/test_semantic_intent_extractor.py
"""
Tests for core/semantic_intent_extractor.py and its integration with
_parse_steps_from_prompt (assert_visible path) in execute_engine.py.

Run: .venv/bin/python -m pytest tests/test_semantic_intent_extractor.py -v
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


# ══════════════════════════════════════════════════════════════
# 1. extract_intent — unit tests
# ══════════════════════════════════════════════════════════════

class TestExtractIntent:

    def _ei(self, prompt):
        from core.semantic_intent_extractor import extract_intent
        return extract_intent(prompt)

    # --- inputs ---

    def test_campo_username(self):
        assert self._ei("campo username") == {"kind": "input", "name": "username"}

    def test_username_field(self):
        assert self._ei("username field") == {"kind": "input", "name": "username"}

    def test_campo_password(self):
        assert self._ei("campo password") == {"kind": "input", "name": "password"}

    def test_password_field(self):
        assert self._ei("password field") == {"kind": "input", "name": "password"}

    def test_campo_email(self):
        assert self._ei("campo email") == {"kind": "input", "name": "email"}

    def test_email_input(self):
        assert self._ei("email input") == {"kind": "input", "name": "email"}

    def test_spanish_usuario(self):
        assert self._ei("campo usuario") == {"kind": "input", "name": "username"}

    def test_spanish_contrasena(self):
        r = self._ei("campo contrasena")
        assert r == {"kind": "input", "name": "password"}

    def test_spanish_correo(self):
        r = self._ei("campo correo")
        assert r == {"kind": "input", "name": "email"}

    def test_input_with_surrounding_words(self):
        r = self._ei("verifica que el campo username sea visible")
        assert r == {"kind": "input", "name": "username"}

    def test_password_alone_no_indicator(self):
        # password is unambiguous enough to match without indicator
        r = self._ei("password sea visible")
        assert r == {"kind": "input", "name": "password"}

    # --- buttons ---

    def test_boton_login(self):
        assert self._ei("botón login") == {"kind": "button", "name": "login"}

    def test_boton_without_accent(self):
        assert self._ei("boton login") == {"kind": "button", "name": "login"}

    def test_login_button(self):
        assert self._ei("login button") == {"kind": "button", "name": "login"}

    def test_submit_button(self):
        assert self._ei("submit button") == {"kind": "button", "name": "submit"}

    def test_search_button(self):
        assert self._ei("search button") == {"kind": "button", "name": "search"}

    def test_spanish_ingresar(self):
        r = self._ei("botón ingresar")
        assert r == {"kind": "button", "name": "login"}

    def test_spanish_buscar(self):
        r = self._ei("botón buscar")
        assert r == {"kind": "button", "name": "search"}

    def test_login_without_indicator_treated_as_button(self):
        # "login" alone (no input indicator) defaults to button
        r = self._ei("login visible")
        assert r == {"kind": "button", "name": "login"}

    # --- None cases ---

    def test_none_for_empty_string(self):
        assert self._ei("") is None

    def test_none_for_none(self):
        assert self._ei(None) is None

    def test_none_for_body(self):
        assert self._ei("body is visible") is None

    def test_none_for_generic_text(self):
        assert self._ei("verify page loaded") is None

    def test_none_for_url_fragment(self):
        assert self._ei("saucedemo.com is visible") is None

    def test_none_for_css_selector_text(self):
        # ".hero-section" — no semantic keywords
        assert self._ei("verify .hero-section is visible") is None

    # --- Disambiguation ---

    def test_campo_login_is_none(self):
        # "campo" = input indicator, "login" = button alias
        # conflicting signals → button detection skipped, login not in input aliases
        r = self._ei("campo login")
        # "campo" signals input but "login" is not an input name → None
        assert r is None

    def test_button_wins_over_input_when_explicit_indicator(self):
        r = self._ei("login button")
        assert r["kind"] == "button"

    # --- contract ---

    def test_result_has_kind_and_name(self):
        r = self._ei("campo username")
        assert "kind" in r
        assert "name" in r
        assert r["kind"] in ("input", "button")
        assert isinstance(r["name"], str)


# ══════════════════════════════════════════════════════════════
# 2. Integration: _parse_steps_from_prompt visible path
#    now produces target-enriched assert_visible steps
# ══════════════════════════════════════════════════════════════

class TestParseStepsVisibleWithSemanticTarget:

    def _parse(self, prompt, url="https://www.saucedemo.com"):
        from services.execute_engine import _parse_steps_from_prompt
        return _parse_steps_from_prompt(prompt, url) or []

    def _assert_visible_steps(self, steps):
        return [s for s in steps if s.get("action") == "assert_visible"]

    def test_campo_username_visible_produces_target(self):
        steps = self._parse("campo username sea visible")
        av = self._assert_visible_steps(steps)
        assert av, "Expected at least one assert_visible step"
        step = next((s for s in av if "username" in str(s.get("target", ""))), av[0])
        assert "target" in step, "assert_visible step must have target field"

    def test_campo_password_visible_produces_target(self):
        steps = self._parse("campo password sea visible")
        av = self._assert_visible_steps(steps)
        assert av
        assert any("target" in s for s in av)

    def test_campo_email_visible_produces_target(self):
        steps = self._parse("campo email sea visible", "https://example.com")
        av = self._assert_visible_steps(steps)
        assert av
        assert any("target" in s for s in av)

    def test_login_button_visible_produces_target(self):
        steps = self._parse("login button sea visible")
        av = self._assert_visible_steps(steps)
        assert av
        assert any("target" in s for s in av)

    def test_boton_login_visible_produces_target(self):
        steps = self._parse("botón login sea visible")
        av = self._assert_visible_steps(steps)
        assert av
        assert any("target" in s for s in av)

    def test_target_primary_matches_selector_for_saucedemo_username(self):
        steps = self._parse("campo username sea visible")
        av = self._assert_visible_steps(steps)
        step = next((s for s in av if "target" in s), None)
        assert step is not None
        assert step["selector"] == step["target"]["primary"]

    def test_target_confidence_high_for_saucedemo(self):
        steps = self._parse("campo username sea visible")
        av = self._assert_visible_steps(steps)
        semantic = [s for s in av if "target" in s]
        assert semantic
        assert semantic[0]["target"]["confidence"] == "high"

    def test_target_confidence_medium_for_generic_site(self):
        steps = self._parse("campo password sea visible", "https://example.com")
        av = self._assert_visible_steps(steps)
        semantic = [s for s in av if "target" in s]
        assert semantic
        assert semantic[0]["target"]["confidence"] == "medium"

    def test_selector_preserved_for_backward_compat(self):
        """selector field must still be set alongside target."""
        steps = self._parse("campo username sea visible")
        av = self._assert_visible_steps(steps)
        semantic = [s for s in av if "target" in s]
        assert semantic
        assert isinstance(semantic[0]["selector"], str) and semantic[0]["selector"]

    def test_unrecognized_prompt_falls_through_without_target(self):
        """Prompts with no semantic intent still produce selector-only steps."""
        steps = self._parse("el body sea visible", "https://example.com")
        av = self._assert_visible_steps(steps)
        # Steps may exist (from _ensure_has_assert), but none should have
        # a semantic target with kind/name (only the selector-based ones).
        for s in av:
            t = s.get("target")
            if t:
                assert "kind" not in t or t.get("resolved_by") != "semantic_step_builder.site_override"

    def test_fill_prompts_not_affected(self):
        """fill prompts do not go through the visible block — no regression."""
        steps = self._parse(
            "fill username field with tomsmith",
            "https://example.com",
        )
        fill_steps = [s for s in steps if s.get("action") == "fill"]
        # Fill steps may or may not have target — they use a different code path.
        # What matters is they are still present and not broken.
        assert fill_steps, "Expected fill steps for fill prompt"
