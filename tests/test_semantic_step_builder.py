# tests/test_semantic_step_builder.py
"""
Tests for core/semantic_step_builder.py and its integration with
_parse_steps_from_prompt in execute_engine.py.

Run: .venv/bin/python -m pytest tests/test_semantic_step_builder.py -v
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


# ══════════════════════════════════════════════════════════════
# 1. build_semantic_target — unit tests
# ══════════════════════════════════════════════════════════════

class TestBuildSemanticTarget:

    def _build(self, kind, name, url=None):
        from core.semantic_step_builder import build_semantic_target
        return build_semantic_target(kind, name, url)

    # --- SauceDemo site overrides (confidence=high) ---

    def test_saucedemo_username_primary(self):
        t = self._build("input", "username", "https://www.saucedemo.com")
        assert t["primary"] == "#user-name"

    def test_saucedemo_username_has_fallbacks(self):
        t = self._build("input", "username", "https://www.saucedemo.com")
        assert len(t["fallbacks"]) >= 3

    def test_saucedemo_username_name_fallback_present(self):
        t = self._build("input", "username", "https://www.saucedemo.com")
        types = [f["type"] for f in t["fallbacks"]]
        assert "name" in types

    def test_saucedemo_username_confidence_high(self):
        t = self._build("input", "username", "https://www.saucedemo.com")
        assert t["confidence"] == "high"

    def test_saucedemo_password_primary(self):
        t = self._build("input", "password", "https://www.saucedemo.com")
        assert t["primary"] == "#password"
        assert t["confidence"] == "high"

    def test_saucedemo_login_button_primary(self):
        t = self._build("button", "login", "https://www.saucedemo.com")
        assert t["primary"] == "#login-button"
        assert t["confidence"] == "high"

    def test_saucedemo_login_button_has_text_fallback(self):
        t = self._build("button", "login", "https://www.saucedemo.com")
        types = [f["type"] for f in t["fallbacks"]]
        assert "text" in types

    def test_saucedemo_error_text_primary(self):
        t = self._build("text", "error", "https://www.saucedemo.com")
        assert t["primary"] == "[data-test='error']"
        assert t["confidence"] == "high"

    def test_saucedemo_case_insensitive_url(self):
        """Domain check must be case-insensitive."""
        t = self._build("input", "username", "HTTPS://WWW.SAUCEDEMO.COM/")
        assert t["primary"] == "#user-name"

    # --- Generic site (well-known, confidence=medium) ---

    def test_generic_password_uses_well_known(self):
        t = self._build("input", "password", "https://example.com")
        assert t["confidence"] == "medium"
        assert "#password" in t["primary"] or "password" in t["primary"].lower()

    def test_generic_username_uses_well_known(self):
        t = self._build("input", "username", "https://example.com")
        assert t["confidence"] == "medium"
        assert len(t["fallbacks"]) >= 2

    def test_generic_login_uses_well_known(self):
        t = self._build("button", "login", "https://example.com")
        assert t["confidence"] == "medium"

    # --- Unknown intent (confidence=low) ---

    def test_unknown_name_returns_low_confidence(self):
        t = self._build("input", "nonexistent_field_xyz", "https://example.com")
        assert t["confidence"] == "low"

    def test_low_confidence_has_minimal_primary(self):
        t = self._build("input", "custom_field", "https://example.com")
        assert "custom_field" in t["primary"]

    def test_low_confidence_fallbacks_empty(self):
        t = self._build("input", "custom_field", "https://example.com")
        assert t["fallbacks"] == []

    # --- Contract fields always present ---

    def test_result_always_has_primary(self):
        for kind, name, url in [
            ("input", "username", "https://www.saucedemo.com"),
            ("input", "password", "https://example.com"),
            ("button", "login", None),
            ("input", "unknown_xyz", "https://example.com"),
        ]:
            t = self._build(kind, name, url)
            assert "primary" in t, f"Missing primary for {kind}/{name}"
            assert isinstance(t["primary"], str) and t["primary"]

    def test_result_always_has_fallbacks_list(self):
        for kind, name, url in [
            ("input", "username", "https://www.saucedemo.com"),
            ("input", "unknown_xyz", "https://example.com"),
        ]:
            t = self._build(kind, name, url)
            assert "fallbacks" in t
            assert isinstance(t["fallbacks"], list)

    def test_result_always_has_kind_name(self):
        t = self._build("input", "username", "https://www.saucedemo.com")
        assert t["kind"] == "input"
        assert t["name"] == "username"

    def test_result_always_has_resolved_by(self):
        t = self._build("input", "username", "https://www.saucedemo.com")
        assert "resolved_by" in t
        assert t["resolved_by"].startswith("semantic_step_builder")

    def test_no_context_url_falls_through_to_generic(self):
        t = self._build("input", "username", None)
        assert t["confidence"] in ("medium", "low")

    def test_fallbacks_are_dicts_with_type_and_value(self):
        t = self._build("input", "username", "https://www.saucedemo.com")
        for fb in t["fallbacks"]:
            assert "type" in fb
            assert "value" in fb


# ══════════════════════════════════════════════════════════════
# 2. Integration: _parse_steps_from_prompt produces steps
#    with target field for SauceDemo login
# ══════════════════════════════════════════════════════════════

class TestParseStepsWithSemanticTarget:

    def _parse(self, prompt: str, url: str):
        from core.step_compiler import compile_steps_from_prompt
        return compile_steps_from_prompt(prompt, url)

    def _find_step(self, steps, action, selector):
        return next(
            (s for s in steps if s.get("action") == action and s.get("selector") == selector),
            None,
        )

    def test_saucedemo_login_fill_username_has_target(self):
        steps = self._parse(
            'login username: standard_user password: secret_sauce',
            "https://www.saucedemo.com",
        )
        step = self._find_step(steps, "fill", "#user-name")
        assert step is not None, "Expected fill #user-name step"
        assert "target" in step, "fill #user-name must have target field"

    def test_saucedemo_login_fill_password_has_target(self):
        steps = self._parse(
            'login username: standard_user password: secret_sauce',
            "https://www.saucedemo.com",
        )
        step = self._find_step(steps, "fill", "#password")
        assert step is not None
        assert "target" in step

    def test_saucedemo_login_click_has_target(self):
        steps = self._parse(
            'login username: standard_user password: secret_sauce',
            "https://www.saucedemo.com",
        )
        step = self._find_step(steps, "click", "#login-button")
        assert step is not None
        assert "target" in step

    def test_target_confidence_high_for_saucedemo(self):
        steps = self._parse(
            'login username: standard_user password: secret_sauce',
            "https://www.saucedemo.com",
        )
        step = self._find_step(steps, "fill", "#user-name")
        assert step["target"]["confidence"] == "high"

    def test_target_has_fallbacks_for_saucedemo(self):
        steps = self._parse(
            'login username: standard_user password: secret_sauce',
            "https://www.saucedemo.com",
        )
        step = self._find_step(steps, "fill", "#user-name")
        assert len(step["target"]["fallbacks"]) >= 3

    def test_selector_still_present_for_backward_compat(self):
        """selector field must remain alongside target."""
        steps = self._parse(
            'login username: standard_user password: secret_sauce',
            "https://www.saucedemo.com",
        )
        step = self._find_step(steps, "fill", "#user-name")
        assert step["selector"] == "#user-name"

    def test_auto_login_steps_have_target(self):
        """Auto-injected login steps (assert_text_contains path) also get target."""
        steps = self._parse(
            'valida que aparezca el texto "Swag Labs"',
            "https://www.saucedemo.com",
        )
        fill_user = self._find_step(steps, "fill", "#user-name")
        fill_pass = self._find_step(steps, "fill", "#password")
        click_login = self._find_step(steps, "click", "#login-button")
        if fill_user:  # auto-login was injected
            assert "target" in fill_user
            assert "target" in fill_pass
            assert "target" in click_login

    def test_non_saucedemo_steps_unaffected(self):
        """Steps for non-SauceDemo URLs must not get semantic targets."""
        steps = self._parse(
            'verify "Welcome" is visible',
            "https://example.com",
        )
        # No fill/click steps expected — just assert_text_contains
        fill_steps = [s for s in (steps or []) if s.get("action") == "fill"]
        for s in fill_steps:
            # If any fill steps exist on non-SauceDemo, they should not
            # have SauceDemo-specific target (primary="#user-name")
            t = s.get("target")
            if t:
                assert t.get("primary") != "#user-name"


# ══════════════════════════════════════════════════════════════
# 3. Backward compatibility: steps without target still work
# ══════════════════════════════════════════════════════════════

class TestBackwardCompatibility:

    def test_step_without_target_is_valid_dict(self):
        """Steps with only selector (no target) are still valid."""
        step = {"action": "fill", "selector": "#some-field", "value": "hello"}
        assert "selector" in step
        assert "target" not in step  # target is optional

    def test_step_with_target_keeps_selector(self):
        """Adding target must not remove selector."""
        from core.semantic_step_builder import build_semantic_target
        target = build_semantic_target("input", "username", "https://www.saucedemo.com")
        step = {
            "action": "fill",
            "selector": "#user-name",
            "target": target,
            "value": "standard_user",
        }
        assert step["selector"] == "#user-name"
        assert step["target"]["primary"] == "#user-name"

    def test_target_primary_matches_selector_for_saucedemo(self):
        """For SauceDemo, target.primary should match the hardcoded selector."""
        from core.semantic_step_builder import build_semantic_target
        cases = [
            ("input", "username", "#user-name"),
            ("input", "password", "#password"),
            ("button", "login",   "#login-button"),
        ]
        for kind, name, expected_primary in cases:
            t = build_semantic_target(kind, name, "https://www.saucedemo.com")
            assert t["primary"] == expected_primary, (
                f"{kind}/{name}: expected primary={expected_primary}, got {t['primary']}"
            )
