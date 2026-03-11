# tests/test_normalize_steps.py
"""
Unit tests for _normalize_steps_to_target() and the step/target schema models.

Run with:
  python -m pytest tests/test_normalize_steps.py -v
Or standalone:
  python tests/test_normalize_steps.py
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from core.step_normalizer import normalize_steps_to_target as _normalize_steps_to_target
from core.schemas import TargetFallback, TargetSpec, StepSpec


# ============================================================
# _normalize_steps_to_target — core behaviour
# ============================================================

def test_flat_selector_gets_target():
    steps = [{"action": "click", "selector": "#login-button"}]
    result = _normalize_steps_to_target(steps)
    assert len(result) == 1
    t = result[0].get("target")
    assert isinstance(t, dict), "target must be a dict"
    assert t["primary"] == "#login-button"
    assert "state" in t
    assert "timeout_ms" in t


def test_selector_is_preserved():
    steps = [{"action": "fill", "selector": "#user-name", "value": "standard_user"}]
    result = _normalize_steps_to_target(steps)
    assert result[0]["selector"] == "#user-name", "original selector key must be preserved"
    assert result[0]["value"] == "standard_user"


def test_existing_valid_target_unchanged():
    original_target = {
        "primary": "#btn",
        "fallbacks": [{"type": "css", "value": ".btn-primary"}],
        "timeout_ms": 5000,
        "state": "visible",
    }
    steps = [{"action": "click", "selector": "#btn", "target": original_target}]
    result = _normalize_steps_to_target(steps)
    assert result[0]["target"] is original_target, "existing target dict must not be replaced"
    assert result[0]["target"]["fallbacks"] == [{"type": "css", "value": ".btn-primary"}]


def test_non_ui_steps_pass_through():
    steps = [
        {"action": "goto", "url": "https://example.com"},
        {"action": "wait_ms", "ms": 500},
        {"action": "assert_url_contains", "value": "inventory"},
    ]
    result = _normalize_steps_to_target(steps)
    assert result == steps, "non-UI steps must be returned unchanged"


def test_step_without_selector_passes_through():
    step = {"action": "assert_text_contains", "text": "Products"}
    result = _normalize_steps_to_target([step])
    assert result[0] == step


def test_all_ui_actions_get_target():
    ui_actions = ["fill", "click", "press", "assert_visible", "assert_not_visible", "assert_text_contains"]
    for action in ui_actions:
        steps = [{"action": action, "selector": "#el"}]
        result = _normalize_steps_to_target(steps)
        assert isinstance(result[0].get("target"), dict), f"action={action} should get target"


def test_timeout_ms_inherited_from_step():
    steps = [{"action": "click", "selector": "#btn", "timeout_ms": 8000}]
    result = _normalize_steps_to_target(steps)
    assert result[0]["target"]["timeout_ms"] == 8000


def test_default_timeout_ms_is_3000():
    steps = [{"action": "click", "selector": "#btn"}]
    result = _normalize_steps_to_target(steps)
    assert result[0]["target"]["timeout_ms"] == 3000


# ============================================================
# _normalize_steps_to_target — safe fallback generation
# ============================================================

def test_id_selector_gets_css_fallback():
    steps = [{"action": "click", "selector": "#submit"}]
    result = _normalize_steps_to_target(steps)
    fallbacks = result[0]["target"]["fallbacks"]
    assert len(fallbacks) == 1
    assert fallbacks[0]["type"] == "css"
    assert fallbacks[0]["value"] == "[id='submit']"


def test_data_testid_gets_two_fallbacks():
    steps = [{"action": "click", "selector": "[data-testid='buy-now']"}]
    result = _normalize_steps_to_target(steps)
    fallbacks = result[0]["target"]["fallbacks"]
    assert len(fallbacks) == 2
    types = {f["type"] for f in fallbacks}
    assert "testid" in types
    assert "css" in types
    css_fb = next(f for f in fallbacks if f["type"] == "css")
    assert css_fb["value"] == "[data-test='buy-now']"


def test_data_test_gets_two_fallbacks():
    steps = [{"action": "click", "selector": "[data-test='login-btn']"}]
    result = _normalize_steps_to_target(steps)
    fallbacks = result[0]["target"]["fallbacks"]
    assert len(fallbacks) == 2
    css_fb = next(f for f in fallbacks if f["type"] == "css")
    assert css_fb["value"] == "[data-testid='login-btn']"


def test_name_selector_gets_name_fallback():
    steps = [{"action": "fill", "selector": "[name='email']", "value": "a@b.com"}]
    result = _normalize_steps_to_target(steps)
    fallbacks = result[0]["target"]["fallbacks"]
    assert len(fallbacks) == 1
    assert fallbacks[0]["type"] == "name"
    assert fallbacks[0]["value"] == "email"


def test_generic_css_selector_gets_no_fallback():
    steps = [{"action": "click", "selector": ".inventory_list"}]
    result = _normalize_steps_to_target(steps)
    assert result[0]["target"]["fallbacks"] == []


def test_complex_selector_gets_no_fallback():
    steps = [{"action": "assert_visible", "selector": "h3[data-test='error']"}]
    result = _normalize_steps_to_target(steps)
    assert result[0]["target"]["fallbacks"] == []


# ============================================================
# Schema models
# ============================================================

def test_target_fallback_model():
    fb = TargetFallback(type="css", value=".btn")
    assert fb.type == "css"
    assert fb.value == ".btn"


def test_target_spec_defaults():
    spec = TargetSpec(primary="#foo")
    assert spec.primary == "#foo"
    assert spec.fallbacks == []
    assert spec.timeout_ms == 3000
    assert spec.state == "visible"
    assert spec.intent is None


def test_target_spec_with_fallbacks():
    spec = TargetSpec(
        primary="#foo",
        fallbacks=[TargetFallback(type="css", value="[id='foo']")],
        timeout_ms=5000,
    )
    assert len(spec.fallbacks) == 1
    assert spec.fallbacks[0].type == "css"


def test_step_spec_legacy_shape():
    step = StepSpec(action="click", selector="#login-button")
    assert step.action == "click"
    assert step.selector == "#login-button"
    assert step.target is None


def test_step_spec_new_shape():
    step = StepSpec(
        action="fill",
        target=TargetSpec(primary="#user-name"),
        value="standard_user",
    )
    assert step.target.primary == "#user-name"
    assert step.selector is None
    assert step.value == "standard_user"


def test_step_spec_both_shapes():
    """Backward compat: selector and target can coexist."""
    step = StepSpec(
        action="click",
        selector="#login-button",
        target=TargetSpec(primary="#login-button"),
    )
    assert step.selector == "#login-button"
    assert step.target.primary == "#login-button"


def test_step_spec_extra_fields_allowed():
    """extra='allow' means unknown fields don't raise."""
    step = StepSpec(action="goto", url="https://example.com", future_field="ok")
    assert step.action == "goto"


# ============================================================
# Standalone runner
# ============================================================

if __name__ == "__main__":
    tests = [
        test_flat_selector_gets_target,
        test_selector_is_preserved,
        test_existing_valid_target_unchanged,
        test_non_ui_steps_pass_through,
        test_step_without_selector_passes_through,
        test_all_ui_actions_get_target,
        test_timeout_ms_inherited_from_step,
        test_default_timeout_ms_is_3000,
        test_id_selector_gets_css_fallback,
        test_data_testid_gets_two_fallbacks,
        test_data_test_gets_two_fallbacks,
        test_name_selector_gets_name_fallback,
        test_generic_css_selector_gets_no_fallback,
        test_complex_selector_gets_no_fallback,
        test_target_fallback_model,
        test_target_spec_defaults,
        test_target_spec_with_fallbacks,
        test_step_spec_legacy_shape,
        test_step_spec_new_shape,
        test_step_spec_both_shapes,
        test_step_spec_extra_fields_allowed,
    ]

    passed = failed = 0
    for fn in tests:
        try:
            fn()
            print(f"  [PASS] {fn.__name__}")
            passed += 1
        except Exception as e:
            print(f"  [FAIL] {fn.__name__}: {e}")
            failed += 1

    print(f"\n{passed} passed, {failed} failed")
    sys.exit(0 if failed == 0 else 1)
