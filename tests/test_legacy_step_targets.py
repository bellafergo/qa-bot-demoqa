# tests/test_legacy_step_targets.py
"""Regression: catalog steps with selector but no structured target (batch runner)."""
from __future__ import annotations

from core.step_normalizer import enrich_missing_web_targets
from core.step_validator import validate_steps


def test_enrich_fill_with_selector_only():
    steps = [
        {"action": "goto", "url": "https://example.com"},
        {"action": "fill", "selector": "#username", "value": "u"},
    ]
    out = enrich_missing_web_targets(steps)
    assert out[1]["target"] == {
        "primary": "#username",
        "fallbacks": [],
        "timeout_ms": 5000,
        "state": "visible",
    }
    assert validate_steps(out).valid


def test_enrich_click_with_selector_only():
    steps = [
        {"action": "goto", "url": "https://example.com"},
        {"action": "click", "selector": "button[type='submit']"},
    ]
    out = enrich_missing_web_targets(steps)
    t = out[1]["target"]
    assert t["primary"] == "button[type='submit']"
    assert t["fallbacks"] == []
    assert t["timeout_ms"] == 5000
    assert t["state"] == "visible"
    assert validate_steps(out).valid


def test_enrich_does_not_replace_existing_target():
    original = {
        "action": "fill",
        "selector": "#ignored",
        "value": "x",
        "target": {
            "primary": "#user-name",
            "fallbacks": [{"type": "css", "value": "#alt"}],
            "timeout_ms": 3200,
            "state": "visible",
        },
    }
    out = enrich_missing_web_targets([dict(original)])[0]
    assert out["target"] == original["target"]


def test_fill_without_selector_still_invalid():
    steps = enrich_missing_web_targets([{"action": "fill", "value": "only-value"}])
    vr = validate_steps(steps)
    assert not vr.valid
    assert any(e.error_type == "missing_selector" for e in vr.errors)


def test_catalog_built_legacy_steps_pass_validation():
    """Simulates historical JSON: fill/click with selector only after _step_to_runner."""
    from models.test_case import TestCase, TestStep

    tc = TestCase(
        test_case_id="TC-LEGACY-CAT-001",
        name="Legacy shape",
        module="demo",
        type="smoke",
        priority="low",
        base_url="https://example.com",
        steps=[
            TestStep(action="goto", value="https://example.com"),
            TestStep(action="fill", selector="#email", value="a@b.co"),
            TestStep(action="click", selector="#submit"),
        ],
        assertions=[],
    )
    from services.test_catalog_service import _build_runner_steps

    built = _build_runner_steps(tc, base_url="https://example.com")
    enriched = enrich_missing_web_targets(built)
    vr = validate_steps(enriched)
    assert vr.valid, [e.message for e in vr.errors]
    fill_step = next(s for s in enriched if s.get("action") == "fill")
    click_step = next(s for s in enriched if s.get("action") == "click")
    assert fill_step["target"]["primary"] == "#email"
    assert click_step["target"]["primary"] == "#submit"
