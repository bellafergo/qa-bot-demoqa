# tests/test_nl_selector_inference.py
"""Regresión: inferencia de selectores desde NL (valor primero, campos comunes)."""
from __future__ import annotations

import pytest

from core.nl_selector_inference import (
    enrich_nl_steps_from_prompt,
    extract_click_clauses_from_prompt,
    extract_fill_clauses_from_prompt,
    has_locator,
)
from core.step_compiler import parse_steps_from_prompt
from core.step_validator import validate_steps


def test_extract_fill_clauses_spanish_email_password():
    p = (
        "Ve a /login, escribe user@test.com en el campo de email, "
        "escribe pass123 en el campo de password, haz clic en Sign in"
    )
    fills = extract_fill_clauses_from_prompt(p)
    assert ("user@test.com", 'input[type="email"]') in fills
    assert ("pass123", 'input[type="password"]') in fills


def test_extract_click_submit_spanish():
    p = "haz clic en el botón de submit"
    assert extract_click_clauses_from_prompt(p) == ['button[type="submit"]']


def test_parse_steps_talent_login_flow_order_and_validation():
    base = "https://talent.example.com"
    p = (
        "Ve a /login, escribe user@test.com en el campo de email, "
        "escribe pass123 en el campo de password, haz clic en Sign in"
    )
    steps = parse_steps_from_prompt(p, base)
    assert steps[0]["action"] == "goto"
    assert steps[0]["url"] == "https://talent.example.com/login"
    actions = [s["action"] for s in steps]
    assert actions.index("fill") < actions.index("click")
    fills = [s for s in steps if s["action"] == "fill"]
    assert len(fills) == 2
    assert fills[0]["selector"] == 'input[type="email"]'
    assert fills[1]["selector"] == 'input[type="password"]'
    clicks = [s for s in steps if s["action"] == "click"]
    assert len(clicks) == 1
    assert clicks[0]["selector"] == 'button[type="submit"]'
    vr = validate_steps(steps)
    assert vr.valid, [e.message for e in vr.errors]


def test_enrich_llm_like_steps_missing_selectors():
    prompt = "escribe hola en el campo de texto"
    raw = [{"action": "fill", "value": "hola"}]
    out = enrich_nl_steps_from_prompt(prompt, raw)
    assert out[0]["selector"] == 'input[type="text"]'
    assert has_locator(out[0])
    assert validate_steps(out).valid


def test_enrich_first_field_spanish():
    prompt = "escribe abc en el primer campo"
    raw = [{"action": "fill", "value": "abc"}]
    out = enrich_nl_steps_from_prompt(prompt, raw)
    assert out[0]["selector"] == "input:first-of-type"
    assert validate_steps(out).valid


def test_enrich_preserves_explicit_selector():
    prompt = "escribe x en el campo de email"
    raw = [{"action": "fill", "selector": "#email", "value": "x"}]
    out = enrich_nl_steps_from_prompt(prompt, raw)
    assert out[0]["selector"] == "#email"


@pytest.mark.parametrize(
    "prompt,raw,expected_sel",
    [
        (
            "haz clic en Sign in",
            [{"action": "click"}],
            'button[type="submit"]',
        ),
    ],
)
def test_enrich_click_from_prompt(prompt, raw, expected_sel):
    out = enrich_nl_steps_from_prompt(prompt, raw)
    assert out[0]["selector"] == expected_sel
    assert validate_steps(out).valid
