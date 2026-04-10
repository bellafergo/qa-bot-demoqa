# tests/test_user_literal_preservation.py
from __future__ import annotations

from core.user_literal_preservation import (
    extract_user_sensitive_literals,
    restore_user_literals_in_steps,
)


def test_extract_email_url_quoted():
    p = (
        "escribe fernanda.costa@zuperio.internal en el email y "
        "'Admin_2026!' en password y https://portal.zuperio.ai/login en url"
    )
    lit = extract_user_sensitive_literals(p)
    assert "fernanda.costa@zuperio.internal" in lit
    assert "Admin_2026!" in lit
    assert "https://portal.zuperio.ai/login" in lit


def test_restore_email_typo_in_fill_value():
    prompt = "escribe fernanda.costa@zuperio.internal en el campo de email"
    steps = [
        {
            "action": "fill",
            "selector": 'input[type="email"]',
            "value": "femanda.costa@zuperio.internal",
        }
    ]
    out = restore_user_literals_in_steps(prompt, steps)
    assert out[0]["value"] == "fernanda.costa@zuperio.internal"


def test_restore_quoted_password_literal():
    prompt = "escribe 'Admin_2026!' en el campo de password"
    steps = [
        {
            "action": "fill",
            "selector": 'input[type="password"]',
            "value": "Admin_2026",
        }
    ]
    out = restore_user_literals_in_steps(prompt, steps)
    assert out[0]["value"] == "Admin_2026!"


def test_restore_url_near_match():
    prompt = "escribe https://portal.zuperio.ai/login en el campo url"
    steps = [
        {
            "action": "fill",
            "selector": "input#url",
            "value": "https://portal.zuperio.ai/logn",
        }
    ]
    out = restore_user_literals_in_steps(prompt, steps)
    assert out[0]["value"] == "https://portal.zuperio.ai/login"


def test_no_overcorrect_unrelated_value():
    prompt = "escribe un email de prueba en el campo de correo"
    steps = [
        {
            "action": "fill",
            "selector": 'input[type="email"]',
            "value": "nobody@example.com",
        }
    ]
    out = restore_user_literals_in_steps(prompt, steps)
    assert out[0]["value"] == "nobody@example.com"
