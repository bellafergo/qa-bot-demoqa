# core/generic_login_targets.py
"""
Targets genéricos para expansión DSL `login` y login ambiguo (cualquier dominio).

Contrato: dicts compatibles con validate_steps (selector + target.primary) y el runner.
"""
from __future__ import annotations

import copy
from typing import Any, Dict


def _base_target(primary: str, fallbacks: list, intent: str) -> Dict[str, Any]:
    return {
        "primary": primary,
        "fallbacks": fallbacks,
        "timeout_ms": 3000,
        "state": "visible",
        "intent": intent,
    }


_USER_FALLBACKS = [
    {"type": "css", "value": 'input[name="email"]'},
    {"type": "css", "value": 'input[autocomplete="username"]'},
    {"type": "css", "value": 'input[type="text"]'},
]

_PASS_FALLBACKS = [
    {"type": "css", "value": 'input[name="password"]'},
    {"type": "css", "value": 'input[autocomplete="current-password"]'},
]

_SUBMIT_FALLBACKS = [
    {"type": "css", "value": 'input[type="submit"]'},
    {"type": "css", "value": 'button:has-text("Sign in")'},
    {"type": "css", "value": 'button:has-text("Iniciar sesión")'},
    {"type": "css", "value": 'button:has-text("Login")'},
]


def make_generic_login_user_target() -> Dict[str, Any]:
    return _base_target(
        'input[type="email"]',
        copy.deepcopy(_USER_FALLBACKS),
        "fill:username",
    )


def make_generic_login_password_target() -> Dict[str, Any]:
    return _base_target(
        'input[type="password"]',
        copy.deepcopy(_PASS_FALLBACKS),
        "fill:password",
    )


def make_generic_login_submit_target() -> Dict[str, Any]:
    return _base_target(
        'button[type="submit"]',
        copy.deepcopy(_SUBMIT_FALLBACKS),
        "click:submit",
    )
