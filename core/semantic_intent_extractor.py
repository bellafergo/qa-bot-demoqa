# core/semantic_intent_extractor.py
"""
Extracts a single semantic intent from free-form prompt text.

Returns {"kind": ..., "name": ...} or None when intent is not clear.

Pure module — no I/O, no side effects, no runner/healer imports.

Supported (MVP)
  inputs:  username, password, email
  buttons: login, submit, search

Examples:
  "campo username"        → {"kind": "input",  "name": "username"}
  "campo password"        → {"kind": "input",  "name": "password"}
  "campo email sea"       → {"kind": "input",  "name": "email"}
  "botón login"           → {"kind": "button", "name": "login"}
  "login button"          → {"kind": "button", "name": "login"}
  "submit button visible" → {"kind": "button", "name": "submit"}
  "search button"         → {"kind": "button", "name": "search"}
  "verify body"           → None
  "some random text"      → None
"""
from __future__ import annotations

import re
from typing import Dict, List, Optional


# ── Canonical name tables ──────────────────────────────────────────────────────
# Maps surface form (lowercase) → canonical name used by semantic_step_builder.

_INPUT_ALIASES: Dict[str, str] = {
    "username":   "username",
    "usuario":    "username",
    "user":       "username",
    "password":   "password",
    "contraseña": "password",
    "contrasena": "password",
    "pass":       "password",
    "pwd":        "password",
    "email":      "email",
    "correo":     "email",
    "mail":       "email",
}

_BUTTON_ALIASES: Dict[str, str] = {
    "login":    "login",
    "signin":   "login",
    "ingresar": "login",
    "entrar":   "login",
    "submit":   "submit",
    "enviar":   "submit",
    "search":   "search",
    "buscar":   "search",
}

# ── Element-type indicators ────────────────────────────────────────────────────
_INPUT_INDICATORS = frozenset({
    "campo", "field", "input", "textbox", "cuadro", "caja",
})
_BUTTON_INDICATORS = frozenset({
    "botón", "boton", "button", "btn",
})


def _tokens(text: str) -> List[str]:
    """
    Return lowercase word tokens.
    Uses \\w+ with Python 3's default Unicode mode so accented characters
    (e.g. "botón", "contraseña") are kept intact as single tokens.
    """
    return re.findall(r"\w+", text.lower())


def extract_intent(prompt: str) -> Optional[Dict[str, str]]:
    """
    Extract a single semantic intent from *prompt*.

    Resolution order:
      1. Button name + (explicit button indicator OR no input indicator present)
      2. Input name (any occurrence)
      3. None — no clear intent found

    Returns {"kind": "input"|"button", "name": <canonical>} or None.
    """
    if not prompt:
        return None

    words = _tokens(prompt)
    word_set = set(words)

    has_input_indicator  = bool(word_set & _INPUT_INDICATORS)
    has_button_indicator = bool(word_set & _BUTTON_INDICATORS)

    # ── 1. Buttons (checked first — login/submit/search are rarely field names) ──
    for word in words:
        canonical = _BUTTON_ALIASES.get(word)
        if canonical is None:
            continue
        # Accept when there is an explicit button indicator, or when there is
        # no competing input indicator that would make the type ambiguous.
        if has_button_indicator or not has_input_indicator:
            return {"kind": "button", "name": canonical}

    # ── 2. Input fields ───────────────────────────────────────────────────────
    for word in words:
        canonical = _INPUT_ALIASES.get(word)
        if canonical is not None:
            return {"kind": "input", "name": canonical}

    return None
