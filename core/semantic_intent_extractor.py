# core/semantic_intent_extractor.py
"""
Extracts semantic intents and action intents from free-form prompt text.

Pure module — no I/O, no side effects, no runner/healer imports.

── extract_intent(prompt) ──────────────────────────────────────────────────────
Returns {"kind": ..., "name": ...} or None.
Used by the visibility block to produce assert_visible steps.

  "campo username"        → {"kind": "input",  "name": "username"}
  "botón login"           → {"kind": "button", "name": "login"}

── extract_action_intent(prompt) ───────────────────────────────────────────────
Returns a step-like dict describing a single UI action, or None.
Used by the action block to produce click/fill/assert_text steps.

  "haz click en login"          → {"action": "click",  "target": {"kind": "button", "name": "login"}}
  "click login button"          → {"action": "click",  "target": {"kind": "button", "name": "login"}}
  "escribe admin en username"   → {"action": "fill",   "target": {"kind": "input",  "name": "username"}, "value": "admin"}
  "llena username con admin"    → {"action": "fill",   "target": {"kind": "input",  "name": "username"}, "value": "admin"}
  "verifica que aparezca Swag"  → {"action": "assert_text_contains", "selector": "body", "text": "Swag"}
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional


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


# ── Shared suffix strippers ────────────────────────────────────────────────────
_BUTTON_SUFFIXES = ("button", "boton", "botón", "btn")
_INPUT_SUFFIXES  = ("field", "campo", "input", "textbox")


def _strip_type_suffix(raw: str, suffixes: tuple) -> str:
    """Remove a trailing type-indicator word (e.g. 'button', 'field') from raw."""
    for sfx in suffixes:
        if raw.endswith(sfx):
            raw = raw[: -len(sfx)].strip()
    return raw


def extract_action_intent(prompt: str) -> Optional[Dict[str, Any]]:
    """
    Extract a single UI action intent from free-form prompt text.

    Returns one of:
      {"action": "click",  "target": {"kind": "button", "name": <canonical>}}
      {"action": "fill",   "target": {"kind": "input",  "name": <canonical>}, "value": <str>}
      {"action": "assert_text_contains", "selector": "body", "text": <str>}
    or None when no clear action is detected.

    Resolution order: click → fill (value-first) → fill (field-first) → assert_text.
    Returns None for empty/None input or unrecognised prompts.
    """
    if not prompt:
        return None

    p = prompt.strip()

    # ── CLICK ──────────────────────────────────────────────────────────────────
    # "haz click en login" / "click en login" / "da click en login" / "click login button"
    _click_m = re.search(
        r"(?:haz\s+click\s+en|haz\s+clic\s+en|da\s+click\s+en|click\s+en|click\s+on|click)"
        r"\s+([\w][\w\s]*?)(?:\s+(?:button|boton|bot\u00f3n|btn))?\s*$",
        p,
        flags=re.IGNORECASE,
    )
    if _click_m:
        raw = _strip_type_suffix(_click_m.group(1).strip().lower(), _BUTTON_SUFFIXES)
        canonical = _BUTTON_ALIASES.get(raw)
        if canonical:
            return {"action": "click", "target": {"kind": "button", "name": canonical}}

    # ── FILL — value-first: "escribe <value> en <field>" ─────────────────────
    _fill_vf = re.search(
        r"(?:escribe|ingresa|teclea)\s+(\S+)\s+en\s+([\w][\w\s]*?)(?:\s+(?:field|campo|input|textbox))?\s*$",
        p,
        flags=re.IGNORECASE,
    )
    if _fill_vf:
        raw_field = _strip_type_suffix(_fill_vf.group(2).strip().lower(), _INPUT_SUFFIXES)
        canonical = _INPUT_ALIASES.get(raw_field)
        if canonical:
            return {
                "action": "fill",
                "target": {"kind": "input", "name": canonical},
                "value": _fill_vf.group(1).strip(),
            }

    # ── FILL — field-first: "llena <field> con <value>" / "fill <field> with <value>" ──
    _fill_ff = re.search(
        r"(?:llena|fill|type)\s+([\w][\w\s]*?)\s+(?:con|with)\s+(\S+)",
        p,
        flags=re.IGNORECASE,
    )
    if _fill_ff:
        raw_field = _strip_type_suffix(_fill_ff.group(1).strip().lower(), _INPUT_SUFFIXES)
        canonical = _INPUT_ALIASES.get(raw_field)
        if canonical:
            return {
                "action": "fill",
                "target": {"kind": "input", "name": canonical},
                "value": _fill_ff.group(2).strip(),
            }

    # ── ASSERT TEXT ────────────────────────────────────────────────────────────
    # "verifica que aparezca X" / "valida que se vea X" / "asegura que aparezca X"
    _at_m = re.search(
        r"(?:verifica|valida|asegura|verify|assert|check)"
        r"\s+(?:que\s+)?(?:aparezca|se\s+vea|is\s+visible|is\s+present)"
        r"(?:\s+el\s+texto)?"
        r"\s+[\"']?(.+?)[\"']?\s*$",
        p,
        flags=re.IGNORECASE,
    )
    if _at_m:
        text = _at_m.group(1).strip().strip("\"'")
        if text:
            return {"action": "assert_text_contains", "selector": "body", "text": text}

    return None
