# core/user_literal_preservation.py
"""
Post-process: restaura literales del usuario en steps (fill/input/...) cuando el LLM
o el parser deforman emails, URLs o strings entre comillas.
"""
from __future__ import annotations

import re
from difflib import SequenceMatcher
from typing import Any, Dict, List, Optional, Set

# Acciones que escriben un valor en un campo (mismo criterio que enriquecimiento NL).
_VALUE_ACTIONS = frozenset({"fill", "input", "type", "enter"})

# Umbral conservador: solo sustituir si el texto generado es claramente el mismo literal deformado.
_MIN_LITERAL_LEN = 4
_MIN_RATIO = 0.92

_QUOTED_SINGLE = re.compile(r"'([^']{%d,})'" % _MIN_LITERAL_LEN)
_QUOTED_DOUBLE = re.compile(r'"([^"\\]{%d,})"' % _MIN_LITERAL_LEN)
_EMAIL = re.compile(
    r"\b[a-zA-Z0-9][a-zA-Z0-9._+-]*@"
    r"[a-zA-Z0-9][a-zA-Z0-9._-]*(?:\.[a-zA-Z0-9][a-zA-Z0-9._-]*)*\b"
)
_URL = re.compile(r"https?://[^\s\]'\"<>),]+", re.IGNORECASE)


def extract_user_sensitive_literals(prompt: str) -> List[str]:
    """Extrae literales candidatos del texto del usuario (orden: más largos primero)."""
    if not (prompt or "").strip():
        return []
    found: Set[str] = set()

    for rx in (_QUOTED_SINGLE, _QUOTED_DOUBLE):
        for m in rx.finditer(prompt):
            found.add(m.group(1))

    for m in _EMAIL.finditer(prompt):
        found.add(m.group(0))

    for m in _URL.finditer(prompt):
        found.add(m.group(0))

    # Quitar subcadenas redundantes (ej. mismo email dentro de una URL citada ya cubierta).
    by_len = sorted(found, key=len, reverse=True)
    kept: List[str] = []
    for s in by_len:
        if len(s) < _MIN_LITERAL_LEN:
            continue
        if any(s != k and s in k for k in kept):
            continue
        kept.append(s)
    return kept


def _ratio(a: str, b: str) -> float:
    return SequenceMatcher(None, a, b).ratio()


def _should_restore(generated: str, literal: str) -> bool:
    if generated == literal:
        return False
    if min(len(generated), len(literal)) < _MIN_LITERAL_LEN:
        return False
    # Evitar sustituir cuando la diferencia es grande (p. ej. otro valor válido).
    if abs(len(generated) - len(literal)) > max(4, int(0.15 * max(len(generated), len(literal)))):
        return False
    return _ratio(generated, literal) >= _MIN_RATIO


def restore_user_literals_in_steps(
    prompt: str,
    steps: Optional[List[Dict[str, Any]]],
) -> List[Dict[str, Any]]:
    """
    Para steps con `value`, si el valor parece una versión deformada de un literal
    presente en el prompt del usuario, reemplázalo por el literal exacto.
    Cada literal del prompt se usa como máximo una vez (orden de aparición en steps).
    """
    if not steps:
        return list(steps or [])

    literals = extract_user_sensitive_literals(prompt)
    if not literals:
        return [dict(s) for s in steps]

    used: Set[str] = set()
    out: List[Dict[str, Any]] = []
    for step in steps:
        s = dict(step)
        action = str(s.get("action") or "").strip().lower()
        if action not in _VALUE_ACTIONS:
            out.append(s)
            continue
        val = s.get("value")
        if not isinstance(val, str):
            out.append(s)
            continue
        v = val.strip()
        if not v:
            out.append(s)
            continue

        best_lit: Optional[str] = None
        best_r = 0.0
        for lit in literals:
            if lit in used:
                continue
            if not _should_restore(v, lit):
                continue
            r = _ratio(v, lit)
            if r > best_r:
                best_r = r
                best_lit = lit

        if best_lit is not None:
            s["value"] = best_lit
            used.add(best_lit)
        out.append(s)
    return out
