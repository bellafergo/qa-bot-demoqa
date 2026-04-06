# core/nl_selector_inference.py
"""
Inferencia de selectores CSS a partir de lenguaje natural (pre-runner).

Se aplica después de que el planner/LLM produzca steps, y antes de validate_steps.
No relaja el contrato del execute engine: todo fill/click sigue llevando selector o target.primary.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Tuple


def _norm_value(v: Any) -> str:
    if v is None:
        return ""
    s = str(v).strip()
    if len(s) >= 2 and s[0] == s[-1] and s[0] in "\"'":
        s = s[1:-1]
    return s.strip()


def has_locator(step: Dict[str, Any]) -> bool:
    if str(step.get("selector") or "").strip():
        return True
    t = step.get("target")
    if isinstance(t, dict) and str(t.get("primary") or "").strip():
        return True
    return False


def make_min_target(primary: str) -> Dict[str, Any]:
    """Target mínimo compatible con normalize_steps_to_target / runner."""
    return {
        "primary": primary,
        "fallbacks": [],
        "timeout_ms": 3000,
        "state": "visible",
    }


def _field_phrase_to_selector(phrase: str) -> Optional[str]:
    low = (phrase or "").strip().lower()
    if not low:
        return None

    if any(x in low for x in ("email", "correo")) and "password" not in low and "contrase" not in low:
        return 'input[type="email"]'
    if any(x in low for x in ("password", "contraseña", "contrasena")):
        return 'input[type="password"]'
    if any(x in low for x in ("búsqueda", "busqueda", "search")):
        return 'input[type="search"]'
    if any(x in low for x in ("primer campo", "first field", "primera caja")):
        return "input:first-of-type"
    if any(x in low for x in ("texto", "text field", "campo de texto", "texto plano")):
        return 'input[type="text"]'

    m = re.search(r'placeholder\s*=\s*["\']([^"\']+)["\']', phrase, re.I)
    if m:
        p = m.group(1).replace("\\", "\\\\").replace('"', '\\"')
        return f'input[placeholder="{p}"]'

    m = re.search(r'placeholder\s+["\']([^"\']+)["\']', phrase, re.I)
    if m:
        p = m.group(1).replace("\\", "\\\\").replace('"', '\\"')
        return f'input[placeholder="{p}"]'

    # Label visible corto: "Email", "Usuario"
    m = re.match(r"^[\s\"']*([A-Za-zÁÉÍÓÚáéíóúñÑ0-9][\w\s]{0,40})[\s\"']*$", phrase.strip())
    if m:
        label = m.group(1).strip()
        if len(label) <= 48 and "\n" not in label:
            esc = label.replace('"', '\\"')
            return f'label:has-text("{esc}") + input'

    return 'input[type="text"]'


def _button_label_to_selector(label: str) -> str:
    raw = (label or "").strip().strip('"').strip("'")
    low = raw.lower()
    if any(x in low for x in ("submit", "enviar", "botón de submit", "boton de submit", "el botón de submit")):
        return 'button[type="submit"]'
    if any(x in low for x in ("sign in", "iniciar sesión", "iniciar sesion", "log in", "login")):
        # Prefer submit en formularios de login; texto como respaldo
        return 'button[type="submit"]'
    if not raw:
        return 'button[type="submit"]'
    esc = raw.replace('"', '\\"')
    return f'button:has-text("{esc}")'


def extract_fill_clauses_from_prompt(prompt: str) -> List[Tuple[str, str]]:
    """
    Devuelve lista ordenada de (value, css_selector) detectada en el texto.
    Cubre orden español/inglés: valor primero, luego descripción del campo.
    """
    p = prompt or ""
    out: List[Tuple[str, str]] = []

    pat_primer = re.compile(
        r"(?is)\b(?:escribe|escribí|escribi|ingresa|teclea|introduce|type|write|enter)\s+"
        r'((?:"[^"]*"|\'[^\']*\'|[^\n,]+?))\s+'
        r"en\s+el\s+primer\s+campo\b",
    )
    for m in pat_primer.finditer(p):
        val = _norm_value(m.group(1))
        if val:
            out.append((val, "input:first-of-type"))

    # español: escribe X en el campo de Y  /  escribe X en campo Y
    pat_es = re.compile(
        r"(?is)\b(?:escribe|escribí|escribi|ingresa|teclea|introduce)\s+"
        r'((?:"[^"]*"|\'[^\']*\'|[^\n,]+?))\s+'
        r"en\s+(?:el\s+)?(?:campo\s+(?:de\s+)?)([^,\n]+?)(?=\s*(?:,|\.?\s+y\s+|\s+haz\s+|\s+da\s+|\s+click|\s+clic|\s*$))",
    )
    for m in pat_es.finditer(p):
        val = _norm_value(m.group(1))
        field = m.group(2).strip().strip(".")
        sel = _field_phrase_to_selector(field)
        if val and sel:
            out.append((val, sel))

    # inglés: type X into the first field
    pat_first_en = re.compile(
        r"(?is)\b(?:type|enter|write|put)\s+"
        r'((?:"[^"]*"|\'[^\']*\'|[^\n,]+?))\s+'
        r"(?:into|in)\s+(?:the\s+)?first\s+field\b",
    )
    for m in pat_first_en.finditer(p):
        val = _norm_value(m.group(1))
        if val and (val, "input:first-of-type") not in out:
            out.append((val, "input:first-of-type"))

    # inglés: type X into the Y field / enter X in the Y field
    pat_en = re.compile(
        r"(?is)\b(?:type|enter|write|put)\s+"
        r'((?:"[^"]*"|\'[^\']*\'|[^\n,]+?))\s+'
        r"(?:into|in)\s+(?:the\s+)?(.+?)(?=\s*(?:,|\.?\s+and\s+|\s+click|\s*$))",
    )
    for m in pat_en.finditer(p):
        val = _norm_value(m.group(1))
        field = m.group(2).strip()
        sel = _field_phrase_to_selector(field)
        if val and sel and (val, sel) not in out:
            out.append((val, sel))

    return out


def extract_click_clauses_from_prompt(prompt: str) -> List[str]:
    """Selectores CSS en orden para clicks descritos en NL."""
    p = prompt or ""
    selectors: List[str] = []

    pat = re.compile(
        r"(?is)\b(?:haz\s+clic\s+en|haz\s+click\s+en|da\s+click\s+en|da\s+clic\s+en|"
        r"click\s+on|click\s+en|press\s+)\s*(.+?)(?=\s*(?:,|\.?\s+y\s+|\s*$))",
    )
    for m in pat.finditer(p):
        chunk = m.group(1).strip().strip(".").strip()
        selectors.append(_button_label_to_selector(chunk))

    return selectors


def enrich_nl_steps_from_prompt(prompt: str, steps: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """
    Completa selector (+ target mínimo) en steps fill/click que vinieron sin localizador.

    Usa el prompt completo para casos valor-primero (español) y clicks con texto.
    """
    if not steps:
        return steps

    fill_clauses = extract_fill_clauses_from_prompt(prompt)
    fill_queue: List[Tuple[str, str]] = list(fill_clauses)

    click_clauses = extract_click_clauses_from_prompt(prompt)
    click_queue: List[str] = list(click_clauses)

    out: List[Dict[str, Any]] = []
    for step in steps:
        s = dict(step)
        action = str(s.get("action") or "").strip().lower()

        if action == "fill" and not has_locator(s):
            val = _norm_value(s.get("value"))
            chosen: Optional[Tuple[str, str]] = None
            for i, (cv, sel) in enumerate(fill_queue):
                if _norm_value(cv) == val:
                    chosen = fill_queue.pop(i)
                    break
            if chosen is None and fill_queue:
                chosen = fill_queue.pop(0)
            if chosen:
                _, sel = chosen
                s["selector"] = sel
                if not isinstance(s.get("target"), dict) or not s["target"].get("primary"):
                    s["target"] = make_min_target(sel)
            elif val and "@" in val:
                s["selector"] = 'input[type="email"]'
                s["target"] = make_min_target(s["selector"])
            elif val:
                s["selector"] = "input:nth-of-type(1)"
                s["target"] = make_min_target(s["selector"])

        if action == "click" and not has_locator(s):
            label_hint = str(s.get("text") or s.get("label") or "").strip()
            sel: Optional[str] = None
            if label_hint:
                sel = _button_label_to_selector(label_hint)
            if not sel and click_queue:
                sel = click_queue.pop(0)
            elif not sel:
                sel = 'button[type="submit"]'
            s["selector"] = sel
            if not isinstance(s.get("target"), dict) or not s["target"].get("primary"):
                s["target"] = make_min_target(sel)

        out.append(s)
    return out
