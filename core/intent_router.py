# core/intent_router.py
from __future__ import annotations

import re


# --- URL detector (más robusto que "http" in text) ---
_URL_RE = re.compile(r"https?://\S+", re.IGNORECASE)


def _has_url(text: str) -> bool:
    return bool(_URL_RE.search(text or ""))


def detect_intent(prompt: str) -> str:
    """
    Return: "execute" | "doc" | "chat"

    Safer rules:
    - doc: QA artifacts / analysis deliverables (matrix, gherkin, INVEST, risks, checklist, plan)
    - execute: ONLY when there is a clear UI/action instruction + a target (URL or "same/la misma")
    - chat: general advice / conversation
    """
    p = (prompt or "").strip().lower()
    if not p:
        return "chat"

    # -------------------------------------------------
    # 1) DOC first (avoid false execute on domain terms)
    # -------------------------------------------------
    # Notes:
    # - Include "risks" / "riesgos" here so "analiza riesgos del checkout" becomes doc.
    # - Keep it broad but focused on deliverables.
    doc_kw = [
        # Spanish
        "matriz", "matriz de pruebas", "casos de prueba", "escenarios",
        "criterios de aceptación", "criterios de aceptacion", "invest",
        "plan de pruebas", "estrategia de pruebas", "checklist",
        "riesgos", "matriz de riesgos", "casos negativos", "casos positivas", "casos positivos",
        "edge cases", "casos borde", "casos límite", "casos limite",
        "reporte qa", "test plan",

        # English
        "test matrix", "test cases", "acceptance criteria", "gherkin",
        "given", "when", "then", "risk", "risks", "negative cases",
        "edge case", "edge cases",
    ]

    # If it's clearly requesting an artifact/analysis -> DOC
    if any(k in p for k in doc_kw):
        return "doc"

    # -------------------------------------------------
    # 2) EXECUTE needs: action verb + target (url or same)
    # -------------------------------------------------
    # Action verbs: UI navigation / validation / test execution
    execute_verbs = [
        # Spanish
        "ejecuta", "ejecutar", "corre", "correr",
        "abre", "abrir", "ve a", "ir a", "entra a", "navega", "navegar",
        "haz click", "da click", "clic", "click",
        "valida", "validar", "verifica", "verificar",
        "probar", "prueba",
        "llena", "llenar", "escribe", "escribir", "ingresa", "ingresar",
        "selecciona", "seleccionar",

        # English
        "run a test", "run test", "execute", "open", "go to", "navigate",
        "click", "verify", "check", "validate", "assert",
        "fill", "type", "enter",
    ]

    has_execute_verb = any(v in p for v in execute_verbs)
    has_url = _has_url(p)

    # "same target" markers (use last base_url)
    has_same = any(x in p for x in [
        "la misma", "misma url", "mismo sitio", "mismo link",
        "same", "same url", "same site", "same page",
    ])

    # Important:
    # - domain words like "checkout/pago/carrito" should NOT trigger execute alone
    # - require verb + (url or same)
    if has_execute_verb and (has_url or has_same):
        return "execute"

    # -------------------------------------------------
    # 3) URL-only: don't assume execute
    # -------------------------------------------------
    if has_url and not has_execute_verb:
        return "chat"

    return "chat"