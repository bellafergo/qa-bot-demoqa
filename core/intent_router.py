# core/intent_router.py
from __future__ import annotations

import re


def detect_intent(prompt: str) -> str:
    """
    Retorna: "execute" | "doc" | "chat"

    Reglas (más seguras):
    - doc: artefactos QA (matriz, casos, gherkin, INVEST, plan, checklist, riesgos)
    - execute: SOLO cuando hay instrucción clara de interacción + (URL o "la misma"/"same")
    - chat: asesoría general
    """
    p = (prompt or "").strip().lower()
    if not p:
        return "chat"

    # ----------------------------
    # DOC: primero, para que análisis/artefactos no caigan en execute
    # ----------------------------
    doc_kw = [
        "matriz",
        "casos de prueba",
        "test cases",
        "gherkin",
        "given",
        "when",
        "then",
        "invest",
        "criterios de aceptación",
        "criterios de aceptacion",
        "plan de pruebas",
        "estrategia de pruebas",
        "checklist",
        "riesgos",
        "matriz de riesgos",
        "casos negativos",
        "edge cases",
        "scripts",
        "automatización",
        "automatizacion",
        "selenium",
        "escenarios",
        "test matrix",
    ]
    if any(k in p for k in doc_kw):
        return "doc"

    # ----------------------------
    # EXECUTE: requiere verbo de acción + target (url o "same/la misma")
    # ----------------------------
    execute_verbs = [
        "ejecuta", "ejecutar", "corre", "correr", "run",
        "abre", "abrir", "ve a", "ir a", "navega", "navegar",
        "haz click", "da click", "clic", "click",
        "login", "inicia sesión", "iniciar sesion",
        "valida", "validar", "verifica", "verificar",
        "probar", "prueba",
        "llena", "llenar", "escribe", "escribir",
        "selecciona", "seleccionar",
        "check", "verify",
    ]

    has_execute_verb = any(v in p for v in execute_verbs)
    has_url = ("http://" in p) or ("https://" in p)
    has_same = (" la misma" in p) or (" misma url" in p) or (" same" in p)

    # Nota: palabras como "checkout", "carrito", "pago" NO deben disparar execute por sí solas.
    # Son DOMINIO, no intención.
    if has_execute_verb and (has_url or has_same):
        return "execute"

    # Si solo pegan una URL sin pedir ejecutar, mejor no asumir execute.
    # (si quieren execute, dirán "ve a / valida / run a test").
    url_only = has_url and not has_execute_verb
    if url_only:
        return "chat"

    return "chat"