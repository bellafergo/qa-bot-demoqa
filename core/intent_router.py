# core/intent_router.py
from __future__ import annotations

def detect_intent(prompt: str) -> str:
    """
    Retorna: "execute" | "doc" | "chat"

    Reglas:
    - execute: acciones en UI / navegación / login / validar / playwright
    - doc: artefactos QA (matriz, casos, gherkin, INVEST, plan, checklist)
    - chat: asesoría general
    """
    p = (prompt or "").strip().lower()
    if not p:
        return "chat"

    exec_kw = [
        "ejecuta", "ejecutar", "corre", "correr", "run", "playwright",
        "abre", "abrir", "ve a", "ir a", "navega", "navegar",
        "haz click", "da click", "clic", "click",
        "login", "inicia sesión", "iniciar sesion",
        "valida", "validar", "verifica", "verificar", "probar", "prueba",
        "checkout", "carrito", "pagar", "pago",
        "llena", "llenar", "escribe", "escribir", "selecciona", "seleccionar",
    ]

    doc_kw = [
        "matriz", "casos de prueba", "test cases", "gherkin", "given", "when", "then",
        "invest", "criterios de aceptación", "criterios de aceptacion",
        "plan de pruebas", "estrategia de pruebas", "checklist",
        "riesgos", "matriz de riesgos", "casos negativos", "edge cases",
        "scripts", "automatización", "automatizacion", "selenium",
    ]

    has_url = ("http://" in p) or ("https://" in p)

    # execute tiene prioridad si hay verbo de acción
    if any(k in p for k in exec_kw):
        return "execute"

    if any(k in p for k in doc_kw):
        return "doc"

    # si viene URL suelta, normalmente es para navegar/validar
    if has_url:
        return "execute"

    return "chat"