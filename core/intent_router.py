# core/intent_router.py
from __future__ import annotations

import re

# --- URL detector (más robusto que "http" in text) ---
_URL_RE = re.compile(r"https?://\S+", re.IGNORECASE)

# --- Purchase confirmation token (seguridad) ---
# El usuario debe escribir EXACTAMENTE este token (case-insensitive).
_PURCHASE_TOKEN = "confirmar_compra"
_PURCHASE_TOKEN_RE = re.compile(rf"(^|\W){_PURCHASE_TOKEN}(\W|$)", re.IGNORECASE)

# --- Site markers (cuando NO hay URL pero sí target claro) ---
_SITE_MARKERS = [
    # e-commerce targets
    "heb", "h-e-b", "heb.com.mx", "heb mexico",
    # puedes agregar más: "walmart", "amazon", etc.
]


def _has_url(text: str) -> bool:
    return bool(_URL_RE.search(text or ""))


def _has_site_marker(text: str) -> bool:
    p = (text or "").lower()
    return any(m in p for m in _SITE_MARKERS)


def has_purchase_confirmation(prompt: str) -> bool:
    """
    Seguridad: SOLO permite compras reales si el usuario incluye el token.
    Úsalo en tu capa EXECUTE para decidir mode="purchase".
    """
    return bool(_PURCHASE_TOKEN_RE.search(prompt or ""))


def detect_intent(prompt: str) -> str:
    """
    Return: "execute" | "doc" | "chat"

    Safer rules:
    - doc: QA artifacts / analysis deliverables (matrix, gherkin, INVEST, risks, checklist, plan)
    - execute: ONLY when there is a clear UI/action instruction + a target:
        - URL
        - "same/la misma"
        - OR a known site marker (e.g., HEB) for runners especiales sin URL
    - chat: general advice / conversation
    """
    p_raw = (prompt or "").strip()
    p = p_raw.lower()
    if not p:
        return "chat"

    # -------------------------------------------------
    # 1) DOC first (avoid false execute on domain terms)
    # -------------------------------------------------
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

    if any(k in p for k in doc_kw):
        return "doc"

    # -------------------------------------------------
    # 2) EXECUTE needs: action verb + target (url/same/site)
    # -------------------------------------------------
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
    has_same = any(x in p for x in [
        "la misma", "misma url", "mismo sitio", "mismo link",
        "same", "same url", "same site", "same page",
    ])
    has_site = _has_site_marker(p)

    # Target = URL o "same" o sitio conocido (como HEB)
    has_target = has_url or has_same or has_site

    # Evitar que "checkout/pago/carrito" dispare execute por sí solo:
    if has_execute_verb and has_target:
        return "execute"

    # -------------------------------------------------
    # 3) URL-only: don't assume execute
    # -------------------------------------------------
    if has_url and not has_execute_verb:
        return "chat"

    return "chat"
