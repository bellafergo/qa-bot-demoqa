# core/lang.py
from __future__ import annotations
import re
from typing import Optional, Dict, Any

_ES_MARKERS = {
    "hola", "buenos", "buenas", "quiero", "necesito", "ayuda", "por favor", "gracias",
    "ejecuta", "ejecutar", "valida", "validar", "prueba", "probar", "iniciar", "sesión",
    "historia", "usuario", "casos", "riesgos", "matriz", "pago", "carrito", "checkout",
    "stock", "promoción", "promocion", "inventario", "tienda", "correo", "contraseña",
    "dónde", "como", "qué", "cuál", "cuanto", "cuándo"
}

def detect_language(text: str, session: Optional[Dict[str, Any]] = None) -> str:
    """
    Returns: 'es' or 'en'
    Heurístico simple (rápido y estable). No usa servicios externos.
    """
    t = (text or "").strip().lower()
    if not t:
        return (session or {}).get("lang") or "es"

    # Si hay caracteres típicos del español, es español
    if re.search(r"[ñáéíóúü¿¡]", t):
        return "es"

    # Tokeniza
    words = re.findall(r"[a-záéíóúüñ']+", t)
    if not words:
        return (session or {}).get("lang") or "es"

    hits = 0
    for w in words[:40]:
        if w in _ES_MARKERS:
            hits += 1

    # Si detecta varias señales, español
    if hits >= 2:
        return "es"

    # Si el usuario ya venía hablando español, mantenlo salvo que haya señales fuertes en inglés
    prev = (session or {}).get("lang")
    if prev == "es" and hits >= 1:
        return "es"

    return "en"