# core/doc_patterns.py
from __future__ import annotations

from copy import deepcopy
from typing import Any, Dict, Literal

Domain = Literal["retail", "pos", "web"]

SUPPORTED_DOMAINS = ("retail", "pos", "web")

# Alias comunes -> dominio soportado
_DOMAIN_ALIASES: Dict[str, Domain] = {
    "ecommerce": "retail",
    "e-commerce": "retail",
    "shop": "retail",
    "store": "retail",
    "tienda": "retail",
    "tienda en linea": "retail",
    "tienda en línea": "retail",
    "checkout": "retail",
    "carrito": "retail",

    "punto de venta": "pos",
    "point of sale": "pos",

    "app": "web",
    "website": "web",
    "saas": "web",
}

DOC_PATTERNS: Dict[Domain, Dict[str, Any]] = {
    "retail": {
        "recommended_smoke": [
            "Login / sesión válida",
            "Buscar producto",
            "Agregar al carrito",
            "Actualizar cantidades",
            "Checkout básico",
        ],
        "recommended_regression": [
            "Cupones / descuentos",
            "Impuestos",
            "Inventario (stock 0 / bajo)",
            "Devoluciones / reembolsos",
            "Permisos / roles",
        ],
        "recommended_edges": [
            "Carrito vacío",
            "Sesión expirada en checkout",
            "Pago rechazado",
            "Dirección inválida",
            "Producto sin precio",
        ],
        "recommended_p0_minimal": [
            "Checkout Golden Path (pago aprobado)",
            "Pago rechazado (mensajes claros + sin cobro doble)",
            "Stock agotado en checkout (bloquea compra correctamente)",
            "Cupón inválido/expirado (no aplica descuento)",
            "Reinicio de sesión en checkout (no pierde carrito / reintenta)",
        ],
        "by_role": {
            "customer": ["Login", "Búsqueda", "Carrito", "Checkout", "Tracking"],
            "admin": ["Alta/edición de producto", "Cambio de precio", "Inventario", "Promociones", "Órdenes / reembolsos"],
        },
        "by_flow": {
            "checkout": ["Cálculo de impuestos", "Aplicación de cupones", "Pago aprobado/rechazado", "Confirmación"],
            "returns": ["Solicitud de devolución", "Reembolso", "Ajuste de inventario"],
        },
    },

    "pos": {
        "recommended_smoke": [
            "Apertura de caja",
            "Venta con producto escaneado",
            "Pago efectivo/tarjeta",
            "Impresión de ticket",
            "Cierre de caja básico",
        ],
        "recommended_regression": [
            "Descuentos manuales y por regla",
            "Cancelación de ticket",
            "Devolución",
            "Corte X/Z",
            "Modo offline / reconexión",
        ],
        "recommended_edges": [
            "Impresora sin papel",
            "Red lenta / timeouts",
            "Código de barras inválido",
            "Producto sin inventario",
            "Cambio de precio en línea durante venta",
        ],
        "recommended_p0_minimal": [
            "Venta simple + ticket",
            "Pago tarjeta (aprobado) + ticket",
            "Pago tarjeta (rechazado) sin duplicar cobro",
            "Cancelación ticket (reverso correcto)",
            "Offline → reconexión (no rompe folios)",
        ],
        "by_role": {
            "cashier": ["Venta", "Cobro", "Cancelación", "Devolución", "Cierre de caja"],
            "supervisor": ["Descuentos", "Apertura/cierre", "Corte X/Z", "Permisos"],
        },
        "by_flow": {
            "sale": ["Escaneo", "Totales", "Pago", "Ticket"],
            "cash": ["Cambio", "Cierre", "Diferencias"],
        },
    },

    "web": {
        "recommended_smoke": ["Login", "Navegación", "CRUD básico", "Permisos"],
        "recommended_regression": ["Validaciones", "Filtros", "Exportaciones", "Paginación"],
        "recommended_edges": ["Campos vacíos", "Longitudes máximas", "Latencia alta", "Sesión expirada"],
        "recommended_p0_minimal": [
            "Login válido",
            "Validación de forms crítica",
            "Permisos/roles (bloquea acceso indebido)",
            "CRUD feliz (alta/edición/consulta)",
            "Sesión expirada (redirige y conserva estado clave)",
        ],
        "by_role": {},
        "by_flow": {},
    },
}


def normalize_domain(domain: str) -> Domain:
    d = (domain or "").strip().lower()
    if d in SUPPORTED_DOMAINS:
        return d  # type: ignore[return-value]
    if d in _DOMAIN_ALIASES:
        return _DOMAIN_ALIASES[d]
    return "web"


def get_patterns(domain: str) -> Dict[str, Any]:
    """
    Devuelve patrones para el dominio, con llaves mínimas garantizadas.
    Importante: devuelve COPIA (deepcopy) para evitar mutaciones accidentales.
    """
    dom: Domain = normalize_domain(domain)
    pats = deepcopy(DOC_PATTERNS[dom])

    # Garantiza llaves mínimas
    pats.setdefault("recommended_smoke", [])
    pats.setdefault("recommended_regression", [])
    pats.setdefault("recommended_edges", [])
    pats.setdefault("recommended_p0_minimal", [])
    pats.setdefault("by_role", {})
    pats.setdefault("by_flow", {})

    return pats


__all__ = ["Domain", "SUPPORTED_DOMAINS", "normalize_domain", "get_patterns", "DOC_PATTERNS"]