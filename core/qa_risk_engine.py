# core/qa_risk_engine.py
from __future__ import annotations
from typing import Any, Dict, List


def infer_domain(prompt: str) -> str:
    p = (prompt or "").lower()

    if any(k in p for k in ["checkout", "carrito", "pagar", "pago", "tarjeta", "paypal", "stripe", "orden", "pedido"]):
        return "checkout/pagos"
    if any(k in p for k in ["envío", "envio", "shipping", "dirección", "direccion", "costo de envío", "costo de envio", "cp ", "código postal", "codigo postal"]):
        return "envio"
    if any(k in p for k in ["pdp", "producto", "detalle de producto", "talla", "color", "variación", "variacion", "variant", "sku"]):
        return "pdp"
    if any(k in p for k in ["plp", "categoría", "categoria", "listado", "filtros", "ordenar", "sort", "filter"]):
        return "plp/browse"
    if any(k in p for k in ["search", "busca", "buscar", "búsqueda", "busqueda", "autocomplete"]):
        return "search"
    if any(k in p for k in ["promo", "promoción", "promocion", "cupón", "cupon", "descuento", "2x1", "oferta", "bundle"]):
        return "promociones"
    if any(k in p for k in ["stock", "inventario", "disponible", "agotado", "backorder", "reservar"]):
        return "stock"
    if any(k in p for k in ["login", "inicia sesión", "iniciar sesion", "password", "contraseña", "contrasena", "usuario", "registro", "signup"]):
        return "cuenta/login"
    if any(k in p for k in ["lento", "performance", "tiempo de carga", "latencia", "lcp", "cls", "tti", "core web vitals"]):
        return "performance"

    return "general"

def build_negative_and_edge_cases(domain: str) -> dict:
    if domain == "checkout/pagos":
        return {
            "negative": [
                "Pago rechazado por el gateway",
                "Timeout en autorización de pago",
                "Monto final no coincide con el carrito",
            ],
            "edge": [
                "Doble click en botón pagar",
                "Retry tras error genera doble orden",
                "Cambio de stock justo al confirmar",
            ],
        }

    if domain == "promociones":
        return {
            "negative": [
                "Cupón expirado aplicado",
                "Cupón válido no se aplica",
            ],
            "edge": [
                "Múltiples cupones encadenados",
                "Promo expira durante checkout",
            ],
        }

    if domain == "stock":
        return {
            "negative": [
                "Producto sin stock permite compra",
            ],
            "edge": [
                "Stock cambia en checkout",
                "Reserva no se libera tras fallo",
            ],
        }

    if domain == "cuenta/login":
        return {
            "negative": [
                "Usuario válido bloqueado",
                "Password correcto marcado como inválido",
            ],
            "edge": [
                "Sesión expira durante checkout",
                "Cambio de password en sesión activa",
            ],
        }

    return {
        "negative": ["Error funcional no esperado"],
        "edge": ["Condición límite no cubierta"],
    }

def risk_template(domain: str) -> Dict[str, List[str]]:
    # P0: conversión/ingresos/operación se rompen
    if domain == "checkout/pagos":
        return {
            "P0": [
                "Cobro duplicado / cargo sin orden confirmada",
                "Totales incorrectos (impuestos/envío/descuentos)",
                "Fallo de autorización y mensajes confusos",
            ],
            "P1": [
                "Reintentos generan órdenes duplicadas",
                "Cupones aplican mal (stacking indebido)",
                "Problemas de stock al confirmar orden",
            ],
            "P2": [
                "UX: validaciones tardías / máscaras débiles",
                "Accesibilidad: errores no anunciados",
            ],
        }

    if domain == "envio":
        return {
            "P0": [
                "Costo de envío incorrecto / cambia al final (abandono)",
                "CP inválido rompe checkout (hard stop)",
                "Opciones de entrega desaparecen por región",
            ],
            "P1": [
                "Fechas de entrega inconsistentes entre PDP vs checkout",
                "Reglas de envío gratis aplican mal",
            ],
            "P2": [
                "UX: autocompletar dirección / validación inline",
            ],
        }

    if domain == "pdp":
        return {
            "P0": [
                "Precio/promoción difiere entre PDP y carrito",
                "Variantes (talla/color) no respetan stock real",
                "Add to cart falla silenciosamente",
            ],
            "P1": [
                "Galería/imagenes no cargan en móvil",
                "Recomendaciones rompen performance",
            ],
            "P2": [
                "UX: selector de variantes confuso / accesibilidad",
            ],
        }

    if domain == "plp/browse":
        return {
            "P0": [
                "Filtros/ordenamiento rompen resultados (pierdes conversión)",
                "Productos sin stock aparecen como disponibles",
                "Paginación/infinite scroll se corta",
            ],
            "P1": [
                "Badges de promo inconsistentes",
                "Conteos de filtros incorrectos",
            ],
            "P2": [
                "UX: filtros persistentes, reset, performance móvil",
            ],
        }

    if domain == "search":
        return {
            "P0": [
                "Búsqueda no encuentra productos por sinónimos/typos (pierde ventas)",
                "Autocomplete sugiere productos sin stock",
                "Search API intermitente/timeout",
            ],
            "P1": [
                "Ranking pobre (irrelevancia arriba)",
                "Facetas inconsistentes con PLP",
            ],
            "P2": [
                "UX: cero resultados sin alternativas útiles",
            ],
        }

    if domain == "promociones":
        return {
            "P0": [
                "Promos rompen totales o permiten descuento indebido",
                "Cupones se aplican a productos excluidos",
                "Promos no se reflejan en checkout/orden",
            ],
            "P1": [
                "Reglas por canal (app/web) inconsistentes",
                "Redondeos por moneda/IVA",
            ],
            "P2": [
                "Copy confuso de términos y condiciones",
            ],
        }

    if domain == "stock":
        return {
            "P0": [
                "Se vende sin inventario (oversell)",
                "Stock cambia en checkout y orden se cae",
                "Reservas de inventario no liberan",
            ],
            "P1": [
                "Backorder mal comunicado",
                "Tiempos de entrega inconsistentes",
            ],
            "P2": [
                "UX: avisos tardíos de disponibilidad",
            ],
        }

    if domain == "cuenta/login":
        return {
            "P0": [
                "Login/registro bloquea funnel (no compra)",
                "Recuperación de contraseña falla o expira",
                "Errores 500 intermitentes en auth",
            ],
            "P1": [
                "Sesión expira durante checkout",
                "Mensajes ambiguos generan soporte",
            ],
            "P2": [
                "UX móvil: autofill, teclado, máscaras",
            ],
        }

    if domain == "performance":
        return {
            "P0": [
                "LCP alto en home/PLP/PDP (baja conversión)",
                "Timeout en APIs críticas (search, cart, checkout)",
            ],
            "P1": [
                "Picos de error en campañas (hot sale/bf)",
                "Rendimiento peor en móvil",
            ],
            "P2": [
                "Imágenes sin optimizar / caché deficiente",
            ],
        }

    return {
        "P0": ["Define el flujo (login/PLP/PDP/búsqueda/carrito/checkout/pagos/envío)."],
        "P1": [],
        "P2": [],
    }


def build_risk_brief(prompt: str) -> Dict[str, Any]:
    domain = infer_domain(prompt)
    risks = risk_template(domain)
    return {
        "domain": domain,
        "p0": risks.get("P0", []),
        "p1": risks.get("P1", []),
        "p2": risks.get("P2", []),
    }