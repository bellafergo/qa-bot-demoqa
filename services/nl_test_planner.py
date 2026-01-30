# services/nl_test_planner.py
"""
Natural Language Test Planner

Converts natural language test descriptions into executable step plans.
Uses heuristic parsing (no LLM) with domain-specific fallbacks.

ALLOWED ACTIONS:
    goto, click, fill, login, search, add_to_cart,
    wait_for_text, assert_text, assert_cart_count, screenshot

SECURITY:
    - Payment/checkout requests set risk_flags=["payment"], requires_confirmation=True
    - Never includes real credentials; uses env placeholders (e.g., HEB_EMAIL)
    - Stops at cart/checkout without executing real payment

EXAMPLES:

1. HEB cart test (Spanish):
   >>> plan_from_text("agregar tomate y leche al carrito en HEB")
   {
       "ok": True,
       "language": "es",
       "intent": "add_to_cart",
       "base_url": "https://www.heb.com.mx",
       "requires_confirmation": False,
       "summary": "Agregar productos al carrito en HEB: tomate, leche",
       "steps": [
           {"action": "goto", "url": "https://www.heb.com.mx"},
           {"action": "search", "value": "tomate"},
           {"action": "add_to_cart", "product": "tomate"},
           {"action": "search", "value": "leche"},
           {"action": "add_to_cart", "product": "leche"},
           {"action": "screenshot"}
       ],
       "risk_flags": [],
       "assumptions": ["Using HEB Mexico site", "First search result will be added"],
       "errors": []
   }

2. Checkout request (flagged as risky):
   >>> plan_from_text("complete checkout and pay with credit card")
   {
       "ok": True,
       "language": "en",
       "intent": "checkout",
       "base_url": None,
       "requires_confirmation": True,
       "summary": "Checkout flow (stopped before payment)",
       "steps": [
           {"action": "goto", "url": "{base_url}"},
           {"action": "click", "selector": "[data-test='cart']", "intent": "open_cart"},
           {"action": "click", "selector": "[data-test='checkout']", "intent": "start_checkout"},
           {"action": "screenshot"}
       ],
       "risk_flags": ["payment"],
       "assumptions": ["Will stop before actual payment", "Requires base_url to be provided"],
       "errors": []
   }

3. Login and search:
   >>> plan_from_text("login to HEB and search for coca cola", base_url="https://heb.com.mx")
   {
       "ok": True,
       "language": "en",
       "intent": "login_and_search",
       "base_url": "https://heb.com.mx",
       "requires_confirmation": False,
       "summary": "Login to HEB and search for: coca cola",
       "steps": [
           {"action": "goto", "url": "https://heb.com.mx"},
           {"action": "login", "email": "{HEB_EMAIL}", "password": "{HEB_PASSWORD}"},
           {"action": "search", "value": "coca cola"},
           {"action": "screenshot"}
       ],
       "risk_flags": [],
       "assumptions": ["Using env placeholders for credentials"],
       "errors": []
   }
"""
from __future__ import annotations

import re
from typing import Any

# =============================================================================
# CONSTANTS
# =============================================================================

ALLOWED_ACTIONS = frozenset({
    "goto",
    "click",
    "fill",
    "login",
    "search",
    "add_to_cart",
    "wait_for_text",
    "assert_text",
    "assert_cart_count",
    "screenshot",
})

PAYMENT_KEYWORDS = frozenset({
    "pay", "payment", "pago", "pagar", "checkout", "purchase", "comprar",
    "credit card", "tarjeta", "billing", "facturación", "complete order",
    "finalizar compra", "place order",
})

HEB_KEYWORDS = frozenset({"heb", "h-e-b", "heb.com", "heb.com.mx"})

SPANISH_INDICATORS = frozenset({
    "agregar", "añadir", "buscar", "carrito", "comprar", "iniciar",
    "sesión", "productos", "al", "en", "el", "la", "los", "las", "y",
    "con", "para", "verificar", "ver",
})

CART_KEYWORDS = frozenset({
    "cart", "carrito", "add", "agregar", "añadir", "basket", "cesta",
})

LOGIN_KEYWORDS = frozenset({
    "login", "log in", "sign in", "iniciar sesión", "ingresar", "acceder",
})

SEARCH_KEYWORDS = frozenset({
    "search", "buscar", "find", "encontrar", "look for",
})

HEB_BASE_URL = "https://www.heb.com.mx"
HEB_DEFAULT_PRODUCTS = ["tomate", "coca cola"]


# =============================================================================
# VALIDATION
# =============================================================================

def validate_plan(plan: dict, max_steps: int = 25) -> tuple[bool, list[str]]:
    """
    Validate a test plan dict.

    Args:
        plan: The plan dict to validate.
        max_steps: Maximum allowed steps (default 25).

    Returns:
        Tuple of (is_valid, list_of_errors).
    """
    errors: list[str] = []

    if not isinstance(plan, dict):
        return False, ["Plan must be a dict"]

    # Required keys (minimal)
    required_keys = {"ok", "steps"}
    missing = required_keys - set(plan.keys())
    if missing:
        errors.append(f"Missing required keys: {', '.join(sorted(missing))}")

    base_url = plan.get("base_url")

    # Validate steps
    steps = plan.get("steps")
    if steps is None:
        errors.append("'steps' is required")
    elif not isinstance(steps, list):
        errors.append("'steps' must be a list")
    else:
        if len(steps) > max_steps:
            errors.append(f"Too many steps: {len(steps)} > {max_steps}")

        for i, step in enumerate(steps):
            if not isinstance(step, dict):
                errors.append(f"Step {i} must be a dict")
                continue

            action = step.get("action")
            if not action:
                errors.append(f"Step {i} missing 'action'")
            elif action not in ALLOWED_ACTIONS:
                errors.append(
                    f"Step {i} has invalid action '{action}'. Allowed: {', '.join(sorted(ALLOWED_ACTIONS))}"
                )

            # Action-specific validation
            if action == "goto":
                url = step.get("url")
                if not url:
                    errors.append(f"Step {i}: 'goto' requires 'url'")
                else:
                    # If url depends on {base_url} then plan must provide base_url
                    if "{base_url}" in str(url) and not base_url:
                        errors.append(f"Step {i}: 'goto' uses '{{base_url}}' but plan.base_url is missing")

            if action == "fill" and not step.get("selector"):
                errors.append(f"Step {i}: 'fill' requires 'selector'")

            if action == "click" and not step.get("selector") and not step.get("intent"):
                errors.append(f"Step {i}: 'click' requires 'selector' or 'intent'")

    # Validate risk_flags
    risk_flags = plan.get("risk_flags")
    if risk_flags is not None and not isinstance(risk_flags, list):
        errors.append("'risk_flags' must be a list")

    # Validate requires_confirmation with payment
    if plan.get("risk_flags") and "payment" in plan.get("risk_flags", []):
        if not plan.get("requires_confirmation"):
            errors.append("Plans with 'payment' risk must have requires_confirmation=True")

    # Optional consistency: if requires_confirmation=True, risk_flags should include payment
    if plan.get("requires_confirmation") and "payment" not in (plan.get("risk_flags") or []):
        errors.append("requires_confirmation=True but missing 'payment' in risk_flags")

    is_valid = len(errors) == 0
    return is_valid, errors


# =============================================================================
# LANGUAGE DETECTION
# =============================================================================

def _detect_language(text: str) -> str:
    """Detect language from text. Returns 'es' or 'en'."""
    text_lower = text.lower()
    words = set(re.findall(r"\b\w+\b", text_lower))

    spanish_count = len(words & SPANISH_INDICATORS)
    # Simple heuristic: if 2+ Spanish indicators, assume Spanish
    return "es" if spanish_count >= 2 else "en"


# =============================================================================
# TEXT PARSING HELPERS
# =============================================================================

def _extract_products(text: str) -> list[str]:
    """Extract product names from natural language text."""
    text_lower = text.lower()

    # Remove common phrases
    cleaned = re.sub(
        r"\b(agregar|añadir|add|buscar|search|al carrito|to cart|"
        r"en heb|on heb|and|y|the|el|la|los|las)\b",
        " ",
        text_lower,
    )

    # Split by common delimiters
    parts = re.split(r"[,;]|\band\b|\by\b", cleaned)

    products = []
    for part in parts:
        # Clean and extract meaningful words
        words = re.findall(r"\b[a-záéíóúñü]{3,}\b", part.strip())
        if words:
            product = " ".join(words).strip()
            if product and product not in {"carrito", "cart", "heb"}:
                products.append(product)

    return products if products else []


def _has_payment_intent(text: str) -> bool:
    """Check if text requests payment/checkout."""
    text_lower = text.lower()
    return any(kw in text_lower for kw in PAYMENT_KEYWORDS)


def _has_login_intent(text: str) -> bool:
    """Check if text requests login."""
    text_lower = text.lower()
    return any(kw in text_lower for kw in LOGIN_KEYWORDS)


def _has_search_intent(text: str) -> bool:
    """Check if text requests search."""
    text_lower = text.lower()
    return any(kw in text_lower for kw in SEARCH_KEYWORDS)


def _has_cart_intent(text: str) -> bool:
    """Check if text mentions cart operations."""
    text_lower = text.lower()
    return any(kw in text_lower for kw in CART_KEYWORDS)


def _is_heb_context(text: str, app_hint: str | None) -> bool:
    """Check if HEB is the target app."""
    text_lower = text.lower()
    hint_lower = (app_hint or "").lower()

    return (
        any(kw in text_lower for kw in HEB_KEYWORDS) or
        any(kw in hint_lower for kw in HEB_KEYWORDS)
    )


def _infer_intent(text: str) -> str:
    """Infer the primary intent from text."""
    has_login = _has_login_intent(text)
    has_search = _has_search_intent(text)
    has_cart = _has_cart_intent(text)
    has_payment = _has_payment_intent(text)

    if has_payment:
        return "checkout"
    if has_login and has_search:
        return "login_and_search"
    if has_login and has_cart:
        return "login_and_cart"
    if has_login:
        return "login"
    if has_cart:
        return "add_to_cart"
    if has_search:
        return "search"

    return "navigate"


# =============================================================================
# STEP GENERATION
# =============================================================================

def _generate_heb_cart_steps(
    products: list[str],
    include_login: bool = False,
    base_url: str | None = None,
) -> list[dict[str, Any]]:
    """Generate steps for HEB cart operations."""
    url = base_url or HEB_BASE_URL
    steps: list[dict[str, Any]] = []

    # Navigate
    steps.append({"action": "goto", "url": url})

    # Login if needed
    if include_login:
        steps.append({
            "action": "login",
            "email": "{HEB_EMAIL}",
            "password": "{HEB_PASSWORD}",
        })

    # Add each product
    for product in products:
        steps.append({"action": "search", "value": product})
        steps.append({"action": "add_to_cart", "product": product})

    # Final screenshot
    steps.append({"action": "screenshot"})

    return steps


def _generate_checkout_steps(base_url: str | None) -> list[dict[str, Any]]:
    """Generate safe checkout steps (stops before payment)."""
    steps: list[dict[str, Any]] = []

    url = base_url or "{base_url}"
    steps.append({"action": "goto", "url": url})
    steps.append({
        "action": "click",
        "selector": "[data-test='cart'], .cart-icon, #cart, [aria-label*='cart']",
        "intent": "open_cart",
    })
    steps.append({
        "action": "click",
        "selector": "[data-test='checkout'], .checkout-btn, #checkout, [aria-label*='checkout']",
        "intent": "start_checkout",
    })
    steps.append({"action": "screenshot"})

    return steps


def _generate_login_steps(base_url: str | None, app_hint: str | None) -> list[dict[str, Any]]:
    """Generate login steps with env placeholders."""
    steps: list[dict[str, Any]] = []

    is_heb = _is_heb_context("", app_hint) or (base_url and "heb" in base_url.lower())

    url = base_url or (HEB_BASE_URL if is_heb else "{base_url}")
    email_var = "{HEB_EMAIL}" if is_heb else "{EMAIL}"
    pass_var = "{HEB_PASSWORD}" if is_heb else "{PASSWORD}"

    steps.append({"action": "goto", "url": url})
    steps.append({"action": "login", "email": email_var, "password": pass_var})
    steps.append({"action": "screenshot"})

    return steps


def _generate_search_steps(
    query: str,
    base_url: str | None,
    include_login: bool = False,
    app_hint: str | None = None,
) -> list[dict[str, Any]]:
    """Generate search steps."""
    steps: list[dict[str, Any]] = []

    is_heb = _is_heb_context("", app_hint) or (base_url and "heb" in base_url.lower())
    url = base_url or (HEB_BASE_URL if is_heb else "{base_url}")

    steps.append({"action": "goto", "url": url})

    if include_login:
        email_var = "{HEB_EMAIL}" if is_heb else "{EMAIL}"
        pass_var = "{HEB_PASSWORD}" if is_heb else "{PASSWORD}"
        steps.append({"action": "login", "email": email_var, "password": pass_var})

    steps.append({"action": "search", "value": query})
    steps.append({"action": "screenshot"})

    return steps


# =============================================================================
# MAIN FUNCTION
# =============================================================================

def plan_from_text(
    text: str,
    base_url: str | None = None,
    app_hint: str | None = None,
    max_steps: int = 25,
) -> dict[str, Any]:
    """
    Convert natural language text to a test plan.
    """
    errors: list[str] = []
    assumptions: list[str] = []
    risk_flags: list[str] = []
    requires_confirmation = False

    # Validate input
    if not text or not isinstance(text, str):
        return {
            "ok": False,
            "language": "en",
            "intent": "unknown",
            "base_url": base_url,
            "requires_confirmation": False,
            "summary": "",
            "steps": [],
            "risk_flags": [],
            "assumptions": [],
            "errors": ["Input text is required"],
        }

    text = text.strip()
    if not text:
        return {
            "ok": False,
            "language": "en",
            "intent": "unknown",
            "base_url": base_url,
            "requires_confirmation": False,
            "summary": "",
            "steps": [],
            "risk_flags": [],
            "assumptions": [],
            "errors": ["Input text is empty"],
        }

    # Detect language and intent
    language = _detect_language(text)
    intent = _infer_intent(text)
    is_heb = _is_heb_context(text, app_hint)

    # Check for payment risk
    if _has_payment_intent(text):
        risk_flags.append("payment")
        requires_confirmation = True
        assumptions.append("Will stop before actual payment" if language == "en" else "Se detendrá antes del pago real")

    # Determine effective base_url
    effective_url = base_url
    if not effective_url and is_heb:
        effective_url = HEB_BASE_URL
        assumptions.append("Using HEB Mexico site" if language == "en" else "Usando sitio HEB México")

    # Generate steps based on intent
    steps: list[dict[str, Any]] = []
    summary = ""

    if intent == "checkout":
        steps = _generate_checkout_steps(effective_url)
        summary = "Checkout flow (stopped before payment)" if language == "en" else "Flujo de checkout (detenido antes del pago)"

        # ✅ FIX: check effective_url, not base_url
        if not effective_url:
            assumptions.append("Requires base_url to be provided" if language == "en" else "Requiere que se proporcione base_url")

    elif intent in ("add_to_cart", "login_and_cart"):
        products = _extract_products(text)
        if not products and is_heb:
            products = HEB_DEFAULT_PRODUCTS.copy()
            assumptions.append(
                f"Using default products: {', '.join(products)}" if language == "en"
                else f"Usando productos por defecto: {', '.join(products)}"
            )

        include_login = intent == "login_and_cart" or _has_login_intent(text)
        steps = _generate_heb_cart_steps(products, include_login=include_login, base_url=effective_url)

        products_str = ", ".join(products) if products else "products"
        summary = (f"Agregar productos al carrito: {products_str}" if language == "es" else f"Add to cart: {products_str}")

        if include_login:
            assumptions.append("Using env placeholders for credentials" if language == "en" else "Usando placeholders de env para credenciales")

        assumptions.append("First search result will be added" if language == "en" else "Se agregará el primer resultado de búsqueda")

    elif intent in ("login", "login_and_search"):
        include_search = intent == "login_and_search"
        if include_search:
            products = _extract_products(text)
            query = products[0] if products else "product"
            steps = _generate_search_steps(query, effective_url, include_login=True, app_hint=app_hint)
            summary = (f"Login and search for: {query}" if language == "en" else f"Iniciar sesión y buscar: {query}")
        else:
            steps = _generate_login_steps(effective_url, app_hint)
            summary = ("Login flow" if language == "en" else "Flujo de inicio de sesión")

        assumptions.append("Using env placeholders for credentials" if language == "en" else "Usando placeholders de env para credenciales")

    elif intent == "search":
        products = _extract_products(text)
        query = products[0] if products else "product"
        steps = _generate_search_steps(query, effective_url, app_hint=app_hint)
        summary = (f"Search for: {query}" if language == "en" else f"Buscar: {query}")

    else:
        # Generic navigate
        url = effective_url or "{base_url}"
        steps = [
            {"action": "goto", "url": url},
            {"action": "screenshot"},
        ]
        summary = (f"Navigate to {url}" if language == "en" else f"Navegar a {url}")
        if not effective_url:
            assumptions.append("Requires base_url to be provided" if language == "en" else "Requiere que se proporcione base_url")

    # Validate step count
    if len(steps) > max_steps:
        errors.append(f"Generated {len(steps)} steps, exceeds max_steps={max_steps}")
        steps = steps[:max_steps]

    # ✅ Final validation (validate full plan incl. base_url)
    plan_candidate: dict[str, Any] = {
        "ok": True,
        "steps": steps,
        "risk_flags": risk_flags,
        "requires_confirmation": requires_confirmation,
        "base_url": effective_url,
    }
    is_valid, validation_errors = validate_plan(plan_candidate, max_steps=max_steps)
    if not is_valid:
        errors.extend(validation_errors)

    return {
        "ok": len(errors) == 0,
        "language": language,
        "intent": intent,
        "base_url": effective_url,
        "requires_confirmation": requires_confirmation,
        "summary": summary,
        "steps": steps,
        "risk_flags": risk_flags,
        "assumptions": assumptions,
        "errors": errors,
    }
