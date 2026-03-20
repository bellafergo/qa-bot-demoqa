# core/step_compiler.py
"""
Compilador planner → runner steps.

Traduce pasos de alto nivel del planner (DSL) a pasos ejecutables por
runners/generic_steps.py (RUNNER_ACTIONS).

Si una acción no puede compilarse de forma segura, levanta CompileError.
"""
from __future__ import annotations

from typing import Any, Dict, List, Optional

from core.schemas import RUNNER_ACTIONS


class CompileError(Exception):
    """Error al compilar un step del planner a runner steps."""

    def __init__(self, message: str, step_index: int = -1, action: Optional[str] = None):
        self.step_index = step_index
        self.action = action
        super().__init__(message)


# Acciones que pasan directo (ya son runner steps)
_PASSTHROUGH_ACTIONS: frozenset = RUNNER_ACTIONS

# Acciones DSL que requieren compilación
_DSL_ACTIONS: frozenset = frozenset({
    "login",
    "search",
    "add_to_cart",
    "screenshot",
    "wait_for_text",
    "assert_text",
    "assert_cart_count",
})


def _is_saucedemo(url: Optional[str]) -> bool:
    return url is not None and "saucedemo.com" in url.lower()


def _is_heb(url: Optional[str]) -> bool:
    return url is not None and "heb" in url.lower()


def _resolve_url(url_val: Optional[str], base_url: Optional[str]) -> str:
    """Resuelve {base_url}, {EMAIL}, etc. a valores concretos si base_url existe."""
    s = (url_val or "").strip()
    if not s:
        return base_url or ""
    if "{base_url}" in s and base_url:
        return s.replace("{base_url}", base_url)
    return s


def _compile_login(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """Expande login a fill/click. Solo SauceDemo implementado de forma segura."""
    if _is_saucedemo(base_url):
        email = step.get("email") or step.get("username") or "{EMAIL}"
        password = step.get("password") or "{PASSWORD}"
        return [
            {"action": "fill", "selector": "#user-name", "value": email},
            {"action": "fill", "selector": "#password", "value": password},
            {"action": "click", "selector": "#login-button"},
        ]
    raise CompileError(
        f"login expansion only implemented for SauceDemo (base_url has saucedemo.com). "
        f"Got base_url={base_url!r}",
        step_index=step_index,
        action="login",
    )


def _compile_search(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """
    Expande search a fill + press Enter.
    Usa selectores genéricos; HEB y otros sites pueden requerir extensión futura.
    """
    value = step.get("value") or step.get("query") or ""
    if not str(value).strip():
        raise CompileError("search requires 'value' or 'query'", step_index=step_index, action="search")

    # Selectores genéricos para búsqueda (muchos sitios usan estos)
    search_sel = "input[type='search'], input[name='q'], input[name='search'], input[placeholder*='search' i], input[placeholder*='buscar' i], input[aria-label*='search' i]"
    return [
        {"action": "fill", "selector": search_sel, "value": str(value).strip()},
        {"action": "press", "selector": search_sel, "key": "Enter"},
        {"action": "wait_ms", "ms": 800},
    ]


def _compile_add_to_cart(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """add_to_cart requiere lógica site-specific. Solo HEB con selector best-effort."""
    if not _is_heb(base_url):
        raise CompileError(
            "add_to_cart requires site-specific implementation. "
            "Currently supported: HEB (base_url with 'heb'). Use heb_checkout runner for full HEB flow.",
            step_index=step_index,
            action="add_to_cart",
        )
    product = step.get("product") or step.get("value") or ""
    if not str(product).strip():
        raise CompileError("add_to_cart requires 'product' or 'value'", step_index=step_index, action="add_to_cart")
    # HEB: selector best-effort; para flujo completo usar runners/heb_checkout
    return [
        {"action": "wait_ms", "ms": 600},
        {"action": "click", "selector": "[data-testid*='add-to-cart'], .add-to-cart, button[aria-label*='cart' i], .product-card button:first-of-type"},
        {"action": "wait_ms", "ms": 500},
    ]


def _compile_screenshot(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """screenshot: pausa para que el runner capture al final. No-op explícito."""
    return [{"action": "wait_ms", "ms": 500}]


def _compile_wait_for_text(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """wait_for_text → assert_text_contains en body."""
    text = step.get("text") or step.get("value") or ""
    if not str(text).strip():
        raise CompileError("wait_for_text requires 'text' or 'value'", step_index=step_index, action="wait_for_text")
    return [{"action": "assert_text_contains", "selector": "body", "text": str(text).strip()}]


def _compile_assert_text(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """assert_text → assert_text_contains."""
    text = step.get("text") or step.get("value") or step.get("expected") or ""
    if not str(text).strip():
        raise CompileError("assert_text requires 'text', 'value' or 'expected'", step_index=step_index, action="assert_text")
    sel = step.get("selector") or "body"
    return [{"action": "assert_text_contains", "selector": sel, "text": str(text).strip()}]


def _compile_assert_cart_count(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """assert_cart_count: site-specific, no implementado de forma genérica."""
    raise CompileError(
        "assert_cart_count requires site-specific implementation (DOM structure). Not yet supported.",
        step_index=step_index,
        action="assert_cart_count",
    )


def compile_to_runner_steps(
    plan_steps: List[Dict[str, Any]],
    base_url: Optional[str] = None,
    context: Optional[Dict[str, Any]] = None,
) -> List[Dict[str, Any]]:
    """
    Compila pasos del planner (DSL alto) a pasos ejecutables por el runner.

    Args:
        plan_steps: Lista de steps del plan (nl_test_planner).
        base_url: URL base para resolver {base_url} y decisiones site-specific.
        context: Opcional; puede incluir app_hint, etc.

    Returns:
        Lista de steps compatibles con RUNNER_ACTIONS (validables por validate_steps).

    Raises:
        CompileError: Si alguna acción no puede compilarse.
    """
    if not plan_steps:
        return []

    context = context or {}
    resolved_base = base_url or context.get("base_url", "")

    result: List[Dict[str, Any]] = []

    for i, step in enumerate(plan_steps):
        if not isinstance(step, dict):
            raise CompileError(f"Step at index {i} is not a dict", step_index=i)

        action = str(step.get("action") or "").strip().lower()
        if not action:
            raise CompileError("Step has no 'action'", step_index=i)

        # Resolver {base_url} en goto
        if action == "goto":
            url = _resolve_url(step.get("url") or step.get("path"), resolved_base)
            result.append({"action": "goto", "url": url or resolved_base})
            continue

        if action in _PASSTHROUGH_ACTIONS:
            result.append(dict(step))
            continue

        if action in _DSL_ACTIONS:
            compilers = {
                "login": _compile_login,
                "search": _compile_search,
                "add_to_cart": _compile_add_to_cart,
                "screenshot": _compile_screenshot,
                "wait_for_text": _compile_wait_for_text,
                "assert_text": _compile_assert_text,
                "assert_cart_count": _compile_assert_cart_count,
            }
            fn = compilers.get(action)
            if fn:
                compiled = fn(step, i, resolved_base or None)
                result.extend(compiled)
            else:
                raise CompileError(f"No compiler for DSL action '{action}'", step_index=i, action=action)
            continue

        raise CompileError(
            f"Unknown action '{action}'. "
            f"Runner actions: {sorted(RUNNER_ACTIONS)}. "
            f"DSL actions (require compilation): {sorted(_DSL_ACTIONS)}.",
            step_index=i,
            action=action,
        )

    return result
