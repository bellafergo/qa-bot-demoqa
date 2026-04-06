# core/step_compiler.py
"""
Compilador planner → runner steps + deducción desde prompt.

Dos flujos:
- compile_to_runner_steps: planner DSL → runner steps (login, search, etc.)
- compile_steps_from_prompt: prompt natural → steps (parseo determinístico para chat/execute)

Si una acción no puede compilarse de forma segura, levanta CompileError.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional
from urllib.parse import urljoin

from core.nl_selector_inference import (
    extract_click_clauses_from_prompt,
    extract_fill_clauses_from_prompt,
    make_min_target,
)
from core.schemas import RUNNER_ACTIONS
from core.semantic_step_builder import build_semantic_target
from core.semantic_intent_extractor import (
    extract_intent as _extract_semantic_intent,
    extract_action_intent as _extract_semantic_action,
)


class CompileError(Exception):
    """Error al compilar un step del planner a runner steps."""

    def __init__(
        self,
        message: str,
        step_index: int = -1,
        action: Optional[str] = None,
        error_type: str = "compile_error",
        details: Optional[Dict[str, Any]] = None,
    ):
        self.step_index = step_index
        self.action = action
        self.error_type = error_type
        self.details = details or {}
        super().__init__(message)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "reason": self.error_type,
            "message": str(self),
            "step_index": self.step_index,
            "action": self.action,
            "details": self.details,
        }


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
        error_type="unsupported_action",
        details={"supported_site": "saucedemo.com", "got_base_url": base_url},
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
        raise CompileError(
            "search requires 'value' or 'query'",
            step_index=step_index,
            action="search",
            error_type="missing_required_field",
            details={"required_fields": ["value", "query"]},
        )

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
            error_type="unsupported_action",
            details={"supported_base_url_contains": ["heb"], "got_base_url": base_url},
        )
    product = step.get("product") or step.get("value") or ""
    if not str(product).strip():
        raise CompileError(
            "add_to_cart requires 'product' or 'value'",
            step_index=step_index,
            action="add_to_cart",
            error_type="missing_required_field",
            details={"required_fields": ["product", "value"]},
        )
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
        raise CompileError(
            "wait_for_text requires 'text' or 'value'",
            step_index=step_index,
            action="wait_for_text",
            error_type="missing_required_field",
            details={"required_fields": ["text", "value"]},
        )
    return [{"action": "assert_text_contains", "selector": "body", "text": str(text).strip()}]


def _compile_assert_text(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
) -> List[Dict[str, Any]]:
    """assert_text → assert_text_contains."""
    text = step.get("text") or step.get("value") or step.get("expected") or ""
    if not str(text).strip():
        raise CompileError(
            "assert_text requires 'text', 'value' or 'expected'",
            step_index=step_index,
            action="assert_text",
            error_type="missing_required_field",
            details={"required_fields": ["text", "value", "expected"]},
        )
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
        error_type="unsupported_action",
        details={"supported": False},
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
            raise CompileError(
                f"Step at index {i} is not a dict (got {type(step).__name__})",
                step_index=i,
                error_type="malformed_step",
                details={"got_type": type(step).__name__},
            )

        action = str(step.get("action") or "").strip().lower()
        if not action:
            raise CompileError(
                "Step has no 'action'",
                step_index=i,
                error_type="missing_required_field",
                details={"required_fields": ["action"]},
            )

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
                raise CompileError(
                    f"No compiler for DSL action '{action}'",
                    step_index=i,
                    action=action,
                    error_type="unsupported_action",
                    details={"dsl_action": action},
                )
            continue

        raise CompileError(
            f"Unknown action '{action}'. "
            f"Valid runner actions: {len(RUNNER_ACTIONS)}. "
            f"Valid DSL actions: {len(_DSL_ACTIONS)}. "
            f"To unblock, either compile DSL actions or use runner actions directly.",
            step_index=i,
            action=action,
            error_type="invalid_action",
            details={"action": action},
        )

    return result


# ── Deducción desde prompt natural (chat/execute) ─────────────────────────────

def _strip_quotes(s: str) -> str:
    ss = (s or "").strip()
    if len(ss) >= 2 and ((ss[0] == ss[-1] == '"') or (ss[0] == ss[-1] == "'")):
        return ss[1:-1]
    return ss


def ensure_has_assert(steps: List[Dict[str, Any]], base_url: str) -> List[Dict[str, Any]]:
    """Añade assert si el flujo no tiene ninguno."""
    if not steps:
        return steps
    if any(str(s.get("action", "")).startswith("assert_") for s in steps):
        return steps

    did_fill_user = any(
        s.get("action") == "fill"
        and str(s.get("selector") or "") in ("#user-name", "input[name='user-name']")
        for s in steps
    )
    did_fill_pass = any(
        s.get("action") == "fill"
        and str(s.get("selector") or "") in ("#password", "input[name='password']")
        for s in steps
    )
    did_click_login = any(
        s.get("action") == "click"
        and str(s.get("selector") or "") in ("#login-button", "button[type='submit']")
        for s in steps
    )
    looks_like_login_flow = (did_fill_user or did_fill_pass) and did_click_login

    if _is_saucedemo(base_url):
        if looks_like_login_flow:
            steps.append({"action": "wait_ms", "ms": 450})
            steps.append({"action": "assert_not_visible", "selector": "[data-test='error']"})
            steps.append({"action": "assert_url_contains", "value": "inventory.html"})
            return steps
        steps.append({"action": "assert_visible", "selector": "#user-name"})
        return steps

    steps.append({"action": "assert_visible", "selector": "body"})
    return steps


def _parse_steps_from_prompt(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    """Parseo determinístico de prompt natural a steps. Usado por chat/execute."""
    p = (prompt or "").strip()
    low = p.lower()
    if not p:
        return None

    def _extract_selectors_anywhere(text: str) -> List[str]:
        out: List[str] = []
        for _, sel in re.findall(r'(["\']?)(#[-\w]+|\.[-\w]+|\[[^\]]+\])\1', text):
            if sel and sel not in out:
                out.append(sel)
        for m in re.finditer(r'(["\'])([a-zA-Z][a-zA-Z0-9_-]*)\1', text):
            sel = m.group(2)
            if sel and sel not in out:
                out.append(sel)
        m = re.search(
            r'(?:selector|elemento|element|tag)\s+([a-zA-Z][a-zA-Z0-9_-]*)',
            text, flags=re.IGNORECASE,
        )
        if m:
            sel = m.group(1)
            if sel and sel not in out:
                out.append(sel)
        return out

    _only_visibility_check = (
        ("visible" in low or "visibles" in low)
        and not any(k in low for k in ["inicia", "iniciar", "fill", "llena", "escribe", "ingresa", "teclea"])
    )
    if _is_saucedemo(base_url) and not _only_visibility_check and any(
        k in low for k in [
            "login", "inicia sesión", "iniciar sesion", "usuario", "username",
            "password", "contraseña", "contrasena",
        ]
    ):
        negative = any(
            k in low for k in [
                "no existe", "invalido", "inválido", "incorrecto",
                "debe fallar", "should fail", "invalid", "wrong",
            ]
        )
        u = None
        pw = None
        m_u = re.search(r'(?:username|usuario)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)', p, flags=re.IGNORECASE)
        if m_u:
            u = _strip_quotes(m_u.group(1))
        m_p = re.search(
            r'(?:password|pass|contraseñ?a)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)', p, flags=re.IGNORECASE
        )
        if m_p:
            pw = _strip_quotes(m_p.group(1))

        steps: List[Dict[str, Any]] = [
            {"action": "goto", "url": base_url},
            {"action": "wait_ms", "ms": 250},
            {"action": "assert_visible", "selector": "#user-name"},
            {"action": "assert_visible", "selector": "#password"},
            {"action": "assert_visible", "selector": "#login-button"},
        ]
        if u is not None:
            steps.append({
                "action": "fill",
                "selector": "#user-name",
                "target": build_semantic_target("input", "username", base_url),
                "value": u,
            })
        if pw is not None:
            steps.append({
                "action": "fill",
                "selector": "#password",
                "target": build_semantic_target("input", "password", base_url),
                "value": pw,
            })
        if (u is not None) or (pw is not None) or any(
            k in low for k in ["haz click", "da click", "click", "submit", "entrar", "login"]
        ):
            steps.append({
                "action": "click",
                "selector": "#login-button",
                "target": build_semantic_target("button", "login", base_url),
            })
            steps.append({"action": "wait_ms", "ms": 450})
            if negative:
                for s in steps:
                    if s.get("action"):
                        s["expected"] = "fail"
                        break
                steps.append({"action": "assert_visible", "selector": "[data-test='error']"})
            else:
                steps.append({"action": "assert_not_visible", "selector": "[data-test='error']"})
                steps.append({"action": "assert_url_contains", "value": "inventory.html"})
        return steps

    steps = [
        {"action": "goto", "url": base_url},
        {"action": "wait_ms", "ms": 250},
    ]

    _path_m = re.search(
        r'(?i)\b(?:ve\s+a|ir\s+a|vaya\s+a|go\s+to|goto|navega\s+a)\s+(/[^,\s\)\]]+)',
        p,
    )
    if _path_m and base_url:
        rel = _path_m.group(1)
        root = base_url.rstrip("/")
        steps[0] = {
            "action": "goto",
            "url": urljoin(root + "/", rel.lstrip("/")),
        }

    _has_form_actions = bool(re.search(r'\b(fill|type|llena|escribe|ingresa|teclea|click)\b', low))
    if ("visibles" in low or "visible" in low) and not _has_form_actions:
        _tv = re.search(
            r'(?:verify|verifica|check|assert)\s+(?:that\s+)?["\']([^"\']+)["\']\s+(?:is\s+)?(?:visible|present|displayed)',
            p, flags=re.IGNORECASE,
        )
        if _tv:
            found_text = _tv.group(1).strip()
            if _is_saucedemo(base_url):
                _login_texts_tv = {"accepted usernames", "password", "login"}
                if not any(t in found_text.lower() for t in _login_texts_tv):
                    _has_login_tv = (
                        any(s.get("action") == "fill" and s.get("selector") == "#user-name" for s in steps)
                        and any(s.get("action") == "fill" and s.get("selector") == "#password" for s in steps)
                        and any(s.get("action") == "click" and s.get("selector") == "#login-button" for s in steps)
                    )
                    if not _has_login_tv:
                        steps.extend([
                            {"action": "fill", "selector": "#user-name", "target": build_semantic_target("input", "username", base_url), "value": "standard_user"},
                            {"action": "fill", "selector": "#password", "target": build_semantic_target("input", "password", base_url), "value": "secret_sauce"},
                            {"action": "click", "selector": "#login-button", "target": build_semantic_target("button", "login", base_url)},
                        ])
            steps.append({"action": "wait_ms", "ms": 300})
            steps.append({"action": "assert_text_contains", "selector": "body", "text": found_text})
            return steps

        _si = _extract_semantic_intent(p)
        if _si:
            _st = build_semantic_target(_si["kind"], _si["name"], base_url)
            steps.append({"action": "assert_visible", "selector": _st["primary"], "target": _st})
            return ensure_has_assert(steps, base_url)

        seen = _extract_selectors_anywhere(p)
        if _is_saucedemo(base_url):
            if "username" in low or "user name" in low or "user-name" in low:
                seen = ["#user-name"]
            elif not seen:
                seen = ["#user-name", "#password", "#login-button"]
        for sel in seen:
            steps.append({"action": "assert_visible", "selector": sel})
        return ensure_has_assert(steps, base_url)

    if not any(c in p for c in '#.[]()"'):
        _ai = _extract_semantic_action(p)
        if _ai:
            _a = _ai["action"]
            if _a == "click":
                _st = build_semantic_target(_ai["target"]["kind"], _ai["target"]["name"], base_url)
                steps.append({"action": "assert_visible", "selector": _st["primary"]})
                steps.append({"action": "click", "selector": _st["primary"], "target": _st})
            elif _a == "fill":
                _st = build_semantic_target(_ai["target"]["kind"], _ai["target"]["name"], base_url)
                steps.append({"action": "fill", "selector": _st["primary"], "target": _st, "value": _ai["value"]})
            elif _a == "assert_text_contains":
                steps.append({"action": "wait_ms", "ms": 300})
                steps.append({"action": "assert_text_contains", "selector": "body", "text": _ai["text"]})
            return ensure_has_assert(steps, base_url)

    _CSS_CHARS = set("#.[]:>+~=\"'()")
    fill_patterns = [
        r'(?:llena|escribe|ingresa|teclea|fill|type)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])\s+(?:con|with)\s+(".*?"|\'.*?\'|\S+)',
        r'(?:llena|escribe|ingresa|teclea|fill|type)\s+([\w][\w\s]*?)\s+(?:con|with)\s+(".*?"|\'.*?\'|\S+)',
    ]
    seen_fill_sels: set = set()
    for pat in fill_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE | re.MULTILINE):
            sel = _strip_quotes(m.group(1)).strip()
            val = _strip_quotes(m.group(2)).strip()
            if sel and val and sel not in seen_fill_sels:
                seen_fill_sels.add(sel)
                if any(c in sel for c in _CSS_CHARS):
                    steps.append({"action": "assert_visible", "selector": sel})
                steps.append({"action": "fill", "selector": sel, "value": val})

    def _fill_value_exists(val: str) -> bool:
        v = (val or "").strip()
        return any(
            str(s.get("action")) == "fill"
            and _strip_quotes(str(s.get("value") or "")).strip() == v
            for s in steps
        )

    for val, sel in extract_fill_clauses_from_prompt(p):
        if _fill_value_exists(val):
            continue
        steps.append({"action": "assert_visible", "selector": sel})
        steps.append(
            {
                "action": "fill",
                "selector": sel,
                "target": make_min_target(sel),
                "value": val,
            },
        )

    # Clicks NL estructurados antes que regex suelta (evita click antes de fills valor-primero)
    _nl_clicks = extract_click_clauses_from_prompt(p)
    if _nl_clicks:
        for csel in _nl_clicks:
            steps.append({"action": "assert_visible", "selector": csel})
            steps.append(
                {"action": "click", "selector": csel, "target": make_min_target(csel)},
            )
    else:
        click_patterns = [
            r'(?:haz\s+click\s+en|haz\s+clic\s+en|da\s+click\s+en|click\s+on|click)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])',
            r'(?:haz\s+click\s+en|haz\s+clic\s+en|da\s+click\s+en|click\s+on|click)\s+([\w][\w\s]*?)(?:\s*$)',
        ]
        seen_click_sels: set = set()
        for pat in click_patterns:
            for m in re.finditer(pat, p, flags=re.IGNORECASE | re.MULTILINE):
                sel = _strip_quotes(m.group(1)).strip()
                if sel and sel not in seen_click_sels:
                    seen_click_sels.add(sel)
                    if any(c in sel for c in _CSS_CHARS):
                        steps.append({"action": "assert_visible", "selector": sel})
                    steps.append({"action": "click", "selector": sel})

    if re.search(r"\b(enter|intro|presiona\s+enter|presiona\s+intro)\b", low):
        last_sel = None
        for s in reversed(steps):
            if s.get("action") == "fill" and s.get("selector"):
                last_sel = s.get("selector")
                break
        if last_sel:
            steps.append({"action": "press", "selector": last_sel, "key": "Enter"})

    text_patterns = [
        r'(?:valida|validar|verify|assert)\s+.*?(?:texto|text).*?(".*?"|\'.*?\')',
        r'(?:assert_text_contains)\s+(".*?"|\'.*?\')',
        r'(?:verify|verifica|check|assert)\s+(?:that\s+)?["\']([^"\']+)["\']\s+(?:is\s+)?(?:visible|present|displayed)',
        r'(?:verify|verifica|check|assert)\s+(?:that\s+)?([\w][^\n"\']*?)\s+is\s+(?:visible|present|displayed)\b',
    ]
    found_text = None
    for pat in text_patterns:
        mm = re.search(pat, p, flags=re.IGNORECASE)
        if mm:
            found_text = _strip_quotes(mm.group(1)).strip()
            if found_text:
                break
    if found_text:
        if _is_saucedemo(base_url):
            _login_texts = {"accepted usernames", "password", "login"}
            _is_login_text = any(t in found_text.lower() for t in _login_texts)
            if not _is_login_text:
                _has_login = (
                    any(s.get("action") == "fill" and s.get("selector") == "#user-name" for s in steps)
                    and any(s.get("action") == "fill" and s.get("selector") == "#password" for s in steps)
                    and any(s.get("action") == "click" and s.get("selector") == "#login-button" for s in steps)
                )
                if not _has_login:
                    steps.extend([
                        {"action": "fill", "selector": "#user-name", "target": build_semantic_target("input", "username", base_url), "value": "standard_user"},
                        {"action": "fill", "selector": "#password", "target": build_semantic_target("input", "password", base_url), "value": "secret_sauce"},
                        {"action": "click", "selector": "#login-button", "target": build_semantic_target("button", "login", base_url)},
                    ])
        steps.append({"action": "wait_ms", "ms": 300})
        steps.append({"action": "assert_text_contains", "selector": "body", "text": found_text})

    if re.search(r"(?:valida|validar|verify|assert).*(?:selector|elemento|element)", low):
        sels = _extract_selectors_anywhere(p)
        for sel in sels[:3]:
            steps.append({"action": "assert_visible", "selector": sel})

    useful = [s for s in steps if s.get("action") not in ("goto", "wait_ms")]
    if not useful:
        return None

    return ensure_has_assert(steps, base_url)


def compile_steps_from_prompt(
    prompt: str,
    base_url: str,
) -> Optional[List[Dict[str, Any]]]:
    """
    Deduce steps desde prompt natural (chat/execute).
    Intenta: login_intent_resolver → parse determinístico.
    Retorna None si no puede deducir steps suficientes.
    """
    from core.login_intent_resolver import build_login_steps
    steps = build_login_steps(base_url=base_url, prompt=prompt)
    if steps:
        return steps
    return _parse_steps_from_prompt(prompt, base_url)


# Alias público para tests que necesitan el parser directo (sin login resolver)
parse_steps_from_prompt = _parse_steps_from_prompt
