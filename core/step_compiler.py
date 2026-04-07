# core/step_compiler.py
"""
Compilador planner → runner steps + deducción desde prompt.

Dos flujos:
- compile_to_runner_steps: planner DSL → runner steps (login, search, etc.)
- compile_steps_from_prompt: prompt natural → steps (parseo determinístico para chat/execute)

Si una acción no puede compilarse de forma segura, levanta CompileError.
"""
from __future__ import annotations

import os
import re
from typing import Any, Dict, List, Optional, Tuple
from urllib.parse import urljoin

from core.generic_login_targets import (
    make_generic_login_password_target,
    make_generic_login_submit_target,
    make_generic_login_user_target,
)
from core.nl_selector_inference import (
    extract_click_clauses_from_prompt,
    extract_fill_clauses_from_prompt,
    make_min_target,
)
from core.schemas import RUNNER_ACTIONS
from core.site_profiles import resolve_site_profile
from core.semantic_step_builder import build_semantic_target
from core.semantic_intent_extractor import (
    extract_intent as _extract_semantic_intent,
    extract_action_intent as _extract_semantic_action,
)
from core.target_url_validation import TargetURLNotAllowed, validate_target_url


_PLACEHOLDER_TOKEN = re.compile(r"\$\{(\w+)\}|\{(\w+)\}")


def _normalize_ws(s: str) -> str:
    return " ".join((s or "").split())


def resolve_interpolated_credentials(
    raw: Any,
    context: Optional[Dict[str, Any]],
    *,
    purpose: str = "value",
) -> str:
    """
    Sustituye {VAR} y ${VAR} usando context['credentials'] y variables de entorno
    VANYA_TEST_<VAR> / VANYA_<VAR>. Valores sin placeholders se devuelven tal cual.
    """
    if raw is None:
        return ""
    s = str(raw).strip()
    if not s:
        return ""
    if "{" not in s and "${" not in s:
        return s

    ctx = context or {}
    creds: Dict[str, Any] = {}
    c = ctx.get("credentials")
    if isinstance(c, dict):
        creds = c

    def _lookup(var: str) -> Optional[str]:
        vn = (var or "").strip()
        if not vn:
            return None
        key_l = vn.lower().replace("-", "_")
        if key_l in creds and creds[key_l] is not None:
            vs = str(creds[key_l]).strip()
            if vs:
                return vs
        env_key = vn.upper().replace("-", "_")
        for prefix in ("VANYA_TEST_", "VANYA_"):
            v = os.getenv(prefix + env_key)
            if v and str(v).strip():
                return str(v).strip()
        return None

    def _repl(m: re.Match[str]) -> str:
        var = (m.group(1) or m.group(2) or "").strip()
        val = _lookup(var)
        if val is None:
            raise CompileError(
                f"Unresolved variable: {var}",
                error_type="unresolved_variable",
                details={"variable": var, "purpose": purpose},
            )
        return val

    return _PLACEHOLDER_TOKEN.sub(_repl, s)


def resolve_fill_values_in_steps(
    steps: List[Dict[str, Any]],
    context: Optional[Dict[str, Any]],
) -> List[Dict[str, Any]]:
    """Resuelve placeholders en todos los fill; añade _value_resolution para evidencia."""
    out: List[Dict[str, Any]] = []
    for s in steps:
        s2 = dict(s)
        act = str(s2.get("action") or "").strip().lower()
        if act == "fill":
            raw = s2.get("value")
            raw_s = "" if raw is None else str(raw)
            if "{" in raw_s or "${" in raw_s:
                names = [
                    (m.group(1) or m.group(2) or "").strip().upper()
                    for m in _PLACEHOLDER_TOKEN.finditer(raw_s)
                ]
                s2["value"] = resolve_interpolated_credentials(raw_s, context, purpose="fill_value")
                s2["_value_resolution"] = {"kind": "interpolated", "variables": names}
            elif "_value_resolution" not in s2:
                s2["_value_resolution"] = {"kind": "literal"}
        out.append(s2)
    return out


def _extract_verification_needles_from_prompt(prompt: str) -> List[str]:
    """Textos que el usuario pide verificar explícitamente (es/en), sin duplicar ruido."""
    p = prompt or ""
    needles: List[str] = []
    seen: set = set()

    def _push(t: str) -> None:
        x = (t or "").strip().strip("'\"").strip()
        x = re.sub(r"^(el\s+texto|the\s+text|texto)\s+", "", x, flags=re.I).strip()
        if len(x) < 2:
            return
        k = _normalize_ws(x).lower()
        if k in seen:
            return
        seen.add(k)
        needles.append(x)

    patterns: List[Tuple[str, int]] = [
        (r"(?is)verifica(?:r)?\s+que\s+aparece\s+el\s+texto\s+(.+?)(?=\s*$|\s*[,.;]|\s+luego\b)", 1),
        (r"(?is)verifica(?:r)?\s+que\s+aparece\s+(.+?)(?=\s*$|\s*[,.;]|\s+luego\b)", 1),
        (r"(?is)verifica(?:r)?\s+que\s+(?:se\s+muestra|existe)\s+[\"']([^\"']+)[\"']", 1),
        (r"(?is)comprueba\s+que\s+(?:aparece|se\s+muestra)\s+[\"']([^\"']+)[\"']", 1),
        (r"(?is)asegúrate\s+de\s+que\s+aparece\s+[\"']([^\"']+)[\"']", 1),
        (r"(?is)asegurate\s+de\s+que\s+aparece\s+[\"']([^\"']+)[\"']", 1),
        (r"(?is)\bdebe\s+aparecer\s+[\"']([^\"']+)[\"']", 1),
        (r"(?is)\bdebe\s+aparecer\s+(.+?)(?=\s*$|\s*[,.;])", 1),
        (r"(?is)\bverify\s+that\s+(?:the\s+text\s+)?[\"']([^\"']+)[\"']\s+(?:appears|is\s+visible)\b", 1),
        (r"(?is)\bensure\s+(?:that\s+)?[\"']([^\"']+)[\"']\s+(?:appears|is\s+visible)\b", 1),
    ]
    for pat, _gi in patterns:
        for m in re.finditer(pat, p):
            _push(m.group(1))

    legacy = [
        r'(?:valida|validar|verify|assert)\s+.*?(?:texto|text).*?(".*?"|\'.*?\')',
        r"(?:assert_text_contains)\s+(\".*?\"|'.*?')",
    ]
    for pat in legacy:
        mm = re.search(pat, p, flags=re.IGNORECASE)
        if mm:
            _push(_strip_quotes(mm.group(1)))

    return needles


def augment_steps_with_prompt_assertions(
    prompt: str,
    steps: List[Dict[str, Any]],
    base_url: str = "",
) -> List[Dict[str, Any]]:
    """
    Añade assert_text_contains cuando el prompt pide verificar texto visible.
    Idempotente respecto a asserts ya presentes (mismo texto normalizado).
    """
    if not steps:
        return steps
    needles = _extract_verification_needles_from_prompt(prompt)
    if not needles:
        return steps

    existing: set = set()
    for s in steps:
        if str(s.get("action") or "") != "assert_text_contains":
            continue
        for k in ("text", "expected"):
            v = s.get(k)
            if v:
                existing.add(_normalize_ws(str(v)).lower())

    out = list(steps)
    for needle in needles:
        nk = _normalize_ws(needle).lower()
        if nk in existing:
            continue
        out.append({"action": "wait_ms", "ms": 450})
        out.append(
            {
                "action": "assert_text_contains",
                "selector": "body",
                "text": needle.strip(),
                "_compiler_meta": {"from_prompt_verification": True},
            },
        )
        existing.add(nk)
    return out


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


def _credential_resolution_meta(raw: Any) -> Optional[Dict[str, Any]]:
    rs = str(raw or "")
    if "{" not in rs and "${" not in rs:
        return None
    names = [
        (m.group(1) or m.group(2) or "").strip().upper()
        for m in _PLACEHOLDER_TOKEN.finditer(rs)
    ]
    return {"kind": "interpolated", "variables": names}


def _compile_login(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
    context: Optional[Dict[str, Any]] = None,
) -> List[Dict[str, Any]]:
    """
    Expande DSL `login` a fill/fill/click + espera breve (navegación / pintado).
    Credenciales: literales del plan o placeholders {EMAIL}/{PASSWORD} resueltos
    vía context['credentials'] o VANYA_TEST_*; sin resolución → CompileError.
    """
    raw_email = step.get("email") or step.get("username")
    raw_pw = step.get("password")
    if raw_email is None or (isinstance(raw_email, str) and not str(raw_email).strip()):
        raw_email = "{EMAIL}"
    if raw_pw is None or (isinstance(raw_pw, str) and not str(raw_pw).strip()):
        raw_pw = "{PASSWORD}"
    email = resolve_interpolated_credentials(raw_email, context, purpose="login_email")
    password = resolve_interpolated_credentials(raw_pw, context, purpose="login_password")
    ut = make_generic_login_user_target()
    pt = make_generic_login_password_target()
    st = make_generic_login_submit_target()
    meta = {"dsl_expanded": "login", "reliable_validation": False}
    fe = _credential_resolution_meta(raw_email)
    fp = _credential_resolution_meta(raw_pw)
    return [
        {
            "action": "fill",
            "selector": ut["primary"],
            "target": ut,
            "value": email,
            "_compiler_meta": meta,
            **({"_value_resolution": fe} if fe else {}),
        },
        {
            "action": "fill",
            "selector": pt["primary"],
            "target": pt,
            "value": password,
            "_compiler_meta": meta,
            **({"_value_resolution": fp} if fp else {}),
        },
        {"action": "click", "selector": st["primary"], "target": st, "_compiler_meta": meta},
        {"action": "wait_ms", "ms": 650, "_compiler_meta": meta},
    ]


def _compile_search(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
    context: Optional[Dict[str, Any]] = None,
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
    context: Optional[Dict[str, Any]] = None,
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
    context: Optional[Dict[str, Any]] = None,
) -> List[Dict[str, Any]]:
    """screenshot: pausa para que el runner capture al final. No-op explícito."""
    return [{"action": "wait_ms", "ms": 500}]


def _compile_wait_for_text(
    step: Dict[str, Any],
    step_index: int,
    base_url: Optional[str],
    context: Optional[Dict[str, Any]] = None,
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
    context: Optional[Dict[str, Any]] = None,
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
    context: Optional[Dict[str, Any]] = None,
) -> List[Dict[str, Any]]:
    """assert_cart_count: site-specific, no implementado de forma genérica."""
    raise CompileError(
        "assert_cart_count requires site-specific implementation (DOM structure). Not yet supported.",
        step_index=step_index,
        action="assert_cart_count",
        error_type="unsupported_action",
        details={"supported": False},
    )


def _selector_blob_from_step(s: Dict[str, Any]) -> str:
    parts = [str(s.get("selector") or "")]
    t = s.get("target")
    if isinstance(t, dict):
        parts.append(str(t.get("primary") or ""))
    return " ".join(parts).lower()


def _plan_has_explicit_login_ui_steps(plan_steps: List[Dict[str, Any]]) -> bool:
    """
    True si el plan ya trae un flujo login manual (fills + password + click).
    En ese caso se omiten pasos DSL `login` para no duplicar.
    """
    fills = 0
    has_pw = False
    has_userish = False
    clicks = 0
    for s in plan_steps:
        if not isinstance(s, dict):
            continue
        a = str(s.get("action") or "").strip().lower()
        if a == "fill":
            fills += 1
            b = _selector_blob_from_step(s)
            if "password" in b or 'type="password"' in b:
                has_pw = True
            if any(
                x in b
                for x in (
                    "email",
                    "user-name",
                    "username",
                    "user_name",
                    'type="email"',
                    'type="text"',
                    "[name=\"email\"]",
                    "autocomplete=\"username\"",
                )
            ):
                has_userish = True
        elif a == "click":
            clicks += 1
    return fills >= 2 and has_pw and has_userish and clicks >= 1


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

    working = list(plan_steps)
    if _plan_has_explicit_login_ui_steps(working):
        working = [
            s for s in working if str(s.get("action") or "").strip().lower() != "login"
        ]

    result: List[Dict[str, Any]] = []

    for i, step in enumerate(working):
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
            final_u = url or resolved_base
            if (final_u or "").strip():
                try:
                    final_u = validate_target_url(final_u, base_url=resolved_base or None)
                except TargetURLNotAllowed as e:
                    raise CompileError(
                        str(e),
                        step_index=i,
                        action="goto",
                        error_type="target_url_not_allowed",
                    ) from e
            result.append({"action": "goto", "url": final_u})
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
                compiled = fn(step, i, resolved_base or None, context)
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

    return resolve_fill_values_in_steps(result, context)


# ── Deducción desde prompt natural (chat/execute) ─────────────────────────────

def _strip_quotes(s: str) -> str:
    ss = (s or "").strip()
    if len(ss) >= 2 and ((ss[0] == ss[-1] == '"') or (ss[0] == ss[-1] == "'")):
        return ss[1:-1]
    return ss


def _semantic_selector_variants(base_url: str, kind: str, name: str) -> frozenset:
    """Primary + css fallbacks from semantic target (profile-aware)."""
    t = build_semantic_target(kind, name, base_url)
    sels: set = set()
    p = t.get("primary")
    if p:
        sels.add(str(p))
    for fb in t.get("fallbacks") or []:
        if isinstance(fb, dict) and fb.get("type") == "css" and fb.get("value"):
            sels.add(str(fb["value"]))
    return frozenset(sels)


def _looks_like_login_flow(steps: List[Dict[str, Any]], base_url: str) -> bool:
    uv = _semantic_selector_variants(base_url, "input", "username")
    pv = _semantic_selector_variants(base_url, "input", "password")
    bv = _semantic_selector_variants(base_url, "button", "login")
    did_u = any(s.get("action") == "fill" and str(s.get("selector") or "") in uv for s in steps)
    did_p = any(s.get("action") == "fill" and str(s.get("selector") or "") in pv for s in steps)
    did_c = any(s.get("action") == "click" and str(s.get("selector") or "") in bv for s in steps)
    return (did_u or did_p) and did_c


def ensure_has_assert(steps: List[Dict[str, Any]], base_url: str) -> List[Dict[str, Any]]:
    """Añade assert si el flujo no tiene ninguno."""
    if not steps:
        return steps
    if any(str(s.get("action", "")).startswith("assert_") for s in steps):
        return steps

    prof = resolve_site_profile(base_url)
    login_flow = _looks_like_login_flow(steps, base_url)

    if prof and prof.enable_nl_login_parse and prof.login_error_selector and prof.post_login_url_contains:
        if login_flow:
            steps.append({"action": "wait_ms", "ms": 450})
            steps.append({"action": "assert_not_visible", "selector": prof.login_error_selector})
            steps.append({"action": "assert_url_contains", "value": prof.post_login_url_contains})
            return steps
        user_pri = build_semantic_target("input", "username", base_url)["primary"]
        steps.append({"action": "assert_visible", "selector": user_pri})
        return steps

    steps.append(
        {
            "action": "assert_visible",
            "selector": "body",
            "_compiler_meta": {
                "terminal_assert": "weak_body_fallback",
                "reliable_validation": False,
            },
        },
    )
    return steps


def _parse_steps_from_prompt(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    """Parseo determinístico de prompt natural a steps. Usado por chat/execute."""
    p = (prompt or "").strip()
    low = p.lower()
    prof = resolve_site_profile(base_url)
    if not p:
        return None

    _TLD_LIKE_CLASSES = frozenset({".com", ".org", ".net", ".mx", ".edu", ".io", ".co", ".app", ".gov"})

    def _extract_selectors_anywhere(text: str) -> List[str]:
        out: List[str] = []

        def _keep(sel: str) -> bool:
            if not sel:
                return False
            s = sel.lower()
            if sel.startswith(".") and s in _TLD_LIKE_CLASSES:
                return False  # avoid ".com" from URLs like demoqa.com
            return True

        for _, sel in re.findall(r'(["\']?)(#[-\w]+|\.[-\w]+|\[[^\]]+\])\1', text):
            if _keep(sel) and sel not in out:
                out.append(sel)
        # Unquoted #id / #userName (common in NL prompts; quoted regex above misses them)
        for m in re.finditer(r"#[-\w]+", text):
            sel = m.group(0)
            if _keep(sel) and sel not in out:
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
    if prof and prof.enable_nl_login_parse and not _only_visibility_check and any(
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

        u_sel = build_semantic_target("input", "username", base_url)["primary"]
        p_sel = build_semantic_target("input", "password", base_url)["primary"]
        b_sel = build_semantic_target("button", "login", base_url)["primary"]
        err_sel = prof.login_error_selector or "[data-test='error']"
        success_path = prof.post_login_url_contains or ""

        steps: List[Dict[str, Any]] = [
            {"action": "goto", "url": base_url},
            {"action": "wait_ms", "ms": 250},
            {"action": "assert_visible", "selector": u_sel},
            {"action": "assert_visible", "selector": p_sel},
            {"action": "assert_visible", "selector": b_sel},
        ]
        if u is not None:
            steps.append({
                "action": "fill",
                "selector": u_sel,
                "target": build_semantic_target("input", "username", base_url),
                "value": u,
            })
        if pw is not None:
            steps.append({
                "action": "fill",
                "selector": p_sel,
                "target": build_semantic_target("input", "password", base_url),
                "value": pw,
            })
        if (u is not None) or (pw is not None) or any(
            k in low for k in ["haz click", "da click", "click", "submit", "entrar", "login"]
        ):
            steps.append({
                "action": "click",
                "selector": b_sel,
                "target": build_semantic_target("button", "login", base_url),
            })
            steps.append({"action": "wait_ms", "ms": 450})
            if negative:
                for s in steps:
                    if s.get("action"):
                        s["expected"] = "fail"
                        break
                steps.append({"action": "assert_visible", "selector": err_sel})
            else:
                steps.append({"action": "assert_not_visible", "selector": err_sel})
                if success_path:
                    steps.append({"action": "assert_url_contains", "value": success_path})
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
            if prof and prof.inject_demo_credentials:
                _login_texts_tv = {"accepted usernames", "password", "login"}
                if not any(t in found_text.lower() for t in _login_texts_tv):
                    uv = _semantic_selector_variants(base_url, "input", "username")
                    pv = _semantic_selector_variants(base_url, "input", "password")
                    bv = _semantic_selector_variants(base_url, "button", "login")
                    u_pri = build_semantic_target("input", "username", base_url)["primary"]
                    p_pri = build_semantic_target("input", "password", base_url)["primary"]
                    b_pri = build_semantic_target("button", "login", base_url)["primary"]
                    _has_login_tv = (
                        any(s.get("action") == "fill" and str(s.get("selector") or "") in uv for s in steps)
                        and any(s.get("action") == "fill" and str(s.get("selector") or "") in pv for s in steps)
                        and any(s.get("action") == "click" and str(s.get("selector") or "") in bv for s in steps)
                    )
                    if not _has_login_tv:
                        steps.extend([
                            {"action": "fill", "selector": u_pri, "target": build_semantic_target("input", "username", base_url), "value": prof.demo_username},
                            {"action": "fill", "selector": p_pri, "target": build_semantic_target("input", "password", base_url), "value": prof.demo_password},
                            {"action": "click", "selector": b_pri, "target": build_semantic_target("button", "login", base_url)},
                        ])
            steps.append({"action": "wait_ms", "ms": 300})
            steps.append({"action": "assert_text_contains", "selector": "body", "text": found_text})
            return steps

        seen = _extract_selectors_anywhere(p)
        if seen:
            for sel in seen:
                steps.append({"action": "assert_visible", "selector": sel})
            return ensure_has_assert(steps, base_url)

        _si = _extract_semantic_intent(p)
        if _si:
            _st = build_semantic_target(_si["kind"], _si["name"], base_url)
            steps.append({"action": "assert_visible", "selector": _st["primary"], "target": _st})
            return ensure_has_assert(steps, base_url)

        seen = _extract_selectors_anywhere(p)
        if prof and prof.inject_demo_credentials:
            u_pri = build_semantic_target("input", "username", base_url)["primary"]
            p_pri = build_semantic_target("input", "password", base_url)["primary"]
            b_pri = build_semantic_target("button", "login", base_url)["primary"]
            if "username" in low or "user name" in low or "user-name" in low:
                seen = [u_pri]
            elif not seen:
                seen = [u_pri, p_pri, b_pri]
        for sel in seen:
            steps.append({"action": "assert_visible", "selector": sel})
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

    _acts = [s.get("action") for s in steps]
    if "fill" not in _acts and "click" not in _acts and not any(c in p for c in '#.[]()"'): 
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
        if prof and prof.inject_demo_credentials:
            _login_texts = {"accepted usernames", "password", "login"}
            _is_login_text = any(t in found_text.lower() for t in _login_texts)
            if not _is_login_text:
                uv = _semantic_selector_variants(base_url, "input", "username")
                pv = _semantic_selector_variants(base_url, "input", "password")
                bv = _semantic_selector_variants(base_url, "button", "login")
                u_pri = build_semantic_target("input", "username", base_url)["primary"]
                p_pri = build_semantic_target("input", "password", base_url)["primary"]
                b_pri = build_semantic_target("button", "login", base_url)["primary"]
                _has_login = (
                    any(s.get("action") == "fill" and str(s.get("selector") or "") in uv for s in steps)
                    and any(s.get("action") == "fill" and str(s.get("selector") or "") in pv for s in steps)
                    and any(s.get("action") == "click" and str(s.get("selector") or "") in bv for s in steps)
                )
                if not _has_login:
                    steps.extend([
                        {"action": "fill", "selector": u_pri, "target": build_semantic_target("input", "username", base_url), "value": prof.demo_username},
                        {"action": "fill", "selector": p_pri, "target": build_semantic_target("input", "password", base_url), "value": prof.demo_password},
                        {"action": "click", "selector": b_pri, "target": build_semantic_target("button", "login", base_url)},
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
    from core.target_url_validation import validate_steps_navigation_urls

    steps = build_login_steps(base_url=base_url, prompt=prompt)
    if not steps:
        steps = _parse_steps_from_prompt(prompt, base_url)
    if steps:
        validate_steps_navigation_urls(steps, base_url)
    return steps


# Alias público para tests que necesitan el parser directo (sin login resolver)
parse_steps_from_prompt = _parse_steps_from_prompt
