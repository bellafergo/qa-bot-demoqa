# core/login_intent_resolver.py
from __future__ import annotations

import re
from typing import Dict, List, Optional, Tuple


def _looks_like_login(prompt: str) -> bool:
    p = (prompt or "").lower()
    # señales fuertes
    strong = [
        "login", "log in", "sign in", "inicia sesión", "iniciar sesion",
        "usuario", "username", "password", "contraseña", "contrasena",
    ]
    return any(k in p for k in strong)


def _extract_creds(prompt: str) -> Tuple[Optional[str], Optional[str]]:
    """
    Soporta formatos:
      - username: foo password: bar
      - user foo pass bar
      - usuario foo contraseña bar
    """
    p = (prompt or "").strip()

    # username: X ... password: Y
    m = re.search(r"(?:username|user|usuario)\s*[:=]\s*([^\s]+)", p, flags=re.IGNORECASE)
    u = m.group(1).strip() if m else None

    m2 = re.search(r"(?:password|pass|contrase(?:ñ|n)a)\s*[:=]\s*([^\s]+)", p, flags=re.IGNORECASE)
    pw = m2.group(1).strip() if m2 else None

    return u, pw


def _is_saucedemo(url: str) -> bool:
    return "saucedemo.com" in (url or "").lower()


def build_login_steps(*, base_url: str, prompt: str) -> Optional[List[Dict[str, object]]]:
    """
    Retorna steps si detecta intención clara de login con credenciales.
    Si no, retorna None y el flujo normal sigue intacto.
    """
    if not _looks_like_login(prompt):
        return None

    username, password = _extract_creds(prompt)
    if not username or not password:
        # sin credenciales, no forzamos login; dejamos que el flujo normal pregunte
        return None

    steps: List[Dict[str, object]] = [
        {"action": "goto", "url": base_url},
        {"action": "wait_ms", "ms": 250},
    ]

    # ======== SauceDemo (determinístico) ========
    if _is_saucedemo(base_url):
        # fill user/pass + click login
        steps += [
            {"action": "assert_visible", "selector": "#user-name"},
            {"action": "fill", "selector": "#user-name", "text": username},
            {"action": "assert_visible", "selector": "#password"},
            {"action": "fill", "selector": "#password", "text": password},
            {"action": "assert_visible", "selector": "#login-button"},
            {"action": "click", "selector": "#login-button"},
            {"action": "wait_ms", "ms": 500},
        ]

        # ✅ Validación REAL (evita falsos PASSED):
        # - Si login falla, aparece error visible
        # - Si login pasa, aparece "Products" o URL inventory
        #
        # Como tu runner no tiene "assert_url_contains", validamos por texto/elementos.
        steps += [
            # Éxito: en inventory aparece el título Products
            {"action": "assert_text_contains", "text": "Products"},
        ]
        return steps

    # ======== Genérico (fallback) ========
    # Intentamos detectar campos comunes; si no existen, mejor no forzar.
    # (No queremos “romper” otros sitios.)
    common_user_selectors = ["#username", 'input[name="username"]', 'input[type="email"]']
    common_pass_selectors = ["#password", 'input[name="password"]', 'input[type="password"]']
    common_submit_selectors = ['button[type="submit"]', 'input[type="submit"]']

    # Aquí no podemos “probar” selectores sin ejecutar, así que:
    # dejamos una ruta conservadora: solo si el prompt trae selectores explícitos.
    # Ej: "llena #user-name con foo y #password con bar y da click #login"
    # Si no, regresamos None para que tu LLM fallback genere algo más acorde al sitio.
    p = (prompt or "")
    has_selector = bool(re.search(r"(#[-\w]+|\[[^\]]+\]|\.{1}[-\w]+)", p))
    if not has_selector:
        return None

    # Si el prompt ya trae selectores, tu _parse_steps_from_prompt lo resolverá mejor.
    return None
