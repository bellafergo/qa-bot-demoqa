# core/login_intent_resolver.py
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Tuple


# ============================================================
# Intent detection
# ============================================================
def _has_login_intent(prompt: str) -> bool:
    """
    Dispara SOLO si hay intención clara de autenticación.
    Evita falsos positivos por frases como "valida que mi usuario exista".
    """
    p = (prompt or "").strip().lower()
    if not p:
        return False

    # verbos/acciones claras de login
    verbs = [
        "login",
        "log in",
        "sign in",
        "inicia sesión",
        "iniciar sesion",
        "inicia sesion",
        "entrar",
        "autentica",
        "autenticar",
        "iniciar sesión en",
        "iniciar sesion en",
    ]

    # señales débiles (NO suficientes solas)
    weak = [
        "username",
        "usuario",
        "password",
        "contraseña",
        "contrasena",
    ]

    has_verbs = any(v in p for v in verbs)
    has_weak = any(w in p for w in weak)

    # Requiere verbo. (Opcionalmente acompañado de weak)
    return has_verbs or (has_verbs and has_weak)


def _negative_expected(prompt: str) -> bool:
    """
    Si el usuario indica que debe fallar (caso negativo),
    marcamos expected="fail" para que el runner lo considere éxito si falla.
    """
    p = (prompt or "").lower()
    return any(k in p for k in [
        "debe fallar",
        "should fail",
        "inválido",
        "invalido",
        "incorrecto",
        "wrong",
        "invalid",
        "no existe",
        "usuario no existe",
        "password incorrecta",
        "contraseña incorrecta",
        "contrasena incorrecta",
    ])


# ============================================================
# Cred extraction
# ============================================================
def _strip_quotes(s: str) -> str:
    s = (s or "").strip()
    if len(s) >= 2 and ((s[0] == s[-1] == '"') or (s[0] == s[-1] == "'")):
        return s[1:-1]
    return s


def _extract_creds(prompt: str) -> Tuple[Optional[str], Optional[str]]:
    """
    Soporta formatos tolerantes:
      - username: foo password: bar
      - username=foo password=bar
      - usuario "foo" contraseña "bar"
      - usuario: foo contrasena: bar
      - user: foo pass: bar
    """
    p = (prompt or "").strip()

    u = None
    pw = None

    m_u = re.search(
        r'(?:\busername\b|\buser\b|\busuario\b)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)',
        p,
        flags=re.IGNORECASE,
    )
    if m_u:
        u = _strip_quotes(m_u.group(1))

    m_p = re.search(
        r'(?:\bpassword\b|\bpass\b|\bcontrase(?:ñ|n)a\b)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)',
        p,
        flags=re.IGNORECASE,
    )
    if m_p:
        pw = _strip_quotes(m_p.group(1))

    # Limpieza simple
    if u:
        u = u.strip()
    if pw:
        pw = pw.strip()

    return (u or None), (pw or None)


# ============================================================
# Site detection
# ============================================================
def _is_saucedemo(url: str) -> bool:
    return "saucedemo.com" in (url or "").lower()


# ============================================================
# Public API
# ============================================================
def build_login_steps(*, base_url: str, prompt: str) -> Optional[List[Dict[str, Any]]]:
    """
    Retorna steps SOLO si detecta un login “accionable” y seguro (por ahora SauceDemo).
    Si no aplica, retorna None para que tu flujo normal siga intacto.
    """
    if not base_url:
        return None

    if not _has_login_intent(prompt):
        return None

    # Scope: SauceDemo (determinístico)
    if not _is_saucedemo(base_url):
        return None

    neg = _negative_expected(prompt)
    username, password = _extract_creds(prompt)

    expected = "fail" if neg else "pass"

    # Siempre validamos que la UI de login está presente
    steps: List[Dict[str, Any]] = [
        {"action": "goto", "url": base_url, "expected": expected},
        {"action": "wait_ms", "ms": 250},
        {"action": "assert_visible", "selector": "#user-name"},
        {"action": "assert_visible", "selector": "#password"},
        {"action": "assert_visible", "selector": "#login-button"},
    ]

    # Si NO hay credenciales, NO forzamos submit (evita “PASSED” engañoso)
    if not username and not password:
        return steps

    # Fill (usa value, que es lo que tu runner lee)
    if username:
        steps.append({"action": "fill", "selector": "#user-name", "value": username})
    if password:
        steps.append({"action": "fill", "selector": "#password", "value": password})

    # Submit
    steps += [
        {"action": "click", "selector": "#login-button"},
        {"action": "wait_ms", "ms": 600},
    ]

    if neg:
        # Caso negativo esperado: error visible
        steps.append({"action": "assert_visible", "selector": "[data-test='error']"})
        return steps

    # Caso positivo: NO debe haber error + debe navegar a inventory + debe verse "Products"
    steps += [
        {"action": "assert_not_visible", "selector": "[data-test='error']"},
        {"action": "assert_url_contains", "value": "inventory.html"},
        # En SauceDemo el título "Products" está en .title
        {"action": "assert_text_contains", "selector": ".title", "expected": "Products"},
    ]
    return steps