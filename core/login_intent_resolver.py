# core/login_intent_resolver.py
from __future__ import annotations

import re
from typing import Dict, List, Optional, Tuple


# ============================================================
# Helpers / constantes
# ============================================================

# Palabras que NO queremos que terminen como usuario/contraseña
# (eran la causa de que "exista" se usara como username)
_STOPWORDS = {
    "exista",
    "exists",
    "exist",
    "usuario",
    "username",
    "user",
    "password",
    "pass",
    "contraseña",
    "contrasena",
    "como",
    "as",
    "con",
    "with",
    "valida",
    "validar",
    "verifica",
    "verificar",
}


def _strip_quotes_and_punct(s: str) -> str:
    """
    Quita comillas y puntuación final típica: "love," -> love
    """
    s = (s or "").strip()
    if len(s) >= 2 and ((s[0] == s[-1] == '"') or (s[0] == s[-1] == "'")):
        s = s[1:-1]

    # Quita :,;,. al final
    s = re.sub(r'[:;,\.]+$', "", s).strip()
    return s


def _clean_cred_token(s: Optional[str]) -> Optional[str]:
    """
    Normaliza un token candidato a credencial.
    - Elimina comillas/puntuación
    - Filtra stopwords (exista, usuario, password, etc.)
    """
    if not s:
        return None
    v = _strip_quotes_and_punct(str(s))
    if not v:
        return None
    if v.lower() in _STOPWORDS:
        return None
    return v


# ============================================================
# Intent detection
# ============================================================

def _has_login_intent(prompt: str) -> bool:
    """
    Detecta intención de login.
    - Dispara si hay verbos de autenticación
    - O si aparecen username + password en el mismo prompt (credenciales explícitas)
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

    if any(v in p for v in verbs):
        return True

    # caso: no dicen "login", pero sí dan username + password
    user_words = ["username", "usuario", "user"]
    pass_words = ["password", "pass", "contraseña", "contrasena"]

    has_user_word = any(w in p for w in user_words)
    has_pass_word = any(w in p for w in pass_words)

    if has_user_word and has_pass_word:
        # Evita casos puramente teóricos/ejemplo
        if "ejemplo" in p or "example" in p or "plantilla" in p:
            return False
        return True

    return False


def _negative_expected(prompt: str) -> bool:
    """
    Si el usuario indica que debe fallar (caso negativo),
    marcamos expected="fail" para que el runner lo considere éxito si falla.
    """
    p = (prompt or "").lower()
    return any(
        k in p
        for k in [
            "debe fallar",
            "should fail",
            "inválido",
            "invalido",
            "incorrecto",
            "wrong",
            "invalid",
            "no existe",
            "no exista",
            "usuario no existe",
            "password incorrecta",
            "contraseña incorrecta",
            "contrasena incorrecta",
            "rechazado",
            "denied",
        ]
    )


# ============================================================
# Cred extraction
# ============================================================

def _extract_creds(prompt: str) -> Tuple[Optional[str], Optional[str]]:
    """
    Soporta formatos tolerantes:
      - username: foo password: bar
      - username=foo password=bar
      - usuario "foo" contraseña "bar"
      - usuario: foo contrasena: bar
      - user: foo pass: bar
      - password:love (pegado)
    """
    p = (prompt or "").strip()

    u: Optional[str] = None
    pw: Optional[str] = None

    # Formatos con separador fuerte (: o =)
    m_u = re.search(
        r'(?:\busername\b|\buser\b|\busuario\b)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)',
        p,
        flags=re.IGNORECASE,
    )
    if m_u:
        u = _clean_cred_token(m_u.group(1))

    m_p = re.search(
        r'(?:\bpassword\b|\bpass\b|\bcontrase(?:ñ|n)a\b)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)',
        p,
        flags=re.IGNORECASE,
    )
    if m_p:
        pw = _clean_cred_token(m_p.group(1))

    # Formato sin : ni =  (user foo / pass bar) si no los encontramos antes
    if not u:
        m_u2 = re.search(
            r'(?:\busername\b|\buser\b|\busuario\b)\s+(".*?"|\'.*?\'|\S+)',
            p,
            flags=re.IGNORECASE,
        )
        if m_u2:
            u = _clean_cred_token(m_u2.group(1))

    if not pw:
        m_p2 = re.search(
            r'(?:\bpassword\b|\bpass\b|\bcontrase(?:ñ|n)a\b)\s+(".*?"|\'.*?\'|\S+)',
            p,
            flags=re.IGNORECASE,
        )
        if m_p2:
            pw = _clean_cred_token(m_p2.group(1))

    return (u or None), (pw or None)


# ============================================================
# Site detection
# ============================================================

def _is_saucedemo(url: str) -> bool:
    return "saucedemo.com" in (url or "").lower()


# ============================================================
# Public API
# ============================================================

def build_login_steps(*, base_url: str, prompt: str) -> Optional[List[Dict[str, object]]]:
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
    steps: List[Dict[str, object]] = [
        # expected aquí alimenta al runner (expected_outcome global)
        {"action": "goto", "url": base_url, "expected": expected},
        {"action": "wait_ms", "ms": 250},
        {"action": "assert_visible", "selector": "#user-name"},
        {"action": "assert_visible", "selector": "#password"},
        {"action": "assert_visible", "selector": "#login-button"},
    ]

    # Si NO hay credenciales, NO forzamos submit.
    # (Sirve como smoke de la pantalla de login, pero no intenta autenticarse.)
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
        # Caso negativo esperado: debe aparecer error
        steps.append({"action": "assert_visible", "selector": "[data-test='error']"})
        return steps

    # Caso positivo: NO debe haber error + debe navegar a inventory + debe verse "Products"
    steps += [
        {"action": "assert_not_visible", "selector": "[data-test='error']"},
        {"action": "assert_url_contains", "value": "inventory.html"},
        # En SauceDemo el título "Products" está en .title
        {"action": "assert_text_contains", "selector": ".title", "text": "Products"},
    ]
    return steps