# services/chat_run_test_name.py
"""
Semantic short names for chat-originated runs (test_name for SQLite / analytics).

Keeps the full user prompt elsewhere; only produces the human-visible test_name.
"""
from __future__ import annotations

import logging
import re
from typing import Optional

from core.settings import settings

logger = logging.getLogger("vanya.chat_run_name")

_MAX_LEN = 60

_SYSTEM = (
    "Eres un asistente de QA. Convierte instrucciones de prueba en nombres de test "
    "cortos y descriptivos."
)

_USER_PREFIX = (
    "Convierte esta instrucción en un nombre de test en español, máximo 60 caracteres, "
    "formato acción + objeto + contexto, sin URL completas, sin comillas, sin prefijos "
    "como 'Test:', y devolviendo solo el nombre final:\n"
)

# http(s) and www
_RE_URL = re.compile(r"https?://[^\s]+|www\.[^\s]+", re.IGNORECASE)
# bare host.tld[/path] (common TLDs)
_RE_BARE_HOST = re.compile(
    r"\b[\w.-]+\.(?:com|net|org|io|dev|es|co|app|ai)(?:/[^\s]*)?\b",
    re.IGNORECASE,
)

_RE_PREFIX = re.compile(
    r"^\s*(?:test|nombre|name)\s*:\s*",
    re.IGNORECASE,
)

# Quita verbos introductorios poco útiles al inicio (después de quitar URLs)
_RE_LEADING_INTRO_VERB = re.compile(
    r"^(?:(?:ve|ir|vete)\s+a\s+|entra\s+a\s+|abre\s+)+",
    re.IGNORECASE,
)


def _collapse_ws(s: str) -> str:
    return re.sub(r"\s+", " ", (s or "").strip())


def _strip_surrounding_quotes(s: str) -> str:
    t = s.strip()
    for pair in (('"', '"'), ("'", "'"), ("«", "»"), ("“", "”")):
        if len(t) >= 2 and t.startswith(pair[0]) and t.endswith(pair[1]):
            t = t[1:-1].strip()
    if len(t) >= 2 and ((t[0] == t[-1] == '"') or (t[0] == t[-1] == "'")):
        t = t[1:-1].strip()
    return t


def _strip_markdown_noise(s: str) -> str:
    t = re.sub(r"^[`#*\s]+|[`*]+$", "", s.strip())
    return t.strip()


def _truncate_at_word(s: str, max_len: int = _MAX_LEN) -> str:
    s = s.strip()
    if len(s) <= max_len:
        return s
    cut = s[:max_len].rstrip()
    sp = cut.rfind(" ")
    if sp > 8:
        return cut[:sp].rstrip()
    return cut


def _title_case_words(s: str) -> str:
    parts: list[str] = []
    for w in _collapse_ws(s).split(" "):
        if not w:
            continue
        if len(w) == 1:
            parts.append(w.upper())
        else:
            parts.append(w[0].upper() + w[1:].lower())
    return " ".join(parts)


def _strip_leading_intro_verbs(s: str) -> str:
    """
    Elimina prefijos tipo 'VE A ', 'IR A ', 'ABRE ', 'ENTRA A ' si deja texto sustancial.
    Repite de forma acotada por si el usuario encadena intros.
    """
    original = _collapse_ws(s)
    t = original
    if len(t) < 4:
        return t
    for _ in range(6):
        n = _RE_LEADING_INTRO_VERB.sub("", t)
        n = _collapse_ws(n)
        if n == t or len(n) < 3:
            break
        t = n
    return t if len(t) >= 3 else original


def fallback_test_name_from_prompt(prompt: str) -> str:
    """
    Deterministic name when LLM is unavailable or invalid.
    """
    raw = (prompt or "").strip()
    raw = _RE_URL.sub(" ", raw)
    raw = _RE_BARE_HOST.sub(" ", raw)
    raw = _collapse_ws(raw)
    raw = _strip_leading_intro_verbs(raw)
    raw = _title_case_words(raw)
    raw = _truncate_at_word(raw, _MAX_LEN)
    if not raw:
        return "Run desde chat"
    return raw


def sanitize_llm_test_name(raw: Optional[str]) -> Optional[str]:
    """
    Validate / normalize model output. Returns None if unusable.
    """
    if raw is None:
        return None
    t = str(raw).strip()
    if not t:
        return None
    t = t.split("\n")[0].strip()
    t = _strip_markdown_noise(t)
    t = _strip_surrounding_quotes(t)
    t = _RE_PREFIX.sub("", t)
    t = _collapse_ws(t)
    # Reject if still obviously a URL line
    if _RE_URL.search(t) or _RE_BARE_HOST.search(t):
        return None
    if len(t) < 2:
        return None
    t = _truncate_at_word(t, _MAX_LEN)
    return t or None


def _llm_generate_name(prompt: str) -> Optional[str]:
    if not (settings.OPENAI_API_KEY or "").strip():
        return None
    try:
        from openai import OpenAI

        client = OpenAI(api_key=settings.OPENAI_API_KEY, timeout=4.0)
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=[
                {"role": "system", "content": _SYSTEM},
                {"role": "user", "content": _USER_PREFIX + (prompt or "").strip()},
            ],
            temperature=0.2,
            max_tokens=100,
        )
        choice = resp.choices[0].message
        content = (choice.content or "").strip() if choice else ""
        return content or None
    except Exception as e:
        logger.debug("LLM test name generation failed: %s", e, exc_info=True)
        return None


def resolve_chat_run_test_name(prompt: str) -> str:
    """
    LLM-based semantic name with sanitization; deterministic fallback on any failure.
    Safe to call from a worker thread (uses its own short OpenAI client timeout).
    """
    p = (prompt or "").strip()
    if not p:
        return "Run desde chat"

    raw = _llm_generate_name(p)
    cleaned = sanitize_llm_test_name(raw)
    if cleaned:
        return cleaned
    return fallback_test_name_from_prompt(p)
