# core/chat_helpers.py
from __future__ import annotations

import json
import re
import time
import uuid
from typing import Any, Dict, List, Optional, Tuple

from core.settings import settings
from core.state import SESSIONS, DOC_CACHE, DOC_CACHE_ORDER
from core.doc_render import trim_doc, render_doc_answer, fallback_minimal_doc
from core.doc_patterns import get_patterns


# ============================================================
# REGEX / CONSTANTS
# ============================================================
_URL_RE = re.compile(r"(https?://[^\s]+)", re.I)

# Señales claras de "análisis / consultoría" (NO ejecutar)
# Incluye tus patrones + los que te estaban fallando en demo
_ADVISE_HINTS = (
    "qué haces", "que haces",
    "qué puedes", "que puedes",
    "recomiendas", "recomienda",
    "riesgos", "impacto", "prioridad",
    "mejor práctica", "mejor practica", "best practice",
    "ayúdame a", "ayudame a",
    "explica", "explicame", "explicación",
    "resume", "resumen", "ejecutivo", "lenguaje ejecutivo",
    "actúa como", "actua como",
    "retoma", "continúa", "continua",
    "qué pruebas", "que pruebas",
    "qué validaciones", "que validaciones",
    "qué harías", "que harías",
)

# Verbos explícitos que SÍ activan intención de ejecución
# (IMPORTANTE: no metas palabras genéricas como "prueba" sola)
_EXECUTE_VERB_PATTERNS = [
    r"\bve a\b",
    r"\babre\b",
    r"\bnavega\b",
    r"\bentra\b",
    r"\bejecuta\b",
    r"\bcorrer\b",
    r"\bcorre\b",
    r"\brun\b",
    r"\busa playwright\b",
    r"\bhaz click\b",
    r"\bhaz clic\b",
    r"\bda click\b",
    r"\bclick\b",
    r"\bclic\b",
    r"\binicia sesi[oó]n\b",
    r"\bloguea\b",
    r"\bvalida en la web\b",
    r"\bprueba en (el )?sitio\b",
]

# Acciones UI: si hay URL + estas señales, también puede ser ejecución.
# OJO: aquí dejamos "valida/verifica" pero solo cuentan si hay URL (o last_url) y NO hay señales de análisis.
_UI_ACTION_WORDS = (
    "click", "clic", "haz click", "da click", "presiona",
    "fill", "llenar", "escribe", "captura", "selecciona",
    "assert", "valida", "verifica",
    "aparezca", "visible", "mensaje", "error",
    "botón", "boton", "campo", "texto",
    "login", "inicia sesión", "inicia sesion",
    "checkout", "carrito", "pagar", "pago",
)

# Palabras relacionadas a ejecución (no necesariamente verbos explícitos)
# Sirven como hints secundarios (conservamos las tuyas)
_EXEC_INTENT_WORDS = (
    "runner", "playwright",
    "prueba en vivo", "evidence", "screenshot",
)

# Heurística: señales de “quiero docs QA”
_DOC_WORDS = (
    "casos de prueba", "matriz", "gherkin", "invest",
    "criterios de aceptación", "criterios de aceptacion",
    "test cases", "plan de pruebas", "estrategia de pruebas",
    "escenarios de prueba", "artefactos qa",
    "pos", "punto de venta", "retail", "ecommerce", "e-commerce",
)

# Acciones válidas para runner
_ALLOWED_ACTIONS = {
    "goto", "fill", "click", "press",
    "assert_visible", "assert_text_contains", "wait_ms",
}


# ============================================================
# TEXT / URL HELPERS
# ============================================================
def norm(text: str) -> str:
    return re.sub(r"\s+", " ", (text or "").strip())


def low(text: str) -> str:
    return (text or "").lower().strip()


def looks_like_url(text: str) -> bool:
    return bool(_URL_RE.search(text or ""))


def normalize_url(url: str) -> str:
    if not url:
        return url
    url = url.strip()
    return url.rstrip(").,;!?:\"'”’]").lstrip("[\"'“‘(")


def extract_first_url(text: str) -> Optional[str]:
    if not text:
        return None
    m = _URL_RE.search(text)
    if not m:
        return None
    return normalize_url(m.group(1))


# ============================================================
# SESSION (in-memory, ligera)
# ============================================================
def _now() -> int:
    return int(time.time())


def cleanup_sessions() -> None:
    now = _now()
    expired: List[str] = []
    for sid, s in list(SESSIONS.items()):
        last_seen = int(s.get("last_seen", now))
        if now - last_seen > settings.SESSION_TTL_S:
            expired.append(sid)
    for sid in expired:
        SESSIONS.pop(sid, None)


def get_session(session_id: Optional[str]) -> Tuple[str, Dict[str, Any]]:
    cleanup_sessions()
    sid = (session_id or "").strip() or str(uuid.uuid4())
    s = SESSIONS.setdefault(
        sid,
        {
            "history": [],
            "last_url": None,
            "last_seen": _now(),
            "doc_last": None,
        },
    )
    s["last_seen"] = _now()
    return sid, s


def push_history(session: Dict[str, Any], role: str, content: str) -> None:
    session["history"].append({"role": role, "content": content})
    if len(session["history"]) > settings.MAX_HISTORY_MSGS:
        session["history"] = session["history"][-settings.MAX_HISTORY_MSGS :]


# ============================================================
# DOC CACHE (LRU simple y seguro)
# ============================================================
def cache_get(key: str) -> Optional[Dict[str, Any]]:
    value = DOC_CACHE.get(key)
    if value is not None:
        if key in DOC_CACHE_ORDER:
            try:
                DOC_CACHE_ORDER.remove(key)
            except ValueError:
                pass
        DOC_CACHE_ORDER.append(key)
    return value


def cache_set(key: str, value: Dict[str, Any]) -> None:
    DOC_CACHE[key] = value
    if key in DOC_CACHE_ORDER:
        try:
            DOC_CACHE_ORDER.remove(key)
        except ValueError:
            pass
    DOC_CACHE_ORDER.append(key)

    while len(DOC_CACHE_ORDER) > settings.DOC_CACHE_MAX:
        old = DOC_CACHE_ORDER.pop(0)
        DOC_CACHE.pop(old, None)


# ============================================================
# TOOL ARGUMENT PARSER
# ============================================================
def strip_code_fences(raw: str) -> str:
    if not raw:
        return ""
    s = raw.strip().lstrip("\ufeff")
    if s.startswith("```"):
        s = re.sub(r"^```[a-zA-Z0-9_-]*\s*", "", s)
        s = re.sub(r"\s*```$", "", s)
    return s.strip()


def parse_tool_args(raw: str) -> Optional[Dict[str, Any]]:
    if not raw:
        return None
    s = strip_code_fences(raw)

    # 1) JSON directo
    try:
        return json.loads(s)
    except json.JSONDecodeError:
        pass

    # 2) Busca primer bloque {...}
    m = re.search(r"\{.*\}", s, re.S)
    if m:
        try:
            return json.loads(m.group(0))
        except json.JSONDecodeError:
            return None

    return None


# ============================================================
# INTENT DETECTION (FIX PRINCIPAL)
# ============================================================
def is_question(prompt: str) -> bool:
    p = low(prompt)
    if "?" in p:
        return True
    return p.startswith(
        (
            "que ", "qué ",
            "como ", "cómo ",
            "cuando", "cuándo",
            "por qué", "porque",
            "debo", "puedo",
        )
    )


def _has_explicit_execute_verbs(prompt: str) -> bool:
    p = low(prompt)
    return any(re.search(rx, p) for rx in _EXECUTE_VERB_PATTERNS)


def _has_advise_cues(prompt: str) -> bool:
    p = low(prompt)
    return any(h in p for h in _ADVISE_HINTS)


def wants_doc(prompt: str) -> bool:
    p = low(prompt)

    # Si es claramente ejecución (por verbo explícito), NO es doc
    if _has_explicit_execute_verbs(p):
        return False

    # Si piden UI actions, probablemente es ejecución (si hay URL) => no doc
    if any(w in p for w in _UI_ACTION_WORDS) or any(w in p for w in _EXEC_INTENT_WORDS):
        return False

    return any(w in p for w in _DOC_WORDS)


def wants_execute(prompt: str, session: Dict[str, Any]) -> bool:
    """
    Regla nueva (más segura):
    - NO ejecutar si el texto huele a análisis/consultoría.
    - Ejecutar si hay verbos explícitos de ejecución (aunque falte URL -> backend puede pedirla).
    - Ejecutar si hay URL (o last_url) + acciones UI claras (click/llenar/validar/login),
      siempre que NO sea una pregunta teórica.
    """
    p = low(prompt)

    # 1) Hard stop: señales de análisis/consultoría -> no ejecutar
    if _has_advise_cues(p):
        return False

    # 2) Verbo explícito de ejecución => intención de ejecutar (aunque falte URL)
    if _has_explicit_execute_verbs(p):
        return True

    # 3) Caso "URL + acción UI" (o continuidad con last_url)
    has_ui_action = any(w in p for w in _UI_ACTION_WORDS)
    has_exec_hint = any(w in p for w in _EXEC_INTENT_WORDS) or any(w in p for w in _EXEC_INTENT_WORDS)

    has_url = looks_like_url(prompt) or bool(session.get("last_url"))

    # Pregunta teórica sin acción => no ejecutar
    if is_question(prompt) and not has_ui_action and not has_exec_hint:
        return False

    # Para ejecutar por heurística, sí necesitamos URL o last_url
    return bool(has_url and (has_ui_action or has_exec_hint))


def wants_advise(prompt: str, session: Dict[str, Any]) -> bool:
    if wants_doc(prompt) or wants_execute(prompt, session):
        return False

    # Si el usuario pregunta o usa cues de consultoría, es ADVISE
    if is_question(prompt) or _has_advise_cues(prompt):
        return True

    return any(h in low(prompt) for h in _ADVISE_HINTS)


# Alias útil para que tu backend sea más legible
def is_execute_intent(prompt: str, session: Dict[str, Any]) -> bool:
    return wants_execute(prompt, session)


# ============================================================
# DOC HELPERS
# ============================================================
def doc_requested_parts(prompt: str) -> Dict[str, bool]:
    p = low(prompt)

    if any(x in p for x in ("todo", "completo", "full", "artefactos")):
        return dict(invest=True, gherkin=True, cases=True, scripts=True)

    wants_invest = ("invest" in p) or ("brechas" in p)
    wants_gherkin = ("gherkin" in p) or ("criterios" in p)
    wants_cases = ("casos" in p) or ("matriz" in p) or ("test cases" in p)
    wants_scripts = ("script" in p) or ("automat" in p) or ("playwright" in p)

    # default sensato
    if not any([wants_invest, wants_gherkin, wants_cases, wants_scripts]):
        return dict(invest=False, gherkin=True, cases=True, scripts=False)

    return dict(invest=wants_invest, gherkin=wants_gherkin, cases=wants_cases, scripts=wants_scripts)


def extract_user_story(prompt: str) -> Optional[str]:
    m = re.search(r"[\"“](.+?)[\"”]", prompt)
    if m:
        return (m.group(1) or "").strip()
    m2 = re.search(r"(historia|user story)\s*:\s*(.+)$", prompt, re.I)
    if m2:
        return (m2.group(2) or "").strip()
    return None


def infer_domain(prompt: str) -> str:
    """
    Normaliza dominio con heurística simple + fallback a settings.DOC_DEFAULT_DOMAIN.
    """
    p = low(prompt)
    if "pos" in p or "punto de venta" in p:
        return "pos"
    if "retail" in p or "ecommerce" in p or "e-commerce" in p or "checkout" in p or "carrito" in p:
        return "retail"
    return (settings.DOC_DEFAULT_DOMAIN or "web").strip().lower()


def get_domain_patterns(domain: str) -> Dict[str, Any]:
    dom = (domain or "web").strip().lower()
    # tu doc_patterns soporta retail/pos/web
    if dom not in ("retail", "pos", "web"):
        dom = "web"
    return get_patterns(dom)


# ============================================================
# URL / EXECUTION HELPERS
# ============================================================
def pick_base_url(req: Any, session: Dict[str, Any], prompt: str) -> Optional[str]:
    # Prioridad: req.base_url
    base = getattr(req, "base_url", None)
    if base and str(base).strip():
        return normalize_url(str(base))

    # Luego: URL en el prompt
    url = extract_first_url(prompt)
    if url:
        return url

    # Luego: continuidad con last_url
    p = low(prompt)
    if session.get("last_url") and any(w in p for w in ("misma", "la misma", "ahora", "siguiente", "después", "despues")):
        return normalize_url(str(session["last_url"]))

    return None


def ensure_goto(steps: List[Dict[str, Any]], base_url: str) -> None:
    if not steps:
        return
    has_goto = any(str(s.get("action", "")).lower() == "goto" for s in steps)
    if not has_goto:
        steps.insert(0, {"action": "goto", "url": base_url})


def update_last_url(session: Dict[str, Any], steps: List[Dict[str, Any]], fallback: Optional[str] = None) -> None:
    for s in steps or []:
        if str(s.get("action", "")).lower() == "goto" and s.get("url"):
            session["last_url"] = normalize_url(str(s["url"]))
            return
    if fallback:
        session["last_url"] = normalize_url(str(fallback))


# ============================================================
# EXEC: Steps parsing
# ============================================================
def _normalize_steps(obj: Any) -> List[Dict[str, Any]]:
    """
    Acepta:
    - {"steps":[...]}
    - [...]
    - {"action":"goto"...} (single)
    """
    if obj is None:
        return []
    if isinstance(obj, dict):
        if "steps" in obj and isinstance(obj["steps"], list):
            return [s for s in obj["steps"] if isinstance(s, dict)]
        if "action" in obj:
            return [obj]  # single step dict
        return []
    if isinstance(obj, list):
        return [s for s in obj if isinstance(s, dict)]
    return []


def _sanitize_steps(steps: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for s in steps:
        action = str(s.get("action", "")).strip().lower()
        if action not in _ALLOWED_ACTIONS:
            continue

        step: Dict[str, Any] = {"action": action}

        # Copia campos permitidos
        for k in ("url", "selector", "text", "role", "value", "timeout_ms"):
            if k in s and s[k] is not None:
                step[k] = s[k]

        # Normaliza wait_ms
        if action == "wait_ms":
            # soporta {"value": 500} o {"timeout_ms": 500}
            if "timeout_ms" not in step:
                v = s.get("value") or s.get("ms") or 500
                try:
                    step["timeout_ms"] = int(v)
                except Exception:
                    step["timeout_ms"] = 500

        out.append(step)

    return out


def extract_steps_from_text(raw: str) -> List[Dict[str, Any]]:
    """
    Intenta extraer steps desde texto que pueda venir como:
    - JSON puro
    - ```json ... ```
    - Texto que contiene un bloque JSON
    """
    if not raw:
        return []

    s = strip_code_fences(raw)

    # 1) JSON directo
    try:
        parsed = json.loads(s)
        return _sanitize_steps(_normalize_steps(parsed))
    except Exception:
        pass

    # 2) Busca el primer bloque JSON con "steps"
    m = re.search(r"\{[\s\S]*\}", s)
    if m:
        try:
            parsed = json.loads(m.group(0))
            return _sanitize_steps(_normalize_steps(parsed))
        except Exception:
            pass

    # 3) Heurística: intentar encontrar lista [...]
    m2 = re.search(r"\[[\s\S]*\]", s)
    if m2:
        try:
            parsed = json.loads(m2.group(0))
            return _sanitize_steps(_normalize_steps(parsed))
        except Exception:
            pass

    return []


# ============================================================
# DOC RENDER HELPERS
# ============================================================
def build_doc_answer(doc: Dict[str, Any]) -> str:
    return render_doc_answer(trim_doc(doc))


def minimal_doc(requested: Dict[str, Any], domain: str, context: str, story: str) -> Dict[str, Any]:
    return fallback_minimal_doc(requested, domain, context, story)


# ============================================================
# EXEC ANSWER RENDER
# ============================================================
def render_execute_answer(result: Dict[str, Any], evidence_url: Optional[str] = None) -> str:
    """
    Render amigable del resultado del runner.
    Compatible con Cloudinary evidence_url.
    """
    status = result.get("status") or ("ok" if result.get("ok") else "error")
    msg = (result.get("message") or result.get("detail") or "").strip()

    # prioridad: argumento (cloudinary), luego en result
    url = evidence_url or result.get("screenshot_url") or result.get("evidence_url")

    if url:
        if msg:
            return f"✅ Ejecutado ({status}). {msg}\nEvidence: {url}"
        return f"✅ Ejecutado ({status}).\nEvidence: {url}"

    if msg:
        return f"✅ Ejecutado ({status}). {msg}"
    return f"✅ Ejecutado ({status})."


__all__ = [
    "norm", "low",
    "looks_like_url", "extract_first_url", "normalize_url",
    "cleanup_sessions", "get_session", "push_history",
    "cache_get", "cache_set",
    "strip_code_fences", "parse_tool_args",
    "is_question",
    "wants_doc", "wants_execute", "wants_advise",
    "is_execute_intent",
    "doc_requested_parts", "extract_user_story", "infer_domain", "get_domain_patterns",
    "pick_base_url", "ensure_goto", "update_last_url",
    "extract_steps_from_text",
    "build_doc_answer", "minimal_doc",
    "render_execute_answer",
]