# core/chat_helpers.py
import json
import re
import time
import uuid
from typing import Any, Dict, List, Optional, Tuple

from core.settings import settings
from core.state import SESSIONS, DOC_CACHE, DOC_CACHE_ORDER
from core.doc_render import trim_doc, render_doc_answer, fallback_minimal_doc

from runner import execute_test

# ============================================================
# TEXT / URL HELPERS
# ============================================================
_URL_RE = re.compile(r"(https?://[^\s]+)", re.I)

def _norm(s: str) -> str:
    return re.sub(r"\s+", " ", (s or "").strip())

def _low(s: str) -> str:
    return (s or "").lower()

def _looks_like_url(s: str) -> bool:
    return bool(_URL_RE.search(s or ""))

def _extract_first_url(text: str) -> Optional[str]:
    if not text:
        return None
    m = _URL_RE.search(text)
    if not m:
        return None
    url = m.group(1).strip()
    url = url.rstrip(").,;!?:\"'”’]")
    url = url.lstrip("[\"'“‘(")
    return url or None

def _normalize_url(url: str) -> str:
    url = (url or "").strip()
    url = url.rstrip(").,;!?:\"'”’]")
    url = url.lstrip("[\"'“‘(")
    return url

# ============================================================
# SESSION (in-memory)
# ============================================================
def _now() -> int:
    return int(time.time())

def _get_session(session_id: Optional[str]) -> Tuple[str, Dict[str, Any]]:
    sid = (session_id or "").strip() or str(uuid.uuid4())
    s = SESSIONS.get(sid)
    if not s:
        s = {"history": [], "last_url": None, "last_seen": _now(), "doc_last": None}
        SESSIONS[sid] = s
    s["last_seen"] = _now()
    return sid, s

def _push_history(session: Dict[str, Any], role: str, content: str):
    session["history"].append({"role": role, "content": content})
    if len(session["history"]) > settings.MAX_HISTORY_MSGS:
        session["history"] = session["history"][-settings.MAX_HISTORY_MSGS :]

# ============================================================
# CACHE (LRU)
# ============================================================
def _cache_get(key: str) -> Optional[Dict[str, Any]]:
    try:
        v = DOC_CACHE.get(key)
        if v:
            if key in DOC_CACHE_ORDER:
                DOC_CACHE_ORDER.remove(key)
            DOC_CACHE_ORDER.append(key)
        return v
    except Exception:
        return None

def _cache_set(key: str, value: Dict[str, Any]):
    try:
        DOC_CACHE[key] = value
        if key in DOC_CACHE_ORDER:
            DOC_CACHE_ORDER.remove(key)
        DOC_CACHE_ORDER.append(key)

        while len(DOC_CACHE_ORDER) > settings.DOC_CACHE_MAX:
            oldest = DOC_CACHE_ORDER.pop(0)
            DOC_CACHE.pop(oldest, None)
    except Exception:
        pass

# ============================================================
# TOOL ARGS PARSER
# ============================================================
def _strip_code_fences(s: str) -> str:
    if not s:
        return s
    s2 = s.strip()
    if s2.startswith("```"):
        s2 = re.sub(r"^```[a-zA-Z0-9_-]*\s*", "", s2)
        s2 = re.sub(r"\s*```$", "", s2)
    return s2.strip()

def _parse_tool_args(raw_args: str) -> Optional[Dict[str, Any]]:
    if not raw_args:
        return None
    raw = _strip_code_fences(raw_args).strip().lstrip("\ufeff")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        m = re.search(r"\{.*?\}", raw, flags=re.S)
        if m:
            try:
                return json.loads(m.group(0))
            except json.JSONDecodeError:
                return None
    return None

# ============================================================
# INTENT ROUTING
# ============================================================
_ADVISE_HINTS = [
    "qué haces", "que haces", "qué puedes", "que puedes", "recomiendas",
    "riesgos", "mejor práctica", "best practice", "ayúdame a", "ayudame a", "explica",
]

def _is_question(prompt: str) -> bool:
    p = _low(prompt).strip()
    if "?" in p:
        return True
    return p.startswith((
        "cuando", "cuándo",
        "como", "cómo",
        "que ", "qué ",
        "por que", "por qué",
        "deberia", "debería",
        "puedo", "podría",
        "debo",
        "es ", "es necesario", "conviene", "vale la pena",
    ))

def _wants_doc(prompt: str) -> bool:
    p = (prompt or "").strip().lower()
    if not p:
        return False

    exec_words = [
        "ejecuta", "ejecutar", "corre", "correr", "playwright", "haz click", "da click",
        "ve a", "navega", "login", "inicia sesión", "validar en la web", "run", "runner",
        "prueba en vivo", "evidence", "screenshot"
    ]
    if any(w in p for w in exec_words):
        return False

    doc_words = [
        "matriz", "matriz de pruebas", "casos de prueba", "casos", "test cases",
        "gherkin", "feature:", "scenario:", "dado", "cuando", "entonces",
        "invest", "criterios de aceptación", "acceptance criteria",
        "plan de pruebas", "estrategia de pruebas", "checklist",
        "pruebas positivas", "pruebas negativas", "edge", "borde",
        "suite", "smoke", "regresión", "regression",
        "pos", "punto de venta", "retail"
    ]
    return any(w in p for w in doc_words)

def _wants_execute_explicit(prompt: str, session: Optional[dict] = None) -> bool:
    p = _low(prompt)
    session = session or {}

    has_url = _looks_like_url(prompt)
    has_last_url = bool(session.get("last_url"))

    ui_actions = [
        "click", "clic", "haz click", "presiona",
        "fill", "llenar", "escribe", "selecciona",
        "assert", "valida", "verifica",
        "aparezca", "visible", "mensaje", "error",
        "botón", "boton", "campo", "texto", "redirige", "redirecciona",
        "login", "inicia sesión", "inicia sesion",
    ]
    has_ui_action = any(x in p for x in ui_actions)

    has_exec_intent = bool(re.search(
        r"\b(ejecutar|ejecuta|correr|corre|run|playwright|navega|abre|entra|automatiza)\b",
        p
    ))

    if _is_question(prompt) and not has_url and not has_ui_action:
        return False

    if not has_url and has_last_url and has_ui_action:
        return True

    if has_url and (has_exec_intent or has_ui_action):
        return True

    if has_exec_intent and has_ui_action and has_last_url:
        return True

    return False

def _wants_execute_followup(prompt: str, session: Dict[str, Any]) -> bool:
    p = _low(prompt)
    if session.get("last_url") and any(x in p for x in ["ahora", "también", "tambien", "en la misma", "en esa", "luego", "después", "despues", "siguiente"]):
        if any(x in p for x in ["valida", "verifica", "visible", "texto", "aparezca", "error", "mensaje", "botón", "boton", "campo"]):
            return True
    return False

def _doc_requested_parts(prompt: str) -> Dict[str, bool]:
    p = _low(prompt)
    wants_invest = ("invest" in p) or ("brechas" in p)
    wants_gherkin = ("gherkin" in p) or ("criterios de aceptación" in p) or ("criterios de aceptacion" in p)
    wants_cases = ("casos de prueba" in p) or ("matriz" in p) or ("test cases" in p)
    wants_scripts = ("script" in p) or ("automat" in p) or ("playwright" in p) or ("page object" in p) or ("p.o.m" in p)

    if any(x in p for x in ["artefactos", "todo", "completo", "full"]):
        return {"invest": True, "gherkin": True, "cases": True, "scripts": True}

    if not any([wants_invest, wants_gherkin, wants_cases, wants_scripts]):
        return {"invest": False, "gherkin": True, "cases": True, "scripts": False}

    return {"invest": wants_invest, "gherkin": wants_gherkin, "cases": wants_cases, "scripts": wants_scripts}

def _extract_user_story(prompt: str) -> Optional[str]:
    m = re.search(r"[“\"'](.+?)[”\"']", prompt)
    if m:
        return m.group(1).strip()
    m2 = re.search(r"(historia|user story)\s*:\s*(.+)$", prompt, flags=re.I)
    if m2:
        return m2.group(2).strip()
    return None

def _infer_domain(prompt: str) -> str:
    p = _low(prompt)
    if "pos" in p or "punto de venta" in p:
        return "pos"
    if "ecommerce" in p or "e-commerce" in p or "tienda en línea" in p or "tienda en linea" in p:
        return "retail"
    if "erp" in p or "oracle" in p or "sap" in p:
        return "web"
    return settings.DOC_DEFAULT_DOMAIN

# ============================================================
# THREAD TITLE
# ============================================================
def _make_title_from_prompt(prompt: str, max_len: int = 60) -> str:
    p = (prompt or "").strip()
    if not p:
        return "New chat"
    p = " ".join(p.split())
    if len(p) > max_len:
        p = p[: max_len - 1] + "…"
    return p

# ============================================================
# BASE URL PICKER
# ============================================================
def _pick_base_url(req, session: Dict[str, Any], prompt: str) -> Optional[str]:
    if getattr(req, "base_url", None) and (req.base_url or "").strip():
        return _normalize_url(req.base_url)

    url = _extract_first_url(prompt)
    if url:
        return url

    p = _low(prompt)
    same_markers = ["la misma", "mismo sitio", "misma página", "ahí", "en esa página", "same", "same page", "same site"]
    cont_markers = ["ahora", "también", "en la misma", "en esa", "siguiente", "luego", "después", "then", "next"]

    if session.get("last_url"):
        last = _normalize_url(str(session["last_url"]))
        if any(x in p for x in same_markers) or any(x in p for x in cont_markers):
            return last

    return None

# ============================================================
# DOC wrappers (delegan a core/doc_render.py)
# ============================================================
def _trim_doc(doc: Dict[str, Any]) -> Dict[str, Any]:
    return trim_doc(doc)

def _render_doc_answer(doc: Dict[str, Any]) -> str:
    return render_doc_answer(doc)

def _fallback_minimal_doc(requested, domain, context, story):
    return fallback_minimal_doc(requested, domain, context, story)

# ============================================================
# RUNNER wrappers
# ============================================================
def _run_runner(steps: List[Dict[str, Any]], base_url: str, headless: bool = True) -> Dict[str, Any]:
    return execute_test(
        steps=steps,
        base_url=base_url,
        headless=headless,
    )

def _render_execute_answer(result: Dict[str, Any]) -> str:
    status = result.get("status") or ("ok" if result.get("ok") else "error")
    msg = result.get("message") or result.get("detail") or ""
    evidence = result.get("evidence") or result.get("screenshot_url") or ""
    if evidence:
        return f"✅ Ejecutado ({status}). {msg}\nEvidence: {evidence}".strip()
    return f"✅ Ejecutado ({status}). {msg}".strip()