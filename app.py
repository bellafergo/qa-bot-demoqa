import os
import json
import re
import time
import uuid
import traceback
from typing import List, Optional, Dict, Any, Tuple

from dotenv import load_dotenv
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from openai import OpenAI

from runner import execute_test

# ============================================================
# INIT
# ============================================================
load_dotenv()
app = FastAPI()

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # luego ciérralo a tu(s) dominio(s)
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ============================================================
# ENV
# ============================================================
OPENAI_API_KEY = (os.getenv("OPENAI_API_KEY") or "").strip()
OPENAI_MODEL = (os.getenv("OPENAI_MODEL") or "gpt-4o-mini").strip()

SESSION_TTL_S = int(os.getenv("SESSION_TTL_S", "3600"))
MAX_HISTORY_MSGS = int(os.getenv("MAX_HISTORY_MSGS", "10"))

# DOC tuning (velocidad)
DOC_DEFAULT_DOMAIN = (os.getenv("DOC_DEFAULT_DOMAIN") or "retail").strip()
DOC_MAX_TEST_CASES = int(os.getenv("DOC_MAX_TEST_CASES", "14"))
DOC_MAX_GHERKIN = int(os.getenv("DOC_MAX_GHERKIN", "8"))
DOC_MAX_FILES = int(os.getenv("DOC_MAX_FILES", "3"))
DOC_MAX_CODE_CHARS = int(os.getenv("DOC_MAX_CODE_CHARS", "3500"))
DOC_HISTORY_MSGS = int(os.getenv("DOC_HISTORY_MSGS", "4"))
DOC_CACHE_MAX = int(os.getenv("DOC_CACHE_MAX", "80"))

# Model knobs
DOC_TEMPERATURE = float(os.getenv("DOC_TEMPERATURE", "0.2"))
ADV_TEMPERATURE = float(os.getenv("ADV_TEMPERATURE", "0.4"))
EXEC_TEMPERATURE = float(os.getenv("EXEC_TEMPERATURE", "0.2"))
DOC_MAX_TOKENS = int(os.getenv("DOC_MAX_TOKENS", "1100"))
ADV_MAX_TOKENS = int(os.getenv("ADV_MAX_TOKENS", "700"))
EXEC_MAX_TOKENS = int(os.getenv("EXEC_MAX_TOKENS", "700"))

# ============================================================
# REQUEST MODEL
# ============================================================
class ChatRunRequest(BaseModel):
    prompt: str
    session_id: Optional[str] = None
    headless: bool = True
    base_url: Optional[str] = None  # opcional


# ============================================================
# SESSION MEMORY (history + last_url + ttl + doc_last)
# ============================================================
_SESSIONS: Dict[str, Dict[str, Any]] = {}

# DOC cache (mismo prompt => respuesta instant)
_DOC_CACHE: Dict[str, Dict[str, Any]] = {}
_DOC_CACHE_ORDER: List[str] = []


def _now() -> int:
    return int(time.time())


def _get_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=OPENAI_API_KEY)


def _cleanup_sessions():
    t = _now()
    dead = []
    for sid, s in _SESSIONS.items():
        last_seen = int(s.get("last_seen", t))
        if t - last_seen > SESSION_TTL_S:
            dead.append(sid)
    for sid in dead:
        _SESSIONS.pop(sid, None)


def _get_session(session_id: Optional[str]) -> Tuple[str, Dict[str, Any]]:
    sid = (session_id or "").strip() or str(uuid.uuid4())
    s = _SESSIONS.get(sid)
    if not s:
        s = {
            "history": [],
            "last_url": None,
            "last_seen": _now(),
            "doc_last": None,
        }
        _SESSIONS[sid] = s
    s["last_seen"] = _now()
    return sid, s


def _push_history(session: Dict[str, Any], role: str, content: str):
    session["history"].append({"role": role, "content": content})
    if len(session["history"]) > MAX_HISTORY_MSGS:
        session["history"] = session["history"][-MAX_HISTORY_MSGS:]


# ============================================================
# NORMALIZE / HELPERS
# ============================================================
def _norm(s: str) -> str:
    return re.sub(r"\s+", " ", (s or "").strip())


def _low(s: str) -> str:
    return (s or "").lower()


def _looks_like_url(s: str) -> bool:
    return bool(re.search(r"https?://", s or ""))


def _pick_base_url(req: ChatRunRequest, session: Dict[str, Any], prompt: str) -> Optional[str]:
    if req.base_url and req.base_url.strip():
        return req.base_url.strip()

    if _looks_like_url(prompt):
        m = re.search(r"(https?://[^\s]+)", prompt)
        if m:
            return m.group(1).strip().rstrip(").,")

    p = _low(prompt)
    if session.get("last_url") and any(x in p for x in ["la misma", "mismo sitio", "misma página", "ahí", "en esa página"]):
        return str(session["last_url"])

    if session.get("last_url") and any(x in p for x in ["ahora", "también", "en la misma", "en esa", "siguiente", "luego", "después"]):
        return str(session["last_url"])

    return None


def _ensure_goto(steps: List[Dict[str, Any]], base_url: str):
    if not steps:
        return
    has_goto = any(str(s.get("action", "")).lower() == "goto" for s in steps)
    if not has_goto:
        steps.insert(0, {"action": "goto", "url": base_url})


def _update_last_url_from_steps(session: Dict[str, Any], steps: List[Dict[str, Any]], fallback: Optional[str] = None):
    for s in steps:
        if str(s.get("action") or "").lower() == "goto" and s.get("url"):
            session["last_url"] = s["url"]
            return
    if fallback:
        session["last_url"] = fallback


# ============================================================
# INTENT ROUTING
# ============================================================
# Nota: NO incluyas "login" solo como trigger de ejecución.
_EXEC_VERBS = [
    "ve a", "ir a", "entra a", "abrir", "abre", "navega",
    "click", "clic", "haz click", "presiona",
    "inicia sesión", "haz login", "logueate", "log in",
    "valida en la página", "valida en la web", "verifica en la página",
    "llenar", "fill", "escribe", "selecciona",
    "ejecuta", "corre", "run", "playwright",
]

_DOC_HINTS = [
    "invest", "gherkin", "criterios de aceptación", "casos de prueba",
    "matriz", "test cases", "matriz de casos",
    "scripts", "automatización", "page object", "p.o.m",
]

_ADVISE_HINTS = [
    "qué haces", "que haces", "qué puedes", "que puedes", "recomiendas",
    "riesgos", "mejor práctica", "best practice", "ayúdame a", "explica",
]


def _wants_doc(prompt: str) -> bool:
    p = _low(prompt)
    return any(k in p for k in _DOC_HINTS)


def _wants_execute_explicit(prompt: str) -> bool:
    p = _low(prompt)

    # si no hay ningún verbo de ejecución, no ejecutar
    if not any(v in p for v in _EXEC_VERBS):
        return False

    # Para que sea "explícito", pedimos al menos UNA de estas señales:
    # - trae URL
    # - o menciona una acción UI concreta (click/llenar/presiona)
    has_url = _looks_like_url(prompt)
    has_ui_action = any(x in p for x in ["click", "clic", "haz click", "presiona", "fill", "llenar", "escribe", "selecciona"])
    has_exec_word = any(x in p for x in ["ejecuta", "corre", "run", "playwright", "navega", "abre", "entra a"])

    return has_url or has_ui_action or has_exec_word


def _wants_execute_followup(prompt: str, session: Dict[str, Any]) -> bool:
    p = _low(prompt)
    if session.get("last_url") and any(x in p for x in ["ahora", "también", "en la misma", "en esa", "luego", "después", "siguiente"]):
        if any(x in p for x in ["valida", "verifica", "que exista", "visible", "texto", "aparezca", "error", "mensaje", "botón", "campo"]):
            return True
    return False


def _wants_advise(prompt: str) -> bool:
    p = _low(prompt)
    # si es doc o execute, no es advise
    if _wants_doc(prompt):
        return False
    if _wants_execute_explicit(prompt):
        return False
    return any(k in p for k in _ADVISE_HINTS) or True  # default advise si no hay otra intención


def _is_check_only(prompt: str) -> bool:
    p = _low(prompt)
    return any(x in p for x in ["solo valida", "solo verificar", "solo comprueba", "únicamente valida", "sin hacer click", "sin iniciar sesión"])


# ============================================================
# DOC: “solo lo pedido” (para velocidad)
# ============================================================
def _doc_requested_parts(prompt: str) -> Dict[str, bool]:
    p = _low(prompt)
    wants_invest = ("invest" in p) or ("evalúa" in p and "historia" in p) or ("brechas" in p)
    wants_gherkin = ("gherkin" in p) or ("criterios de aceptación" in p)
    wants_cases = ("casos de prueba" in p) or ("matriz" in p) or ("test cases" in p)
    wants_scripts = ("script" in p) or ("automat" in p) or ("playwright" in p) or ("p.o.m" in p) or ("page object" in p)

    # si pide “artefactos” sin especificar, damos todo (pero limitado)
    if any(x in p for x in ["artefactos", "todo", "completo", "full"]):
        return {"invest": True, "gherkin": True, "cases": True, "scripts": True}

    # si no detectamos nada, por seguridad: gherkin + cases
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
    if "ecommerce" in p or "e-commerce" in p or "tienda en línea" in p:
        return "ecommerce"
    if "erp" in p or "oracle" in p or "sap" in p:
        return "erp"
    return DOC_DEFAULT_DOMAIN


def _cache_get(key: str) -> Optional[Dict[str, Any]]:
    return _DOC_CACHE.get(key)


def _cache_set(key: str, value: Dict[str, Any]):
    if key in _DOC_CACHE:
        _DOC_CACHE[key] = value
        return
    _DOC_CACHE[key] = value
    _DOC_CACHE_ORDER.append(key)
    while len(_DOC_CACHE_ORDER) > DOC_CACHE_MAX:
        old = _DOC_CACHE_ORDER.pop(0)
        _DOC_CACHE.pop(old, None)


# ============================================================
# SYSTEM PROMPTS (compactos para velocidad, + robust locators)
# ============================================================
SYSTEM_PROMPT = """Eres Vanya, un Agente de QA. Responde claro y directo.
Modos:
- ADVISE: si el usuario hace preguntas teóricas o pide ejemplos (INVEST, Gherkin, casos, scripts).
- EXECUTE: si el usuario pide acciones/validaciones en una web real (ve a, valida en la página, click, etc.), debes generar pasos y ejecutar con run_qa_test.
No te contradigas: sí puedes ejecutar pruebas web cuando el usuario lo pide claramente.
"""

SYSTEM_PROMPT_EXECUTE = """Eres Vanya. Cuando el usuario pida validar/navegar/click/login, DEBES devolver un tool-call a run_qa_test.

OBJETIVO: pasos simples, estables, con mínimos timeouts.

REGLAS DE SELECTORES (orden de preferencia):
1) role + text (preferido, muy estable). Ejemplos:
   - click: {"action":"click","role":"button","text":"Login"}
   - fill:  {"action":"fill","role":"textbox","text":"Username","value":"standard_user"}
   - assert_visible: {"action":"assert_visible","role":"button","text":"Login"}
2) selector CSS (solo si es claramente estable): data-testid, #id, [name=...]
3) text solo (fallback): {"action":"click","text":"Login"}

REGLAS DE CONTEXTO:
- Si el usuario NO da URL y existe base_url (contexto), usa esa URL.
- Si el usuario dice “en la misma página / ahora / también”, NO pidas URL: usa base_url.

REGLAS DE PASOS:
- Si hay URL, el primer paso debe ser goto.
- Acciones permitidas: goto, fill, click, press, assert_visible, assert_text_contains, wait_ms.
- Usa timeout_ms 20000 por defecto si no se especifica.
- No pidas permiso extra si la instrucción ya es clara.

OUTPUT:
- Devuelve SOLO el tool-call run_qa_test con steps. Sin explicación larga.
"""

SYSTEM_PROMPT_DOC = """Eres Vanya. Generas artefactos QA (INVEST, Gherkin, casos de prueba y scripts Playwright Python).
Enfócate en lo que el usuario pide (si pide solo Gherkin, no inventes scripts).
Si faltan datos:
- agrega assumptions (supuestos razonables)
- agrega questions_to_clarify (preguntas mínimas)
Devuelve SIEMPRE un tool-call generate_qa_artifacts con campos completos (vacíos si no aplican).
Manténlo concreto, sin relleno.
NO pidas URL/credenciales cuando el usuario pidió documentación (matriz/gherkin/casos).
"""

# ============================================================
# TOOL: RUNNER (Playwright steps)
# ============================================================
QA_TOOL = {
    "type": "function",
    "function": {
        "name": "run_qa_test",
        "description": "Ejecuta acciones en un navegador real para validar una web.",
        "parameters": {
            "type": "object",
            "properties": {
                "steps": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "action": {
                                "type": "string",
                                "enum": [
                                    "goto",
                                    "fill",
                                    "click",
                                    "press",
                                    "assert_visible",
                                    "assert_text_contains",
                                    "wait_ms",
                                ],
                            },
                            "url": {"type": "string"},
                            "selector": {"type": "string"},
                            "text": {"type": "string"},
                            "role": {
                                "type": "string",
                                "description": "Accessible role (e.g., button, textbox, link, heading, checkbox). Preferir role+text para estabilidad."
                            },
                            "value": {"type": "string"},
                            "timeout_ms": {"type": "integer"},
                        },
                        "required": ["action"],
                    },
                }
            },
            "required": ["steps"],
        },
    },
}

# ============================================================
# TOOL: QA DOC ARTIFACTS
# ============================================================
QA_DOC_TOOL = {
    "type": "function",
    "function": {
        "name": "generate_qa_artifacts",
        "description": "Genera artefactos QA desde una historia o requerimiento textual.",
        "parameters": {
            "type": "object",
            "properties": {
                "requested": {
                    "type": "object",
                    "properties": {
                        "invest": {"type": "boolean"},
                        "gherkin": {"type": "boolean"},
                        "cases": {"type": "boolean"},
                        "scripts": {"type": "boolean"},
                    },
                    "required": ["invest", "gherkin", "cases", "scripts"],
                },
                "user_story": {"type": "string"},
                "domain": {"type": "string"},
                "context": {"type": "string"},
                "assumptions": {"type": "array", "items": {"type": "string"}},
                "questions_to_clarify": {"type": "array", "items": {"type": "string"}},

                "invest": {
                    "type": "object",
                    "properties": {
                        "scores": {
                            "type": "object",
                            "properties": {
                                "independent": {"type": "integer"},
                                "negotiable": {"type": "integer"},
                                "valuable": {"type": "integer"},
                                "estimable": {"type": "integer"},
                                "small": {"type": "integer"},
                                "testable": {"type": "integer"},
                            },
                        },
                        "total": {"type": "integer"},
                        "verdict": {"type": "string"},
                        "gaps": {"type": "array", "items": {"type": "string"}},
                        "improved_story": {"type": "string"},
                    },
                },

                "gherkin": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "tag": {"type": "string"},
                            "scenario": {"type": "string"},
                            "given": {"type": "array", "items": {"type": "string"}},
                            "when": {"type": "array", "items": {"type": "string"}},
                            "then": {"type": "array", "items": {"type": "string"}},
                        },
                        "required": ["scenario", "given", "when", "then"],
                    },
                },

                "test_cases": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": {"type": "string"},
                            "priority": {"type": "string"},
                            "type": {"type": "string"},
                            "automatable": {"type": "boolean"},
                            "title": {"type": "string"},
                            "preconditions": {"type": "array", "items": {"type": "string"}},
                            "steps": {"type": "array", "items": {"type": "string"}},
                            "expected": {"type": "string"},
                        },
                        "required": ["id", "priority", "type", "automatable", "title", "steps", "expected"],
                    },
                },

                "automation_scripts": {
                    "type": "object",
                    "properties": {
                        "framework": {"type": "string"},
                        "structure": {"type": "string"},
                        "notes": {"type": "array", "items": {"type": "string"}},
                        "selectors_recommendation": {"type": "array", "items": {"type": "string"}},
                        "how_to_run": {"type": "array", "items": {"type": "string"}},
                        "files": {
                            "type": "array",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "path": {"type": "string"},
                                    "content": {"type": "string"},
                                },
                                "required": ["path", "content"],
                            },
                        },
                    },
                },
            },
            "required": [
                "requested",
                "user_story",
                "domain",
                "context",
                "assumptions",
                "questions_to_clarify",
                "gherkin",
                "test_cases",
                "automation_scripts",
            ],
        },
    },
}

# ============================================================
# DOC renderer (markdown para frontend)
# ============================================================
def _md_escape(s: str) -> str:
    return (s or "").replace("|", "\\|").replace("`", "\\`")


def _render_doc_answer(doc: Dict[str, Any]) -> str:
    req = doc.get("requested") or {}
    story = doc.get("user_story") or ""
    domain = doc.get("domain") or ""
    context = doc.get("context") or ""
    assumptions = doc.get("assumptions") or []
    questions = doc.get("questions_to_clarify") or []

    out = []
    out.append(f"## 📌 Input\n**Dominio:** `{_md_escape(domain)}`  \n**Historia / Requerimiento:** {_md_escape(story)}")
    if context:
        out.append(f"\n**Contexto:** {_md_escape(context)}")

    if assumptions:
        out.append("\n## 🧩 Assumptions\n" + "\n".join([f"- {_md_escape(a)}" for a in assumptions]))
    if questions:
        out.append("\n## ❓ Preguntas mínimas\n" + "\n".join([f"- {_md_escape(q)}" for q in questions]))

    if req.get("invest"):
        inv = doc.get("invest") or {}
        scores = (inv.get("scores") or {})
        out.append("\n## ✅ INVEST")
        out.append(
            "| Criterio | Score (0-2) |\n|---|---:|\n"
            f"| Independent | {scores.get('independent','—')} |\n"
            f"| Negotiable | {scores.get('negotiable','—')} |\n"
            f"| Valuable | {scores.get('valuable','—')} |\n"
            f"| Estimable | {scores.get('estimable','—')} |\n"
            f"| Small | {scores.get('small','—')} |\n"
            f"| Testable | {scores.get('testable','—')} |\n"
        )
        out.append(f"\n**Total:** {inv.get('total','—')}  \n**Veredicto:** {inv.get('verdict','—')}")
        gaps = inv.get("gaps") or []
        out.append("\n### Brechas\n" + ("\n".join([f"- {_md_escape(g)}" for g in gaps]) if gaps else "- _(sin brechas)_"))
        if inv.get("improved_story"):
            out.append("\n### Historia reescrita\n" + _md_escape(inv["improved_story"]))

    if req.get("gherkin"):
        gherkin = doc.get("gherkin") or []
        out.append("\n## 🥒 Criterios de aceptación (Gherkin)")
        for sc in gherkin:
            tag = sc.get("tag")
            if tag:
                out.append(f"\n@{_md_escape(tag)}")
            out.append(f"Scenario: {_md_escape(sc.get('scenario',''))}")
            for x in sc.get("given", [])[:8]:
                out.append(f"  Given {_md_escape(x)}")
            for x in sc.get("when", [])[:8]:
                out.append(f"  When {_md_escape(x)}")
            for x in sc.get("then", [])[:10]:
                out.append(f"  Then {_md_escape(x)}")

    if req.get("cases"):
        tcs = doc.get("test_cases") or []
        out.append("\n## 🧪 Matriz de casos de prueba")
        out.append("| ID | Prio | Tipo | Auto | Caso |\n|---|---|---|---:|---|")
        for tc in tcs:
            out.append(
                f"| {_md_escape(tc.get('id',''))} | {_md_escape(tc.get('priority',''))} | {_md_escape(tc.get('type',''))} | "
                f"{'✅' if tc.get('automatable') else '—'} | {_md_escape(tc.get('title',''))} |"
            )

    if req.get("scripts"):
        scr = doc.get("automation_scripts") or {}
        out.append("\n## 🤖 Automatización (Playwright Python)")
        out.append(f"**Framework:** `{_md_escape(scr.get('framework','pytest + playwright'))}`  \n**Estructura:** `{_md_escape(scr.get('structure','Page Object Model'))}`")
        if scr.get("how_to_run"):
            out.append("\n### Cómo correr\n" + "\n".join([f"- {_md_escape(x)}" for x in scr["how_to_run"]]))
        if scr.get("selectors_recommendation"):
            out.append("\n### Recomendación de selectores\n" + "\n".join([f"- `{_md_escape(x)}`" for x in scr["selectors_recommendation"]]))
        files = scr.get("files") or []
        if files:
            out.append("\n### Archivos generados")
            for f in files:
                out.append(f"- `{_md_escape(f.get('path',''))}`")

    return "\n".join(out).strip()


def _truncate_code(s: str) -> str:
    s = s or ""
    if len(s) <= DOC_MAX_CODE_CHARS:
        return s
    return s[:DOC_MAX_CODE_CHARS] + "\n# ... (truncado por límite de tamaño)\n"


def _trim_doc(doc: Dict[str, Any]) -> Dict[str, Any]:
    gherkin = doc.get("gherkin") or []
    if isinstance(gherkin, list) and len(gherkin) > DOC_MAX_GHERKIN:
        doc["gherkin"] = gherkin[:DOC_MAX_GHERKIN]

    tcs = doc.get("test_cases") or []
    if isinstance(tcs, list) and len(tcs) > DOC_MAX_TEST_CASES:
        doc["test_cases"] = tcs[:DOC_MAX_TEST_CASES]

    scr = doc.get("automation_scripts") or {}
    files = scr.get("files") or []
    if isinstance(files, list) and len(files) > DOC_MAX_FILES:
        files = files[:DOC_MAX_FILES]

    for f in files:
        if isinstance(f, dict) and "content" in f:
            f["content"] = _truncate_code(f.get("content", ""))

    scr["files"] = files
    doc["automation_scripts"] = scr
    return doc


# ============================================================
# ENDPOINTS
# ============================================================
@app.get("/health")
def health():
    _cleanup_sessions()
    return {"ok": True}


@app.get("/meta")
def meta():
    _cleanup_sessions()
    return {
        "ok": True,
        "render_git_commit": os.getenv("RENDER_GIT_COMMIT"),
        "model": OPENAI_MODEL,
        "has_openai_key": bool(OPENAI_API_KEY),
        "session_ttl_s": SESSION_TTL_S,
        "sessions_in_memory": len(_SESSIONS),
        "doc_cache_items": len(_DOC_CACHE),
    }


@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    _cleanup_sessions()
    sid, session = _get_session(req.session_id)

    prompt = _norm(req.prompt)
    if not prompt:
        raise HTTPException(status_code=400, detail="prompt vacío")

    client = _get_client()

    wants_execute = _wants_execute_explicit(prompt) or _wants_execute_followup(prompt, session)
    wants_doc = _wants_doc(prompt)

    # ============================================================
    # PRIORIDAD 1: DOC MODE (rápido y solo lo pedido)
    # ============================================================
    if wants_doc and not wants_execute:
        try:
            requested = _doc_requested_parts(prompt)
            domain = _infer_domain(prompt)
            story = _extract_user_story(prompt) or prompt
            context = ""  # opcional

            def _norm_cache_text(s: str, limit: int = 1200) -> str:
                s = (s or "").strip()
                s = " ".join(s.split())
                return s[:limit]

            cache_key = "v2|doc|" + "|".join([
                _norm_cache_text(domain, 120),
                json.dumps(requested, sort_keys=True),
                _norm_cache_text(story, 1200),
            ])

            cached = _cache_get(cache_key)
            if cached:
                session["doc_last"] = cached
                answer = _render_doc_answer(cached)
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", "Usé cache de artefactos QA (DOC).")
                return {"mode": "doc", "session_id": sid, "answer": answer, "doc_artifacts": cached, "cached": True}

            def _doc_history_filter(msgs):
                bad = ("NEED INFO", "Para ejecutar", "URL", "credenciales", "run_qa_test")
                out = []
                for m in msgs:
                    c = (m.get("content") or "")
                    if any(b.lower() in c.lower() for b in bad):
                        continue
                    out.append(m)
                return out

            messages = [{"role": "system", "content": SYSTEM_PROMPT_DOC}]
            hist = _doc_history_filter(session["history"][-DOC_HISTORY_MSGS:])
            messages.extend(hist)

            user_payload = (
                "Tarea: Genera artefactos QA **solo de documentación** (NO ejecutes, NO pidas URL).\n"
                "Entrega: Debes devolver **un tool-call** a generate_qa_artifacts con TODOS los campos.\n\n"
                f"REQUESTED: {requested}\n"
                f"DOMAIN: {domain}\n"
                f"USER_STORY_OR_REQUEST:\n{story}\n\n"
                "Reglas:\n"
                "- Si el usuario NO pidió ejecución, NO menciones URL/credenciales como requisito.\n"
                "- Si falta info, llena assumptions y questions_to_clarify (sin bloquear la entrega).\n"
                f"- Máximo {DOC_MAX_GHERKIN} escenarios Gherkin.\n"
                f"- Máximo {DOC_MAX_TEST_CASES} casos de prueba.\n"
                f"- Máximo {DOC_MAX_FILES} archivos de código, y cada uno <= {DOC_MAX_CODE_CHARS} chars.\n"
                "- Si el usuario pidió solo Gherkin, NO generes scripts.\n"
                "- Si el usuario pidió solo matriz/casos, NO generes Gherkin.\n"
                "- Evita texto extra fuera del tool-call.\n"
            )
            messages.append({"role": "user", "content": user_payload})

            resp = client.chat.completions.create(
                model=OPENAI_MODEL,
                messages=messages,
                tools=[QA_DOC_TOOL],
                tool_choice={"type": "function", "function": {"name": "generate_qa_artifacts"}},
                temperature=DOC_TEMPERATURE,
                max_tokens=DOC_MAX_TOKENS,
            )

            msg = resp.choices[0].message
            tool_calls = getattr(msg, "tool_calls", None) or []

            # fallback DOC mínimo (sin toolcall)
            if not tool_calls:
                minimal_doc = {
                    "requested": requested,
                    "domain": domain,
                    "context": context,
                    "user_story": story,
                    "assumptions": ["No se proporcionaron reglas de password/lockout específicas."],
                    "questions_to_clarify": ["¿Qué políticas de seguridad aplican? (MFA, lockout, rate-limit, captcha)"],
                    "invest": {},
                    "gherkin": [],
                    "test_cases": [
                        {
                            "id": "TC-LOGIN-001",
                            "title": "Login exitoso",
                            "priority": "P0",
                            "type": "pos",
                            "automatable": True,
                            "preconditions": ["Usuario registrado y activo"],
                            "steps": ["Abrir login", "Capturar email válido", "Capturar password válida", "Click ingresar"],
                            "expected": "Redirige a home/cuenta y muestra sesión iniciada"
                        },
                        {
                            "id": "TC-LOGIN-002",
                            "title": "Password incorrecta",
                            "priority": "P0",
                            "type": "neg",
                            "automatable": True,
                            "preconditions": ["Usuario registrado"],
                            "steps": ["Abrir login", "Email válido", "Password inválida", "Click ingresar"],
                            "expected": "Mensaje de error y no inicia sesión"
                        },
                    ],
                    "automation_scripts": {
                        "framework": "",
                        "structure": "",
                        "notes": [],
                        "selectors_recommendation": [],
                        "how_to_run": [],
                        "files": []
                    }
                }
                minimal_doc = _trim_doc(minimal_doc)
                session["doc_last"] = minimal_doc
                _cache_set(cache_key, minimal_doc)

                answer = _render_doc_answer(minimal_doc)
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", "Fallback DOC mínimo (sin tool-call).")
                return {"mode": "doc", "session_id": sid, "answer": answer, "doc_artifacts": minimal_doc, "cached": False}

            args = json.loads(tool_calls[0].function.arguments)

            # patch mínimos
            args.setdefault("requested", requested)
            args.setdefault("user_story", story)
            args.setdefault("domain", domain)
            args.setdefault("context", context)
            args.setdefault("assumptions", [])
            args.setdefault("questions_to_clarify", [])
            args.setdefault("invest", {})
            args.setdefault("gherkin", [])
            args.setdefault("test_cases", [])
            args.setdefault("automation_scripts", {
                "framework": "pytest + playwright",
                "structure": "Page Object Model",
                "notes": [],
                "selectors_recommendation": [],
                "how_to_run": [],
                "files": []
            })

            # Anti-alucinación: si NO pidieron scripts, vacía scripts
            if not bool(requested.get("scripts", False)):
                args["automation_scripts"] = {
                    "framework": "",
                    "structure": "",
                    "notes": [],
                    "selectors_recommendation": [],
                    "how_to_run": [],
                    "files": []
                }

            # Si NO pidieron gherkin/cases, limpia para no inventar
            if not bool(requested.get("gherkin", False)):
                args["gherkin"] = []
            if not bool(requested.get("cases", False)):
                args["test_cases"] = []

            doc = _trim_doc(args)

            session["doc_last"] = doc
            _cache_set(cache_key, doc)

            answer = _render_doc_answer(doc)
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", "Generé artefactos QA (DOC).")
            return {"mode": "doc", "session_id": sid, "answer": answer, "doc_artifacts": doc, "cached": False}

        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # ============================================================
    # PRIORIDAD 2: Asesoría (rápida)
    # ============================================================
    if not wants_execute:
        try:
            p_low = _low(prompt)

            # soporte “dame los scripts” desde doc_last
            if session.get("doc_last") and any(x in p_low for x in ["dame los scripts", "muéstrame los scripts", "archivos", "código", "playwright"]):
                doc = session["doc_last"]
                answer = "Listo. Los scripts están en **doc_artifacts.automation_scripts.files** (path + content)."
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", answer)
                return {"mode": "doc", "session_id": sid, "answer": answer, "doc_artifacts": doc}

            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            if session.get("last_url"):
                messages.append({"role": "system", "content": f"Contexto: last_url={session['last_url']}"})
            messages.extend(session["history"][-4:])
            messages.append({"role": "user", "content": prompt})

            resp = client.chat.completions.create(
                model=OPENAI_MODEL,
                messages=messages,
                temperature=ADV_TEMPERATURE,
                max_tokens=ADV_MAX_TOKENS,
            )
            msg = resp.choices[0].message
            answer = getattr(msg, "content", None) or "Ok. ¿Qué necesitas?"
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)
            return {"mode": "advise", "session_id": sid, "answer": answer}

        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # ============================================================
    # PRIORIDAD 3: EXECUTION MODE (Playwright)
    # ============================================================
    base_url = _pick_base_url(req, session, prompt)
    if not base_url:
        return {
            "mode": "need_info",
            "session_id": sid,
            "answer": "Para ejecutar necesito la URL (o dime “la misma” si quieres usar la última) y qué validar exactamente.",
        }

    try:
        messages = [{"role": "system", "content": SYSTEM_PROMPT_EXECUTE}]
        messages.append({"role": "system", "content": f"Contexto: base_url={base_url}; check_only={_is_check_only(prompt)}"})
        messages.extend(session["history"][-4:])
        messages.append({"role": "user", "content": prompt})

        resp = client.chat.completions.create(
            model=OPENAI_MODEL,
            messages=messages,
            tools=[QA_TOOL],
            tool_choice={"type": "function", "function": {"name": "run_qa_test"}},
            temperature=EXEC_TEMPERATURE,
            max_tokens=EXEC_MAX_TOKENS,
        )

        msg = resp.choices[0].message
        tool_calls = getattr(msg, "tool_calls", None) or []
        if not tool_calls:
            return {
                "mode": "need_info",
                "session_id": sid,
                "answer": "Puedo ejecutar, pero necesito más claridad. Dime qué elemento validar (ej: “campo password visible”) o qué texto debe aparecer.",
            }

        args = json.loads(tool_calls[0].function.arguments)
        steps = args.get("steps", [])
        if not isinstance(steps, list) or not steps:
            return {
                "mode": "need_info",
                "session_id": sid,
                "answer": "Entendí que quieres ejecutar, pero no pude generar pasos. Repite indicando URL + validación concreta.",
            }

        _ensure_goto(steps, base_url)
        _update_last_url_from_steps(session, steps, fallback=base_url)

        run_result = execute_test(steps, headless=req.headless)
        status = (run_result.get("status") or "").lower()
        resumen = "PASSED" if status in ("passed", "pass", "ok") else "FAILED"

        _push_history(session, "user", prompt)
        _push_history(session, "assistant", f"Ejecuté la prueba. Resultado: {resumen}")

        return {
            "mode": "execute",
            "session_id": sid,
            "generated_steps": steps,
            "run_result": run_result,
        }

    except Exception as e:
        traceback.print_exc()
        raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")