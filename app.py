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
DOC_DEFAULT_DOMAIN = (os.getenv("DOC_DEFAULT_DOMAIN") or "retail").strip()
DOC_MAX_TEST_CASES = int(os.getenv("DOC_MAX_TEST_CASES", "18"))  # límite para no explotar tokens

def _now() -> int:
    return int(time.time())

def _get_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=OPENAI_API_KEY)

# ============================================================
# REQUEST MODEL
# ============================================================
class ChatRunRequest(BaseModel):
    prompt: str
    session_id: Optional[str] = None
    headless: bool = True
    base_url: Optional[str] = None  # opcional

# ============================================================
# SESSION MEMORY (history + last_url + ttl + doc_artifacts)
# ============================================================
_SESSIONS: Dict[str, Dict[str, Any]] = {}

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
            "history": [],          # list[{role, content}]
            "last_url": None,       # last used url
            "last_seen": _now(),
            "doc_last": None,       # last generated doc artifacts (dict)
        }
        _SESSIONS[sid] = s
    s["last_seen"] = _now()
    return sid, s

def _push_history(session: Dict[str, Any], role: str, content: str):
    session["history"].append({"role": role, "content": content})
    if len(session["history"]) > MAX_HISTORY_MSGS:
        session["history"] = session["history"][-MAX_HISTORY_MSGS:]

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
                                    "wait_for_selector",
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
# TOOL: QA DOC ARTIFACTS (INVEST + Gherkin + TestCases + Scripts)
# ============================================================
QA_DOC_TOOL = {
    "type": "function",
    "function": {
        "name": "generate_qa_artifacts",
        "description": "Genera artefactos QA (INVEST, Gherkin, casos, scripts Playwright Python) desde una historia de usuario.",
        "parameters": {
            "type": "object",
            "properties": {
                "user_story": {"type": "string"},
                "context": {"type": "string"},
                "domain": {"type": "string", "description": "Ej: retail, POS, ecommerce, ERP"},
                "assumptions": {"type": "array", "items": {"type": "string"}},
                "invest": {
                    "type": "object",
                    "properties": {
                        "scores": {
                            "type": "object",
                            "properties": {
                                "independent": {"type": "integer", "minimum": 0, "maximum": 2},
                                "negotiable": {"type": "integer", "minimum": 0, "maximum": 2},
                                "valuable": {"type": "integer", "minimum": 0, "maximum": 2},
                                "estimable": {"type": "integer", "minimum": 0, "maximum": 2},
                                "small": {"type": "integer", "minimum": 0, "maximum": 2},
                                "testable": {"type": "integer", "minimum": 0, "maximum": 2},
                            },
                            "required": ["independent","negotiable","valuable","estimable","small","testable"]
                        },
                        "total": {"type": "integer"},
                        "verdict": {"type": "string"},
                        "issues": {"type": "array", "items": {"type": "string"}},
                        "improved_story": {"type": "string"},
                        "questions_to_clarify": {"type": "array", "items": {"type": "string"}},
                    },
                    "required": ["scores","total","verdict","issues","improved_story","questions_to_clarify"]
                },
                "gherkin": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "feature": {"type": "string"},
                            "scenario": {"type": "string"},
                            "tags": {"type": "array", "items": {"type": "string"}},
                            "given": {"type": "array", "items": {"type": "string"}},
                            "when": {"type": "array", "items": {"type": "string"}},
                            "then": {"type": "array", "items": {"type": "string"}},
                        },
                        "required": ["feature","scenario","given","when","then"]
                    }
                },
                "test_cases": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": {"type": "string"},
                            "title": {"type": "string"},
                            "type": {"type": "string", "description": "positive|negative|edge|security|performance"},
                            "priority": {"type": "string", "description": "P0|P1|P2|P3"},
                            "preconditions": {"type": "array", "items": {"type": "string"}},
                            "steps": {"type": "array", "items": {"type": "string"}},
                            "expected": {"type": "array", "items": {"type": "string"}},
                            "automation_candidate": {"type": "boolean"},
                            "notes": {"type": "string"},
                        },
                        "required": ["id","title","type","priority","preconditions","steps","expected","automation_candidate"]
                    }
                },
                "automation_scripts": {
                    "type": "object",
                    "properties": {
                        "framework": {"type": "string", "description": "playwright-python"},
                        "structure": {"type": "string", "description": "page-object"},
                        "files": {
                            "type": "array",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "path": {"type": "string"},
                                    "content": {"type": "string"}
                                },
                                "required": ["path","content"]
                            }
                        },
                        "run_instructions": {"type": "array", "items": {"type": "string"}},
                        "selector_recommendations": {"type": "array", "items": {"type": "string"}}
                    },
                    "required": ["framework","structure","files","run_instructions","selector_recommendations"]
                }
            },
            "required": ["user_story","context","domain","assumptions","invest","gherkin","test_cases","automation_scripts"],
        },
    },
}

# ============================================================
# PROMPTS
# ============================================================
SYSTEM_PROMPT = """
Eres Vanya, una Agente de QA Inteligente capaz de ASESORAR y EJECUTAR pruebas.

DOS MODOS:
A) Modo Asesor (sin ejecutar):
- Respondes preguntas, matrices de casos, criterios, riesgos, recomendaciones.
- No ejecutes nada si el usuario solo pregunta teoría o pide casos.

B) Modo Runner (con ejecución):
- SOLO ejecutas cuando el usuario lo pide explícitamente (ej. "ve a", "abre", "navega", "entra", "ejecuta", "corre")
  o cuando es un FOLLOW-UP de validación y ya existe una última URL en sesión.

MEMORIA:
- Si el usuario no menciona URL pero dice "la misma" y hay last_url, reusa last_url.

SELECTORES:
- Prefiere selectores robustos.
- En SauceDemo usa: #user-name, #password, #login-button, y para "Products" usa span.title (NO uses h1).

REGLAS:
- Si piden "existe/visible" (check-only), NO hagas login ni pasos extra.
- Si falta información para ejecutar, pide lo mínimo (URL/validación/credenciales si aplica).
"""

SYSTEM_PROMPT_DOC = f"""
Eres Vanya, QA Lead + QA Automation Engineer (Playwright Python) especializada en {DOC_DEFAULT_DOMAIN}.

OBJETIVO: generar artefactos de QA accionables y superiores:
1) INVEST: califica 0-2 por criterio + explica brechas + reescribe historia (mejorada) y lista preguntas mínimas.
2) Gherkin: escenarios claros con positivos/negativos/edge; sin redundancia; con tags útiles (@p0, @neg, @edge, @pos).
3) Casos de prueba: lista detallada con prioridad P0-P3, automatizable sí/no, datos y notas; incluye POS/retail: caídas de red, reintentos, permisos por rol, fallos de pago.
4) Automatización: genera Playwright Python con Page Object Model:
   - pytest + playwright sync
   - data-driven (json)
   - assertions robustas
   - evita sleeps; usa expect/locator + timeouts
   - recomienda selectores: data-testid, role, aria-label, ids

REGLAS:
- No inventes requisitos. Si falta info, colócalo en questions_to_clarify y declara assumptions.
- No uses casos genéricos tipo "verificar que funciona".
- Limita test_cases a máximo {DOC_MAX_TEST_CASES}.
- Devuelve SIEMPRE por tool_call generate_qa_artifacts (JSON).
"""

# ============================================================
# INTENT (routing)
# ============================================================
_URL_RE = re.compile(r"https?://\S+", re.IGNORECASE)

_EXECUTE_VERBS = [
    r"\bve a\b",
    r"\babre\b",
    r"\bnavega\b",
    r"\bentra\b",
    r"\bejecuta\b",
    r"\bcorre\b",
    r"\brun\b",
]

_FOLLOWUP_EXECUTE_HINTS = [
    r"\bla misma\b",
    r"\ben la misma\b",
    r"\bahora valida\b",
    r"\bvalida\b",
    r"\bverifica\b",
    r"\bconfirma\b",
    r"\brevisa\b",
    r"\bintenta\b",
]

_ADVISE_HINTS = [
    r"\bmatriz\b",
    r"\bcasos?\b",
    r"\bescenarios?\b",
    r"\bcriterios?\b",
    r"\bchecklist\b",
    r"\briesgos?\b",
    r"\brecomendaciones?\b",
    r"\bqué haces\b",
    r"\bque haces\b",
    r"\bqué puedes\b",
    r"\bque puedes\b",
    r"\bcuándo\b",
    r"\bcuando\b",
    r"\bdeber[ií]a\b",
]

# DOC MODE triggers (para superar a Valkiria)
_DOC_HINTS = [
    r"\binvest\b",
    r"\bgherkin\b",
    r"\bhistoria de usuario\b",
    r"\buser story\b",
    r"\bcriterios de aceptaci[oó]n\b",
    r"\bmatriz\b",
    r"\bcasos de prueba\b",
    r"\btest cases\b",
    r"\bscripts?\b",
    r"\bautomatizaci[oó]n\b",
    r"\bselenium\b",
    r"\bplaywright\b",
]

_CHECK_ONLY_HINTS = [
    r"\bexista\b",
    r"\bexiste\b",
    r"\bvisible\b",
    r"\bpresente\b",
    r"\bse vea\b",
]

_FLOW_HINTS = [
    r"\blogin\b",
    r"\binicia sesi[oó]n\b",
    r"\bsign in\b",
    r"\bcheckout\b",
    r"\bcomprar\b",
    r"\bagregar al carrito\b",
]

def _has_url(text: str) -> bool:
    return bool(_URL_RE.search(text or ""))

def _wants_doc(prompt: str) -> bool:
    p = (prompt or "").lower()
    return any(re.search(h, p) for h in _DOC_HINTS)

def _wants_advise(prompt: str) -> bool:
    p = (prompt or "").lower()
    return any(re.search(h, p) for h in _ADVISE_HINTS)

def _wants_execute_explicit(prompt: str) -> bool:
    p = (prompt or "").lower()
    return any(re.search(h, p) for h in _EXECUTE_VERBS)

def _wants_execute_followup(prompt: str, session: Dict[str, Any]) -> bool:
    if not session.get("last_url"):
        return False
    p = (prompt or "").lower()
    return any(re.search(h, p) for h in _FOLLOWUP_EXECUTE_HINTS)

def _is_check_only(prompt: str) -> bool:
    p = (prompt or "").lower()
    has_check = any(re.search(h, p) for h in _CHECK_ONLY_HINTS)
    has_flow = any(re.search(h, p) for h in _FLOW_HINTS)
    return bool(has_check and not has_flow)

# ============================================================
# Tool-call parsing
# ============================================================
def _extract_steps_from_tool_calls(tool_calls: Any) -> Optional[List[Dict[str, Any]]]:
    if not tool_calls:
        return None
    for tc in tool_calls:
        fn = getattr(tc, "function", None)
        if not fn:
            continue
        if getattr(fn, "name", None) != "run_qa_test":
            continue
        args_raw = getattr(fn, "arguments", "") or ""
        try:
            args = json.loads(args_raw) if isinstance(args_raw, str) else (args_raw or {})
        except Exception:
            args = {}
        steps = args.get("steps")
        if isinstance(steps, list):
            return steps
    return None

def _extract_doc_from_tool_calls(tool_calls: Any) -> Optional[Dict[str, Any]]:
    if not tool_calls:
        return None
    for tc in tool_calls:
        fn = getattr(tc, "function", None)
        if not fn:
            continue
        if getattr(fn, "name", None) != "generate_qa_artifacts":
            continue
        args_raw = getattr(fn, "arguments", "") or ""
        try:
            args = json.loads(args_raw) if isinstance(args_raw, str) else (args_raw or {})
        except Exception:
            args = {}
        if isinstance(args, dict) and args.get("invest") and args.get("gherkin") and args.get("test_cases"):
            return args
    return None

def _ensure_goto(steps: List[Dict[str, Any]], url: str):
    if not any(str(s.get("action") or "").lower() == "goto" for s in steps):
        steps.insert(0, {"action": "goto", "url": url})

def _update_last_url_from_steps(session: Dict[str, Any], steps: List[Dict[str, Any]]):
    for s in steps:
        if str(s.get("action") or "").lower() == "goto" and s.get("url"):
            session["last_url"] = s["url"]
            return

def _pick_base_url(req: ChatRunRequest, session: Dict[str, Any], prompt: str) -> Optional[str]:
    m = _URL_RE.search(prompt or "")
    if m:
        return m.group(0)
    if req.base_url:
        return req.base_url.strip()
    if session.get("last_url"):
        return str(session["last_url"])
    return None

# ============================================================
# Formatting helpers (backend -> frontend text)
# ============================================================
def _md_escape(s: Any) -> str:
    return str(s or "").replace("\r\n", "\n").strip()

def _render_doc_answer(doc: Dict[str, Any]) -> str:
    user_story = _md_escape(doc.get("user_story"))
    context = _md_escape(doc.get("context"))
    domain = _md_escape(doc.get("domain"))

    invest = doc.get("invest") or {}
    scores = (invest.get("scores") or {})
    total = invest.get("total")
    verdict = _md_escape(invest.get("verdict"))
    issues = invest.get("issues") or []
    improved_story = _md_escape(invest.get("improved_story"))
    questions = invest.get("questions_to_clarify") or []
    assumptions = doc.get("assumptions") or []

    gherkin = doc.get("gherkin") or []
    test_cases = doc.get("test_cases") or []
    scripts = doc.get("automation_scripts") or {}

    # INVEST table
    invest_table = (
        "| Criterio | Score (0-2) |\n"
        "|---|---:|\n"
        f"| Independent | {scores.get('independent','')} |\n"
        f"| Negotiable | {scores.get('negotiable','')} |\n"
        f"| Valuable | {scores.get('valuable','')} |\n"
        f"| Estimable | {scores.get('estimable','')} |\n"
        f"| Small | {scores.get('small','')} |\n"
        f"| Testable | {scores.get('testable','')} |\n"
    )

    # Gherkin blocks
    gherkin_md = []
    for sc in gherkin[:12]:
        feature = _md_escape(sc.get("feature"))
        scenario = _md_escape(sc.get("scenario"))
        tags = sc.get("tags") or []
        given = sc.get("given") or []
        when = sc.get("when") or []
        then = sc.get("then") or []
        tag_line = " ".join(tags) if tags else ""
        gherkin_md.append(
            f"**Feature:** {feature}\n\n"
            f"{(tag_line + '\\n') if tag_line else ''}"
            f"Scenario: {scenario}\n"
            + "\n".join([f"  Given {x}" for x in given])
            + ("\n" if given else "")
            + "\n".join([f"  When {x}" for x in when])
            + ("\n" if when else "")
            + "\n".join([f"  Then {x}" for x in then])
        )
    gherkin_md_str = "\n\n---\n\n".join(gherkin_md) if gherkin_md else "_(sin escenarios)_"

    # Test cases table
    tc_rows = []
    for tc in test_cases[:DOC_MAX_TEST_CASES]:
        tc_rows.append(
            "| {id} | {prio} | {typ} | {auto} | {title} |".format(
                id=_md_escape(tc.get("id")),
                prio=_md_escape(tc.get("priority")),
                typ=_md_escape(tc.get("type")),
                auto="✅" if tc.get("automation_candidate") else "—",
                title=_md_escape(tc.get("title")),
            )
        )
    tc_table = (
        "| ID | Prio | Tipo | Auto | Caso |\n"
        "|---|---|---|---:|---|\n"
        + ("\n".join(tc_rows) if tc_rows else "| — | — | — | — | — |")
    )

    # Scripts summary
    files = scripts.get("files") or []
    run_instructions = scripts.get("run_instructions") or []
    selector_recs = scripts.get("selector_recommendations") or []
    files_list = "\n".join([f"- `{_md_escape(f.get('path'))}`" for f in files[:12]]) or "- _(sin archivos)_"

    run_lines = "\n".join([f"- {_md_escape(x)}" for x in run_instructions]) or "- _(sin instrucciones)_"
    selector_lines = "\n".join([f"- {_md_escape(x)}" for x in selector_recs]) or "- _(sin recomendaciones)_"

    # NOTE: no mandamos el contenido completo de archivos como texto largo por defecto en answer,
    # pero sí lo devolvemos en JSON aparte (doc_artifacts). El front puede ofrecer "ver archivos".
    return (
        f"## 📌 Historia (original)\n{user_story}\n\n"
        f"## 🧩 Contexto / Dominio\n- **Dominio:** {domain}\n"
        f"{('- ' + context) if context else ''}\n\n"
        f"## ✅ INVEST\n{invest_table}\n"
        f"**Total:** {total}  \n"
        f"**Veredicto:** {verdict}\n\n"
        f"### Brechas detectadas\n" + ("\n".join([f"- { _md_escape(x) }" for x in issues]) if issues else "- _(sin brechas)_") + "\n\n"
        f"### Historia reescrita (mejorada)\n{improved_story}\n\n"
        f"### Supuestos (si aplica)\n" + ("\n".join([f"- { _md_escape(x) }" for x in assumptions]) if assumptions else "- _(sin supuestos)_") + "\n\n"
        f"### Preguntas mínimas para cerrar ambigüedad\n" + ("\n".join([f"- { _md_escape(x) }" for x in questions]) if questions else "- _(sin preguntas)_") + "\n\n"
        f"## 🥒 Criterios de aceptación (Gherkin)\n{gherkin_md_str}\n\n"
        f"## 🧪 Matriz de Casos de Prueba\n{tc_table}\n\n"
        f"## 🤖 Automatización (Playwright Python)\n"
        f"**Estructura:** `{_md_escape(scripts.get('structure'))}`  \n"
        f"**Framework:** `{_md_escape(scripts.get('framework'))}`\n\n"
        f"### Archivos generados\n{files_list}\n\n"
        f"### Cómo correr\n{run_lines}\n\n"
        f"### Recomendación de selectores\n{selector_lines}\n\n"
        f"Si quieres, puedo adaptar los scripts a tu app real si me dices: **URL**, y si tienes `data-testid`/IDs en tu UI."
    )

# ============================================================
# ENDPOINTS
# ============================================================
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
        "doc_default_domain": DOC_DEFAULT_DOMAIN,
    }

@app.get("/health")
def health():
    _cleanup_sessions()
    return {"ok": True}

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    _cleanup_sessions()
    sid, session = _get_session(req.session_id)

    prompt = (req.prompt or "").strip()
    if not prompt:
        raise HTTPException(status_code=400, detail="prompt vacío")

    client = _get_client()

    # -----------------------------
    # Intent routing
    # -----------------------------
    wants_doc = _wants_doc(prompt)
    wants_advise = _wants_advise(prompt)
    wants_execute = _wants_execute_explicit(prompt) or _wants_execute_followup(prompt, session)
    check_only = _is_check_only(prompt)

    # PRIORIDAD 1: DOC MODE (INVEST/Gherkin/Casos/Scripts)
    # (aunque también tenga palabras tipo "casos", aquí lo tratamos como artefactos)
    if wants_doc and not wants_execute:
        try:
            # Contexto corto (para que sea superior)
            messages = [{"role": "system", "content": SYSTEM_PROMPT_DOC}]
            # Si hay historial, ayuda a mantener tono/estándar
            if session.get("history"):
                messages.append({"role": "system", "content": "Usa estilo profesional, tablas y listas accionables. Evita texto relleno."})
            messages.extend(session["history"][-6:])

            # Empaquetamos "texto libre" como user_story y contexto vacío si no lo dan.
            # El modelo igual reescribe en improved_story.
            # Truco: si el usuario escribió "Contexto:" o "Dominio:" se lo dejamos como parte del prompt.
            doc_user_story = prompt
            doc_context = ""

            # Forzamos tool-call para consistencia
            messages.append({"role": "user", "content": doc_user_story})

            resp = client.chat.completions.create(
                model=OPENAI_MODEL,
                messages=messages,
                tools=[QA_DOC_TOOL],
                tool_choice={"type": "function", "function": {"name": "generate_qa_artifacts"}},
            )

            msg = resp.choices[0].message
            doc = _extract_doc_from_tool_calls(getattr(msg, "tool_calls", None))
            if not doc:
                # fallback seguro
                answer = (
                    "Puedo generar INVEST, Gherkin, casos de prueba y scripts Playwright Python, "
                    "pero necesito una historia de usuario (texto libre) y, si aplica, contexto de negocio."
                )
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", answer)
                return {"mode": "advise", "session_id": sid, "answer": answer}

            # Enriquecer campos mínimos si vienen vacíos (robustez)
            doc.setdefault("user_story", doc_user_story)
            doc.setdefault("context", doc_context)
            doc.setdefault("domain", DOC_DEFAULT_DOMAIN)
            # recorta test cases por seguridad
            if isinstance(doc.get("test_cases"), list) and len(doc["test_cases"]) > DOC_MAX_TEST_CASES:
                doc["test_cases"] = doc["test_cases"][:DOC_MAX_TEST_CASES]

            # guardamos en sesión para "dame los scripts" en follow-up
            session["doc_last"] = doc

            answer = _render_doc_answer(doc)

            _push_history(session, "user", prompt)
            _push_history(session, "assistant", "Generé artefactos QA (INVEST/Gherkin/Casos/Scripts).")

            return {
                "mode": "doc",
                "session_id": sid,
                "answer": answer,          # markdown para tu frontend
                "doc_artifacts": doc,      # JSON completo (incluye content de archivos)
            }

        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # PRIORIDAD 2: Asesoría (sin ejecución)
    # Si es asesoría pura, no ejecutar (como ya tenías)
    if (wants_advise and not _wants_execute_explicit(prompt)) and not wants_execute:
        try:
            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            if session.get("last_url"):
                messages.append({"role": "system", "content": f"Contexto: last_url={session['last_url']}"})
            messages.extend(session["history"][-6:])
            messages.append({"role": "user", "content": prompt})

            resp = client.chat.completions.create(
                model=OPENAI_MODEL,
                messages=messages,
            )
            msg = resp.choices[0].message
            answer = getattr(msg, "content", None) or "Ok. ¿Qué necesitas?"
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)
            return {"mode": "advise", "session_id": sid, "answer": answer}
        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # Si NO hay señal de ejecución, por default asesoría
    if not wants_execute:
        try:
            # Soporte: si el usuario pide "dame los scripts" y tenemos doc_last
            p_low = prompt.lower()
            if session.get("doc_last") and any(x in p_low for x in ["dame los scripts", "muéstrame los scripts", "archivos", "código", "playwright"]):
                doc = session["doc_last"]
                # devolvemos un resumen y el JSON (front puede mostrar archivos)
                answer = "Tengo los scripts listos en **doc_artifacts.automation_scripts.files**. Si quieres, dime si los empaqueto por carpetas o te los pego aquí por archivo."
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", answer)
                return {"mode": "doc", "session_id": sid, "answer": answer, "doc_artifacts": doc}

            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            if session.get("last_url"):
                messages.append({"role": "system", "content": f"Contexto: last_url={session['last_url']}"})
            messages.extend(session["history"][-6:])
            messages.append({"role": "user", "content": prompt})

            resp = client.chat.completions.create(
                model=OPENAI_MODEL,
                messages=messages,
            )
            msg = resp.choices[0].message
            answer = getattr(msg, "content", None) or "Ok. ¿Qué necesitas?"
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)
            return {"mode": "advise", "session_id": sid, "answer": answer}
        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # -----------------------------
    # Execution mode: requires URL
    # -----------------------------
    base_url = _pick_base_url(req, session, prompt)
    if not base_url:
        return {
            "mode": "need_info",
            "session_id": sid,
            "answer": (
                "Para ejecutar necesito la URL (o dime 'la misma' si quieres usar la última). "
                "También dime qué validar exactamente."
            ),
        }

    # Force tool-call when router decides execute
    try:
        messages = [{"role": "system", "content": SYSTEM_PROMPT}]
        messages.append({"role": "system", "content": f"Contexto: base_url={base_url}; check_only={check_only}"})
        messages.extend(session["history"][-6:])
        messages.append({"role": "user", "content": prompt})

        resp = client.chat.completions.create(
            model=OPENAI_MODEL,
            messages=messages,
            tools=[QA_TOOL],
            tool_choice={"type": "function", "function": {"name": "run_qa_test"}},
        )

        msg = resp.choices[0].message
        steps = _extract_steps_from_tool_calls(getattr(msg, "tool_calls", None))

        if steps is None:
            return {
                "mode": "need_info",
                "session_id": sid,
                "answer": (
                    "Puedo ejecutar, pero necesito más claridad. "
                    "Dime qué elemento validar (ej: 'campo password visible') "
                    "o qué texto debe aparecer."
                ),
            }

        _ensure_goto(steps, base_url)
        _update_last_url_from_steps(session, steps)

        run_result = execute_test(steps, headless=req.headless)

        status = (run_result.get("status") or "").lower()
        resumen = "PASSED" if status in ("passed", "pass", "ok") else "FAILED"

        _push_history(session, "user", prompt)
        _push_history(session, "assistant", f"Ejecuté la prueba. Resultado: {resumen}")

        return {
            "mode": "execute",
            "session_id": sid,
            "scope": "check_only" if check_only else "general",
            "generated_steps": steps,
            "run_result": run_result,
        }

    except Exception as e:
        traceback.print_exc()
        raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")