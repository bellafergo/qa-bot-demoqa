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
DOC_MAX_GHERKIN = int(os.getenv("DOC_MAX_GHERKIN", "10"))

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
A) Asesor (sin ejecutar):
- Respondes teoría, riesgos, recomendaciones.
- NO ejecutes si el usuario no lo pide explícitamente.

B) Runner (con ejecución Playwright):
- Ejecuta SOLO si el usuario lo pide explícitamente (ve a/abre/navega/entra/ejecuta/corre)
  o si es follow-up de validación y existe last_url.

MEMORIA:
- Si el usuario no menciona URL pero dice "la misma" y hay last_url, reusa last_url.

SELECTORES:
- Prefiere selectores robustos.
- En SauceDemo usa: #user-name, #password, #login-button, y para "Products" usa span.title.

REGLAS:
- Si piden "existe/visible" (check-only), NO hagas login ni pasos extra.
- Si falta info para ejecutar, pide lo mínimo (URL/validación/credenciales si aplica).
"""

SYSTEM_PROMPT_DOC = f"""
Eres Vanya, QA Lead + QA Automation Engineer (Playwright Python) especializada en {DOC_DEFAULT_DOMAIN}.

OBJETIVO: generar artefactos QA ACCIONABLES y superiores:

1) INVEST:
- Califica 0-2 por criterio.
- SIEMPRE detecta brechas reales (aunque la historia sea corta): datos, reglas, errores, límites, seguridad.
- Reescribe historia mejorada (incluye condiciones y valor).
- Haz preguntas mínimas (6-10) para cerrar ambigüedad.
- Declara assumptions cuando falte info.

2) Gherkin:
- Un solo Feature por funcionalidad (no repitas Feature 5 veces).
- Incluye: positivo, negativos y edge.
- Usa tags útiles: @p0 @p1 @neg @edge @security @smoke.
- Usa Scenario Outline + Examples cuando aplique.

3) Casos de prueba:
- P0-P3, tipo (positive/negative/edge/security/performance), automatizable, pasos y expected claros.
- Incluye cosas de retail/POS/ecommerce: caídas de red, reintentos, permisos por rol, fallos de pago, idempotencia (no doble cobro).
- Máximo {DOC_MAX_TEST_CASES} casos.

4) Automatización:
- Playwright Python + pytest + Page Object Model + data-driven.
- asserts robustos, sin sleeps.
- recomienda selectores: data-testid, get_by_role, aria-label, ids.

REGLAS:
- No inventes requisitos: si falta info, ponlo en questions_to_clarify y assumptions.
- Devuelve SIEMPRE por tool_call generate_qa_artifacts (JSON completo con todos los campos requeridos).
"""

# ============================================================
# INTENT (routing)
# ============================================================
_URL_RE = re.compile(r"https?://\S+", re.IGNORECASE)

_EXECUTE_VERBS = [
    r"\bve a\b", r"\babre\b", r"\bnavega\b", r"\bentra\b", r"\bejecuta\b", r"\bcorre\b", r"\brun\b"
]

_FOLLOWUP_EXECUTE_HINTS = [
    r"\bla misma\b", r"\ben la misma\b", r"\bahora valida\b", r"\bvalida\b", r"\bverifica\b", r"\bconfirma\b",
    r"\brevisa\b", r"\bintenta\b"
]

_ADVISE_HINTS = [
    r"\briesgos?\b", r"\brecomendaciones?\b", r"\bqué haces\b", r"\bque haces\b", r"\bqué puedes\b", r"\bque puedes\b",
    r"\bcuándo\b", r"\bcuando\b", r"\bdeber[ií]a\b"
]

_DOC_HINTS = [
    r"\binvest\b", r"\bgherkin\b", r"\bcriterios de aceptaci[oó]n\b",
    r"\bmatriz\b", r"\bcasos de prueba\b", r"\btest cases\b",
    r"\bscripts?\b", r"\bautomatizaci[oó]n\b", r"\bplaywright\b"
]

_CHECK_ONLY_HINTS = [r"\bexista\b", r"\bexiste\b", r"\bvisible\b", r"\bpresente\b", r"\bse vea\b"]
_FLOW_HINTS = [r"\blogin\b", r"\binicia sesi[oó]n\b", r"\bsign in\b", r"\bcheckout\b", r"\bcomprar\b", r"\bcarrito\b"]

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
# DOC helpers: extract story + infer defaults
# ============================================================
_QUOTED_STORY_RE = re.compile(
    r"[“\"'‘](Como\s+.+?quiero\s+.+?)[”\"'’]",
    re.IGNORECASE | re.DOTALL
)

_STORY_LABEL_RE = re.compile(r"(historia|user story)\s*:\s*(.+)$", re.IGNORECASE | re.DOTALL)

def _extract_user_story(prompt: str) -> Optional[str]:
    text = (prompt or "").strip()
    if not text:
        return None

    m = _QUOTED_STORY_RE.search(text)
    if m:
        return m.group(1).strip()

    m2 = _STORY_LABEL_RE.search(text)
    if m2:
        return m2.group(2).strip()

    # Si el prompt entero YA es una historia (comienza con "Como")
    if re.match(r"^\s*como\s+.+?\s+quiero\s+.+", text, re.IGNORECASE):
        return text.strip()

    return None

def _infer_story_from_doc_request(prompt: str) -> str:
    p = (prompt or "").lower()
    if "login" in p or "iniciar sesión" in p or "sign in" in p:
        return "Como usuario quiero iniciar sesión para acceder a mi cuenta de forma segura."
    if "pagar" in p or "checkout" in p or "compra" in p:
        return "Como usuario quiero pagar mi compra para completar el checkout sin errores ni dobles cargos."
    return "Como usuario quiero completar una acción clave del sistema para lograr mi objetivo."

def _infer_context(prompt: str) -> str:
    # contexto corto, opcional
    p = (prompt or "").strip()
    # si el usuario puso bullets, lo dejamos como contexto sugerido
    if "contexto" in p.lower() or "dominio" in p.lower():
        return p[:600]
    return ""

# ============================================================
# Tool-call parsing (tolerante)
# ============================================================
def _safe_json_loads(maybe_json: Any) -> Dict[str, Any]:
    if isinstance(maybe_json, dict):
        return maybe_json
    if isinstance(maybe_json, str):
        try:
            return json.loads(maybe_json)
        except Exception:
            return {}
    return {}

def _extract_steps_from_tool_calls(tool_calls: Any) -> Optional[List[Dict[str, Any]]]:
    if not tool_calls:
        return None
    for tc in tool_calls:
        fn = getattr(tc, "function", None)
        if not fn:
            continue
        if getattr(fn, "name", None) != "run_qa_test":
            continue
        args = _safe_json_loads(getattr(fn, "arguments", "") or "")
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
        args = _safe_json_loads(getattr(fn, "arguments", "") or "")
        if isinstance(args, dict):
            return args
    return None

def _patch_doc(doc: Dict[str, Any], user_story: str, domain: str, context: str) -> Dict[str, Any]:
    # Asegura mínimos aunque el modelo deje algo vacío
    doc = doc or {}
    doc.setdefault("user_story", user_story)
    doc.setdefault("domain", domain)
    doc.setdefault("context", context)
    doc.setdefault("assumptions", doc.get("assumptions") or [])

    invest = doc.get("invest") or {}
    invest.setdefault("scores", invest.get("scores") or {
        "independent": 1, "negotiable": 1, "valuable": 1, "estimable": 1, "small": 1, "testable": 1
    })
    invest.setdefault("issues", invest.get("issues") or ["Faltan reglas de negocio / validaciones / mensajes de error."])
    invest.setdefault("improved_story", invest.get("improved_story") or user_story)
    invest.setdefault("questions_to_clarify", invest.get("questions_to_clarify") or ["¿Cuáles son las reglas de negocio y mensajes esperados?"])
    # total/verdict
    try:
        total = int(invest.get("total") or sum(int(v) for v in invest["scores"].values()))
    except Exception:
        total = 6
    invest["total"] = total
    invest.setdefault("verdict", invest.get("verdict") or ("Necesita revisión" if total < 9 else "Aceptable"))
    doc["invest"] = invest

    gherkin = doc.get("gherkin") or []
    if not isinstance(gherkin, list):
        gherkin = []
    doc["gherkin"] = gherkin[:DOC_MAX_GHERKIN]

    test_cases = doc.get("test_cases") or []
    if not isinstance(test_cases, list):
        test_cases = []
    doc["test_cases"] = test_cases[:DOC_MAX_TEST_CASES]

    scripts = doc.get("automation_scripts") or {}
    if not isinstance(scripts, dict):
        scripts = {}
    scripts.setdefault("framework", "playwright-python")
    scripts.setdefault("structure", "page-object")
    scripts.setdefault("files", scripts.get("files") or [])
    scripts.setdefault("run_instructions", scripts.get("run_instructions") or [])
    scripts.setdefault("selector_recommendations", scripts.get("selector_recommendations") or [])
    doc["automation_scripts"] = scripts

    return doc

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

    invest_table = (
        "| Criterio | Score (0-2) |\n|---|---:|\n"
        f"| Independent | {scores.get('independent','')} |\n"
        f"| Negotiable | {scores.get('negotiable','')} |\n"
        f"| Valuable | {scores.get('valuable','')} |\n"
        f"| Estimable | {scores.get('estimable','')} |\n"
        f"| Small | {scores.get('small','')} |\n"
        f"| Testable | {scores.get('testable','')} |\n"
    )

    gherkin_md = []
    for sc in gherkin[:DOC_MAX_GHERKIN]:
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
        "| ID | Prio | Tipo | Auto | Caso |\n|---|---|---|---:|---|\n"
        + ("\n".join(tc_rows) if tc_rows else "| — | — | — | — | — |")
    )

    files = scripts.get("files") or []
    run_instructions = scripts.get("run_instructions") or []
    selector_recs = scripts.get("selector_recommendations") or []

    files_list = "\n".join([f"- `{_md_escape(f.get('path'))}`" for f in files[:12]]) or "- _(sin archivos)_"
    run_lines = "\n".join([f"- {_md_escape(x)}" for x in run_instructions]) or "- _(sin instrucciones)_"
    selector_lines = "\n".join([f"- {_md_escape(x)}" for x in selector_recs]) or "- _(sin recomendaciones)_"

    return (
        f"## 📌 Historia (original)\n{user_story}\n\n"
        f"## 🧩 Contexto / Dominio\n- **Dominio:** {domain}\n"
        f"{('- ' + context) if context else ''}\n\n"
        f"## ✅ INVEST\n{invest_table}\n"
        f"**Total:** {total}  \n"
        f"**Veredicto:** {verdict}\n\n"
        f"### Brechas detectadas\n" + ("\n".join([f"- {_md_escape(x)}" for x in issues]) if issues else "- _(sin brechas)_") + "\n\n"
        f"### Historia reescrita (mejorada)\n{improved_story}\n\n"
        f"### Supuestos (si aplica)\n" + ("\n".join([f"- {_md_escape(x)}" for x in assumptions]) if assumptions else "- _(sin supuestos)_") + "\n\n"
        f"### Preguntas mínimas para cerrar ambigüedad\n" + ("\n".join([f"- {_md_escape(x)}" for x in questions]) if questions else "- _(sin preguntas)_") + "\n\n"
        f"## 🥒 Criterios de aceptación (Gherkin)\n{gherkin_md_str}\n\n"
        f"## 🧪 Matriz de Casos de Prueba\n{tc_table}\n\n"
        f"## 🤖 Automatización (Playwright Python)\n"
        f"**Estructura:** `{_md_escape(scripts.get('structure'))}`  \n"
        f"**Framework:** `{_md_escape(scripts.get('framework'))}`\n\n"
        f"### Archivos generados\n{files_list}\n\n"
        f"### Cómo correr\n{run_lines}\n\n"
        f"### Recomendación de selectores\n{selector_lines}\n\n"
        f"Si quieres, adapto los scripts a tu app real si me dices: **URL**, y si tienes `data-testid`/IDs."
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

    wants_execute = _wants_execute_explicit(prompt) or _wants_execute_followup(prompt, session)
    wants_doc = _wants_doc(prompt)
    wants_advise = _wants_advise(prompt)
    check_only = _is_check_only(prompt)

    # ============================================================
    # PRIORIDAD 1: DOC MODE (si piden INVEST/Gherkin/Casos/Scripts)
    # Nota: DOC gana incluso si hay palabras "valida/verifica" mientras NO haya verbos de navegación explícitos.
    # ============================================================
    if wants_doc and not wants_execute:
        try:
            extracted_story = _extract_user_story(prompt)
            user_story = extracted_story or _infer_story_from_doc_request(prompt)
            context = _infer_context(prompt)
            domain = "ecommerce" if ("ecommerce" in prompt.lower() or "e-commerce" in prompt.lower()) else DOC_DEFAULT_DOMAIN

            messages = [{"role": "system", "content": SYSTEM_PROMPT_DOC}]
            messages.extend(session["history"][-6:])

            # Input estructurado para que el modelo no se pierda
            messages.append({
                "role": "user",
                "content": (
                    "Genera artefactos QA para lo siguiente.\n\n"
                    f"USER_STORY:\n{user_story}\n\n"
                    f"CONTEXT:\n{context or '(no provisto)'}\n\n"
                    f"DOMAIN:\n{domain}\n\n"
                    "NOTAS:\n"
                    "- Si faltan datos, decláralos en assumptions y questions_to_clarify.\n"
                    "- No devuelvas texto libre: devuelve el JSON completo del tool_call.\n"
                )
            })

            resp = client.chat.completions.create(
                model=OPENAI_MODEL,
                messages=messages,
                tools=[QA_DOC_TOOL],
                tool_choice={"type": "function", "function": {"name": "generate_qa_artifacts"}},
            )

            msg = resp.choices[0].message
            doc_raw = _extract_doc_from_tool_calls(getattr(msg, "tool_calls", None))
            if not doc_raw:
                # Segundo intento (sin preguntar al usuario): repetimos con presión extra
                messages.append({"role": "system", "content": "IMPORTANTE: Debes llenar TODOS los campos requeridos del esquema. No omitas test_cases ni automation_scripts."})
                resp2 = client.chat.completions.create(
                    model=OPENAI_MODEL,
                    messages=messages,
                    tools=[QA_DOC_TOOL],
                    tool_choice={"type": "function", "function": {"name": "generate_qa_artifacts"}},
                )
                msg2 = resp2.choices[0].message
                doc_raw = _extract_doc_from_tool_calls(getattr(msg2, "tool_calls", None))

            if not doc_raw:
                # fallback súper corto (ya sin “necesito historia”)
                answer = (
                    "Puedo generar INVEST/Gherkin/casos/scripts. "
                    "No pude estructurar el JSON esta vez. Vuelve a pegar la historia así:\n"
                    "Historia: Como <rol> quiero <objetivo> para <valor>."
                )
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", answer)
                return {"mode": "advise", "session_id": sid, "answer": answer}

            doc = _patch_doc(doc_raw, user_story=user_story, domain=domain, context=context)

            session["doc_last"] = doc
            answer = _render_doc_answer(doc)

            _push_history(session, "user", prompt)
            _push_history(session, "assistant", "Generé artefactos QA (INVEST/Gherkin/Casos/Scripts).")

            return {
                "mode": "doc",
                "session_id": sid,
                "answer": answer,
                "doc_artifacts": doc,
            }

        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # ============================================================
    # PRIORIDAD 2: Asesoría (sin ejecución)
    # ============================================================
    if (wants_advise and not _wants_execute_explicit(prompt)) and not wants_execute:
        try:
            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            if session.get("last_url"):
                messages.append({"role": "system", "content": f"Contexto: last_url={session['last_url']}"})
            messages.extend(session["history"][-6:])
            messages.append({"role": "user", "content": prompt})

            resp = client.chat.completions.create(model=OPENAI_MODEL, messages=messages)
            msg = resp.choices[0].message
            answer = getattr(msg, "content", None) or "Ok. ¿Qué necesitas?"

            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)
            return {"mode": "advise", "session_id": sid, "answer": answer}
        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # Default asesoría si no hay ejecución
    if not wants_execute:
        try:
            p_low = prompt.lower()
            if session.get("doc_last") and any(x in p_low for x in ["dame los scripts", "muéstrame los scripts", "archivos", "código", "playwright"]):
                doc = session["doc_last"]
                answer = "Listo. Los scripts están en **doc_artifacts.automation_scripts.files** (path + content)."
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", answer)
                return {"mode": "doc", "session_id": sid, "answer": answer, "doc_artifacts": doc}

            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            if session.get("last_url"):
                messages.append({"role": "system", "content": f"Contexto: last_url={session['last_url']}"})
            messages.extend(session["history"][-6:])
            messages.append({"role": "user", "content": prompt})

            resp = client.chat.completions.create(model=OPENAI_MODEL, messages=messages)
            msg = resp.choices[0].message
            answer = getattr(msg, "content", None) or "Ok. ¿Qué necesitas?"
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)
            return {"mode": "advise", "session_id": sid, "answer": answer}
        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # ============================================================
    # Execution mode
    # ============================================================
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