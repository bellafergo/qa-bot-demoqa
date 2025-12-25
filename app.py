import os
import json
import re
import time
import uuid
import traceback
import logging
import base64
from typing import List, Optional, Dict, Any, Tuple

from dotenv import load_dotenv
load_dotenv()

from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from starlette.requests import Request

from pydantic import BaseModel
from openai import OpenAI
from sqlalchemy.orm import Session

from pathlib import Path
from fastapi.staticfiles import StaticFiles

from db import init_db, SessionLocal, Thread, Message, utcnow
from runner import execute_test

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(name)s - %(message)s"
)
logger = logging.getLogger("vanya")

import cloudinary
import cloudinary.uploader

CLOUDINARY_URL = os.getenv("CLOUDINARY_URL", "").strip()
if CLOUDINARY_URL:
    cloudinary.config(secure=True)

def upload_evidence_to_cloudinary(
    png_bytes: bytes,
    public_id: str,
    folder: str = "vanya/evidence"
):
    """
    Sube evidencia (png bytes) a Cloudinary y regresa metadata + URL segura.
    """
    if not CLOUDINARY_URL:
        raise RuntimeError("CLOUDINARY_URL no está configurado")

    try:
        res = cloudinary.uploader.upload(
            png_bytes,
            folder=folder,
            public_id=public_id,
            resource_type="image",
            overwrite=True,
        )
        return {
            "url": res.get("secure_url"),
            "public_id": res.get("public_id"),
            "bytes": res.get("bytes"),
            "format": res.get("format"),
            "width": res.get("width"),
            "height": res.get("height"),
        }
    except Exception as e:
        logger.exception("Error subiendo evidencia a Cloudinary")
        raise

# ============================================================
# INIT
# ============================================================

app = FastAPI()

# ---- Evidence static hosting (para screenshots Playwright)

BASE_DIR = Path(__file__).resolve().parent
EVIDENCE_DIR = Path(os.getenv("EVIDENCE_DIR", str(BASE_DIR / "evidence")))

EVIDENCE_DIR.mkdir(parents=True, exist_ok=True)

app.mount(
    "/evidence",
    StaticFiles(directory=str(EVIDENCE_DIR)),
    name="evidence",
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:5173",
        "http://localhost:5174",
        "https://valtre-vanya.vercel.app",
        "https://valtre-vanya.vercel.app/",
        "https://valtre-vanya.vercel.app".rstrip("/"),
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.exception_handler(Exception)
async def unhandled_exception_handler(request: Request, exc: Exception):
    logger.error("Unhandled error", exc_info=True)
    return JSONResponse(
        status_code=500,
        content={"detail": f"{type(exc).__name__}: {str(exc)}"},
    )

@app.on_event("startup")
def on_startup():
    if os.getenv("DATABASE_URL"):
        init_db()

# (Opcional) endpoint para probar handler
@app.get("/_boom")
def _boom():
    raise Exception("boom")

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
    base_url: Optional[str] = None
    thread_id: Optional[str] = None

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
    _cleanup_sessions()  # limpia antes de crear/usar
    sid = (session_id or "").strip() or str(uuid.uuid4())
    s = _SESSIONS.get(sid)
    if not s:
        s = {"history": [], "last_url": None, "last_seen": _now(), "doc_last": None}
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

# ✅ AQUÍ VA _URL_RE (global y antes de usarlo)
_URL_RE = re.compile(r"(https?://[^\s]+)", re.I)


def _iso(x):
    """Convierte datetimes a ISO8601 UTC (string). Seguro y consistente."""
    if not x:
        return None

    if hasattr(x, "tzinfo"):
        if x.tzinfo is None:
            x = x.replace(tzinfo=utcnow().tzinfo)
        return x.astimezone(utcnow().tzinfo).isoformat()

    return str(x)

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

def _ensure_goto(steps: List[Dict[str, Any]], base_url: str):
    """
    Garantiza que el primer paso sea goto.
    Útil cuando el modelo olvida incluirlo y tu runner lo necesita.
    """
    if not steps:
        return
    has_goto = any(str(s.get("action", "")).lower() == "goto" for s in steps)
    if not has_goto:
        steps.insert(0, {"action": "goto", "url": base_url})


def _pick_base_url(req: ChatRunRequest, session: Dict[str, Any], prompt: str) -> Optional[str]:
    if req.base_url and req.base_url.strip():
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


def _update_last_url_from_steps(session: Dict[str, Any], steps: List[Dict[str, Any]], fallback: Optional[str] = None):
    for s in steps or []:
        if str(s.get("action") or "").lower() == "goto" and s.get("url"):
            session["last_url"] = _normalize_url(str(s["url"]))
            return
    if fallback:
        session["last_url"] = _normalize_url(str(fallback))


def _strip_code_fences(s: str) -> str:
    if not s:
        return s
    s2 = s.strip()
    if s2.startswith("```"):
        s2 = re.sub(r"^```[a-zA-Z0-9_-]*\s*", "", s2)
        s2 = re.sub(r"\s*```$", "", s2)
    return s2.strip()


def _parse_tool_args(raw_args: str) -> Optional[Dict[str, Any]]:
    """
    Parsea JSON de forma robusta. Si falla el parseo directo, 
    intenta extraer el contenido entre llaves.
    """
    if not raw_args:
        return None

    raw = _strip_code_fences(raw_args).strip().lstrip("\ufeff")

    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        # Intento de rescate si el LLM envió texto extra
        m = re.search(r"\{.*?\}", raw, flags=re.S)
        if m:
            try:
                return json.loads(m.group(0))
            except json.JSONDecodeError:
                return None
    return None

def _make_title_from_prompt(prompt: str, max_len: int = 60) -> str:
    p = (prompt or "").strip()
    if not p:
        return "New chat"
    # limpia saltos y espacios
    p = " ".join(p.split())
    # corta largo
    if len(p) > max_len:
        p = p[: max_len - 1] + "…"
    return p


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

# ============================================================
# DB HELPERS (threads + messages)
# ============================================================
def _db_create_thread(db: Session, title: str = "New chat") -> Thread:
    t = Thread(title=title)
    db.add(t)
    db.commit()
    db.refresh(t)
    return t


def _db_add_message(
    db: Session,
    thread_id: str,
    role: str,
    content: str,
    meta: Optional[dict] = None,   # ✅ NUEVO
):
    """
    Guarda un mensaje. meta se usa para guardar runner/doc/etc (y que aparezca en historial).
    Requiere que Message tenga un campo meta_json (JSON/JSONB).
    """
    m = Message(thread_id=thread_id, role=role, content=content)

    # ✅ si tu modelo Message ya tiene meta_json, lo setea
    if meta is not None and hasattr(m, "meta_json"):
        m.meta_json = meta

    db.add(m)


def _touch_thread(db: Session, thread_id: str):
    t = db.query(Thread).filter(Thread.id == thread_id).first()
    if t:
        t.updated_at = utcnow()
        db.add(t)


def _db_add_message_and_touch(
    db: Session,
    thread_id: str,
    role: str,
    content: str,
    meta: Optional[dict] = None,   # ✅ NUEVO
):
    _db_add_message(db, thread_id, role, content, meta=meta)
    _touch_thread(db, thread_id)


def _db_save_assistant(thread_id: str, content: str, meta: Optional[dict] = None):  # ✅ NUEVO
    """
    Helper para guardar mensaje assistant con meta opcional.
    """
    if not thread_id:
        return
    db2: Session = SessionLocal()
    try:
        _db_add_message_and_touch(db2, thread_id, "assistant", content, meta=meta)
        db2.commit()
    except Exception:
        db2.rollback()
    finally:
        db2.close()

# ============================================================
# INTENT ROUTING
# ============================================================
_ADVISE_HINTS = [
    "qué haces", "que haces", "qué puedes", "que puedes", "recomiendas",
    "riesgos", "mejor práctica", "best practice", "ayúdame a", "explica",
]


def _wants_doc(prompt: str) -> bool:
    p = _low(prompt)
    qa_triggers = [
        "casos de prueba", "matriz de casos", "matriz",
        "gherkin", "historia de usuario", "user story",
        "invest", "criterios de aceptación", "acceptance criteria",
        "artefactos qa", "documentación qa", "escenarios de prueba",
        "test cases",
    ]
    return any(k in p for k in qa_triggers)


def _wants_execute_explicit(prompt: str, session: Optional[dict] = None) -> bool:
    p = _low(prompt)
    session = session or {}

    has_url = _looks_like_url(prompt)
    has_last_url = bool(session.get("last_url"))

    has_ui_action = any(x in p for x in [
        "click", "clic", "haz click", "presiona",
        "fill", "llenar", "escribe", "selecciona",
        "assert", "valida", "verifica",
        "aparezca", "visible", "mensaje", "error",
        "botón", "campo", "texto", "redirige", "redirecciona"
    ])

    has_exec_intent = bool(re.search(
        r"\b(ejecutar|ejecuta|correr|corre|run|playwright|navega|abre|entra)\b",
        p
    ))

    # Pregunta teórica sin URL ni acción -> NO ejecutar
    if _is_question(prompt) and not has_url and not has_ui_action:
        return False

    # No hay URL, pero sí last_url y pide acción -> ejecutar
    if not has_url and has_last_url and has_ui_action:
        return True

    # Hay URL y (intención o acción) -> ejecutar
    if has_url and (has_exec_intent or has_ui_action):
        return True

    return False


def _wants_execute_followup(prompt: str, session: Dict[str, Any]) -> bool:
    p = _low(prompt)
    if session.get("last_url") and any(x in p for x in ["ahora", "también", "en la misma", "en esa", "luego", "después", "siguiente"]):
        if any(x in p for x in ["valida", "verifica", "visible", "texto", "aparezca", "error", "mensaje", "botón", "campo"]):
            return True
    return False


def _wants_advise(prompt: str, session: Optional[dict] = None) -> bool:
    p = _low(prompt)
    session = session or {}
    if _wants_doc(prompt):
        return False
    if _wants_execute_explicit(prompt, session) or _wants_execute_followup(prompt, session):
        return False
    return any(k in p for k in _ADVISE_HINTS)

# ============================================================
# DOC: requested parts
# ============================================================
def _doc_requested_parts(prompt: str) -> Dict[str, bool]:
    p = _low(prompt)
    wants_invest = ("invest" in p) or ("brechas" in p)
    wants_gherkin = ("gherkin" in p) or ("criterios de aceptación" in p)
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
# SYSTEM PROMPTS
# ============================================================
SYSTEM_PROMPT = """Eres Vanya, Lead SDET experta en Retail y E-commerce.
Tu objetivo es asegurar que el flujo de compra sea impecable y que ningún defecto afecte conversión, ingresos o experiencia del cliente.

Modos de operación:

- ADVISE: Consultoría técnica. Evalúas calidad bajo INVEST y priorizas riesgos de conversión (pagos, inventario, performance, UX).
- EXECUTE: Automatización activa. Validas flujos reales con foco en el Golden Path del cliente.

Regla de Oro:
Si detectas riesgos en checkout, pagos, promociones o manejo de stock, márcalos siempre como CRÍTICO.
Responde claro, directo y con mentalidad de negocio.
"""

SYSTEM_PROMPT_EXECUTE = """Eres Vanya. Tu misión es ejecutar pruebas web de Retail de forma robusta.
Si el usuario pide validar/navegar/click/login, devuelve ÚNICAMENTE un tool-call a run_qa_test.

Acciones permitidas:
goto, fill, click, press, assert_visible, assert_text_contains, wait_ms.

Reglas Críticas:

- En Retail, la UI puede ser inestable: espera siempre visibilidad antes de interactuar.
- Usa wait_ms estratégicamente antes de aserciones críticas.
- Si el usuario dice “la misma página”, usa last_url/base_url.
- Prioriza aserciones de visibilidad en botones de Comprar, Agregar al carrito y Checkout.
- La salida debe ser SOLO el tool-call run_qa_test.
"""

SYSTEM_PROMPT_DOC = """Eres Vanya. Generas artefactos QA de alto nivel para Retail
(INVEST, Gherkin, Casos de Prueba, Scripts Playwright Python).

Reglas de Calidad:

- Incluye siempre edge cases de Retail (cupones expirados, stock agotado, errores de pasarela).
- Prioriza escenarios por impacto en conversión y riesgo técnico.
- Si generas scripts Playwright, valida Desktop y Mobile.
- Si faltan datos, agrega assumptions y questions_to_clarify.
- Devuelve SIEMPRE un tool-call generate_qa_artifacts.
"""

# ============================================================
# TOOLS
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
                                "enum": ["goto", "fill", "click", "press", "assert_visible", "assert_text_contains", "wait_ms"],
                            },
                            "url": {"type": "string"},
                            "selector": {"type": "string"},
                            "text": {"type": "string"},
                            "role": {"type": "string"},
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
                "invest": {"type": "object"},
                "gherkin": {"type": "array", "items": {"type": "object"}},
                "test_cases": {"type": "array", "items": {"type": "object"}},
                "automation_scripts": {"type": "object"},
            },
            "required": [
                "requested", "user_story", "domain", "context",
                "assumptions", "questions_to_clarify",
                "gherkin", "test_cases", "automation_scripts",
            ],
        },
    },
}

# ============================================================
# DOC renderer + trim
# ============================================================
def _md_escape(s: str) -> str:
    return (s or "").replace("|", "\\|").replace("`", "\\`")


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
    files = (scr.get("files") or []) if isinstance(scr, dict) else []
    if isinstance(files, list) and len(files) > DOC_MAX_FILES:
        files = files[:DOC_MAX_FILES]

    for f in files:
        if isinstance(f, dict) and "content" in f:
            f["content"] = _truncate_code(f.get("content", ""))

    if isinstance(scr, dict):
        scr["files"] = files
        doc["automation_scripts"] = scr
    else:
        doc["automation_scripts"] = {"framework": "", "structure": "", "notes": [], "selectors_recommendation": [], "how_to_run": [], "files": []}

    return doc


def _render_doc_answer(doc: Dict[str, Any]) -> str:
    req = doc.get("requested") or {}
    story = doc.get("user_story") or ""
    domain = doc.get("domain") or ""
    context = doc.get("context") or ""
    assumptions = doc.get("assumptions") or []
    questions = doc.get("questions_to_clarify") or []
    invest = doc.get("invest") or {}

    out = []
    out.append(
        "## 📌 Input\n"
        f"**Dominio:** `{_md_escape(domain)}`  \n"
        f"**Historia / Requerimiento:** {_md_escape(story)}"
    )
    if context:
        out.append(f"\n**Contexto:** {_md_escape(context)}")

    if assumptions:
        out.append("\n## 🧩 Assumptions\n" + "\n".join([f"- {_md_escape(a)}" for a in assumptions]))

    if questions:
        out.append("\n## ❓ Preguntas mínimas\n" + "\n".join([f"- {_md_escape(q)}" for q in questions]))

    # ✅ INVEST (tabla)
    if req.get("invest"):
        # soporta 2 formatos:
        # 1) invest = {"Independent": 2, "Negotiable": 1, ... , "total": 8, "verdict": "...", "gaps":[...]}
        # 2) invest = {"scores": {...}, "total": ..., "verdict": ..., "gaps": [...]}
        scores = invest.get("scores") if isinstance(invest, dict) else None
        if not scores and isinstance(invest, dict):
            # intenta leer directo
            scores = {k: v for k, v in invest.items() if k.lower() in ["independent","negotiable","valuable","estimable","small","testable"]}

        total = invest.get("total") if isinstance(invest, dict) else None
        verdict = invest.get("verdict") if isinstance(invest, dict) else None
        gaps = invest.get("gaps") if isinstance(invest, dict) else None
        rewritten = invest.get("rewritten_story") if isinstance(invest, dict) else None

        out.append("\n## ✅ INVEST")
        out.append("| Criterio | Score |\n|---|---:|")
        for k in ["Independent", "Negotiable", "Valuable", "Estimable", "Small", "Testable"]:
            v = ""
            if isinstance(scores, dict):
                v = scores.get(k) or scores.get(k.lower()) or ""
            out.append(f"| {k} | {_md_escape(str(v))} |")

        if total is not None:
            out.append(f"\n**Total:** {_md_escape(str(total))}")
        if verdict:
            out.append(f"**Veredicto:** {_md_escape(str(verdict))}")

        if gaps:
            out.append("\n### Brechas")
            for g in gaps:
                out.append(f"- {_md_escape(str(g))}")

        if rewritten:
            out.append("\n### Historia reescrita")
            out.append(_md_escape(str(rewritten)))

    # ✅ Test cases (matriz)
    if req.get("cases"):
        tcs = doc.get("test_cases") or []
        out.append("\n## 🧪 Matriz de casos de prueba")
        out.append("| ID | Prio | Tipo | Auto | Caso |\n|---|---|---|---:|---|")
        for tc in tcs:
            out.append(
                f"| {_md_escape(str(tc.get('id','')))} | {_md_escape(str(tc.get('priority','')))} | "
                f"{_md_escape(str(tc.get('type','')))} | "
                f"{'✅' if tc.get('automatable') else '—'} | {_md_escape(str(tc.get('title','')))} |"
            )

    # ✅ Gherkin
    if req.get("gherkin"):
        gherkin = doc.get("gherkin") or []
        out.append("\n## 🥒 Criterios de aceptación (Gherkin)")
        for sc in gherkin:
            tag = sc.get("tag")
            if tag:
                out.append(f"\n@{_md_escape(str(tag))}")
            out.append(f"Scenario: {_md_escape(str(sc.get('scenario','')))}")
            for x in (sc.get("given") or [])[:8]:
                out.append(f"  Given {_md_escape(str(x))}")
            for x in (sc.get("when") or [])[:8]:
                out.append(f"  When {_md_escape(str(x))}")
            for x in (sc.get("then") or [])[:10]:
                out.append(f"  Then {_md_escape(str(x))}")

    return "\n".join(out).strip()


def _fallback_minimal_doc(requested, domain, context, story):
    minimal_doc = {
        "requested": requested,
        "domain": domain,
        "context": context,
        "user_story": story,
        "assumptions": ["No se proporcionaron reglas de password/lockout específicas."],
        "questions_to_clarify": ["¿Qué políticas de seguridad aplican? (MFA, lockout, rate-limit, captcha)?"],
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
                "expected": "Redirige a home/cuenta y muestra sesión iniciada",
            },
            {
                "id": "TC-LOGIN-002",
                "title": "Password incorrecta",
                "priority": "P0",
                "type": "neg",
                "automatable": True,
                "preconditions": ["Usuario registrado"],
                "steps": ["Abrir login", "Email válido", "Password inválida", "Click ingresar"],
                "expected": "Mensaje de error y no inicia sesión",
            },
        ],
        "automation_scripts": {"framework": "", "structure": "", "notes": [], "selectors_recommendation": [], "how_to_run": [], "files": []},
    }
    return _trim_doc(minimal_doc)

# ============================================================
# ENDPOINTS
# ============================================================
@app.get("/health")
def health():
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
        "has_db": bool(os.getenv("DATABASE_URL")),
    }


@app.delete("/threads/{thread_id}")
def delete_thread(thread_id: str):
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise HTTPException(status_code=404, detail="Thread not found")

        db.query(Message).filter(Message.thread_id == thread_id).delete(
            synchronize_session=False
        )

        db.delete(t)
        db.commit()
        return {"ok": True}
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()


# ============================================================
# THREADS (listado para sidebar)
# ============================================================
@app.get("/threads")
def list_threads():
    db: Session = SessionLocal()
    try:
        threads = (
            db.query(Thread)
            .order_by(Thread.updated_at.desc())
            .all()
        )
        return [
            {"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)}
            for t in threads
        ]
    finally:
        db.close()

@app.delete("/threads/{thread_id}")
def delete_thread(thread_id: str):
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise HTTPException(status_code=404, detail="Thread not found")

        # Borra primero mensajes (por FK / consistencia)
        db.query(Message).filter(Message.thread_id == thread_id).delete(synchronize_session=False)

        # Borra el thread
        db.delete(t)
        db.commit()
        return {"ok": True}
    except Exception as e:
        db.rollback()
        raise
    finally:
        db.close()


@app.get("/threads")
def list_threads():
    db: Session = SessionLocal()
    try:
        threads = (
            db.query(Thread)
            .order_by(Thread.updated_at.desc())
            .all()
        )
        return [
            {"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)}
            for t in threads
        ]
    finally:
        db.close()

# ============================================================
# THREAD DETAIL + MESSAGES (para abrir chats del sidebar)
# ============================================================
@app.get("/threads/{thread_id}")
def get_thread(thread_id: str):
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise HTTPException(status_code=404, detail="Thread not found")

        msgs = (
            db.query(Message)
            .filter(Message.thread_id == thread_id)
            .order_by(Message.created_at.asc())
            .all()
        )

        return {
            "id": t.id,
            "title": t.title,
            "updated_at": _iso(t.updated_at) if hasattr(t, "updated_at") else None,
            "messages": [
                {
                    "id": m.id,
                    "role": m.role,
                    "content": m.content,
                    "created_at": _iso(m.created_at),
                    "meta": getattr(m, "meta_json", None),  # ✅ CLAVE
                }
                for m in msgs
            ],
        }
    finally:
        db.close()


@app.get("/threads/{thread_id}/messages")
def get_thread_messages(thread_id: str):
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise HTTPException(status_code=404, detail="Thread not found")

        msgs = (
            db.query(Message)
            .filter(Message.thread_id == thread_id)
            .order_by(Message.created_at.asc())
            .all()
        )

        return [
            {
                "id": m.id,
                "role": m.role,
                "content": m.content,
                "created_at": _iso(m.created_at),
            }
            for m in msgs
        ]
    finally:
        db.close()

@app.delete("/threads/{thread_id}")
def delete_thread(thread_id: str):
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise HTTPException(status_code=404, detail="Thread not found")

        db.query(Message).filter(Message.thread_id == thread_id).delete(synchronize_session=False)
        db.delete(t)
        db.commit()
        return {"ok": True}
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()


# ============================================================
# THREADS (sidebar tipo ChatGPT)
# ============================================================

from fastapi import HTTPException  # mejor arriba del archivo, pero aquí funciona

@app.post("/threads")
def create_thread():
    db: Session = SessionLocal()
    try:
        t = Thread()
        db.add(t)
        db.commit()
        db.refresh(t)
        return {"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)}
    finally:
        db.close()


@app.get("/threads")
def list_threads():
    db: Session = SessionLocal()
    try:
        threads = db.query(Thread).order_by(Thread.updated_at.desc()).all()
        return [
            {"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)}
            for t in threads
        ]
    finally:
        db.close()


@app.get("/threads/{thread_id}")
def get_thread(thread_id: str):
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise HTTPException(status_code=404, detail="Thread not found")

        msgs = (
            db.query(Message)
            .filter(Message.thread_id == thread_id)
            .order_by(Message.created_at.asc())
            .all()
        )

        return {
            "id": t.id,
            "title": t.title,
            "updated_at": _iso(t.updated_at),
            "messages": [
                {
                    "id": getattr(m, "id", None),
                    "role": m.role,
                    "content": m.content,
                    "created_at": _iso(m.created_at),
                    "meta": getattr(m, "meta_json", None),  # si no existe, regresa None
                }
                for m in msgs
            ],
        }
    finally:
        db.close()


@app.delete("/threads/{thread_id}")
def delete_thread(thread_id: str):
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise HTTPException(status_code=404, detail="Thread not found")

        # borra hijos primero (por FK)
        db.query(Message).filter(Message.thread_id == thread_id).delete(
            synchronize_session=False
        )

        db.delete(t)
        db.commit()
        return {"ok": True}
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()


@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    try:
        _cleanup_sessions()
        sid, session = _get_session(req.session_id)

        prompt = _norm(req.prompt)
        if not prompt:
            raise HTTPException(status_code=400, detail="prompt vacío")

        # ⚠️ si aquí truena OpenAI/env, lo queremos ver en detail
        client = _get_client()

        # -------------------------------
        # THREAD: asegurar thread_id + guardar user (robusto)
        # -------------------------------
        active_thread_id = (req.thread_id or "").strip()
        db: Session = SessionLocal()

        try:
            # 1) Si viene thread_id, valida que exista
            if active_thread_id:
                exists = db.query(Thread).filter(Thread.id == active_thread_id).first()
                if not exists:
                    # thread_id inválido/borrado -> crea uno nuevo
                    t = _db_create_thread(db, title="New chat")
                    active_thread_id = t.id
                    db.commit()  # ✅ asegura que exista en DB antes de insertar mensajes
            else:
                # 2) Si no viene, crea uno nuevo
                t = _db_create_thread(db, title="New chat")
                active_thread_id = t.id
                db.commit()  # ✅ idem

            # 3) Guarda mensaje del usuario
            _db_add_message_and_touch(db, active_thread_id, "user", prompt)
            db.commit()

            # 4) Si sigue "New chat", pon título con contexto
            t2 = db.query(Thread).filter(Thread.id == active_thread_id).first()
            if t2 and (not t2.title or t2.title.strip() == "New chat"):
                t2.title = _make_title_from_prompt(prompt)
                db.add(t2)
                db.commit()

        except Exception as e:
            db.rollback()
            raise HTTPException(
                status_code=500,
                detail=f"DB error (user msg): {type(e).__name__}: {str(e)}"
            )
        finally:
            db.close()

        wants_doc = _wants_doc(prompt)
        wants_execute = _wants_execute_explicit(prompt, session) or _wants_execute_followup(prompt, session)

        # ============================================================
        # EXECUTE MODE (AQUÍ VA EL FIX DE screenshot_url + return runner)
        # ============================================================
        if wants_execute:
            # 1) corre tu runner como lo tengas hoy
            #    (usa tu lógica existente para base_url / steps / run_qa_test / etc.)
            #
            # Debes terminar con estas variables (como ya las tienes en tu código):
            # - result: dict (status, logs, duration_ms, screenshot_b64, evidence_id, etc.)
            # - steps: list (los pasos Playwright)
            # - answer: str (texto final para el chat)
            # - uploaded_evidence: dict o None  (lo que subes a cloudinary)
            #
            # EJEMPLO: (NO cambies si ya lo tienes, solo asegúrate que existan)
            # result = run_qa_test(...)
            # uploaded_evidence = upload_evidence_to_cloudinary(...)
            # answer = ...
            # steps = ...

            # ---- 👇👇👇 PON AQUÍ TU CÓDIGO EXISTENTE DEL RUNNER 👇👇👇
            # result, steps, answer, uploaded_evidence = ...
            # ---- 👆👆👆 HASTA AQUÍ TU CÓDIGO EXISTENTE DEL RUNNER 👆👆👆

            # 2) ✅ FIX: arma runner_meta con screenshot_url (cloudinary) y guárdalo en DB
            runner_meta = {
                "status": result.get("status"),
                "error": result.get("error"),
                "evidence_id": (uploaded_evidence["id"] if uploaded_evidence else result.get("evidence_id")),
                "steps": result.get("steps", []),
                "logs": result.get("logs", []),
                "duration_ms": result.get("duration_ms"),
                "meta": result.get("meta", {}),
                "evidence": [uploaded_evidence] if uploaded_evidence else [],
                # 🔑 CLAVE PARA QUE EL FRONTEND PINTE LA IMAGEN
                "screenshot_url": (uploaded_evidence["url"] if uploaded_evidence else None),
                # (opcional) fallback si no hubo upload
                "screenshot_b64": result.get("screenshot_b64"),
            }

            # 3) Guarda el mensaje del asistente + meta (execute)
            db2: Session = SessionLocal()
            try:
                _db_add_message_and_touch(
                    db2,
                    active_thread_id,
                    "assistant",
                    answer,
                    meta={"mode": "execute", "runner": runner_meta},
                )
                db2.commit()
            except Exception as e:
                db2.rollback()
                raise HTTPException(
                    status_code=500,
                    detail=f"DB error (assistant msg): {type(e).__name__}: {str(e)}"
                )
            finally:
                db2.close()

            # 4) ✅ FIX: regresa runner_meta al frontend para que se vea “al instante”
            return {
                "ok": True,
                "mode": "execute",
                "session_id": sid,
                "thread_id": active_thread_id,
                "answer": answer,
                "runner": runner_meta,
                "steps": steps,
            }

        # ============================================================
        # DOC / CHAT MODE (deja tu lógica actual)
        # ============================================================
        # Aquí va tu flujo actual para doc/chat (OpenAI, matrices, gherkin, etc.)
        # Asegúrate de guardar también el mensaje del asistente en DB como ya lo haces.
        #
        # return {...}

    except HTTPException:
        raise
    except Exception as e:
        print("CHAT_RUN ERROR:", repr(e))
        print(traceback.format_exc())
        raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {e}")

    # ============================================================
    # PRIORIDAD 1: DOC MODE
    # ============================================================
    if wants_doc and not wants_execute:
        try:
            requested = _doc_requested_parts(prompt)
            domain = _infer_domain(prompt)
            story = _extract_user_story(prompt) or prompt
            context = ""

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
                _push_history(session, "assistant", answer)
                _db_save_assistant(active_thread_id, answer)
                return {
                    "mode": "doc",
                    "session_id": sid,
                    "thread_id": active_thread_id,
                    "answer": answer,
                    "doc_artifacts": cached,
                    "cached": True,
                }

            def _doc_history_filter(msgs):
                bad = ("NEED INFO", "Para ejecutar", "URL", "credenciales", "run_qa_test", "playwright")
                out = []
                for m in msgs:
                    c = (m.get("content") or "")
                    if any(b.lower() in c.lower() for b in bad):
                        continue
                    out.append(m)
                return out

            messages = [{"role": "system", "content": SYSTEM_PROMPT_DOC}]
            messages.extend(_doc_history_filter(session["history"][-DOC_HISTORY_MSGS:]))

            user_payload = (
                "Tarea: Genera artefactos QA **solo de documentación** (NO ejecutes, NO pidas URL).\n"
                "Entrega: Devuelve **un tool-call** a generate_qa_artifacts con TODOS los campos.\n\n"
                f"REQUESTED: {requested}\n"
                f"DOMAIN: {domain}\n"
                f"USER_STORY_OR_REQUEST:\n{story}\n\n"
                "Reglas:\n"
                "- NO incluyas texto fuera del tool-call.\n"
                "- Si falta info, llena assumptions y questions_to_clarify (sin bloquear la entrega).\n"
                f"- Máximo {DOC_MAX_GHERKIN} escenarios Gherkin.\n"
                f"- Máximo {DOC_MAX_TEST_CASES} casos de prueba.\n"
                f"- Máximo {DOC_MAX_FILES} archivos de código, y cada uno <= {DOC_MAX_CODE_CHARS} chars.\n"
                "- Si REQUESTED.gherkin es false, gherkin debe ser [].\n"
                "- Si REQUESTED.cases es false, test_cases debe ser [].\n"
                "- Si REQUESTED.scripts es false: automation_scripts.files DEBE ser [] y NO incluyas código.\n"
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

            if not tool_calls:
                minimal_doc = _fallback_minimal_doc(requested, domain, context, story)
                session["doc_last"] = minimal_doc
                _cache_set(cache_key, minimal_doc)
                answer = _render_doc_answer(minimal_doc)
                _push_history(session, "user", prompt)
                _push_history(session, "assistant", answer)
                _db_save_assistant(active_thread_id, answer)
                return {
                    "mode": "doc",
                    "session_id": sid,
                    "thread_id": active_thread_id,
                    "answer": answer,
                    "doc_artifacts": minimal_doc,
                    "cached": False,
                }

            raw_args = tool_calls[0].function.arguments
            args = _parse_tool_args(raw_args) or {}

            # Fallback INVEST si lo pidieron pero el modelo no llenó estructura
            if requested.get("invest") and not args.get("invest"):
                args["invest"] = {
                    "scores": {
                        "Independent": "",
                        "Negotiable": "",
                        "Valuable": "",
                        "Estimable": "",
                        "Small": "",
                        "Testable": "",
                    },
                    "total": "",
                    "verdict": "",
                    "gaps": [],
                    "rewritten_story": ""
                }

            # patch mínimos + clamps
            args.setdefault("requested", requested)
            args.setdefault("user_story", story)
            args.setdefault("domain", domain)
            args.setdefault("context", context)
            args.setdefault("assumptions", [])
            args.setdefault("questions_to_clarify", [])
            args.setdefault("invest", {})
            args.setdefault("gherkin", [])
            args.setdefault("test_cases", [])
            args.setdefault("automation_scripts", {"framework": "", "structure": "", "notes": [], "selectors_recommendation": [], "how_to_run": [], "files": []})

            if not bool(requested.get("scripts", False)):
                args["automation_scripts"] = {"framework": "", "structure": "", "notes": [], "selectors_recommendation": [], "how_to_run": [], "files": []}
            if not bool(requested.get("gherkin", False)):
                args["gherkin"] = []
            if not bool(requested.get("cases", False)):
                args["test_cases"] = []

            doc = _trim_doc(args)
            session["doc_last"] = doc
            _cache_set(cache_key, doc)

            answer = _render_doc_answer(doc)
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)
            _db_save_assistant(active_thread_id, answer)

            return {
                "mode": "doc",
                "session_id": sid,
                "thread_id": active_thread_id,
                "answer": answer,
                "doc_artifacts": doc,
                "cached": False,
            }

        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # ============================================================
    # PRIORIDAD 2: ADVISE MODE
    # ============================================================
    if not wants_execute:
        try:
            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            messages.extend(session["history"][-MAX_HISTORY_MSGS:])
            messages.append({"role": "user", "content": prompt})

            resp = client.chat.completions.create(
                model=OPENAI_MODEL,
                messages=messages,
                temperature=ADV_TEMPERATURE,
                max_tokens=ADV_MAX_TOKENS,
            )

            answer = (resp.choices[0].message.content or "").strip() or "OK"
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)
            _db_save_assistant(active_thread_id, answer)

            return {"mode": "advise", "session_id": sid, "thread_id": active_thread_id, "answer": answer}

        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

     # ============================================================
    # PRIORIDAD 3: EXECUTE MODE
    # ============================================================
    base_url = _pick_base_url(req, session, prompt)
    if not base_url:
        need = (
            "Para ejecutar necesito la URL (o dime “la misma” si quieres usar la última) y qué validar exactamente.\n"
            "Faltan datos para ejecutar:\n"
            "- URL (o di “la misma”)\n"
            "- Qué validar (botón/campo/texto esperado)\n"
            "- Credenciales (si aplica)\n"
        )
        _push_history(session, "user", prompt)
        _push_history(session, "assistant", need)
        _db_save_assistant(active_thread_id, need)
        return {"mode": "need_info", "session_id": sid, "thread_id": active_thread_id, "answer": need}

    try:
        # 1) Pedimos steps al modelo vía tool-call run_qa_test
        messages = [{"role": "system", "content": SYSTEM_PROMPT_EXECUTE}]
        # historial corto para estabilidad
        messages.extend(session["history"][-max(3, min(MAX_HISTORY_MSGS, 6)):])
        messages.append({
            "role": "user",
            "content": (
                "Genera steps Playwright para validar en la web.\n"
                f"BASE_URL: {base_url}\n"
                f"REQUEST:\n{prompt}\n\n"
                "Reglas:\n"
                "- Devuelve SOLO tool-call run_qa_test.\n"
                "- Si la request no incluye navegación, asume la página BASE_URL.\n"
                "- Usa selectores robustos: data-testid/#id/name, si no hay usa text.\n"
            )
        })

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
            # Si el modelo no devolvió tool-call, no tronamos 500: pedimos precisión mínima
            need = (
                "No pude generar pasos de ejecución.\n"
                "Dime:\n"
                "- URL (o di “la misma”)\n"
                "- Qué validar exactamente (texto/selector esperado)\n"
                "- Credenciales (si aplica)\n"
            )
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", need)
            _db_save_assistant(active_thread_id, need)
            return {"mode": "need_info", "session_id": sid, "thread_id": active_thread_id, "answer": need}

        raw_args = tool_calls[0].function.arguments
        parsed = _parse_tool_args(raw_args) or {}
        steps = parsed.get("steps") or []

        # -------------------------------------------------
        # 1) Validación básica
        # -------------------------------------------------
        if not isinstance(steps, list) or not steps:
            need = (
                "No recibí pasos válidos para ejecutar la prueba.\n"
                "Indícame claramente qué validar y en qué URL."
            )
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", need)
            _db_save_assistant(active_thread_id, need)
            return {
                "mode": "need_info",
                "session_id": sid,
                "thread_id": active_thread_id,
                "answer": need,
            }

        # -------------------------------------------------
        # 2) Normalizar steps
        # -------------------------------------------------
        _ensure_goto(steps, base_url)
        _update_last_url_from_steps(session, steps, fallback=base_url)

        # -------------------------------------------------
        # 3) Ejecutar runner
        # -------------------------------------------------
        result = execute_test(steps=steps, headless=req.headless)

        
        uploaded_evidence = None

        # -------------------------------------------------
        # 4) Subir evidencia a Cloudinary (UNA sola vez)
        # -------------------------------------------------
        try:
            b64 = result.get("screenshot_b64")
            if b64:
                # Limpia prefijo data:image si existe
                if "," in b64:
                    b64 = b64.split(",", 1)[1]

                png_bytes = base64.b64decode(b64)
                evidence_id = result.get("evidence_id") or f"EV-{uuid.uuid4().hex[:10]}"

                uploaded = upload_evidence_to_cloudinary(
                    png_bytes=png_bytes,
                    public_id=evidence_id,
                )

                uploaded_evidence = {
                    "id": evidence_id,
                    "url": uploaded["url"],
                    "provider": "cloudinary",
                    "public_id": uploaded["public_id"],
                    "mime": "image/png",
                    "width": uploaded.get("width"),
                    "height": uploaded.get("height"),
                }

                # Reducimos peso en DB (pero dejamos fallback)
                result["screenshot_b64"] = None

        except Exception:
            logger.exception("❌ Falló subida de evidencia a Cloudinary")

        # -------------------------------------------------
        # 5) Mensaje corto al usuario
        # -------------------------------------------------
        status = (result.get("status") or "").lower()
        error = result.get("error")

        evidence_label = None
        if uploaded_evidence and uploaded_evidence.get("url"):
            evidence_label = uploaded_evidence["url"]
        else:
            evidence_label = result.get("evidence_id")

        if status == "passed":
            answer = f"✅ Prueba ejecutada: PASSED (evidence: {evidence_label})"
        else:
            answer = f"❌ Prueba ejecutada: FAIL (evidence: {evidence_label})\nDetalle: {error}"

        # -------------------------------------------------
        # 6) Historial de sesión
        # -------------------------------------------------
        _push_history(session, "user", prompt)
        _push_history(session, "assistant", answer)

        # -------------------------------------------------
        # 7) Persistencia FINAL en DB
        # -------------------------------------------------
        _db_save_assistant(
            active_thread_id,
            answer,
            meta={
                "mode": "execute",
                "runner": {
                    "status": result.get("status"),
                    "error": result.get("error"),
                    "evidence_id": uploaded_evidence["id"] if uploaded_evidence else result.get("evidence_id"),
                    "steps": result.get("steps", []),
                    "logs": result.get("logs", []),
                    "duration_ms": result.get("duration_ms"),
                    "meta": result.get("meta", {}),
                    "evidence": [uploaded_evidence] if uploaded_evidence else [],
                    # 👇 CLAVE PARA EL FRONTEND
                    "screenshot_url": uploaded_evidence["url"] if uploaded_evidence else None,
                },
            },
        )
        return {
            "mode": "execute",
            "session_id": sid,
            "thread_id": active_thread_id,
            "answer": answer,
            "run_result": result,
            "steps": steps,
        }

    except Exception as e:
        traceback.print_exc()
        err = f"Error ejecutando prueba: {type(e).__name__}: {str(e)}"
        _db_save_assistant(active_thread_id, err)
        raise HTTPException(status_code=500, detail=err)
    