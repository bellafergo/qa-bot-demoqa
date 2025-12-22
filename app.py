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
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel

from openai import OpenAI
from runner import execute_test

# ============================================================
# INIT
# ============================================================
load_dotenv()
app = FastAPI()

# ============================================================
# CORS
# ============================================================
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # luego puedes cerrar a tu dominio Vercel
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ============================================================
# MODELOS
# ============================================================
class ChatRunRequest(BaseModel):
    prompt: str
    base_url: Optional[str] = None
    headless: bool = True
    session_id: Optional[str] = None  # memoria corta (última URL)

# ============================================================
# OPENAI
# ============================================================
OPENAI_API_KEY = (os.getenv("OPENAI_API_KEY") or "").strip()
MODEL = (os.getenv("OPENAI_MODEL") or "gpt-4o-mini").strip()

def get_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=OPENAI_API_KEY)

# ============================================================
# SESSION STORE (memoria corta in-memory)
# ============================================================
_SESSIONS: Dict[str, Dict[str, Any]] = {}
SESSION_TTL_S = int(os.getenv("SESSION_TTL_S", "3600"))

def _now() -> int:
    return int(time.time())

def _get_session(session_id: Optional[str]) -> Tuple[str, Dict[str, Any]]:
    sid = (session_id or "").strip() or str(uuid.uuid4())
    s = _SESSIONS.get(sid)
    if not s:
        s = {"created_at": _now(), "last_seen": _now(), "last_url": None}
        _SESSIONS[sid] = s
    s["last_seen"] = _now()
    return sid, s

def _cleanup_sessions():
    t = _now()
    dead = []
    for sid, s in _SESSIONS.items():
        if t - int(s.get("last_seen", t)) > SESSION_TTL_S:
            dead.append(sid)
    for sid in dead:
        _SESSIONS.pop(sid, None)

# ============================================================
# TOOL DEFINICIÓN (Function Calling)
# ============================================================
QA_TOOL = {
    "type": "function",
    "function": {
        "name": "run_qa_test",
        "description": "Ejecuta una prueba automatizada de QA en navegador con Playwright",
        "parameters": {
            "type": "object",
            "properties": {
                "steps": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "action": {"type": "string"},
                            "url": {"type": "string"},
                            "selector": {"type": "string"},  # opcional
                            "text": {"type": "string"},      # opcional (auto-detección)
                            "role": {"type": "string"},      # opcional (auto-detección)
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
# PROMPTS
# ============================================================
SYSTEM_PROMPT_EXECUTE = """
Eres Vanya, un Agente Virtual de QA.

CAPACIDADES (consistente):
- Modo Asesor (sin ejecución): análisis, casos, matrices, criterios, recomendaciones.
- Modo Runner (con ejecución): SOLO si el usuario pide ejecutar de forma clara.

REGLA CRÍTICA:
- Solo llama a run_qa_test cuando el usuario pida EJECUTAR explícitamente (por ejemplo: "ve a", "abre", "navega", "ejecuta", "corre").
- Si el usuario pide casos/matriz/estrategia o pregunta "qué puedes / qué haces / cuándo", NO ejecutes; responde texto.

ACCIONES PERMITIDAS EN STEPS:
- goto (url)
- wait_for (selector o text/role, timeout_ms opcional)
- fill (selector o text/role, value)
- click (selector o text/role)
- press (selector o text/role, text="Enter")
- assert_visible (selector o text/role)
- assert_text_contains (selector o text/role, text)
- wait_ms (value en ms)

REGLAS DE EJECUCIÓN:
- Si hay URL, el primer paso debe ser goto.
- NO agregues pasos extra no solicitados. Si solo piden "existe/visible", no hagas login.
- Evita pedir selectores como requisito inicial; usa text/role cuando no tengas selector.
"""

SYSTEM_PROMPT_ADVISE = """
Eres Vanya, un Agente Virtual de QA en Modo Asesor (sin ejecutar nada).
Responde claro y práctico.
Si el usuario quiere ejecución, pídele:
- URL (o confirma la última URL si existe)
- qué validar exactamente
- credenciales/datos solo si aplica
Nunca pidas selectores como requisito inicial.
"""

# ============================================================
# HELPERS
# ============================================================
def _norm(s: str) -> str:
    return (s or "").strip()

def _ensure_goto(steps: List[Dict[str, Any]], fallback_url: str):
    if not any((str(s.get("action") or "").lower() == "goto") for s in steps):
        steps.insert(0, {"action": "goto", "url": fallback_url})

def _extract_steps_from_tool_calls(tool_calls: Any) -> Optional[List[Dict[str, Any]]]:
    if not tool_calls:
        return None
    for tc in tool_calls:
        fn = getattr(tc, "function", None)
        if not fn:
            continue
        name = getattr(fn, "name", None)
        if name != "run_qa_test":
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

# ============================================================
# INTENT (MEJORA CLAVE)
# ============================================================
_URL_RE = re.compile(r"https?://\S+", re.IGNORECASE)

# ✅ SOLO verbos claros de ejecución (se quitaron “prueba/test/valida” porque son ambiguos)
_EXECUTE_VERBS = [
    r"\bve a\b",
    r"\babre\b",
    r"\bnavega\b",
    r"\bentra\b",
    r"\bejecuta\b",
    r"\bcorre\b",
    r"\bcorrer\b",
    r"\brun\b",
    r"\bla misma\b",  # permite reusar last_url
]

# ✅ Keywords de asesoría / contenido (casos, matrices, estrategia)
_ADVISE_KEYWORDS = [
    r"\bcasos?\b",
    r"\bmatriz\b",
    r"\bescenarios?\b",
    r"\bcriterios?\b",
    r"\bchecklist\b",
    r"\bestrategia\b",
    r"\bplan\b",
    r"\briesgos?\b",
    r"\brecomendaciones?\b",
    r"\bmejoras?\b",
    r"\bexplica\b",
    r"\bqué puedes\b",
    r"\bque puedes\b",
    r"\bqué haces\b",
    r"\bque haces\b",
    r"\bcuándo\b",
    r"\bcuando\b",
    r"\bdeber[ií]a\b",
    r"\bshould\b",
    r"\bhow\b",
    r"\bwhat can you\b",
]

# ✅ Check-only: sigue igual
_CHECK_ONLY_HINTS = [
    r"\bexista\b", r"\bexiste\b", r"\bvisible\b", r"\bpresente\b", r"\bse vea\b",
    r"\bhabilitado\b", r"\bdisabled\b", r"\benabled\b"
]
_FLOW_HINTS = [
    r"\blogin\b", r"\binicia sesi[oó]n\b", r"\bsign in\b", r"\bsignin\b",
    r"\bcomprar\b", r"\bcheckout\b", r"\bagregar al carrito\b", r"\badd to cart\b"
]

def _has_url(prompt: str) -> bool:
    return bool(_URL_RE.search(prompt or ""))

def _wants_advise(prompt: str) -> bool:
    p = (prompt or "").lower()
    return any(re.search(h, p) for h in _ADVISE_KEYWORDS)

def _wants_execute(prompt: str) -> bool:
    p = (prompt or "").lower()
    return any(re.search(h, p) for h in _EXECUTE_VERBS)

def _intent_is_check_only(prompt: str) -> bool:
    p = (prompt or "").lower()
    has_check = any(re.search(h, p) for h in _CHECK_ONLY_HINTS)
    has_flow = any(re.search(h, p) for h in _FLOW_HINTS)
    return bool(has_check and not has_flow)

def _filter_steps_by_scope(steps: List[Dict[str, Any]], check_only: bool) -> List[Dict[str, Any]]:
    if not check_only:
        return steps

    allowed = {"goto", "wait_for", "assert_visible", "assert_text_contains", "wait_ms"}
    filtered: List[Dict[str, Any]] = []
    for s in steps:
        act = str(s.get("action") or "").lower()
        if act in allowed:
            filtered.append(s)

    # Si el modelo no generó asserts, metemos uno mínimo
    if not any(str(s.get("action", "")).lower().startswith("assert_") for s in filtered):
        hint = None
        for s in steps:
            if s.get("selector") or s.get("text") or s.get("role"):
                hint = {"selector": s.get("selector"), "text": s.get("text"), "role": s.get("role")}
                break
        filtered.append({"action": "assert_visible", **(hint or {"text": "Login"})})

    return filtered

def _resolve_base_url(req: ChatRunRequest, session: Dict[str, Any], prompt: str) -> str:
    # prioridad: req.base_url -> last_url (si usuario dice "la misma") -> last_url -> default
    if req.base_url:
        return req.base_url.strip()
    if session.get("last_url"):
        return str(session["last_url"])
    return "https://example.com"

def _maybe_update_last_url(steps: List[Dict[str, Any]], session: Dict[str, Any], fallback_url: str):
    for s in steps:
        if str(s.get("action") or "").lower() == "goto" and s.get("url"):
            session["last_url"] = s["url"]
            return
    session["last_url"] = fallback_url

# ============================================================
# ENDPOINTS
# ============================================================
@app.get("/health")
def health():
    _cleanup_sessions()
    return {"ok": True}

@app.get("/meta")
def meta():
    return {
        "ok": True,
        "render_git_commit": os.getenv("RENDER_GIT_COMMIT"),
        "model": MODEL,
        "has_openai_key": bool(OPENAI_API_KEY),
        "openai_sdk": "chat.completions",
        "session_ttl_s": SESSION_TTL_S,
    }

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    client = get_client()
    _cleanup_sessions()

    sid, session = _get_session(req.session_id)
    prompt = _norm(req.prompt)

    if not prompt:
        raise HTTPException(status_code=400, detail="prompt vacío")

    # ✅ Decisión robusta:
    # - Si parece asesoría (casos/matriz/pregunta meta) => advise
    # - Si parece ejecución (verbos claros) => execute
    # - Si no, por default => advise
    wants_advise = _wants_advise(prompt)
    wants_execute = _wants_execute(prompt)
    check_only = _intent_is_check_only(prompt)

    # Si el usuario está preguntando / pidiendo contenido -> NO ejecutar
    if wants_advise and not wants_execute:
        try:
            completion = client.chat.completions.create(
                model=MODEL,
                messages=[
                    {"role": "system", "content": SYSTEM_PROMPT_ADVISE},
                    {"role": "user", "content": prompt},
                ],
            )
            msg = completion.choices[0].message
            answer = getattr(msg, "content", None) or (
                "Puedo ayudarte a definir casos de prueba o ejecutar una prueba si me dices URL y qué validar."
            )
            return {"mode": "advise", "session_id": sid, "answer": answer}
        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # Si NO hay verbos claros de ejecución -> advise
    if not wants_execute:
        try:
            completion = client.chat.completions.create(
                model=MODEL,
                messages=[
                    {"role": "system", "content": SYSTEM_PROMPT_ADVISE},
                    {"role": "user", "content": prompt},
                ],
            )
            msg = completion.choices[0].message
            answer = getattr(msg, "content", None) or (
                "Puedo ayudarte a definir casos de prueba o ejecutar una prueba si me dices URL y qué validar."
            )
            return {"mode": "advise", "session_id": sid, "answer": answer}
        except Exception as e:
            traceback.print_exc()
            raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {str(e)}")

    # ✅ Si pidió ejecutar, validamos si tenemos URL o memoria
    has_url = _has_url(prompt) or bool(req.base_url) or bool(session.get("last_url")) or ("la misma" in prompt.lower())
    if not has_url:
        return {
            "mode": "need_info",
            "session_id": sid,
            "answer": (
                "Para ejecutar necesito:\n"
                "1) La URL (o dime 'la misma' si quieres usar la última)\n"
                "2) Qué validar exactamente (ej: 'el botón Login es visible')\n"
                "Si hay login, también usuario/contraseña."
            ),
        }

    # ============================================================
    # EJECUCIÓN con tools
    # ============================================================
    try:
        base_url = _resolve_base_url(req, session, prompt)

        completion = client.chat.completions.create(
            model=MODEL,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT_EXECUTE},
                {"role": "user", "content": (
                    f"Contexto:\n- base_url: {base_url}\n"
                    f"- nota: si el usuario dice 'la misma', usa base_url\n\n"
                    f"Usuario:\n{prompt}"
                )},
            ],
            tools=[QA_TOOL],
            tool_choice="auto",
        )

        msg = completion.choices[0].message
        steps = _extract_steps_from_tool_calls(getattr(msg, "tool_calls", None))

        if steps is None:
            return {
                "mode": "need_info",
                "session_id": sid,
                "answer": (
                    "Puedo ejecutar, pero me falta claridad.\n"
                    "Dime exactamente qué quieres validar (ej: 'Login visible' o 'texto Products aparece') "
                    "y si uso la URL actual o me das otra."
                ),
            }

        steps = _filter_steps_by_scope(steps, check_only=check_only)
        _ensure_goto(steps, base_url)
        _maybe_update_last_url(steps, session, base_url)

        run_result = execute_test(steps, headless=req.headless)

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

# ============================================================
# FRONT (opcional)
# ============================================================
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "Vanya Fase 1 activa"}