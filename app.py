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
    allow_origins=["*"],  # luego ciérralo a tu dominio
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
MAX_HISTORY_MSGS = int(os.getenv("MAX_HISTORY_MSGS", "10"))  # para no saturar

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
# SESSION MEMORY (history + last_url + ttl)
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
            "history": [],     # list[{role, content}]
            "last_url": None,
            "last_seen": _now(),
        }
        _SESSIONS[sid] = s
    s["last_seen"] = _now()
    return sid, s

def _push_history(session: Dict[str, Any], role: str, content: str):
    session["history"].append({"role": role, "content": content})
    # recorta
    if len(session["history"]) > MAX_HISTORY_MSGS:
        session["history"] = session["history"][-MAX_HISTORY_MSGS:]

# ============================================================
# TOOL (Function Calling)
# Nota: mantenemos selector/text/value; runner usa selector hoy.
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
# PROMPTS (unificado pero con reglas fuertes)
# ============================================================
SYSTEM_PROMPT = """
Eres Vanya, una Agente de QA Inteligente capaz de ASESORAR y EJECUTAR pruebas.

DOS MODOS:
A) Modo Asesor (sin ejecutar):
- Respondes preguntas, matrices de casos, criterios, riesgos, recomendaciones.
- No ejecutes nada si el usuario solo pregunta teoría o pide casos.

B) Modo Runner (con ejecución):
- SOLO ejecutas cuando el usuario lo pide explícitamente (ej. "ve a", "abre", "navega", "ejecuta", "corre")
  o cuando es un FOLLOW-UP de validación y ya existe una última URL en sesión.

MEMORIA:
- Si el usuario no menciona URL pero dice "la misma" y hay last_url, reusa last_url.

SELECTORES:
- Prefiere selectores robustos. En SauceDemo usa: #user-name, #password, #login-button.
- Para "Login" también es válido usar selector por texto simple si no hay CSS claro.

REGLAS:
- Si piden "existe/visible" (check-only), NO hagas login ni pasos extra.
- Si falta información para ejecutar, pide lo mínimo (URL/validación/credenciales si aplica).
"""

# ============================================================
# INTENT (evita ejecutar por accidente + soporta follow-ups)
# ============================================================
_URL_RE = re.compile(r"https?://\S+", re.IGNORECASE)

# Ejecutar solo con verbos claros
_EXECUTE_VERBS = [
    r"\bve a\b",
    r"\babre\b",
    r"\bnavega\b",
    r"\bentra\b",
    r"\bejecuta\b",
    r"\bcorre\b",
    r"\brun\b",
]

# Follow-up ejecutable si ya hay last_url (clave para tu demo)
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

# Palabras de asesoría/contenido
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
# TOOL-CALL parsing
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

def _ensure_goto(steps: List[Dict[str, Any]], url: str):
    if not any(str(s.get("action") or "").lower() == "goto" for s in steps):
        steps.insert(0, {"action": "goto", "url": url})

def _update_last_url_from_steps(session: Dict[str, Any], steps: List[Dict[str, Any]]):
    for s in steps:
        if str(s.get("action") or "").lower() == "goto" and s.get("url"):
            session["last_url"] = s["url"]
            return

def _pick_base_url(req: ChatRunRequest, session: Dict[str, Any], prompt: str) -> Optional[str]:
    # prioridad: URL explícita en prompt
    m = _URL_RE.search(prompt or "")
    if m:
        return m.group(0)
    # luego base_url enviada por el front
    if req.base_url:
        return req.base_url.strip()
    # luego last_url en sesión
    if session.get("last_url"):
        return str(session["last_url"])
    return None

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

    # 1) Intent routing
    wants_advise = _wants_advise(prompt)
    wants_execute = _wants_execute_explicit(prompt) or _wants_execute_followup(prompt, session)
    check_only = _is_check_only(prompt)

    # Si es asesoría pura, NO ejecutar
    if wants_advise and not _wants_execute_explicit(prompt):
        try:
            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            # contexto de last_url
            if session.get("last_url"):
                messages.append({"role": "system", "content": f"Contexto: last_url={session['last_url']}"})
            # historial corto
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

    # 2) Execution requires URL or last_url
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

    # 3) Execution: force tool-call when router decides execute
    try:
        messages = [{"role": "system", "content": SYSTEM_PROMPT}]
        # contexto fuerte
        messages.append({"role": "system", "content": f"Contexto: base_url={base_url}; check_only={check_only}"})
        messages.extend(session["history"][-6:])
        messages.append({"role": "user", "content": prompt})

        resp = client.chat.completions.create(
            model=OPENAI_MODEL,
            messages=messages,
            tools=[QA_TOOL],
            # 🔥 CLAVE: cuando decidimos ejecutar, lo forzamos
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

        # asegurar goto
        _ensure_goto(steps, base_url)

        # guarda last_url
        _update_last_url_from_steps(session, steps)

        # ejecuta
        run_result = execute_test(steps, headless=req.headless)

        # registra historial (correcto con status)
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