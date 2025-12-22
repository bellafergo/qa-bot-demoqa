import os
import traceback
from typing import List, Optional, Dict, Any

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
    allow_origins=["*"],  # luego puedes cerrarlo a tu dominio Vercel
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
# TOOL DEFINICIÓN (CLAVE FASE 1)
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
                            "selector": {"type": "string"},
                            "value": {"type": "string"},
                            "text": {"type": "string"},
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

SYSTEM_PROMPT_EXECUTE = """
Eres Vanya, un QA Automation Agent.

Si el usuario quiere EJECUTAR una prueba:
- LLAMA a la herramienta run_qa_test
- Devuelve SOLO la llamada a la herramienta
- Usa pasos Playwright válidos

Acciones permitidas:
- goto (url)
- wait_for_selector (selector, timeout_ms opcional)
- fill (selector, value)
- click (selector)
- press (selector opcional, text="Enter")
- assert_visible (selector)
- assert_text_contains (selector, text)
- wait_ms (value o text en ms)

Reglas:
- NO generes acciones screenshot
- Si hay URL, el primer paso debe ser goto
- Si el usuario solo pregunta capacidades, NO ejecutes
"""

# ============================================================
# HELPERS
# ============================================================
def _get_attr(obj: Any, key: str, default=None):
    if isinstance(obj, dict):
        return obj.get(key, default)
    return getattr(obj, key, default)

def _safe_output_text(resp: Any) -> str:
    return _get_attr(resp, "output_text", "") or ""

def _extract_tool_calls(resp: Any) -> List[Dict[str, Any]]:
    out = _get_attr(resp, "output", []) or []
    calls = []

    for item in out:
        if _get_attr(item, "type") != "tool_call":
            continue

        name = _get_attr(item, "name")
        arguments = _get_attr(item, "arguments", {})

        if isinstance(arguments, str):
            import json
            try:
                arguments = json.loads(arguments)
            except Exception:
                arguments = {}

        calls.append({"name": name, "arguments": arguments})

    return calls

def _ensure_goto(steps: List[Dict[str, Any]], fallback_url: str):
    if not any((s.get("action") or "").lower() == "goto" for s in steps):
        steps.insert(0, {"action": "goto", "url": fallback_url})

# ============================================================
# ENDPOINTS
# ============================================================
@app.get("/health")
def health():
    return {"ok": True}

@app.get("/meta")
def meta():
    return {
        "ok": True,
        "render_git_commit": os.getenv("RENDER_GIT_COMMIT"),
        "model": MODEL,
        "has_openai_key": bool(OPENAI_API_KEY),
    }

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    client = get_client()

    try:
        resp = client.responses.create(
            model=MODEL,
            input=[
                {"role": "system", "content": SYSTEM_PROMPT_EXECUTE},
                {"role": "user", "content": req.prompt},
            ],
            tools=[QA_TOOL],
            tool_choice="auto",
        )

        tool_calls = _extract_tool_calls(resp)

        # EJECUCIÓN
        for call in tool_calls:
            if call.get("name") == "run_qa_test":
                steps = call.get("arguments", {}).get("steps", [])
                if not isinstance(steps, list):
                    steps = []

                fallback = req.base_url or "https://example.com"
                _ensure_goto(steps, fallback)

                run_result = execute_test(steps, headless=req.headless)

                return {
                    "mode": "execute",
                    "generated_steps": steps,
                    "run_result": run_result,
                }

        # INFO / NO EJECUTA
        return {
            "mode": "info",
            "answer": _safe_output_text(resp)
            or "Puedo ejecutar pruebas web (login, navegación, validaciones) o ayudarte a diseñar casos de prueba. ¿Qué te gustaría hacer?",
        }

    except Exception as e:
        traceback.print_exc()
        raise HTTPException(
            status_code=500,
            detail=f"{type(e).__name__}: {str(e)}"
        )

# ============================================================
# FRONT (opcional)
# ============================================================
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "Vanya Fase 1 activa"}