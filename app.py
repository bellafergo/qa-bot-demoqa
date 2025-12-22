import os
import json
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
- Si el usuario solo pregunta capacidades, NO ejecutes (responde texto breve)
"""

# ============================================================
# HELPERS
# ============================================================
def _ensure_goto(steps: List[Dict[str, Any]], fallback_url: str):
    if not any((s.get("action") or "").lower() == "goto" for s in steps):
        steps.insert(0, {"action": "goto", "url": fallback_url})

def _extract_steps_from_tool_calls(tool_calls: Any) -> Optional[List[Dict[str, Any]]]:
    """
    tool_calls en openai SDK suele venir como lista de objetos con:
      tc.function.name
      tc.function.arguments (string JSON)
    """
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
        "openai_sdk": "chat.completions (no responses)",
    }

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    client = get_client()

    try:
        completion = client.chat.completions.create(
            model=MODEL,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT_EXECUTE},
                {"role": "user", "content": req.prompt},
            ],
            tools=[QA_TOOL],
            tool_choice="auto",
        )

        msg = completion.choices[0].message

        # 1) ¿Hay tool call?
        steps = _extract_steps_from_tool_calls(getattr(msg, "tool_calls", None))

        if steps is not None:
            fallback = req.base_url or "https://example.com"
            _ensure_goto(steps, fallback)

            run_result = execute_test(steps, headless=req.headless)

            return {
                "mode": "execute",
                "generated_steps": steps,
                "run_result": run_result,
            }

        # 2) Si no hay tool call => modo info
        answer = getattr(msg, "content", None) or "Puedo ejecutar una prueba web o ayudarte a planear casos. ¿Qué quieres hacer?"
        return {"mode": "info", "answer": answer}

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