import os
import traceback
from typing import List, Optional, Dict, Any

from dotenv import load_dotenv
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

from openai import OpenAI
from runner import execute_test

load_dotenv()
app = FastAPI()

# =========================
# CORS
# =========================
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # luego lo puedes cerrar a tu Vercel domain
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# =========================
# MODELOS
# =========================
class StepItem(BaseModel):
    action: str
    url: Optional[str] = None
    selector: Optional[str] = None
    value: Optional[str] = None
    text: Optional[str] = None
    timeout_ms: Optional[int] = 15000

class ChatRunRequest(BaseModel):
    prompt: str
    base_url: Optional[str] = None
    headless: bool = True

# =========================
# OPENAI
# =========================
OPENAI_API_KEY = (os.getenv("OPENAI_API_KEY") or "").strip()
MODEL = (os.getenv("OPENAI_MODEL") or "gpt-4o-mini").strip()

def get_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=OPENAI_API_KEY)

# =========================
# TOOL DEFINICIÓN
# =========================
QA_TOOL = {
    "type": "function",
    "function": {
        "name": "run_qa_test",
        "description": "Ejecuta una prueba automatizada de QA en navegador",
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
Eres un QA Automation Agent.

Cuando el usuario pida ejecutar una prueba, SIEMPRE llama a la herramienta run_qa_test
y arma steps tipo Playwright.

Acciones permitidas:
- goto (requiere url)
- wait_for_selector (selector, timeout_ms opcional)
- fill (selector, value)
- click (selector)
- press (selector opcional, text = tecla, ej "Enter")
- assert_visible (selector)
- assert_text_contains (selector, text)
- wait_ms (value o text en milisegundos)

Reglas:
- NO generes "screenshot" como acción.
- Si hay una URL objetivo, el primer paso debe ser goto.
- Si el usuario solo pregunta capacidades, NO ejecutes: responde breve.
"""

# =========================
# HELPERS
# =========================
def _get_attr(obj: Any, key: str, default=None):
    """Soporta objetos (SDK) o dicts."""
    if obj is None:
        return default
    if isinstance(obj, dict):
        return obj.get(key, default)
    return getattr(obj, key, default)

def _safe_output_text(resp: Any) -> str:
    # responses API suele traer output_text
    txt = _get_attr(resp, "output_text", "")
    return txt or ""

def _extract_tool_calls(resp: Any) -> List[Dict[str, Any]]:
    """
    Normaliza tool calls a lista de dicts:
    [{"name": "...", "arguments": {...}}]
    """
    out = _get_attr(resp, "output", []) or []
    calls: List[Dict[str, Any]] = []

    for item in out:
        item_type = _get_attr(item, "type", None)
        if item_type != "tool_call":
            continue

        name = _get_attr(item, "name", None)
        arguments = _get_attr(item, "arguments", None)

        # En algunos casos arguments viene como string JSON
        if isinstance(arguments, str):
            # evitamos depender de json si viene ya bien parseado;
            # pero si llega string, intentamos parsear
            import json
            try:
                arguments = json.loads(arguments)
            except Exception:
                arguments = {}

        if not isinstance(arguments, dict):
            arguments = {}

        calls.append({"name": name, "arguments": arguments})

    return calls

def _ensure_goto(steps: List[Dict[str, Any]], fallback_url: str):
    has_goto = any((s.get("action") or "").lower() == "goto" for s in steps)
    if not has_goto:
        steps.insert(0, {"action": "goto", "url": fallback_url})

# =========================
# ENDPOINTS
# =========================
@app.get("/health")
def health():
    return {"ok": True}

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

        # Si llamó a la herramienta, ejecutamos
        for call in tool_calls:
            if call.get("name") == "run_qa_test":
                args = call.get("arguments") or {}
                steps = args.get("steps") or []

                if not isinstance(steps, list):
                    steps = []

                # Guardrail: goto
                fallback = req.base_url or "https://example.com"
                _ensure_goto(steps, fallback_url=fallback)

                run_result = execute_test(steps, headless=req.headless)

                return {
                    "mode": "execute",
                    "generated_steps": steps,
                    "run_result": run_result,
                }

        # Si NO llamó herramienta → lo tratamos como info
        return {
            "mode": "info",
            "answer": _safe_output_text(resp) or "¿Qué te gustaría probar? Puedo ejecutar flujos web (login, compra, etc.) o darte planeación de pruebas.",
        }

    except HTTPException:
        raise

    except Exception as e:
        # ✅ FIX PASO 3: imprime traceback en logs de Render + devuelve detail útil
        traceback.print_exc()
        raise HTTPException(
            status_code=500,
            detail=f"{type(e).__name__}: {str(e)}"
        )

# =========================
# FRONT
# =========================
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "Vanya Fase 1 activa"}