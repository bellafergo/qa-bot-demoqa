import os
import re
from typing import List, Optional, Dict, Any, Literal

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
    allow_origins=["*"],
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

class StepPlan(BaseModel):
    steps: List[StepItem]

Intent = Literal["info", "plan", "execute"]

class ChatRunRequest(BaseModel):
    prompt: str
    base_url: Optional[str] = None
    headless: bool = True

# =========================
# OPENAI
# =========================
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
MODEL = "gpt-4o-mini"

def get_client():
    if not OPENAI_API_KEY:
        raise HTTPException(500, "Falta OPENAI_API_KEY")
    return OpenAI(api_key=OPENAI_API_KEY)

# =========================
# TOOL DEFINICIÓN (CLAVE)
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
                            "timeout_ms": {"type": "integer"}
                        },
                        "required": ["action"]
                    }
                }
            },
            "required": ["steps"]
        }
    }
}

# =========================
# PROMPT EJECUCIÓN
# =========================
SYSTEM_PROMPT_EXECUTE = """
Eres un QA Automation Agent.
Cuando el usuario quiera ejecutar una prueba:
- Llama a la herramienta run_qa_test
- Usa pasos Playwright válidos
- Incluye goto como primer paso si hay URL
NO expliques nada, solo usa la herramienta.
"""

# =========================
# ENDPOINT
# =========================
@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    client = get_client()

    response = client.responses.create(
        model=MODEL,
        input=[
            {"role": "system", "content": SYSTEM_PROMPT_EXECUTE},
            {"role": "user", "content": req.prompt}
        ],
        tools=[QA_TOOL],
        tool_choice="auto"
    )

    # Buscar llamada a tool
    for item in response.output:
        if item["type"] == "tool_call" and item["name"] == "run_qa_test":
            steps = item["arguments"]["steps"]

            # Guardrail goto
            if not any(s.get("action") == "goto" for s in steps):
                steps.insert(0, {
                    "action": "goto",
                    "url": req.base_url or "https://example.com"
                })

            result = execute_test(steps, headless=req.headless)

            return {
                "mode": "execute",
                "generated_steps": steps,
                "run_result": result
            }

    # Si no llamó herramienta → info
    return {
        "mode": "info",
        "answer": response.output_text
    }

# =========================
# HEALTH
# =========================
@app.get("/health")
def health():
    return {"ok": True}

# =========================
# FRONT
# =========================
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "Vanya Fase 1 activa"}