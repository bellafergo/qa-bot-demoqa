import os
from dotenv import load_dotenv

load_dotenv()

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field
from typing import Any, Dict, List, Optional, Literal

from openai import OpenAI
from runner import execute_test

app = FastAPI()

# =========================
#            CORS
# =========================
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# =========================
#    MODELOS DE DATOS
# =========================

class StepItem(BaseModel):
    action: str
    url: Optional[str] = None
    selector: Optional[str] = None
    value: Optional[str] = None
    text: Optional[str] = None
    timeout_ms: Optional[int] = 15000

class StepPlan(BaseModel):
    steps: List[StepItem] = Field(default_factory=list)

class ChatRunRequest(BaseModel):
    prompt: str
    base_url: Optional[str] = None
    headless: bool = True

# =========================
#       CONFIGURACIÓN
# =========================

def get_openai_client() -> OpenAI:
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        raise HTTPException(status_code=500, detail="Falta la API Key de OpenAI")
    return OpenAI(api_key=api_key)

SYSTEM_PROMPT = """
Eres un ingeniero QA senior. Convierte la intención del usuario en pasos de Playwright.
Acciones permitidas: goto, wait_for_selector, fill, click, assert_visible, assert_text_contains.
Devuelve siempre JSON.
"""

# =========================
#        ENDPOINTS
# =========================

@app.get("/health")
def health():
    return {"ok": True, "message": "Vanya está despierta"}

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    try:
        client = get_openai_client()
        
        completion = client.beta.chat.completions.parse(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": req.prompt}
            ],
            response_format=StepPlan,
        )
        
        plan = completion.choices[0].message.parsed
        steps = [s.model_dump(exclude_none=True) for s in plan.steps]

        result = execute_test(steps, headless=req.headless)
        
        return {
            "generated_steps": steps,
            "run_result": result
        }
    except Exception as e:
        print(f"Error: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# ==========================================
#   SERVIR FRONTEND
# ==========================================
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "API de Vanya funcionando"}