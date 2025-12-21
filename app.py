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
# ✅ CONFIGURACIÓN FINAL: Permite la conexión desde Vercel y local sin restricciones
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

class RunRequest(BaseModel):
    steps: List[Dict[str, Any]]
    headless: bool = True

AllowedAction = Literal["goto", "wait_for_selector", "fill", "click", "assert_visible", "assert_text_contains"]

class StepItem(BaseModel):
    action: AllowedAction
    url: Optional[str] = None
    selector: Optional[str] = None
    value: Optional[str] = None
    text: Optional[str] = None
    timeout_ms: Optional[int] = 15000

class StepPlan(BaseModel):
    steps: List[StepItem] = Field(default_factory=list)

class ChatRequest(BaseModel):
    prompt: str
    base_url: Optional[str] = None

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
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=api_key)

SYSTEM_PROMPT = "Eres un ingeniero QA senior. Convierte la intención del usuario en pasos de Playwright en formato JSON."

# =========================
#   NORMALIZADOR DE STEPS
# =========================

def normalize_steps(steps: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    fixed: List[Dict[str, Any]] = []
    for s in steps:
        if s.get("action") == "assert_text_contains" and "value" in s:
            s["text"] = s.pop("value")
        fixed.append(s)
    return fixed

# =========================
#        ENDPOINTS
# =========================

@app.get("/health")
def health():
    return {"ok": True, "message": "Vanya está despierta"}

@app.post("/run")
def run_test(req: RunRequest):
    return execute_test(req.steps, headless=req.headless)

@app.post("/chat")
def chat(req: ChatRequest):
    try:
        client = get_openai_client()
        completion = client.beta.chat.completions.parse(
            model="gpt-4o-mini",
            messages=[{"role": "system", "content": SYSTEM_PROMPT}, {"role": "user", "content": req.prompt}],
            response_format=StepPlan,
        )
        plan = completion.choices[0].message.parsed
        steps = normalize_steps([s.model_dump(exclude_none=True) for s in plan.steps])
        return {"steps": steps}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# ==========================================
#   SERVIR FRONTEND (AL FINAL)
# ==========================================
if os.path.exists("frontend"):
    app.mount("/", StaticFiles(directory="frontend", html=True), name="frontend")
else:
    @app.get("/")
    def home():
        return {"ok": True, "message": "Servidor activo"}