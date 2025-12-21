import os
from typing import List, Optional, Dict, Any, Tuple
from dotenv import load_dotenv
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field
from openai import OpenAI

# Importamos tu ejecución local
from runner import execute_test

load_dotenv()

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

OPENAI_API_KEY = os.getenv("OPENAI_API_KEY", "").strip()

def get_openai_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY en Render.")
    return OpenAI(api_key=OPENAI_API_KEY)

SYSTEM_PROMPT = """
Eres un ingeniero QA senior. Convierte la intención del usuario en pasos ejecutables para Playwright.

REGLAS:
1. Devuelve SIEMPRE JSON: {"steps":[{...}]}
2. Si el usuario pide ir a una web, el primer paso DEBE ser "action": "goto" con su "url".
3. Para buscar en Google: 
   - goto https://www.google.com
   - fill con selector 'textarea[name="q"]' o 'input[name="q"]'
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

        # 1. Preguntar a la IA por los pasos
        completion = client.beta.chat.completions.parse(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": req.prompt},
            ],
            response_format=StepPlan,
        )

        plan = completion.choices[0].message.parsed
        # Convertimos a lista de diccionarios limpia
        steps_to_execute = [s.model_dump(exclude_none=True) for s in plan.steps]

        # ✅ FIX IMPORTANTE: 
        # Aseguramos que el primer paso sea un GOTO si la IA no lo puso
        if not any(s.get("action") == "goto" for s in steps_to_execute):
            fallback_url = req.base_url or "https://www.google.com"
            steps_to_execute.insert(0, {"action": "goto", "url": fallback_url})

        # 2. EJECUTAR LOCALMENTE (En Render)
        # Aquí NO borramos la URL de los pasos, porque el runner local la necesita
        run_result = execute_test(steps_to_execute, headless=req.headless)

        return {
            "generated_steps": steps_to_execute,
            "run_result": run_result
        }

    except Exception as e:
        print(f"Error /chat_run: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# ==========================================
#   SERVIR FRONTEND
# ==========================================
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "API de Vanya funcionando"}