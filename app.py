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
# ✅ Configuración limpia: permite todas las conexiones
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

AllowedAction = Literal[
    "goto",
    "wait_for_selector",
    "fill",
    "click",
    "assert_visible",
    "assert_text_contains",
]

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
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY en el archivo .env")
    return OpenAI(api_key=api_key)

SYSTEM_PROMPT = """
Eres un ingeniero QA senior especializado en automatización web con Playwright.
Convierte la intención del usuario en un plan de pruebas automatizadas.

REGLAS OBLIGATORIAS:
- Usa SOLO estas acciones: goto, wait_for_selector, fill, click, assert_visible, assert_text_contains.
- Para escribir en campos usa SIEMPRE: action: "fill" con los campos `selector` y `value`.
- Para validar texto usa SIEMPRE: action: "assert_text_contains" con los campos `selector` y `text`.
- Si el flujo es un formulario, incluye el submit (click) antes de cualquier validación.
- Antes de cada assert_* incluye un wait_for_selector del selector a validar.
- Devuelve EXCLUSIVAMENTE JSON válido.
"""

# =========================
#   NORMALIZADOR DE STEPS
# =========================

def normalize_steps(steps: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    for s in steps:
        if s.get("action") == "assert_text_contains" and "value" in s and "text" not in s:
            s["text"] = s.pop("value")
        if s.get("selector") == "#name":
            s["selector"] = "#output #name"

    has_submit = any(s.get("action") == "click" and s.get("selector") == "#submit" for s in steps)
    first_assert_idx = next((i for i, s in enumerate(steps) if str(s.get("action", "")).startswith("assert")), None)

    if first_assert_idx is not None and not has_submit:
        steps.insert(first_assert_idx, {"action": "click", "selector": "#submit"})

    fixed: List[Dict[str, Any]] = []
    for s in steps:
        if str(s.get("action", "")).startswith("assert") and s.get("selector"):
            fixed.append({"action": "wait_for_selector", "selector": s["selector"], "timeout_ms": 15000})
        fixed.append(s)
    return fixed

# =========================
#        ENDPOINTS
# =========================

@app.get("/health")
def health():
    return {"ok": True, "message": "QA bot corriendo correctamente"}

@app.post("/run")
def run_test(req: RunRequest):
    return execute_test(req.steps, headless=req.headless)

@app.post("/chat")
def chat(req: ChatRequest):
    try:
        client = get_openai_client()
        user_msg = req.prompt.strip()
        completion = client.beta.chat.completions.parse(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": user_msg},
            ],
            response_format=StepPlan,
        )
        msg = completion.choices[0].message
        plan = getattr(msg, "parsed", None)
        steps = [s.model_dump(exclude_none=True) for s in plan.steps]
        steps = normalize_steps(steps)
        return {"steps": steps}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    client = get_openai_client()
    completion = client.beta.chat.completions.parse(
        model="gpt-4o-mini",
        messages=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": req.prompt},
        ],
        response_format=StepPlan,
    )
    msg = completion.choices[0].message
    plan = getattr(msg, "parsed", None)
    steps = [s.model_dump(exclude_none=True) for s in plan.steps]
    steps = normalize_steps(steps)
    result = execute_test(steps, headless=req.headless)
    return {"generated_steps": steps, "run_result": result}

# ==========================================
#   SERVIR FRONTEND (DEBE IR AL FINAL)
# ==========================================
if os.path.exists("frontend"):
    app.mount("/", StaticFiles(directory="frontend", html=True), name="frontend")
else:
    @app.get("/")
    def home():
        return {"ok": True, "message": "API activa."}