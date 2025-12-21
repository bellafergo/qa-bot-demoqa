import os
from dotenv import load_dotenv

load_dotenv()

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
from typing import Any, Dict, List, Optional, Literal

from openai import OpenAI
from runner import execute_test

app = FastAPI()

# =========================
#            CORS
# =========================
# ✅ Ajusta esta lista según el puerto donde abras el frontend
# (Vite suele ser 5173, Live Server 5500, python -m http.server 5500/8080)
ALLOWED_ORIGINS = [
    "http://127.0.0.1:5173",
    "http://localhost:5173",
    "http://127.0.0.1:5500",
    "http://localhost:5500",
    "http://127.0.0.1:8080",
    "http://localhost:8080",
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=ALLOWED_ORIGINS,
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
- Usa SOLO estas acciones:
  goto, wait_for_selector, fill, click, assert_visible, assert_text_contains.
- Para escribir en campos usa SIEMPRE:
  action: "fill" con los campos `selector` y `value`.
- Para validar texto usa SIEMPRE:
  action: "assert_text_contains" con los campos `selector` y `text`.
  (NUNCA uses `value` para assert_text_contains).
- Si el flujo es un formulario, incluye el submit (click) antes de cualquier validación.
- Antes de cada assert_* incluye un wait_for_selector del selector a validar.
- Para DemoQA Text Box:
  1) goto https://demoqa.com/text-box
  2) llena campos
  3) click #submit
  4) valida en #output #name (NO en #name suelto)
- No inventes acciones ni campos.
- Devuelve EXCLUSIVAMENTE JSON válido con esta estructura:

{
  "steps": [
    {
      "action": "...",
      "url": "...",
      "selector": "...",
      "value": "...",
      "text": "...",
      "timeout_ms": 15000
    }
  ]
}
"""

# =========================
#   NORMALIZADOR DE STEPS
# =========================

def normalize_steps(steps: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    # 1) assert_text_contains debe usar "text" (no "value")
    for s in steps:
        if s.get("action") == "assert_text_contains" and "value" in s and "text" not in s:
            s["text"] = s.pop("value")

    # 2) DemoQA suele renderizar el nombre en #output #name
    for s in steps:
        if s.get("selector") == "#name":
            s["selector"] = "#output #name"

    # 3) Si hay asserts pero no hay submit, insertar click #submit antes del primer assert
    has_submit = any(s.get("action") == "click" and s.get("selector") == "#submit" for s in steps)
    first_assert_idx = next(
        (i for i, s in enumerate(steps) if str(s.get("action", "")).startswith("assert")),
        None
    )

    if first_assert_idx is not None and not has_submit:
        steps.insert(first_assert_idx, {"action": "click", "selector": "#submit"})
        steps.insert(first_assert_idx, {"action": "wait_for_selector", "selector": "#submit", "timeout_ms": 15000})

    # 4) Antes de cada assert, agregar wait_for_selector (más estable)
    fixed: List[Dict[str, Any]] = []
    for s in steps:
        if str(s.get("action", "")).startswith("assert") and s.get("selector"):
            fixed.append({"action": "wait_for_selector", "selector": s["selector"], "timeout_ms": int(s.get("timeout_ms", 15000))})
        fixed.append(s)

    return fixed

# =========================
#        ENDPOINTS
# =========================

@app.get("/")
def home():
    return {"ok": True, "message": "QA bot corriendo correctamente"}

@app.get("/demoqa/textbox/steps")
def demoqa_textbox_steps():
    return {
        "steps": [
            {"action": "goto", "url": "https://demoqa.com/text-box"},
            {"action": "wait_for_selector", "selector": "#userName", "timeout_ms": 15000},
            {"action": "fill", "selector": "#userName", "value": "Fernanda QA"},
            {"action": "fill", "selector": "#userEmail", "value": "fernanda.qa@example.com"},
            {"action": "fill", "selector": "#currentAddress", "value": "Monterrey"},
            {"action": "fill", "selector": "#permanentAddress", "value": "Nuevo León"},
            {"action": "click", "selector": "#submit"},
            {"action": "wait_for_selector", "selector": "#output #name", "timeout_ms": 15000},
            {"action": "assert_text_contains", "selector": "#output #name", "text": "Fernanda QA"},
        ]
    }

@app.post("/run")
def run_test(req: RunRequest):
    return execute_test(req.steps, headless=req.headless)

@app.post("/chat")
def chat(req: ChatRequest):
    try:
        client = get_openai_client()

        user_msg = req.prompt.strip()
        if req.base_url:
            user_msg += f"\nBase URL contextual: {req.base_url}"

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
        if plan is None:
            raise HTTPException(
                status_code=500,
                detail="No llegó message.parsed. Actualiza openai: pip install -U openai",
            )

        steps = [s.model_dump(exclude_none=True) for s in plan.steps]
        steps = normalize_steps(steps)

        if not steps:
            raise HTTPException(status_code=500, detail="OpenAI devolvió 0 steps. Escribe una instrucción más específica.")

        return {"steps": steps}

    except Exception as e:
        print(f"Error en endpoint /chat: {e}")
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    # 1) Crear cliente OpenAI
    client = get_openai_client()

    user_msg = req.prompt.strip()
    if req.base_url:
        user_msg += f"\nBase URL contextual: {req.base_url}"

    # 2) Generar pasos con OpenAI
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

    if plan is None or not plan.steps:
        raise HTTPException(status_code=500, detail="OpenAI no generó pasos. Ajusta el prompt.")

    # 3) Normalizar steps
    steps = [s.model_dump(exclude_none=True) for s in plan.steps]
    steps = normalize_steps(steps)

    if not steps:
        raise HTTPException(status_code=500, detail="OpenAI devolvió 0 steps después de normalizar.")

    # 4) Ejecutar Playwright
    result = execute_test(steps, headless=req.headless)

    # 5) Responder
    return {
        "generated_steps": steps,
        "run_result": result
    }