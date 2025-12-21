import os
from typing import List, Optional, Dict, Any

import requests
from dotenv import load_dotenv
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

from openai import OpenAI

# Si tienes runner local (solo se usa si NO hay RUNNER_URL)
from runner import execute_test

load_dotenv()

app = FastAPI()

# =========================
#            CORS
# =========================
# Si tu frontend está en Vercel, esto permite llamadas desde ahí.
# Si quieres cerrarlo, reemplaza ["*"] por ["https://valtre-vanya.vercel.app"]
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
    url: Optional[str] = None  # OJO: el runner en DO no acepta url dentro de step, lo normalizaremos
    selector: Optional[str] = None
    value: Optional[str] = None
    text: Optional[str] = None
    timeout_ms: Optional[int] = 15000

class StepPlan(BaseModel):
    steps: List[StepItem] = Field(default_factory=list)

class ChatRunRequest(BaseModel):
    prompt: str
    base_url: Optional[str] = None  # url base opcional
    headless: bool = True

# =========================
#       CONFIGURACIÓN
# =========================

OPENAI_API_KEY = os.getenv("OPENAI_API_KEY", "").strip()
OPENAI_MODEL = os.getenv("OPENAI_MODEL", "gpt-4o-mini").strip()

RUNNER_URL = os.getenv("RUNNER_URL", "").strip()          # Ej: http://104.248.74.76:8000/run
RUNNER_TOKEN = os.getenv("RUNNER_TOKEN", "").strip()      # Header x-runner-token
RUNNER_TIMEOUT_S = int(os.getenv("RUNNER_TIMEOUT_S", "120"))

def get_openai_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta la API Key de OpenAI (OPENAI_API_KEY).")
    return OpenAI(api_key=OPENAI_API_KEY)

SYSTEM_PROMPT = """
Eres un ingeniero QA senior. Convierte la intención del usuario en pasos ejecutables.

REGLAS IMPORTANTES:
- Acciones permitidas: goto, wait_for_selector, fill, click, assert_visible, assert_text_contains.
- NO generes la acción "screenshot". La evidencia se toma automáticamente.
- Devuelve SIEMPRE JSON con la forma: {"steps":[{...}]}

NOTAS:
- "goto" debe incluir "url".
- "wait_for_selector", "click", "assert_visible" deben incluir "selector".
- "fill" debe incluir "selector" y "value".
- "assert_text_contains" debe incluir "selector" y "text".
"""

# =========================
#   HELPERS NORMALIZACIÓN
# =========================

def _normalize_steps_for_runner(steps: List[Dict[str, Any]], fallback_url: Optional[str] = None):
    """
    Normaliza steps generados por el LLM para cumplir con el runner de DigitalOcean:
    - El runner requiere "url" raíz en el body.
    - El runner NO espera "url" dentro de cada step.
    - El runner ya trae take_screenshot=true, así que filtramos cualquier screenshot (por seguridad).
    """
    # 1) filtra screenshot si se coló
    clean_steps = [s for s in steps if (s.get("action") or "").lower() != "screenshot"]

    # 2) obtiene target_url del primer goto que tenga url
    target_url = None
    for s in clean_steps:
        if (s.get("action") or "").lower() == "goto" and s.get("url"):
            target_url = s["url"]
            break

    # 3) si no hay goto/url, usa fallback
    if not target_url:
        target_url = fallback_url or "https://example.com"

    # 4) elimina url de steps (para cumplir con schema del runner DO)
    for s in clean_steps:
        s.pop("url", None)

    return target_url, clean_steps

def _call_remote_runner(url: str, steps: List[Dict[str, Any]], headless: bool = True) -> Dict[str, Any]:
    """
    Llama al runner en DigitalOcean.
    Según tu openapi.json:
      POST /run con body: { url: str, steps: [...], take_screenshot: bool }
      header opcional: x-runner-token
    """
    if not RUNNER_URL:
        raise RuntimeError("RUNNER_URL no está configurado.")

    headers = {"Content-Type": "application/json"}
    if RUNNER_TOKEN:
        headers["x-runner-token"] = RUNNER_TOKEN

    payload = {
        "url": url,
        "steps": steps,
        "take_screenshot": True,
        # headless no viene en tu schema, así que no lo mandamos (si lo soportas, lo agregas en el runner).
    }

    resp = requests.post(RUNNER_URL, json=payload, headers=headers, timeout=RUNNER_TIMEOUT_S)
    if resp.status_code >= 400:
        raise HTTPException(status_code=502, detail=f"Runner error {resp.status_code}: {resp.text}")

    return resp.json()

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
            model=OPENAI_MODEL,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": req.prompt},
            ],
            response_format=StepPlan,
        )

        plan = completion.choices[0].message.parsed
        raw_steps = [s.model_dump(exclude_none=True) for s in plan.steps]

        # Normaliza para tu runner (DO)
        fallback = req.base_url
        target_url, normalized_steps = _normalize_steps_for_runner(raw_steps, fallback_url=fallback)

        # Ejecuta:
        # - Si hay RUNNER_URL -> runner remoto (DigitalOcean)
        # - Si NO -> runner local (execute_test)
        if RUNNER_URL:
            run_result = _call_remote_runner(url=target_url, steps=normalized_steps, headless=req.headless)
        else:
            # runner local: aquí sí podrías usar headless
            run_result = execute_test([{"action": "goto", "url": target_url}] + normalized_steps, headless=req.headless)

        return {
            "generated_steps": raw_steps,          # lo que el LLM propuso
            "normalized": {
                "url": target_url,
                "steps": normalized_steps,
                "take_screenshot": True,
            },
            "run_result": run_result,
        }

    except HTTPException:
        raise
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
