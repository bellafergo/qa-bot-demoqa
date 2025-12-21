import os
import re
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
#       CONFIG
# =========================
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY", "").strip()
OPENAI_MODEL = os.getenv("OPENAI_MODEL", "gpt-4o-mini").strip()

def get_openai_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY.")
    return OpenAI(api_key=OPENAI_API_KEY)

# Dominios que NO quieres automatizar en demo (prod, captcha, etc.)
BLOCKED_DOMAINS = [
    "instagram.com",
]

def _extract_domain(url: str) -> str:
    m = re.search(r"https?://([^/]+)", url or "")
    return (m.group(1) or "").lower() if m else ""

def _is_blocked_url(url: str) -> bool:
    d = _extract_domain(url)
    return any(b in d for b in BLOCKED_DOMAINS)

SYSTEM_PROMPT = """
Eres Vanya, QA senior. Convierte la instrucción del usuario en pasos para Playwright.

Devuelve SOLO JSON con este formato exacto:
{"steps":[ ... ]}

Acciones permitidas:
goto(url)
wait_for_selector(selector)
fill(selector,value)
click(selector)
press(selector,text)
assert_visible(selector)
assert_text_contains(selector,text)
wait_ms(value)

REGLAS:
- NO uses 'screenshot' como acción.
- SIEMPRE deja una condición de salida al final: un assert_visible o assert_text_contains.
- Si el usuario pide "validar éxito", conviértelo en un assert concreto (texto o elemento).
- Evita sitios con CAPTCHA (Google, redes, etc.)
"""

def _auto_fix_steps(user_prompt: str, steps: List[Dict[str, Any]], fallback_url: Optional[str]) -> List[Dict[str, Any]]:
    """
    Autocorrección:
    - añade goto si falta
    - bloquea dominios problemáticos
    - añade assert final si falta (condición de salida)
    - evita esperas infinitas añadiendo assert después de acciones de navegación
    """
    fixed = [s for s in steps if (s.get("action") or "").lower() != "screenshot"]

    # 1) Asegura GOTO
    has_goto = any((s.get("action") == "goto" and s.get("url")) for s in fixed)
    if not has_goto:
        url = fallback_url or "https://example.com"
        fixed.insert(0, {"action": "goto", "url": url})

    # 2) Bloquea dominios no aptos (si aparece alguno en goto)
    for s in fixed:
        if s.get("action") == "goto" and s.get("url") and _is_blocked_url(s["url"]):
            raise HTTPException(
                status_code=400,
                detail=f"Este sitio ({_extract_domain(s['url'])}) no es apto para automatización en demo (CAPTCHA/protecciones). "
                       f"Usa un sandbox como the-internet.herokuapp.com o example.com."
            )

    # 3) Si el usuario pidió “login exitoso”, fuerza un assert concreto (para herokuapp login)
    # (esto hace que la prueba no se quede colgada y termine)
    wants_login_success = any(k in user_prompt.lower() for k in ["inicia sesión", "login", "iniciar sesión", "log in"]) and \
                          any(k in user_prompt.lower() for k in ["exitos", "correct", "success"])
    if wants_login_success:
        # Si detectamos el sitio demo, ponemos el assert correcto al final
        last_goto = next((s for s in fixed if s.get("action") == "goto" and s.get("url")), None)
        if last_goto and "the-internet.herokuapp.com/login" in (last_goto.get("url") or ""):
            # aseguramos que haya click login si no existe
            if not any(s.get("action") == "click" for s in fixed):
                fixed.append({"action": "click", "selector": "button[type='submit']"})
            # assert final
            fixed.append({"action": "assert_text_contains", "selector": "#flash", "text": "You logged into a secure area!"})

    # 4) Asegura condición de salida al final
    has_assert = any((s.get("action") or "").startswith("assert_") for s in fixed)
    if not has_assert:
        # default seguro: example.com
        fixed.append({"action": "assert_text_contains", "selector": "h1", "text": "Example Domain"})

    return fixed

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

        # ✅ autocorrección aquí
        fixed_steps = _auto_fix_steps(req.prompt, raw_steps, req.base_url)

        run_result = execute_test(fixed_steps, headless=req.headless)

        return {
            "generated_steps": fixed_steps,
            "run_result": run_result
        }

    except HTTPException:
        raise
    except Exception as e:
        print(f"Error /chat_run: {e}")
        raise HTTPException(status_code=500, detail=str(e))

# (Opcional) servir frontend estático si lo usas
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "API de Vanya funcionando"}