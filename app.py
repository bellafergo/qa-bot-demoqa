import os
import re
from typing import List, Optional, Dict, Any, Literal

from dotenv import load_dotenv
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

from openai import OpenAI
from runner import execute_test  # runner local en Render

load_dotenv()

app = FastAPI()

# =========================
#            CORS
# =========================
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # puedes cambiar a ["https://valtre-vanya.vercel.app"]
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

Intent = Literal["execute", "plan", "info"]

class IntentResult(BaseModel):
    intent: Intent
    confidence: float = Field(ge=0.0, le=1.0, default=0.7)
    reason: Optional[str] = None
    normalized_prompt: Optional[str] = None

class ChatRunRequest(BaseModel):
    prompt: str
    base_url: Optional[str] = None
    headless: bool = True

# =========================
#       CONFIGURACIÓN
# =========================

OPENAI_API_KEY = os.getenv("OPENAI_API_KEY", "").strip()
OPENAI_MODEL_STEPS = os.getenv("OPENAI_MODEL", "gpt-4o-mini").strip()
OPENAI_MODEL_INTENT = os.getenv("OPENAI_MODEL_INTENT", "gpt-4o-mini").strip()

def get_openai_client() -> OpenAI:
    if not OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY en Render.")
    return OpenAI(api_key=OPENAI_API_KEY)

# =========================
#   PROMPTS
# =========================

INTENT_PROMPT = """
Eres un clasificador de intención para un asistente de QA que puede:
- INFO: responder capacidades y orientación (sin ejecutar navegador).
- PLAN: generar estrategia QA, checklist, matriz, casos de prueba (sin ejecutar).
- EXECUTE: ejecutar una prueba automática en navegador con Playwright.

Clasifica el mensaje del usuario en uno de:
- "info"
- "plan"
- "execute"

Reglas:
- Si NO hay un objetivo accionable (URL o acción de UI como click/fill/login/validar), NO es execute.
- "¿qué puedes hacer?" -> info.
- "genera casos/matriz/criterios" -> plan.
- "ve a/ingresa/inicia sesión/valida/ejecuta prueba" -> execute.

Devuelve SOLO JSON:
{
  "intent": "info|plan|execute",
  "confidence": 0.0-1.0,
  "reason": "breve",
  "normalized_prompt": "si intent=execute, reescribe el prompt en una instrucción clara; si no, null"
}
"""

SYSTEM_PROMPT_STEPS = """
Eres un ingeniero QA senior. Convierte la intención del usuario en pasos ejecutables para Playwright.

FORMATO:
Devuelve SIEMPRE JSON: {"steps":[{...}]}

ACCIONES SOPORTADAS:
- goto (usa url)
- wait_for_selector (usa selector, timeout_ms opcional)
- fill (usa selector y value)
- press (usa selector y text)   # text puede ser "Enter"
- click (usa selector)
- assert_visible (usa selector)
- assert_text_contains (usa selector y text)
- wait_ms (usa value o text como ms)

REGLAS IMPORTANTES:
- NO generes "screenshot". La evidencia se captura automáticamente.
- Si el usuario menciona login, prioriza selectors estables y waits.
- Para buscar en Google:
  1) goto https://www.google.com
  2) wait_for_selector textarea[name="q"] o input[name="q"]
  3) fill ...
  4) press Enter ...
"""

SYSTEM_PROMPT_INFO = """
Eres Vanya, un QA Intelligence Agent. Responde de forma clara y comercial:
- Qué puedes hacer
- Qué tipo de pruebas soportas
- Qué necesitas del usuario
- Limitaciones (CAPTCHAs/2FA, sitios que bloquean bots, credenciales reales)
Responde en español, breve y con bullets.
"""

SYSTEM_PROMPT_PLAN = """
Eres un QA Lead. Cuando el usuario pide planeación:
- Genera checklist y casos de prueba (positivo/negativo)
- Sugiere datos de prueba
- Señala riesgos y criterios de aceptación
Responde en español, estructurado y accionable.
"""

# =========================
#   HARD GUARD: “capabilities/info” SIEMPRE INFO
# =========================

_INFO_PATTERNS = [
    r"\bque puedes hacer\b",
    r"\bqué puedes hacer\b",
    r"\bque haces\b",
    r"\bqué haces\b",
    r"\bcomo funcionas\b",
    r"\bcómo funcionas\b",
    r"\bcomo funciona\b",
    r"\bcómo funciona\b",
    r"\bcapacidades\b",
    r"\bayuda\b",
    r"\binstrucciones\b",
    r"\bque eres\b",
    r"\bqué eres\b",
]

def _force_info(prompt: str) -> bool:
    p = (prompt or "").strip().lower()
    if not p:
        return True
    if len(p) <= 60:  # preguntas cortas suelen ser info
        for pat in _INFO_PATTERNS:
            if re.search(pat, p):
                return True
    # Si es pregunta general sin URL ni verbos de UI, también
    if ("http://" not in p and "https://" not in p) and ("?" in p) and not any(k in p for k in ["ve a", "ingresa", "inicia sesión", "login", "valida", "verifica", "click", "llen", "escribe"]):
        for pat in _INFO_PATTERNS:
            if re.search(pat, p):
                return True
    return False

# =========================
#   HEURÍSTICA FALLBACK
# =========================

_EXECUTE_HINTS = [
    "ve a", "ingresa a", "abre", "navega", "da click", "haz click", "click",
    "inicia sesión", "login", "llena", "escribe", "presiona", "valida", "verifica",
    "ejecuta", "corre la prueba", "prueba automatizada"
]
_PLAN_HINTS = [
    "casos de prueba", "matriz", "checklist", "criterios de aceptación", "gherkin",
    "escenarios", "plan de pruebas", "estrategia", "cobertura", "test plan"
]

def _heuristic_intent(prompt: str) -> Intent:
    p = (prompt or "").strip().lower()
    if not p:
        return "info"
    if _force_info(prompt):
        return "info"
    if any(h in p for h in _PLAN_HINTS):
        return "plan"
    if any(h in p for h in _EXECUTE_HINTS) or "http://" in p or "https://" in p:
        return "execute"
    return "info"

def classify_intent(client: OpenAI, prompt: str) -> IntentResult:
    # HARD GUARD antes del LLM
    if _force_info(prompt):
        return IntentResult(intent="info", confidence=0.99, reason="Hard-guard: pregunta de capacidades", normalized_prompt=None)

    try:
        completion = client.beta.chat.completions.parse(
            model=OPENAI_MODEL_INTENT,
            messages=[
                {"role": "system", "content": INTENT_PROMPT},
                {"role": "user", "content": prompt},
            ],
            response_format=IntentResult,
        )
        result: IntentResult = completion.choices[0].message.parsed

        # Si dice execute con baja confianza, degradamos
        if result.intent == "execute" and result.confidence < 0.55:
            h = _heuristic_intent(prompt)
            if h != "execute":
                return IntentResult(intent=h, confidence=0.6, reason="Degradado por baja confianza", normalized_prompt=None)

        # Extra hard guard por si el LLM se equivoca
        if result.intent == "execute" and _force_info(prompt):
            return IntentResult(intent="info", confidence=0.95, reason="Hard-guard post: capacidades", normalized_prompt=None)

        return result

    except Exception:
        h = _heuristic_intent(prompt)
        return IntentResult(intent=h, confidence=0.6, reason="Fallback heurístico", normalized_prompt=None)

# =========================
#   ENDPOINTS
# =========================

@app.get("/health")
def health():
    return {"ok": True, "message": "Vanya está despierta"}

@app.post("/chat_run")
def chat_run(req: ChatRunRequest):
    client = get_openai_client()

    intent_res = classify_intent(client, req.prompt)

    if intent_res.intent == "info":
        completion = client.chat.completions.create(
            model=OPENAI_MODEL_STEPS,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT_INFO},
                {"role": "user", "content": req.prompt},
            ],
        )
        answer = completion.choices[0].message.content or ""
        return {"mode": "info", "intent": intent_res.model_dump(), "answer": answer}

    if intent_res.intent == "plan":
        completion = client.chat.completions.create(
            model=OPENAI_MODEL_STEPS,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT_PLAN},
                {"role": "user", "content": req.prompt},
            ],
        )
        answer = completion.choices[0].message.content or ""
        return {"mode": "plan", "intent": intent_res.model_dump(), "answer": answer}

    # EXECUTE
    try:
        user_for_steps = intent_res.normalized_prompt or req.prompt

        completion = client.beta.chat.completions.parse(
            model=OPENAI_MODEL_STEPS,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT_STEPS},
                {"role": "user", "content": user_for_steps},
            ],
            response_format=StepPlan,
        )

        plan = completion.choices[0].message.parsed
        steps = [s.model_dump(exclude_none=True) for s in plan.steps]

        # Guardrail goto
        if not any((s.get("action") or "").lower() == "goto" for s in steps):
            fallback_url = req.base_url or "https://example.com"
            steps.insert(0, {"action": "goto", "url": fallback_url})

        run_result = execute_test(steps, headless=req.headless)

        return {
            "mode": "execute",
            "intent": intent_res.model_dump(),
            "generated_steps": steps,
            "run_result": run_result,
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# ==========================================
#   SERVIR FRONTEND (opcional)
# ==========================================
if os.path.exists("frontend"):
    app.mount("/client", StaticFiles(directory="frontend", html=True), name="frontend")

@app.get("/")
def home():
    return {"ok": True, "message": "API de Vanya funcionando"}