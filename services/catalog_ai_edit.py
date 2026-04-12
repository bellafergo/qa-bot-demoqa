# services/catalog_ai_edit.py
"""
LLM-assisted edit proposal for catalog test steps/assertions (preview only; no persist).
"""
from __future__ import annotations

import json
import logging
from typing import Any, Dict, List, Tuple

from core.chat_helpers import parse_tool_args
from core.settings import settings
from models.test_case import TestAssertion, TestStep

logger = logging.getLogger("vanya.catalog_ai_edit")

MAX_INSTRUCTION_LEN = 8000
MAX_STEPS = 200
MAX_ASSERTIONS = 100


def validate_ai_edit_payload(data: Any) -> Tuple[List[Dict[str, Any]], List[Dict[str, Any]], List[str]]:
    """
    Ensure LLM JSON matches catalog step/assertion shape (Pydantic + extra fields allowed on models).
    """
    if not isinstance(data, dict):
        raise ValueError("La respuesta de la IA no es un objeto JSON válido.")

    steps_raw = data.get("steps")
    assertions_raw = data.get("assertions")
    if not isinstance(steps_raw, list) or not isinstance(assertions_raw, list):
        raise ValueError("La respuesta debe incluir listas `steps` y `assertions`.")

    if len(steps_raw) > MAX_STEPS or len(assertions_raw) > MAX_ASSERTIONS:
        raise ValueError("La propuesta excede el tamaño máximo permitido.")

    cs = data.get("change_summary")
    if cs is None:
        change_summary: List[str] = []
    elif isinstance(cs, str) and cs.strip():
        change_summary = [cs.strip()]
    elif isinstance(cs, list):
        change_summary = [str(x) for x in cs if str(x).strip()]
    else:
        raise ValueError("`change_summary` debe ser una lista de textos u omitirse.")

    steps_out: List[Dict[str, Any]] = []
    for i, s in enumerate(steps_raw):
        if not isinstance(s, dict):
            raise ValueError(f"steps[{i}] debe ser un objeto.")
        steps_out.append(TestStep.model_validate(s).model_dump(mode="json"))

    assertions_out: List[Dict[str, Any]] = []
    for i, a in enumerate(assertions_raw):
        if not isinstance(a, dict):
            raise ValueError(f"assertions[{i}] debe ser un objeto.")
        assertions_out.append(TestAssertion.model_validate(a).model_dump(mode="json"))

    return steps_out, assertions_out, change_summary


def run_catalog_ai_edit(
    test_case_id: str,
    name: str,
    module: str,
    priority: str,
    steps: List[Dict[str, Any]],
    assertions: List[Dict[str, Any]],
    instruction: str,
) -> Dict[str, Any]:
    """
    Call OpenAI to propose updated steps/assertions. Raises ValueError on bad input/validation.
    """
    from openai import OpenAI

    instruction = (instruction or "").strip()
    if not instruction:
        raise ValueError("La instrucción no puede estar vacía.")
    if len(instruction) > MAX_INSTRUCTION_LEN:
        raise ValueError("La instrucción es demasiado larga.")

    if not settings.OPENAI_API_KEY:
        raise ValueError("OPENAI_API_KEY no está configurada en el servidor.")

    system = (
        "Eres un asistente que edita casos de prueba del catálogo Vanya. "
        "Debes responder SOLO con un JSON válido (sin markdown) con exactamente estas claves:\n"
        '- "steps": array de objetos de paso (cada uno con al menos "action" string; '
        'campos opcionales: target, value, url, selector, key, ms, text, etc.).\n'
        '- "assertions": array de objetos (cada uno con al menos "type" string; '
        'opcional: value, target, selector).\n'
        '- "change_summary": array de strings breves en español describiendo qué cambiaste.\n\n'
        "Preserva el estilo del catálogo: acciones como goto, input, click, press, wait_ms, "
        "assert_url_contains, assert_text_contains, assert_visible, etc. "
        "No inventes campos obligatorios que rompan el esquema. "
        "Si el usuario pide cambiar tipo (smoke/regresión) o prioridad, reflejalo en los pasos/assertions "
        "pero NO devuelvas esos campos en el JSON (solo steps, assertions, change_summary)."
    )

    user_payload = {
        "test_case_id": test_case_id,
        "name": name,
        "module": module,
        "priority": priority,
        "steps": steps,
        "assertions": assertions,
        "instruction": instruction,
    }

    client = OpenAI(api_key=settings.OPENAI_API_KEY)
    try:
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            temperature=min(settings.EXEC_TEMPERATURE, 0.35),
            max_tokens=min(settings.EXEC_MAX_TOKENS * 4, 4096),
            response_format={"type": "json_object"},
            messages=[
                {"role": "system", "content": system},
                {
                    "role": "user",
                    "content": (
                        "Modifica el siguiente caso según la instrucción. "
                        "Devuelve el JSON completo con steps y assertions resultantes.\n\n"
                        + json.dumps(user_payload, ensure_ascii=False)
                    ),
                },
            ],
        )
    except Exception as e:
        logger.exception("OpenAI catalog ai-edit failed: %s", e)
        raise ValueError("No se pudo contactar al modelo de IA. Intentá de nuevo más tarde.") from e

    raw = (resp.choices[0].message.content or "").strip()
    parsed = parse_tool_args(raw)
    if not isinstance(parsed, dict):
        raise ValueError("La IA no devolvió JSON interpretable.")

    steps_out, assertions_out, change_summary = validate_ai_edit_payload(parsed)
    return {
        "steps": steps_out,
        "assertions": assertions_out,
        "change_summary": change_summary,
    }
