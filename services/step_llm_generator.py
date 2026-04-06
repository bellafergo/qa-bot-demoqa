# services/step_llm_generator.py
"""
Fallback LLM para generación de steps cuando la compilación determinística falla.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

from core.settings import settings
from core import chat_helpers as H

logger = logging.getLogger("vanya.step_llm")


def generate_steps_llm(
    prompt: str,
    base_url: str,
    messages: List[Dict[str, str]],
) -> Optional[List[Dict[str, Any]]]:
    """
    Genera steps via LLM cuando compile_steps_from_prompt no produce resultado.

    Args:
        prompt: Texto de la solicitud del usuario.
        base_url: URL base para el test.
        messages: Historial de mensajes (sistema + usuario) para contexto.

    Returns:
        Lista de steps o None si falla.
    """
    try:
        from fastapi import HTTPException
        from openai import OpenAI

        if not settings.OPENAI_API_KEY:
            raise HTTPException(status_code=500, detail="Missing OPENAI_API_KEY")
        client = OpenAI(api_key=settings.OPENAI_API_KEY)

        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages
            + [
                {
                    "role": "user",
                    "content": (
                        f"Base URL: {base_url}\n"
                        f"User request:\n{prompt}\n\n"
                        "Generate Playwright steps using ONLY these actions:\n"
                        "- goto with \"url\" (absolute or path relative to base)\n"
                        "- fill: REQUIRED keys \"selector\" (CSS) AND \"value\". Optional \"target\" dict with primary=fallback CSS.\n"
                        "- click: REQUIRED \"selector\" (CSS). Never emit click without selector.\n"
                        "- press(selector,key)\n"
                        "- wait_ms(ms)\n- assert_visible(selector)\n- assert_not_visible(selector)\n"
                        "- assert_url_contains(value)\n- assert_text_contains(selector,text)\n\n"
                        "CRITICAL: Every fill and click MUST include a non-empty \"selector\" string (CSS).\n"
                        "If the user says the value first (e.g. Spanish: \"escribe X en el campo de email\"), "
                        "you must infer the field CSS:\n"
                        "- email / campo de email → input[type=\"email\"]\n"
                        "- password / contraseña → input[type=\"password\"]\n"
                        "- search / búsqueda → input[type=\"search\"]\n"
                        "- text field / campo de texto → input[type=\"text\"]\n"
                        "- first field / primer campo → input:first-of-type\n"
                        "For submit / Sign in / Iniciar sesión: prefer button[type=\"submit\"], "
                        "or button:has-text(\"Sign in\") if needed.\n\n"
                        "- For assert_text_contains: use selector=\"body\" and text=expected string.\n"
                        "- Prefer stable CSS ([type=...], [name=...]) over random classes.\n\n"
                        'Return ONLY valid JSON: {"steps":[{"action":"goto","url":"..."}, ...]}\n'
                        "No commentary."
                    ),
                }
            ],
            temperature=settings.EXEC_TEMPERATURE,
            max_tokens=settings.EXEC_MAX_TOKENS,
        )
        raw = (resp.choices[0].message.content or "").strip()
        return H.extract_steps_from_text(raw)
    except Exception as e:
        logger.exception("generate_steps_llm failed: %s", e)
        return None
