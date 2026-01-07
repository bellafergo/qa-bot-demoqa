# tests/test_intent_and_doc.py
from __future__ import annotations

import os
import sys

# 🔧 Asegura que el root del proyecto esté en sys.path
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if ROOT_DIR not in sys.path:
    sys.path.insert(0, ROOT_DIR)

import pytest

from services.chat_service import (
    _detect_intent,
    _normalize_doc_json,
)
from services.execute_engine import _parse_steps_from_prompt
from core.schemas import ChatRunResponse


# ============================================================
# INTENT: execute / doc / advise(chat)
# ============================================================

def test_intent_execute_web():
    prompt = "Valida en https://demoqa.com/text-box que el campo #userName aparece visible"
    intent = _detect_intent(prompt)
    assert intent == "execute"


def test_intent_doc_artifact():
    prompt = "Genera un artefacto QA completo para login con email y password, con casos positivos, negativos y edge cases"
    intent = _detect_intent(prompt)
    assert intent == "doc"


def test_intent_advise_invest():
    prompt = 'Analiza esta historia con INVEST, riesgos y preguntas: "Como cliente quiero guardar productos en wishlist para revisarlos después"'
    intent = _detect_intent(prompt)
    # advise se mapea desde "chat"
    assert intent == "chat"


# ============================================================
# DOC normalization (nuevo contrato executive/qa/artifact)
# ============================================================

def test_doc_normalization_v2():
    raw_doc = {
        "executive": "Executive summary text",
        "qa": "QA analysis text",
        "artifact": "QA artifact detail",
    }

    doc_json = _normalize_doc_json(raw_doc, None)

    # Debe crear executive_view/qa_view para la UI
    assert "executive_view" in doc_json
    assert "qa_view" in doc_json

    ev = doc_json["executive_view"]
    assert ev["title"] == "QA Artifact"
    assert ev["objective"] == "Executive summary text"

    qa_view = doc_json["qa_view"]
    sections = qa_view.get("sections") or []
    titles = [s.get("title") for s in sections]
    assert "QA analysis" in titles
    assert "QA artifact" in titles


def test_doc_normalization_fallback():
    raw_doc = {"unexpected": "whatever"}
    raw_text = "Modelo devolvió algo raro"

    doc_json = _normalize_doc_json(raw_doc, raw_text)

    assert "executive_view" in doc_json
    assert "qa_view" in doc_json
    assert doc_json["executive_view"]["title"] == "QA Artifact"
    # En fallback, el objective es el texto fijo
    assert "Model did not return valid JSON" in doc_json["executive_view"]["objective"]


# ============================================================
# EXECUTE parser: pasos determinísticos básicos
# ============================================================

def test_parse_steps_visible_username():
    prompt = "Valida en https://demoqa.com/text-box que el campo #userName aparece visible"
    base_url = "https://demoqa.com/text-box"

    steps = _parse_steps_from_prompt(prompt, base_url)
    assert steps is not None
    assert isinstance(steps, list)
    # Debe incluir un goto a la URL
    assert any(s.get("action") == "goto" and s.get("url") == base_url for s in steps)
    # Y un assert_visible sobre #userName
    assert any(
        s.get("action") == "assert_visible" and s.get("selector") == "#userName"
        for s in steps
    )


# ============================================================
# Esquema de respuesta: forma mínima
# ============================================================

def test_chat_run_response_schema_minimal():
    """
    No llamamos al endpoint real (evitamos tocar OpenAI),
    solo verificamos que el esquema acepte el dict mínimo
    que solemos devolver.
    """
    raw = {
        "mode": "advise",
        "persona": "lead",
        "session_id": "session-123",
        "thread_id": "thread-123",
        "answer": "Hola, ¿en qué parte de tus pruebas o QA necesitas ayuda?",
        "confidence_0_1": 0.7,
        "confidence_label": "medium",
    }

    parsed = ChatRunResponse(**raw)
    assert parsed.mode == "advise"
    assert parsed.persona == "lead"
    assert parsed.answer.startswith("Hola")
