# core/schemas.py
from __future__ import annotations

from typing import Any, Dict, Optional
from pydantic import BaseModel, Field


class RunnerMeta(BaseModel):
    """
    Contrato estable para la información del runner que la UI necesita.
    """
    status: str = "unknown"
    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    duration_ms: Optional[int] = None
    pdf_error: Optional[str] = None

    class Config:
        extra = "allow"  # deja pasar campos adicionales sin romper


class ChatRunResponse(BaseModel):
    """
    Respuesta estándar de /chat_run.

    Nota: por ahora la usamos solo en tests para validar forma.
    Más adelante podemos hacer que handle_chat_run la construya siempre.
    """
    mode: str
    persona: str
    session_id: str
    thread_id: str
    answer: str

    runner: Optional[RunnerMeta] = None
    status_label: Optional[str] = None

    evidence_id: Optional[str] = None
    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    duration_ms: Optional[int] = None

    doc_json: Optional[Dict[str, Any]] = None

    confidence_0_1: Optional[float] = Field(default=None)
    confidence_label: Optional[str] = None

    class Config:
        extra = "allow"  # por ahora no rompemos si hay más campos
