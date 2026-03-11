# core/schemas.py
from __future__ import annotations

from typing import Any, Dict, List, Optional, Union
from pydantic import BaseModel, Field


# ============================================================
# Step / Target contract
# ============================================================

class TargetFallback(BaseModel):
    """
    A single fallback strategy for locator resolution.
    type: "css" | "text" | "role" | "name" | "testid"
    value: selector string, role dict, or plain text depending on type.
    """
    type: str
    value: Any

    class Config:
        extra = "allow"


class TargetSpec(BaseModel):
    """
    Structured locator descriptor passed to the selector healer.
    primary is tried first; fallbacks are tried in order.
    """
    primary: str
    fallbacks: List[TargetFallback] = Field(default_factory=list)
    timeout_ms: int = 3000
    state: str = "visible"
    intent: Optional[str] = None

    class Config:
        extra = "allow"


class StepSpec(BaseModel):
    """
    Execution step. Supports both legacy (selector) and new (target) shapes.
    """
    action: str
    # legacy flat selector — preserved for backward compat
    selector: Optional[str] = None
    # new structured target
    target: Optional[TargetSpec] = None
    # action-specific fields
    url: Optional[str] = None
    value: Optional[str] = None
    text: Optional[str] = None
    key: Optional[str] = None
    ms: Optional[int] = None
    expected: Optional[str] = None
    timeout_ms: Optional[int] = None

    class Config:
        extra = "allow"  # allow additional fields without breaking


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
