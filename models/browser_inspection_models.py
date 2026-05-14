# models/browser_inspection_models.py
"""
Pydantic contracts for deterministic browser inspection (Phase 1 — no LLM).

See ``services.browser_inspector_service`` and ``POST /inspect-url``.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field, field_validator


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class InspectUrlRequest(BaseModel):
    url: str = Field(..., min_length=1, max_length=2048)
    project_id: Optional[str] = Field(default=None, max_length=256)
    timeout_ms: int = Field(default=15_000, ge=3_000, le=60_000)


class SelectorCandidate(BaseModel):
    """Single normalized selector hint for future test generation."""

    kind: str = Field(description="button | input | link | other")
    selector: str
    priority: str = Field(description="testid | id | aria | name | role | href | fallback")
    label: Optional[str] = None


class BrowserInspectionResult(BaseModel):
    inspection_id: str
    url: str
    final_url: str
    title: str = ""
    status_code: Optional[int] = None
    screenshot_url: Optional[str] = None
    console_errors: List[Dict[str, Any]] = Field(default_factory=list)
    network_errors: List[Dict[str, Any]] = Field(default_factory=list)
    headings: List[Dict[str, Any]] = Field(default_factory=list)
    links: List[Dict[str, Any]] = Field(default_factory=list)
    buttons: List[Dict[str, Any]] = Field(default_factory=list)
    inputs: List[Dict[str, Any]] = Field(default_factory=list)
    forms: List[Dict[str, Any]] = Field(default_factory=list)
    images_without_alt: List[Dict[str, Any]] = Field(default_factory=list)
    selector_candidates: List[SelectorCandidate] = Field(default_factory=list)
    performance: Dict[str, Any] = Field(default_factory=dict)
    warnings: List[str] = Field(default_factory=list)
    created_at: str = Field(default_factory=_utc_now_iso)
    # Inventory sizes from raw Playwright inventory (not trimmed response lists).
    inventory_counts: Dict[str, int] = Field(default_factory=dict)
    inspection_succeeded: bool = True
    persisted_run_id: Optional[str] = None
    persisted: bool = False
    persistence_warning: Optional[str] = None

    @field_validator("inventory_counts", mode="before")
    @classmethod
    def _default_counts(cls, v: Any) -> Dict[str, int]:
        return v if isinstance(v, dict) else {}
