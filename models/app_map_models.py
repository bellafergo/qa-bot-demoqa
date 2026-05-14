# models/app_map_models.py
"""
Semantic single-page app map (Phase 2 — deterministic heuristics, no LLM).

Built on top of ``BrowserInspectionResult`` from ``/inspect-url`` pipeline.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

from models.browser_inspection_models import SelectorCandidate

PageType = Literal[
    "login_page",
    "dashboard",
    "landing_page",
    "form_page",
    "crud_table",
    "search_interface",
    "ecommerce_page",
    "checkout_page",
    "profile_settings",
    "error_page",
    "unknown",
]

ExecutionMode = Literal["cloud"]  # TODO: extend with local_agent | hybrid when nodes exist.


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class InspectUrlMapRequest(BaseModel):
    """Request for ``POST /inspect-url/map`` (inspection + semantic map)."""

    url: str = Field(..., min_length=1, max_length=2048)
    project_id: Optional[str] = Field(default=None, max_length=256)
    timeout_ms: int = Field(default=15_000, ge=3_000, le=60_000)
    execution_mode: ExecutionMode = Field(
        default="cloud",
        description="Cloud-only today; local_agent/hybrid reserved for on-prem runners.",
    )
    # Future persistence / hybrid (ignored by server logic today except echo in persist_hints).
    local_agent_id: Optional[str] = Field(default=None, max_length=256)
    retention_policy: Optional[str] = Field(default=None, max_length=128)


class AppMapResponse(BaseModel):
    inspection_id: str
    url: str
    final_url: str
    page_type: List[PageType]
    confidence: Literal["high", "medium", "low"]
    detected_patterns: List[str] = Field(default_factory=list)
    main_navigation: List[Dict[str, Any]] = Field(default_factory=list)
    primary_actions: List[Dict[str, Any]] = Field(default_factory=list)
    forms: List[Dict[str, Any]] = Field(default_factory=list)
    tables: List[Dict[str, Any]] = Field(default_factory=list)
    search_elements: List[Dict[str, Any]] = Field(default_factory=list)
    risk_notes: List[str] = Field(default_factory=list)
    suggested_test_flows: List[str] = Field(default_factory=list)
    selector_candidates: List[SelectorCandidate] = Field(default_factory=list)
    warnings: List[str] = Field(default_factory=list)
    created_at: str = Field(default_factory=_utc_now_iso)
    # Contract for future qa_runs / object storage — not persisted yet.
    persist_hints: Dict[str, Any] = Field(default_factory=dict)
    persisted_run_id: Optional[str] = None
    persisted: bool = False
    persistence_warning: Optional[str] = None
