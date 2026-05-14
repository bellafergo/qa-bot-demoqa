# models/browser_inspection_diff_models.py
"""Phase 3B — deterministic diff between two persisted browser inspections (no IA)."""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


ChangeLevel = Literal["none", "low", "medium", "high"]


class BrowserInspectionDiffRequest(BaseModel):
    base_inspection_id: str = Field(..., min_length=1, max_length=256)
    target_inspection_id: str = Field(..., min_length=1, max_length=256)
    project_id: Optional[str] = Field(default=None, max_length=256)


class CountsDelta(BaseModel):
    links_count: int = 0
    buttons_count: int = 0
    inputs_count: int = 0
    forms_count: int = 0
    images_without_alt_count: int = 0
    selector_candidates_count: int = 0


class BrowserInspectionChanges(BaseModel):
    title_changed: bool = False
    final_url_changed: bool = False
    status_code_changed: bool = False
    page_type_changed: bool = False
    counts_delta: CountsDelta = Field(default_factory=CountsDelta)
    console_errors_delta: int = 0
    network_errors_delta: int = 0
    warnings_delta: int = 0
    risk_notes_delta: int = 0
    primary_actions_changed: List[str] = Field(default_factory=list)
    suggested_test_flows_changed: List[str] = Field(default_factory=list)


class BrowserInspectionDiffResponse(BaseModel):
    base_inspection_id: str
    target_inspection_id: str
    change_level: ChangeLevel
    summary: str
    changes: BrowserInspectionChanges
    regression_signals: List[str] = Field(default_factory=list)
    improvement_signals: List[str] = Field(default_factory=list)
    warnings: List[str] = Field(default_factory=list)
    created_at: str = Field(default_factory=_utc_now_iso)
    # Phase 3D — lightweight screenshot hash compare (optional; degrades if URLs missing).
    visual_change_detected: bool = False
    visual_change_level: Optional[ChangeLevel] = None
    visual_hash_changed: Optional[bool] = None
    visual_similarity_score: Optional[float] = None
