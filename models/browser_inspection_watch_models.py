# models/browser_inspection_watch_models.py
"""Phase 3C — scheduled re-inspection watches (cloud-only execution today)."""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Literal, Optional

from pydantic import BaseModel, Field


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


ChangeThreshold = Literal["low", "medium", "high"]
WatchExecutionMode = Literal["cloud"]
WatchCompareMode = Literal["last", "baseline"]
WatchLastStatus = Literal["healthy", "changed", "failed", "never_run", "disabled"]


class BrowserInspectionWatchCreate(BaseModel):
    url: str = Field(..., min_length=1, max_length=2048)
    project_id: Optional[str] = Field(default=None, max_length=256)
    interval_minutes: int = Field(default=60, ge=5, le=1440)
    change_threshold: ChangeThreshold = Field(default="medium")
    enabled: bool = True
    execution_mode: WatchExecutionMode = Field(
        default="cloud",
        description="Cloud-only; local_agent/hybrid reserved for future nodes.",
    )
    compare_mode: WatchCompareMode = Field(default="last")


class BrowserInspectionWatchPatch(BaseModel):
    url: Optional[str] = Field(default=None, min_length=1, max_length=2048)
    project_id: Optional[str] = Field(default=None, max_length=256)
    interval_minutes: Optional[int] = Field(default=None, ge=5, le=1440)
    change_threshold: Optional[ChangeThreshold] = None
    enabled: Optional[bool] = None
    compare_mode: Optional[WatchCompareMode] = None


class BrowserInspectionWatchResponse(BaseModel):
    watch_id: str
    url: str
    project_id: Optional[str] = None
    interval_minutes: int = 60
    change_threshold: ChangeThreshold = "medium"
    enabled: bool = True
    execution_mode: WatchExecutionMode = "cloud"
    compare_mode: WatchCompareMode = "last"
    baseline_inspection_id: Optional[str] = None
    baseline_set_at: Optional[str] = None
    baseline_updated_by: Optional[str] = None
    last_run_at: Optional[str] = None
    last_inspection_id: Optional[str] = None
    last_diff_id: Optional[str] = None
    last_status: WatchLastStatus = "never_run"
    current_status: WatchLastStatus = Field(
        default="never_run",
        description="Same as last_status; stable key for UI list rows.",
    )
    last_effective_change_level: Optional[str] = None
    last_change_level: Optional[str] = Field(
        default=None,
        description="Same as last_effective_change_level; UI-friendly alias.",
    )
    last_visual_change_level: Optional[str] = None
    last_alert_at: Optional[str] = None
    last_run_error: Optional[str] = None
    created_at: str
    updated_at: str


class WatchBaselineSetRequest(BaseModel):
    """Pin baseline inspection for compare_mode=baseline."""

    inspection_id: Optional[str] = Field(default=None, max_length=256)
    use_latest: bool = False
    baseline_updated_by: Optional[str] = Field(default=None, max_length=256)


class BrowserInspectionWatchRunNowResponse(BaseModel):
    watch_id: str
    inspection_id: str
    diff_id: Optional[str] = None
    base_inspection_id: Optional[str] = None
    compare_mode: WatchCompareMode = "last"
    warnings: list[str] = Field(default_factory=list)
    change_level: str = "none"
    alert_triggered: bool = False
    alert_kind: Optional[str] = None
    summary: Optional[str] = None
    created_at: str = Field(default_factory=_utc_now_iso)
    visual_change_detected: Optional[bool] = None
    visual_change_level: Optional[str] = None
    visual_similarity_score: Optional[float] = None
    alert_dedupe_suppressed: bool = False
