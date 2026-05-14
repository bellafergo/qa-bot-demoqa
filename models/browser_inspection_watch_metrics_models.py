# models/browser_inspection_watch_metrics_models.py
"""Phase 3E — operational metrics for browser inspection watches."""
from __future__ import annotations

from typing import Optional

from pydantic import BaseModel, Field


class WatchMetricsResponse(BaseModel):
    watch_id: str
    total_runs: int = 0
    total_diffs: int = 0
    alerts_triggered: int = 0
    alerts_deduped: int = 0
    last_change_level: Optional[str] = None
    last_visual_change_level: Optional[str] = None
    last_run_at: Optional[str] = None
    last_alert_at: Optional[str] = None
    failed_runs: int = 0
    visual_fetch_failures: int = 0
    current_status: str = Field(default="never_run", description="healthy|changed|failed|never_run|disabled")


class WatchEventItem(BaseModel):
    event_id: str
    event_type: str
    created_at: str
    target_inspection_id: Optional[str] = None
    base_inspection_id: Optional[str] = None
    change_level: Optional[str] = None
    summary: Optional[str] = None
    alert_triggered: bool = False


class WatchEventsPageResponse(BaseModel):
    """Cursor-paginated events (Phase 3F)."""

    items: list[WatchEventItem] = Field(default_factory=list)
    next_cursor: Optional[str] = None
