# models/browser_visual_diff_models.py
"""Lightweight visual comparison metadata (Phase 3D — no IA, no pixel-perfect diff)."""
from __future__ import annotations

from typing import Literal, Optional

from pydantic import BaseModel, Field

VisualChangeLevel = Literal["none", "low", "medium", "high"]


class BrowserVisualDiffResult(BaseModel):
    """Result of comparing two screenshot URLs (bounded fetch + average-hash)."""

    visual_change_detected: bool = False
    visual_change_level: Optional[VisualChangeLevel] = None
    visual_hash_changed: Optional[bool] = None
    visual_similarity_score: Optional[float] = Field(
        default=None,
        description="1.0 = identical average-hash; lower when Hamming distance increases.",
    )
    base_visual_hash: Optional[str] = Field(default=None, max_length=32)
    target_visual_hash: Optional[str] = Field(default=None, max_length=32)
    warnings: list[str] = Field(default_factory=list)
