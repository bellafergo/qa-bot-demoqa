# models/draft_models.py
"""
Pydantic models for the persistent Drafts system.

Drafts are proposed test cases stored in the `drafts` table.
They are independent from the active test catalog until explicitly approved.
"""
from __future__ import annotations

from datetime import datetime
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field


class DraftCreate(BaseModel):
    name:        str
    module:      str                                     = "unknown"
    rationale:   str                                     = ""
    confidence:  Literal["low", "medium", "high"]        = "medium"
    source:      str                                     = "manual"
    steps:       List[Dict[str, Any]]                   = Field(default_factory=list)
    assertions:  List[Dict[str, Any]]                   = Field(default_factory=list)
    meta:        Dict[str, Any]                         = Field(default_factory=dict)


class DraftUpdate(BaseModel):
    name:        Optional[str]                           = None
    module:      Optional[str]                           = None
    rationale:   Optional[str]                           = None
    confidence:  Optional[Literal["low", "medium", "high"]] = None
    steps:       Optional[List[Dict[str, Any]]]         = None
    assertions:  Optional[List[Dict[str, Any]]]         = None


class Draft(BaseModel):
    draft_id:    str
    name:        str
    module:      str
    rationale:   str
    confidence:  str
    source:      str
    status:      str                                     # draft | approved | discarded
    steps:       List[Dict[str, Any]]
    assertions:  List[Dict[str, Any]]
    meta:        Dict[str, Any]
    created_at:  datetime
    updated_at:  datetime


class DraftApproveResponse(BaseModel):
    draft_id:     str
    test_case_id: str
    status:       str = "approved"


class AISuggestResponse(BaseModel):
    draft_id:               str
    suggested_name:         Optional[str]            = None
    suggested_steps:        List[Dict[str, Any]]     = Field(default_factory=list)
    suggested_assertions:   List[Dict[str, Any]]     = Field(default_factory=list)
    rationale_improvements: str                      = ""
    confidence:             str                      = "medium"
    note:                   str                      = ""


# ── Batch create ───────────────────────────────────────────────────────────────

class DraftBatchCreate(BaseModel):
    drafts: List[DraftCreate]


class DraftBatchItemResult(BaseModel):
    index:    int
    name:     str
    draft_id: Optional[str] = None
    error:    Optional[str] = None


class DraftBatchResult(BaseModel):
    saved:       List[DraftBatchItemResult] = Field(default_factory=list)
    errors:      List[DraftBatchItemResult] = Field(default_factory=list)
    saved_count: int = 0
    error_count: int = 0
