# models/test_generation_models.py
"""
Pydantic models for the AI Test Generation layer.
"""
from __future__ import annotations

import uuid
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field, model_validator


def _new_draft_id() -> str:
    return str(uuid.uuid4())[:8]


# ── Input ─────────────────────────────────────────────────────────────────────

class TestGenerationRequest(BaseModel):
    """Request to generate draft test cases from text / module signals."""

    prompt:           Optional[str]  = None   # free-form intent / feature description
    requirement_text: Optional[str]  = None   # user story or formal requirement text
    title:            Optional[str]  = None   # short feature title

    module:           Optional[str]  = None   # explicit module hint
    type:             Optional[str]  = None   # preferred test type
    priority:         Optional[str]  = None   # preferred priority

    source:           Optional[str]  = None   # "prompt" | "requirement" | "pr-analysis"
    changed_modules:  List[str]      = Field(default_factory=list)
    risk_level:       Optional[str]  = None   # "low" | "medium" | "high"

    max_drafts:       int            = 5

    @model_validator(mode="after")
    def at_least_one_signal(self) -> "TestGenerationRequest":
        if not any([self.prompt, self.requirement_text, self.title]):
            raise ValueError(
                "At least one of 'prompt', 'requirement_text', or 'title' must be provided."
            )
        return self


# ── Draft ─────────────────────────────────────────────────────────────────────

class DraftGeneratedTest(BaseModel):
    """A candidate test case proposed by the generator — not yet in the catalog."""

    draft_id:   str  = Field(default_factory=_new_draft_id)
    name:       str
    module:     str
    type:       str
    priority:   str
    rationale:  str
    steps:      List[Dict[str, Any]] = Field(default_factory=list)
    assertions: List[Dict[str, Any]] = Field(default_factory=list)
    confidence: Literal["low", "medium", "high"] = "medium"
    source_signal: str = ""


# ── Output ────────────────────────────────────────────────────────────────────

class TestGenerationResponse(BaseModel):
    """Result of a test generation request."""

    drafts:              List[DraftGeneratedTest]
    total_drafts:        int
    generation_strategy: str
    notes:               List[str] = Field(default_factory=list)


# ── Approval ──────────────────────────────────────────────────────────────────

class ApproveDraftRequest(BaseModel):
    """Request to promote draft tests into the active Test Catalog."""

    drafts:   List[DraftGeneratedTest]
    activate: bool = True   # True → status=active; False → status=inactive


class ApproveDraftResponse(BaseModel):
    """Result of a draft approval request."""

    created_test_case_ids: List[str]
    skipped_draft_ids:     List[str]
    total_created:         int
    total_skipped:         int
