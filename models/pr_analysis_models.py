# models/pr_analysis_models.py
"""
Pydantic models for the PR Impact Analysis layer.
"""
from __future__ import annotations

import uuid
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field, model_validator


def _new_id() -> str:
    return str(uuid.uuid4())[:8]


# ── Input ─────────────────────────────────────────────────────────────────────

class PRAnalysisRequest(BaseModel):
    """Represents a pull request or diff to be analyzed for QA impact."""

    pr_id:              Optional[str]       = None
    title:              Optional[str]       = None
    description:        Optional[str]       = None
    changed_files:      List[str]           = Field(default_factory=list)
    diff_text:          Optional[str]       = None

    # Caller can explicitly name affected modules (merged with inferred ones)
    changed_modules:    List[str]           = Field(default_factory=list)

    author:             Optional[str]       = None
    branch:             Optional[str]       = None

    # Behaviour flags
    auto_enqueue:           bool = False
    generate_draft_tests:   bool = False
    environment:            str  = "default"
    project_id:             Optional[str] = None


# ── Output atoms ──────────────────────────────────────────────────────────────

class DraftTestSuggestion(BaseModel):
    """A candidate test case proposed by the analyzer — not yet in the catalog."""

    draft_id:             str  = Field(default_factory=_new_id)
    name:                 str
    module:               str
    rationale:            str
    suggested_steps:      List[Dict[str, Any]] = Field(default_factory=list)
    suggested_assertions: List[Dict[str, Any]] = Field(default_factory=list)
    source_signal:        str  = ""
    confidence:           Literal["low", "medium", "high"] = "medium"


# ── Primary output ────────────────────────────────────────────────────────────

class PRAnalysisResult(BaseModel):
    """Full QA impact analysis result for a pull request."""

    pr_id:                  Optional[str]  = None

    # What was inferred
    inferred_modules:       List[str]      = Field(default_factory=list)
    inferred_risk_level:    Literal["low", "medium", "high"] = "low"
    risk_reasons:           List[str]      = Field(default_factory=list)

    # Changed-file surface: UI paths, API/backend paths, or both (heuristic)
    change_surface:         Literal["ui", "api", "mixed"] = "mixed"

    # Matched catalog tests
    matched_test_case_ids:  List[str]      = Field(default_factory=list)
    matched_tests_count:    int            = 0

    # Same matches as above, split by catalog test_type (api vs ui/desktop)
    recommended_api_tests:  List[str]      = Field(default_factory=list)
    recommended_ui_tests:   List[str]      = Field(default_factory=list)

    # Draft suggestions (populated when generate_draft_tests=True)
    suggested_new_tests:    List[DraftTestSuggestion] = Field(default_factory=list)

    # Orchestrator job (populated when auto_enqueue=True and matches exist)
    orchestrator_job_id:    Optional[str]  = None

    # Per-test match explanations: { test_case_id: "reason string" }
    test_match_reasons:     Dict[str, str] = Field(default_factory=dict)

    # Human-readable summary
    summary:                str            = ""
    confidence:             Literal["low", "medium", "high"] = "medium"


# ── Project-scoped PR Analysis v1 (System Memory + Risk Engine) ───────────────

class ProjectPRAnalysisRequest(BaseModel):
    """Manual or PR-based change list scoped to a catalog project."""

    changed_files: List[str] = Field(default_factory=list)
    repo: Optional[str] = None
    branch: Optional[str] = None
    pr_id: Optional[str] = None
    title: Optional[str] = None
    description: Optional[str] = None
    # path → unified diff patch (e.g. from GitHub list PR files)
    file_patches: Dict[str, str] = Field(default_factory=dict)


ChangeClass = Literal[
    "comments",
    "docs",
    "formatting",
    "imports",
    "test_only",
    "config",
    "schema",
]


class FileChangeClassification(BaseModel):
    """Deterministic change-type classification for one file (CCE output)."""

    file_path: str
    primary_class: ChangeClass
    confidence: float = Field(ge=0.0, le=1.0)
    signals: List[str] = Field(default_factory=list)


class FileModuleMapping(BaseModel):
    file_path: str
    module: str
    confidence: float = Field(ge=0.0, le=1.0)
    reason: str = ""


class ImpactedModuleReport(BaseModel):
    module: str
    module_risk_score: float = 0.0
    module_risk_level: str = "LOW"
    matched_files: List[str] = Field(default_factory=list)
    reasons: List[str] = Field(default_factory=list)


class PRRecommendedTest(BaseModel):
    test_case_id: str
    name: str = ""
    module: str = ""
    reason: str = ""


class ProjectPRAnalysisReport(BaseModel):
    """PR Analysis Report v1 — project baseline risk + PR-scoped fields + CCE."""

    project_id: str
    project_name: str = ""
    changed_files_count: int = 0
    impacted_modules: List[ImpactedModuleReport] = Field(default_factory=list)
    file_mappings: List[FileModuleMapping] = Field(default_factory=list)
    file_classifications: List[FileChangeClassification] = Field(default_factory=list)

    # Project baseline (System Memory / Risk Engine — not PR-specific)
    project_risk_score: float = Field(default=0.0, ge=0.0, le=100.0)
    project_risk_level: str = "LOW"

    # PR-specific risk (Phase 0: mirrors project baseline until PR Risk Composer)
    pr_risk_score: float = Field(default=0.0, ge=0.0, le=100.0)
    pr_risk_level: str = "LOW"

    # Deprecated — mirrors pr_risk_* for backward-compatible clients
    risk_score: float = Field(default=0.0, ge=0.0, le=100.0)
    risk_level: str = "LOW"

    recommended_tests: List[PRRecommendedTest] = Field(default_factory=list)
    reasoning: List[str] = Field(default_factory=list)
    summary: str = ""
    engine_version: str = "pr-v1.1"

    @model_validator(mode="after")
    def _sync_deprecated_risk_aliases(self) -> "ProjectPRAnalysisReport":
        """Keep deprecated risk_score/risk_level aligned with pr_risk_*."""
        object.__setattr__(self, "risk_score", self.pr_risk_score)
        object.__setattr__(self, "risk_level", self.pr_risk_level)
        return self
