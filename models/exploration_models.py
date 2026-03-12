# models/exploration_models.py
"""
Pydantic models for the Autonomous Exploration + Coverage Expansion block.
"""
from __future__ import annotations

import uuid
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


def _new_page_id() -> str:
    return f"PAGE-{uuid.uuid4().hex[:8].upper()}"


def _new_flow_id() -> str:
    return f"FLOW-{uuid.uuid4().hex[:8].upper()}"


# ── Exploration request ───────────────────────────────────────────────────────

class ExplorationRequest(BaseModel):
    """Bounded exploration request for a target application."""
    start_url:              str
    max_pages:              int  = 10
    max_click_depth:        int  = 2
    include_external_links: bool = False
    allowed_domain:         Optional[str] = None
    generate_draft_tests:   bool = True
    module_override:        Optional[str] = None


# ── Discovered artifacts ──────────────────────────────────────────────────────

class DiscoveredAction(BaseModel):
    """A single interactive element found on a page."""
    action_type: str            # "link" | "button" | "submit" | "menu_item"
    label:       str            # visible text or aria-label
    target_url:  Optional[str] = None
    is_unsafe:   bool = False   # blocked by safety filter


class DiscoveredPage(BaseModel):
    """A single page discovered during exploration."""
    page_id:            str = Field(default_factory=_new_page_id)
    url:                str
    title:              str = ""
    depth:              int = 0
    page_type:          str = "unknown"   # login | dashboard | listing | detail | form | settings | unknown
    discovered_actions: List[DiscoveredAction] = Field(default_factory=list)
    outgoing_links:     List[str]              = Field(default_factory=list)
    key_elements:       List[str]              = Field(default_factory=list)
    confidence:         float = 1.0


class DiscoveredFlow(BaseModel):
    """A navigation path connecting two discovered pages."""
    flow_id:         str = Field(default_factory=_new_flow_id)
    name:            str
    start_url:       str
    end_url:         str
    actions:         List[str] = Field(default_factory=list)   # action labels along the path
    depth:           int = 0
    inferred_module: str = "ui"
    confidence:      float = 1.0


# ── Results ───────────────────────────────────────────────────────────────────

class ExplorationResult(BaseModel):
    """Full result of a bounded exploration run."""
    start_url:        str
    discovered_pages: List[DiscoveredPage]  = Field(default_factory=list)
    discovered_flows: List[DiscoveredFlow]  = Field(default_factory=list)
    total_pages:      int = 0
    total_flows:      int = 0
    notes:            List[str] = Field(default_factory=list)


class CoverageExpansionResult(BaseModel):
    """Result of comparing discovered flows against the existing Test Catalog."""
    discovered_flows: List[DiscoveredFlow]           = Field(default_factory=list)
    covered_flows:    List[DiscoveredFlow]           = Field(default_factory=list)
    uncovered_flows:  List[DiscoveredFlow]           = Field(default_factory=list)
    coverage_ratio:   float = 0.0
    suggested_drafts: List[Dict[str, Any]]           = Field(default_factory=list)
    notes:            List[str]                      = Field(default_factory=list)
