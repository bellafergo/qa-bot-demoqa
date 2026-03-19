# models/test_version_models.py
"""
Pydantic models for the test versioning system.

Each TestVersionSummary / TestVersionDetail represents a complete snapshot
of a test case at a given version_number.  The test_cases table always holds
the HEAD (current) state; test_versions holds every historical snapshot.
"""
from __future__ import annotations

from datetime import datetime
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class TestVersionSummary(BaseModel):
    """Compact version record — used in list responses."""
    id:             str
    test_case_id:   str
    version_number: int
    name:           str
    module:         str
    type:           str
    priority:       str
    status:         str
    test_type:      str
    source:         str       # manual | draft | ai | dom | pr | explorer | rollback
    change_note:    str
    created_at:     datetime


class TestVersionDetail(TestVersionSummary):
    """Full snapshot — includes steps, assertions, tags, base_url."""
    steps:      List[Dict[str, Any]] = Field(default_factory=list)
    assertions: List[Dict[str, Any]] = Field(default_factory=list)
    tags:       List[str]            = Field(default_factory=list)
    base_url:   Optional[str]        = None


class RollbackRequest(BaseModel):
    """Request body for POST /tests/{id}/rollback."""
    version: int
    reason:  str = ""


class RollbackResponse(BaseModel):
    """Rollback result — always reflects the new version created."""
    ok:                      bool
    test_case_id:            str
    rolled_back_from_version: int
    new_version:             int
    message:                 str = ""
