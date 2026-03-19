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


class DiffEntry(BaseModel):
    """One line in a step or assertion diff."""
    type:  str   # "added" | "removed"
    value: str   # human-readable representation of the step/assertion


class FieldChange(BaseModel):
    """A single field that changed between two versions."""
    from_value: str = ""   # serialized as "from" in JSON via alias
    to_value:   str = ""   # serialized as "to"   in JSON via alias

    model_config = {"populate_by_name": True}


class VersionDiff(BaseModel):
    """Result of comparing two version snapshots."""
    test_case_id: str
    from_version: int
    to_version:   int
    identical:    bool
    diff: dict   # { "steps": List[DiffEntry], "assertions": List[DiffEntry], "fields": Dict[str, FieldChange] }


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
