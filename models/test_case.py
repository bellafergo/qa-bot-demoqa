# models/test_case.py
"""
Pydantic models for the Test Catalog.

TestCase  — a single reusable test case stored in the catalog.
TestCaseCreate — the request body for POST /tests.
TestCaseUpdate — partial update body for PATCH /tests/{id}.
"""
from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


def _new_uuid() -> str:
    return str(uuid.uuid4())


# ── Step / Assertion leaf types ───────────────────────────────────────────────

class TestStep(BaseModel):
    """One action step in a test case.

    Supports both the catalog format (action/target/value) and
    direct runner format (action/selector/url/key/ms/text).
    The orchestrator normalizes to runner format before execution.
    """
    action: str                             # goto | input | click | press | wait_ms | assert_*
    target: Optional[str] = None           # element intent / natural-language label
    value:  Optional[str] = None           # fill value, URL for goto, key for press
    url:    Optional[str] = None           # explicit URL (runner compat)
    selector: Optional[str] = None        # explicit CSS/locator (runner compat)
    key:    Optional[str] = None           # key for press action
    ms:     Optional[int] = None           # ms for wait_ms action
    text:   Optional[str] = None          # expected text for assert_text_contains

    model_config = {"extra": "allow"}     # tolerate additional runner-specific fields


class TestAssertion(BaseModel):
    """One assertion to validate after steps execute."""
    type: str                              # text_visible | url_contains | element_visible | element_not_visible
    value: Optional[str] = None           # expected text or URL fragment
    target: Optional[str] = None          # element intent / selector (for element_* types)
    selector: Optional[str] = None       # explicit CSS selector

    model_config = {"extra": "allow"}


# ── Main TestCase model ───────────────────────────────────────────────────────

class TestCase(BaseModel):
    """A test case stored in the catalog."""
    id:           str      = Field(default_factory=_new_uuid)
    test_case_id: str                      # TC-DEMO-001, TC-LOGIN-042, …
    name:         str
    module:       str                      # demoqa | login | cart | checkout | …
    type:         Literal["smoke", "regression", "functional", "negative", "e2e"]
    priority:     Literal["low", "medium", "high", "critical"]
    status:       Literal["active", "inactive"] = "active"
    test_type:    Literal["ui", "api"] = "ui"   # runner path: ui → Playwright, api → API runner
    version:      int = 1
    tags:         List[str] = Field(default_factory=list)
    base_url:     Optional[str] = None    # override execution base URL
    steps:        List[TestStep]
    assertions:   List[TestAssertion] = Field(default_factory=list)
    created_at:   datetime = Field(default_factory=_now_utc)
    updated_at:   datetime = Field(default_factory=_now_utc)

    model_config = {"extra": "ignore"}


# ── Request / response bodies ─────────────────────────────────────────────────

class TestCaseCreate(BaseModel):
    """Request body for POST /tests."""
    test_case_id: str
    name:         str
    module:       str
    type:         Literal["smoke", "regression", "functional", "negative", "e2e"]
    priority:     Literal["low", "medium", "high", "critical"]
    status:       Literal["active", "inactive"] = "active"
    test_type:    Literal["ui", "api"] = "ui"
    version:      int = 1
    tags:         List[str] = Field(default_factory=list)
    base_url:     Optional[str] = None
    steps:        List[Dict[str, Any]]
    assertions:   List[Dict[str, Any]] = Field(default_factory=list)

    model_config = {"extra": "ignore"}


class TestCaseSummary(BaseModel):
    """Compact representation used in list responses."""
    id:           str
    test_case_id: str
    name:         str
    module:       str
    type:         str
    priority:     str
    status:       str
    test_type:    str = "ui"
    version:      int
    tags:         List[str]
    steps_count:  int
    created_at:   datetime
    updated_at:   datetime
