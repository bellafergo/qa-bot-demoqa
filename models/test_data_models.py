# models/test_data_models.py
"""
Pydantic models for the Synthetic Test Data Generation layer.
"""
from __future__ import annotations

from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


# ── Input ─────────────────────────────────────────────────────────────────────

class TestDataRequest(BaseModel):
    """Request to generate synthetic test data for one entity type."""

    entity_type:  str                       # user | product | order | payment | address
    count:        int            = 1        # number of entities to generate
    seed:         Optional[int]  = None     # optional seed for deterministic output
    constraints:  Dict[str, Any] = Field(default_factory=dict)  # e.g. {"email_domain": "example.com"}


# ── Output ────────────────────────────────────────────────────────────────────

class TestDataResponse(BaseModel):
    """Generated synthetic data for a single entity type."""

    entity_type:  str
    data:         List[Dict[str, Any]]
    count:        int
    seed_used:    Optional[int]  = None
    notes:        List[str]      = Field(default_factory=list)
