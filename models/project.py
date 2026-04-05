# models/project.py
"""Project (multi-tenant / workspace) metadata for catalog scoping."""
from __future__ import annotations

import re
from datetime import datetime, timezone
from typing import Optional

from pydantic import BaseModel, Field, field_validator


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


_SLUG_RE = re.compile(r"^[a-z0-9][a-z0-9_-]{0,62}$")


class Project(BaseModel):
    """A logical project grouping catalog tests."""

    id:          str       # slug, primary key
    name:        str
    description: str       = ""
    color:       str       = "#6366f1"
    base_url:    Optional[str] = None
    created_at:  datetime  = Field(default_factory=_now_utc)
    updated_at:  datetime  = Field(default_factory=_now_utc)

    model_config = {"extra": "ignore"}

    @field_validator("id")
    @classmethod
    def validate_id_slug(cls, v: str) -> str:
        s = (v or "").strip().lower()
        if not _SLUG_RE.match(s):
            raise ValueError(
                "id must be a slug: start with alphanumeric, then [a-z0-9_-] (max 63 chars)"
            )
        return s


class ProjectCreate(BaseModel):
    id:          str
    name:        str
    description: str = ""
    color:       str = "#6366f1"
    base_url:    Optional[str] = None

    model_config = {"extra": "ignore"}

    @field_validator("id")
    @classmethod
    def validate_id_slug(cls, v: str) -> str:
        s = (v or "").strip().lower()
        if not _SLUG_RE.match(s):
            raise ValueError(
                "id must be a slug: start with alphanumeric, then [a-z0-9_-] (max 63 chars)"
            )
        return s


class ProjectUpdate(BaseModel):
    name:        Optional[str] = None
    description: Optional[str] = None
    color:       Optional[str] = None
    base_url:    Optional[str] = None

    model_config = {"extra": "ignore"}
