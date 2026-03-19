# services/db/test_version_repository.py
"""
SQLAlchemy ORM model and repository for the test_versions table.

Every time a test case is created, edited, or rolled back, a complete
snapshot is written here.  Rows are never deleted or updated — only appended.

version_number is per test_case_id and monotonically increasing.
Rollback creates a new row rather than overwriting an old one.
"""
from __future__ import annotations

import json
import logging
import uuid
from datetime import datetime, timezone
from typing import List, Optional

from sqlalchemy import Column, Integer, String, Text, UniqueConstraint

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.test_versions")


# ── ORM row ───────────────────────────────────────────────────────────────────

def _utcnow_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _new_id() -> str:
    return str(uuid.uuid4())


class TestVersionRow(Base):
    __tablename__ = "test_versions"
    __table_args__ = (
        UniqueConstraint("test_case_id", "version_number", name="uq_tc_version"),
    )

    id              = Column(String,  primary_key=True, default=_new_id)
    test_case_id    = Column(String,  nullable=False, index=True)
    version_number  = Column(Integer, nullable=False)
    name            = Column(String,  nullable=False, default="")
    module          = Column(String,  default="")
    type            = Column(String,  default="smoke")
    priority        = Column(String,  default="medium")
    status          = Column(String,  default="active")
    test_type       = Column(String,  default="ui")
    steps_json      = Column(Text,    default="[]")
    assertions_json = Column(Text,    default="[]")
    tags_json       = Column(Text,    default="[]")
    base_url        = Column(String)
    source          = Column(String,  default="manual")   # manual|draft|rollback|…
    change_note     = Column(String,  default="")
    created_at      = Column(String,  nullable=False, default=_utcnow_iso)


# ── Conversion ────────────────────────────────────────────────────────────────

def _to_detail(row: TestVersionRow):
    from models.test_version_models import TestVersionDetail
    return TestVersionDetail(
        id             = row.id,
        test_case_id   = row.test_case_id,
        version_number = row.version_number,
        name           = row.name or "",
        module         = row.module or "",
        type           = row.type or "smoke",
        priority       = row.priority or "medium",
        status         = row.status or "active",
        test_type      = row.test_type or "ui",
        source         = row.source or "manual",
        change_note    = row.change_note or "",
        created_at     = _parse_dt(row.created_at),
        steps          = json.loads(row.steps_json      or "[]"),
        assertions     = json.loads(row.assertions_json or "[]"),
        tags           = json.loads(row.tags_json        or "[]"),
        base_url       = row.base_url,
    )


def _to_summary(row: TestVersionRow):
    from models.test_version_models import TestVersionSummary
    return TestVersionSummary(
        id             = row.id,
        test_case_id   = row.test_case_id,
        version_number = row.version_number,
        name           = row.name or "",
        module         = row.module or "",
        type           = row.type or "smoke",
        priority       = row.priority or "medium",
        status         = row.status or "active",
        test_type      = row.test_type or "ui",
        source         = row.source or "manual",
        change_note    = row.change_note or "",
        created_at     = _parse_dt(row.created_at),
    )


def _parse_dt(value) -> datetime:
    if isinstance(value, datetime):
        return value
    try:
        return datetime.fromisoformat(str(value))
    except Exception:
        return datetime.now(timezone.utc)


# ── Repository ────────────────────────────────────────────────────────────────

class TestVersionRepository:

    def next_version_number(self, test_case_id: str) -> int:
        """Return the next version number (max existing + 1, or 1 if none)."""
        with get_session() as s:
            from sqlalchemy import func
            result = (
                s.query(func.max(TestVersionRow.version_number))
                .filter(TestVersionRow.test_case_id == test_case_id)
                .scalar()
            )
            return (result or 0) + 1

    def snapshot(
        self,
        tc,
        *,
        version_number: Optional[int] = None,
        source:         str = "manual",
        change_note:    str = "",
    ):
        """
        Write a complete snapshot of *tc* (a TestCase Pydantic model).

        If *version_number* is not provided, the next available number is used.
        Returns TestVersionDetail.
        """
        vnum = version_number if version_number is not None else self.next_version_number(tc.test_case_id)
        steps_data      = [s.model_dump() if hasattr(s, "model_dump") else dict(s) for s in (tc.steps or [])]
        assertions_data = [a.model_dump() if hasattr(a, "model_dump") else dict(a) for a in (tc.assertions or [])]
        tags_data       = list(tc.tags or [])

        row = TestVersionRow(
            id              = _new_id(),
            test_case_id    = tc.test_case_id,
            version_number  = vnum,
            name            = tc.name,
            module          = tc.module,
            type            = tc.type,
            priority        = tc.priority,
            status          = tc.status,
            test_type       = getattr(tc, "test_type", "ui") or "ui",
            steps_json      = json.dumps(steps_data),
            assertions_json = json.dumps(assertions_data),
            tags_json       = json.dumps(tags_data),
            base_url        = tc.base_url,
            source          = source,
            change_note     = change_note,
            created_at      = _utcnow_iso(),
        )

        with get_session() as s:
            s.add(row)

        logger.debug("test_versions: snapshot %s v%s (%s)", tc.test_case_id, vnum, source)
        return _to_detail(row)

    def list_versions(self, test_case_id: str):
        """Return all versions for a test, newest first."""
        with get_session() as s:
            rows = (
                s.query(TestVersionRow)
                .filter(TestVersionRow.test_case_id == test_case_id)
                .order_by(TestVersionRow.version_number.desc())
                .all()
            )
            return [_to_summary(r) for r in rows]

    def get_version(self, test_case_id: str, version_number: int):
        """Return a single version snapshot, or None."""
        with get_session() as s:
            row = (
                s.query(TestVersionRow)
                .filter(
                    TestVersionRow.test_case_id   == test_case_id,
                    TestVersionRow.version_number == version_number,
                )
                .first()
            )
            return _to_detail(row) if row else None


# ── Singleton ─────────────────────────────────────────────────────────────────

test_version_repo = TestVersionRepository()
