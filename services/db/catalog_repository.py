# services/db/catalog_repository.py
"""
SQLAlchemy ORM model and repository for test_cases table.
"""
from __future__ import annotations

import json
import logging
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, Integer, String, Text, Index
from sqlalchemy.orm import Session

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.catalog")


# ── ORM row model ─────────────────────────────────────────────────────────────

class TestCaseRow(Base):
    __tablename__ = "test_cases"

    id            = Column(String,  primary_key=True)
    test_case_id  = Column(String,  unique=True, nullable=False, index=True)
    name          = Column(String,  nullable=False)
    module        = Column(String)
    type          = Column(String)
    priority      = Column(String)
    status        = Column(String,  default="active", index=True)
    version       = Column(Integer, default=1)
    tags_json     = Column(Text,    default="[]")
    base_url      = Column(String)
    steps_json      = Column(Text, nullable=False, default="[]")
    assertions_json = Column(Text, nullable=False, default="[]")
    created_at    = Column(String,  nullable=False)
    updated_at    = Column(String,  nullable=False)


# ── Conversion helpers ────────────────────────────────────────────────────────

def _row_to_model(row: TestCaseRow):
    """Convert a DB row to a TestCase Pydantic model."""
    from models.test_case import TestCase, TestStep, TestAssertion

    steps_data      = json.loads(row.steps_json      or "[]")
    assertions_data = json.loads(row.assertions_json or "[]")
    tags            = json.loads(row.tags_json        or "[]")

    return TestCase(
        id           = row.id,
        test_case_id = row.test_case_id,
        name         = row.name,
        module       = row.module or "",
        type         = row.type,
        priority     = row.priority,
        status       = row.status,
        version      = row.version or 1,
        tags         = tags,
        base_url     = row.base_url,
        steps        = [TestStep(**s) for s in steps_data],
        assertions   = [TestAssertion(**a) for a in assertions_data],
        created_at   = row.created_at,
        updated_at   = row.updated_at,
    )


def _model_to_row(tc) -> Dict[str, Any]:
    """Convert a TestCase Pydantic model to a dict for DB insertion."""
    steps_data      = [s.model_dump() for s in tc.steps]
    assertions_data = [a.model_dump() for a in tc.assertions]

    return dict(
        id              = tc.id,
        test_case_id    = tc.test_case_id,
        name            = tc.name,
        module          = tc.module,
        type            = tc.type,
        priority        = tc.priority,
        status          = tc.status,
        version         = tc.version,
        tags_json       = json.dumps(tc.tags),
        base_url        = tc.base_url,
        steps_json      = json.dumps(steps_data),
        assertions_json = json.dumps(assertions_data),
        created_at      = tc.created_at.isoformat(),
        updated_at      = tc.updated_at.isoformat(),
    )


# ── Repository ────────────────────────────────────────────────────────────────

class CatalogRepository:

    def is_empty(self) -> bool:
        with get_session() as s:
            return s.query(TestCaseRow).count() == 0

    def get_test_case(self, test_case_id: str):
        with get_session() as s:
            row = s.query(TestCaseRow).filter_by(test_case_id=test_case_id).first()
            if row is None:
                return None
            return _row_to_model(row)

    def list_test_cases(
        self,
        *,
        module: Optional[str] = None,
        type_: Optional[str] = None,
        priority: Optional[str] = None,
        status: Optional[str] = "active",
        tags: Optional[List[str]] = None,
        limit: int = 200,
    ):
        with get_session() as s:
            q = s.query(TestCaseRow)
            if module:
                q = q.filter(TestCaseRow.module.ilike(module))
            if type_:
                q = q.filter(TestCaseRow.type == type_)
            if priority:
                q = q.filter(TestCaseRow.priority == priority)
            if status:
                q = q.filter(TestCaseRow.status == status)
            q = q.order_by(TestCaseRow.created_at.desc())
            rows = q.all()

        models = [_row_to_model(r) for r in rows]

        # Tags filter in Python (stored as JSON, simpler than SQL)
        if tags:
            tag_set = {t.lower() for t in tags}
            models = [m for m in models if tag_set.issubset({t.lower() for t in m.tags})]

        return models[:limit]

    def create_test_case(self, tc):
        data = _model_to_row(tc)
        with get_session() as s:
            row = TestCaseRow(**data)
            s.add(row)
        logger.debug("catalog_repo: inserted %s", tc.test_case_id)
        return tc

    def delete_test_case(self, test_case_id: str) -> bool:
        with get_session() as s:
            row = s.query(TestCaseRow).filter_by(test_case_id=test_case_id).first()
            if row is None:
                return False
            s.delete(row)
        logger.debug("catalog_repo: deleted %s", test_case_id)
        return True

    def clear_all(self) -> None:
        """Wipe all rows — used in tests only."""
        with get_session() as s:
            s.query(TestCaseRow).delete()


# Module-level singleton
catalog_repo = CatalogRepository()
