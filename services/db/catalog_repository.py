# services/db/catalog_repository.py
"""
SQLAlchemy ORM model and repository for test_cases table.
"""
from __future__ import annotations

import json
import logging
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, Integer, String, Text, Index, or_
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
    test_type     = Column(String,  default="ui")      # "ui" | "api"
    project_id    = Column(String,  default="default", index=True)
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
        test_type    = row.test_type or "ui",
        project_id   = getattr(row, "project_id", None) or "default",
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
        test_type       = getattr(tc, "test_type", "ui") or "ui",
        project_id      = getattr(tc, "project_id", None) or "default",
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
        test_type: Optional[str] = None,
        search: Optional[str] = None,
        tags: Optional[List[str]] = None,
        project_id: Optional[str] = None,
        limit: int = 200,
    ):
        with get_session() as s:
            q = s.query(TestCaseRow)
            if project_id is not None and str(project_id).strip():
                q = q.filter(TestCaseRow.project_id == str(project_id).strip())
            if module:
                q = q.filter(TestCaseRow.module.ilike(f"%{module}%"))
            if type_:
                q = q.filter(TestCaseRow.type == type_)
            if priority:
                q = q.filter(TestCaseRow.priority == priority)
            if status:
                q = q.filter(TestCaseRow.status == status)
            if test_type:
                q = q.filter(TestCaseRow.test_type == test_type)
            if search and search.strip():
                term = f"%{search.strip()}%"
                q = q.filter(or_(
                    TestCaseRow.test_case_id.ilike(term),
                    TestCaseRow.name.ilike(term),
                    TestCaseRow.module.ilike(term),
                    TestCaseRow.type.ilike(term),
                    TestCaseRow.test_type.ilike(term),
                ))
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

    def update_test_case(self, test_case_id: str, data: dict):
        """
        Overwrite mutable fields of an existing test case row.

        *data* keys accepted: name, module, type, priority, status, test_type,
        project_id, steps, assertions, tags, base_url, version.

        Returns the updated TestCase, or None if not found.
        """
        import json as _json
        from datetime import datetime, timezone

        with get_session() as s:
            row = s.query(TestCaseRow).filter_by(test_case_id=test_case_id).first()
            if row is None:
                return None

            if "name"      in data and data["name"]      is not None: row.name      = data["name"]
            if "module"    in data and data["module"]    is not None: row.module    = data["module"]
            if "type"      in data and data["type"]      is not None: row.type      = data["type"]
            if "priority"  in data and data["priority"]  is not None: row.priority  = data["priority"]
            if "status"    in data and data["status"]    is not None: row.status    = data["status"]
            if "test_type" in data and data["test_type"] is not None: row.test_type = data["test_type"]
            if "project_id" in data and data["project_id"] is not None:
                row.project_id = str(data["project_id"]).strip() or "default"
            if "base_url"  in data:                                   row.base_url  = data["base_url"]
            if "version"   in data and data["version"]   is not None: row.version   = data["version"]
            if "steps"      in data and data["steps"]      is not None:
                row.steps_json      = _json.dumps(data["steps"])
            if "assertions" in data and data["assertions"] is not None:
                row.assertions_json = _json.dumps(data["assertions"])
            if "tags"       in data and data["tags"]       is not None:
                row.tags_json = _json.dumps(data["tags"])

            row.updated_at = datetime.now(timezone.utc).isoformat()
            s.flush()
            return _row_to_model(row)

    def delete_test_case(self, test_case_id: str) -> bool:
        with get_session() as s:
            row = s.query(TestCaseRow).filter_by(test_case_id=test_case_id).first()
            if row is None:
                return False
            s.delete(row)
        logger.debug("catalog_repo: deleted %s", test_case_id)
        return True

    def count_by_status(self) -> dict:
        """Return {status: count} for all test cases."""
        from sqlalchemy import func
        with get_session() as s:
            rows = (
                s.query(TestCaseRow.status, func.count(TestCaseRow.id))
                .group_by(TestCaseRow.status)
                .all()
            )
        return {status: count for status, count in rows}

    def all_modules(self) -> list:
        """Return list of (test_case_id, module) tuples for all test cases."""
        with get_session() as s:
            rows = s.query(TestCaseRow.test_case_id, TestCaseRow.module).all()
        return [(r[0], r[1] or "") for r in rows]

    def count_tests_for_project(self, project_id: str) -> int:
        pid = (project_id or "").strip() or "default"
        with get_session() as s:
            return (
                s.query(TestCaseRow)
                .filter(TestCaseRow.project_id == pid)
                .count()
            )

    def count_by_test_type(self) -> dict:
        """Return {test_type: count} — e.g. {"ui": 40, "api": 5}."""
        from sqlalchemy import func
        with get_session() as s:
            rows = (
                s.query(TestCaseRow.test_type, func.count(TestCaseRow.id))
                .group_by(TestCaseRow.test_type)
                .all()
            )
        return {(t or "ui"): c for t, c in rows}

    def count_test_cases_by_module(self) -> dict:
        """Return {module: count} for all test cases."""
        from sqlalchemy import func
        with get_session() as s:
            rows = (
                s.query(TestCaseRow.module, func.count(TestCaseRow.id))
                .group_by(TestCaseRow.module)
                .all()
            )
        return {(m or ""): c for m, c in rows}

    def clear_all(self) -> None:
        """Wipe all rows — used in tests only."""
        with get_session() as s:
            s.query(TestCaseRow).delete()


# Module-level singleton
catalog_repo = CatalogRepository()
