# services/db/test_run_repository.py
"""
SQLAlchemy ORM model and repository for test_runs table.
"""
from __future__ import annotations

import json
import logging
from typing import List, Optional

from sqlalchemy import Column, Integer, String, Text

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.test_run")

# ============================================================
# Future maintenance hook (SQLite TTL / cleanup)
# ============================================================
# Today we rely on run_store.py for in-memory TTL.
# The SQLite "test_runs" table is not automatically purged.
#
# For a future TTL/cleanup job, the safest minimal insertion point is inside
# this repository (e.g. at the start of list_runs/get_run) because it is the
# single source for read queries. Prefer a best-effort, non-blocking SQL
# DELETE based on executed_at age, guarded by an env var (e.g. RUNS_SQLITE_TTL_S).
# This avoids changing higher layers and keeps cleanup colocated with the data access.


# ── ORM row model ─────────────────────────────────────────────────────────────

class TestRunRow(Base):
    __tablename__ = "test_runs"

    run_id           = Column(String,  primary_key=True)
    test_case_id     = Column(String,  nullable=False, index=True)
    test_name        = Column(String)
    executed_at      = Column(String,  nullable=False)
    environment      = Column(String)
    status           = Column(String,  nullable=False)
    duration_ms      = Column(Integer)
    evidence_url     = Column(String)
    report_url       = Column(String)
    evidence_id      = Column(String)
    logs_json        = Column(Text, default="[]")
    steps_result_json= Column(Text, default="[]")
    meta_json        = Column(Text, default="{}")


# ── Conversion helpers ────────────────────────────────────────────────────────

def _row_to_model(row: TestRunRow):
    from models.test_run import TestRun
    return TestRun(
        run_id       = row.run_id,
        test_case_id = row.test_case_id,
        test_name    = row.test_name,
        executed_at  = row.executed_at,
        environment  = row.environment or "default",
        status       = row.status,
        duration_ms  = row.duration_ms,
        evidence_url = row.evidence_url,
        report_url   = row.report_url,
        evidence_id  = row.evidence_id,
        logs         = json.loads(row.logs_json          or "[]"),
        steps_result = json.loads(row.steps_result_json  or "[]"),
        meta         = json.loads(row.meta_json          or "{}"),
    )


# ── Repository ────────────────────────────────────────────────────────────────

class TestRunRepository:

    def create_run(self, run) -> None:
        """Persist a TestRun record."""
        from models.test_run import TestRun
        row = TestRunRow(
            run_id            = run.run_id,
            test_case_id      = run.test_case_id,
            test_name         = run.test_name,
            executed_at       = run.executed_at.isoformat(),
            environment       = run.environment,
            status            = run.status,
            duration_ms       = run.duration_ms,
            evidence_url      = run.evidence_url,
            report_url        = run.report_url,
            evidence_id       = run.evidence_id,
            logs_json         = json.dumps(run.logs),
            steps_result_json = json.dumps(run.steps_result),
            meta_json         = json.dumps(run.meta),
        )
        with get_session() as s:
            s.merge(row)   # upsert — safe if re-persisted
        logger.debug("test_run_repo: saved run %s", run.run_id)

    def get_run(self, run_id: str):
        with get_session() as s:
            row = s.query(TestRunRow).filter_by(run_id=run_id).first()
            return _row_to_model(row) if row else None

    def list_runs(
        self,
        *,
        test_case_id: Optional[str] = None,
        limit: int = 100,
    ):
        with get_session() as s:
            q = s.query(TestRunRow)
            # Async chat/execute runs are stored with test_case_id="_async".
            # By default we exclude them from generic history listings so
            # dashboards/recent runs reflect completed catalog executions.
            # If a caller explicitly requests test_case_id="_async", we include them.
            if test_case_id:
                q = q.filter(TestRunRow.test_case_id == test_case_id)
            else:
                q = q.filter(TestRunRow.test_case_id != "_async")
            q = q.order_by(TestRunRow.executed_at.desc()).limit(limit)
            rows = q.all()
        return [_row_to_model(r) for r in rows]

    def count_by_status(self) -> dict:
        """Return {status: count} for all test runs."""
        from sqlalchemy import func
        with get_session() as s:
            rows = (
                s.query(TestRunRow.status, func.count(TestRunRow.run_id))
                .filter(TestRunRow.test_case_id != "_async")
                .group_by(TestRunRow.status)
                .all()
            )
        return {status: count for status, count in rows}

    def get_last_executed_at(self) -> Optional[str]:
        """Return the most recent executed_at ISO string, or None."""
        from sqlalchemy import func
        with get_session() as s:
            # Exclude async/intermediate runs from "last executed" marker.
            result = (
                s.query(func.max(TestRunRow.executed_at))
                .filter(TestRunRow.test_case_id != "_async")
                .scalar()
            )
        return result

    def count_runs_by_test_case(self) -> dict:
        """Return {test_case_id: {status: count}} aggregation."""
        from sqlalchemy import func
        with get_session() as s:
            rows = (
                s.query(
                    TestRunRow.test_case_id,
                    TestRunRow.status,
                    func.count(TestRunRow.run_id),
                )
                .filter(TestRunRow.test_case_id != "_async")
                .group_by(TestRunRow.test_case_id, TestRunRow.status)
                .all()
            )
        result: dict = {}
        for tc_id, status, count in rows:
            result.setdefault(tc_id, {})[status] = count
        return result

    def clear_all(self) -> None:
        """Wipe all rows — used in tests only."""
        with get_session() as s:
            s.query(TestRunRow).delete()


# Module-level singleton
test_run_repo = TestRunRepository()
