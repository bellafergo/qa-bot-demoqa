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
            if test_case_id:
                q = q.filter(TestRunRow.test_case_id == test_case_id)
            q = q.order_by(TestRunRow.executed_at.desc()).limit(limit)
            rows = q.all()
        return [_row_to_model(r) for r in rows]

    def clear_all(self) -> None:
        """Wipe all rows — used in tests only."""
        with get_session() as s:
            s.query(TestRunRow).delete()


# Module-level singleton
test_run_repo = TestRunRepository()
