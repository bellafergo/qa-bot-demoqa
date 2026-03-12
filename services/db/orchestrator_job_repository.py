# services/db/orchestrator_job_repository.py
"""
SQLAlchemy ORM model and repository for orchestrator_jobs table.
"""
from __future__ import annotations

import json
import logging
from typing import List, Optional

from sqlalchemy import Column, Integer, String, Text

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.orch_job")


# ── ORM row model ─────────────────────────────────────────────────────────────

class OrchestratorJobRow(Base):
    __tablename__ = "orchestrator_jobs"

    job_id             = Column(String,  primary_key=True)
    job_type           = Column(String,  nullable=False)
    status             = Column(String,  nullable=False)
    test_case_ids_json = Column(Text,    nullable=False, default="[]")
    total_count        = Column(Integer, nullable=False, default=0)
    completed_count    = Column(Integer, nullable=False, default=0)
    passed_count       = Column(Integer, nullable=False, default=0)
    failed_count       = Column(Integer, nullable=False, default=0)
    error_count        = Column(Integer, nullable=False, default=0)
    run_ids_json       = Column(Text,    default="[]")
    results_json       = Column(Text,    default="[]")
    error_message      = Column(String)
    environment        = Column(String)
    created_at         = Column(String,  nullable=False)
    started_at         = Column(String)
    finished_at        = Column(String)


# ── Conversion helpers ────────────────────────────────────────────────────────

def _row_to_model(row: OrchestratorJobRow):
    from models.orchestrator_job import OrchestratorJob
    from datetime import datetime, timezone

    def _parse_dt(v):
        if not v:
            return None
        try:
            dt = datetime.fromisoformat(v)
            if dt.tzinfo is None:
                dt = dt.replace(tzinfo=timezone.utc)
            return dt
        except Exception:
            return None

    return OrchestratorJob(
        job_id          = row.job_id,
        job_type        = row.job_type,
        status          = row.status,
        test_case_ids   = json.loads(row.test_case_ids_json or "[]"),
        total_count     = row.total_count     or 0,
        completed_count = row.completed_count or 0,
        passed_count    = row.passed_count    or 0,
        failed_count    = row.failed_count    or 0,
        error_count     = row.error_count     or 0,
        run_ids         = json.loads(row.run_ids_json   or "[]"),
        results         = json.loads(row.results_json   or "[]"),
        error_message   = row.error_message,
        environment     = row.environment or "default",
        created_at      = _parse_dt(row.created_at),
        started_at      = _parse_dt(row.started_at),
        finished_at     = _parse_dt(row.finished_at),
    )


# ── Repository ────────────────────────────────────────────────────────────────

class OrchestratorJobRepository:

    def create_job(self, job) -> None:
        """Insert a new job record."""
        row = OrchestratorJobRow(
            job_id             = job.job_id,
            job_type           = job.job_type,
            status             = job.status,
            test_case_ids_json = json.dumps(job.test_case_ids),
            total_count        = job.total_count,
            completed_count    = job.completed_count,
            passed_count       = job.passed_count,
            failed_count       = job.failed_count,
            error_count        = job.error_count,
            run_ids_json       = json.dumps(job.run_ids),
            results_json       = json.dumps(job.results),
            error_message      = job.error_message,
            environment        = job.environment,
            created_at         = job.created_at.isoformat(),
            started_at         = job.started_at.isoformat() if job.started_at else None,
            finished_at        = job.finished_at.isoformat() if job.finished_at else None,
        )
        with get_session() as s:
            s.add(row)
        logger.debug("orch_job_repo: created job %s", job.job_id)

    def update_job(self, job) -> None:
        """Upsert current in-memory job state to DB."""
        row = OrchestratorJobRow(
            job_id             = job.job_id,
            job_type           = job.job_type,
            status             = job.status,
            test_case_ids_json = json.dumps(job.test_case_ids),
            total_count        = job.total_count,
            completed_count    = job.completed_count,
            passed_count       = job.passed_count,
            failed_count       = job.failed_count,
            error_count        = job.error_count,
            run_ids_json       = json.dumps(job.run_ids),
            results_json       = json.dumps(job.results),
            error_message      = job.error_message,
            environment        = job.environment,
            created_at         = job.created_at.isoformat(),
            started_at         = job.started_at.isoformat() if job.started_at else None,
            finished_at        = job.finished_at.isoformat() if job.finished_at else None,
        )
        with get_session() as s:
            s.merge(row)
        logger.debug("orch_job_repo: updated job %s → %s", job.job_id, job.status)

    def get_job(self, job_id: str):
        with get_session() as s:
            row = s.query(OrchestratorJobRow).filter_by(job_id=job_id).first()
            return _row_to_model(row) if row else None

    def list_jobs(self, limit: int = 100):
        with get_session() as s:
            rows = (
                s.query(OrchestratorJobRow)
                .order_by(OrchestratorJobRow.created_at.desc())
                .limit(limit)
                .all()
            )
        return [_row_to_model(r) for r in rows]

    def count_by_status(self) -> dict:
        """Return {status: count} for all orchestrator jobs."""
        from sqlalchemy import func
        with get_session() as s:
            rows = (
                s.query(OrchestratorJobRow.status, func.count(OrchestratorJobRow.job_id))
                .group_by(OrchestratorJobRow.status)
                .all()
            )
        return {status: count for status, count in rows}

    def get_last_created_at(self) -> Optional[str]:
        """Return the most recent created_at ISO string, or None."""
        from sqlalchemy import func
        with get_session() as s:
            result = s.query(func.max(OrchestratorJobRow.created_at)).scalar()
        return result

    def clear_all(self) -> None:
        """Wipe all rows — used in tests only."""
        with get_session() as s:
            s.query(OrchestratorJobRow).delete()


# Module-level singleton
orch_job_repo = OrchestratorJobRepository()
