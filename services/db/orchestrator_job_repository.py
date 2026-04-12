# services/db/orchestrator_job_repository.py
"""
SQLAlchemy ORM model and repository for orchestrator_jobs table.
"""
from __future__ import annotations

import json
import logging
from typing import List, Optional, Set

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
    # Parallel-execution block extensions
    retry_count        = Column(Integer, default=0)
    skipped_count      = Column(Integer, default=0)
    scheduling_notes   = Column(String)
    # Trigger context — set at enqueue time, never overwritten
    context_json       = Column(Text)
    # Retry correlation — parent job when this job is a retry of failed tests
    parent_job_id      = Column(String)
    project_id         = Column(String, nullable=True)


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
        job_id           = row.job_id,
        job_type         = row.job_type,
        status           = row.status,
        test_case_ids    = json.loads(row.test_case_ids_json or "[]"),
        total_count      = row.total_count     or 0,
        completed_count  = row.completed_count or 0,
        passed_count     = row.passed_count    or 0,
        failed_count     = row.failed_count    or 0,
        error_count      = row.error_count     or 0,
        run_ids          = json.loads(row.run_ids_json   or "[]"),
        results          = json.loads(row.results_json   or "[]"),
        error_message    = row.error_message,
        environment      = row.environment or "default",
        created_at       = _parse_dt(row.created_at),
        started_at       = _parse_dt(row.started_at),
        finished_at      = _parse_dt(row.finished_at),
        retry_count      = row.retry_count    or 0,
        skipped_count    = row.skipped_count  or 0,
        scheduling_notes = row.scheduling_notes,
        context_json     = row.context_json,
        parent_job_id    = getattr(row, "parent_job_id", None) or None,
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
            retry_count        = getattr(job, "retry_count", 0) or 0,
            skipped_count      = getattr(job, "skipped_count", 0) or 0,
            scheduling_notes   = getattr(job, "scheduling_notes", None),
            context_json       = getattr(job, "context_json", None),
            parent_job_id      = getattr(job, "parent_job_id", None),
            project_id         = getattr(job, "project_id", None) or "default",
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
            retry_count        = getattr(job, "retry_count", 0) or 0,
            skipped_count      = getattr(job, "skipped_count", 0) or 0,
            scheduling_notes   = getattr(job, "scheduling_notes", None),
            context_json       = getattr(job, "context_json", None),
            parent_job_id      = getattr(job, "parent_job_id", None),
            project_id         = getattr(job, "project_id", None) or "default",
        )
        with get_session() as s:
            s.merge(row)
        logger.debug("orch_job_repo: updated job %s → %s", job.job_id, job.status)

    def get_job(self, job_id: str):
        with get_session() as s:
            row = s.query(OrchestratorJobRow).filter_by(job_id=job_id).first()
            return _row_to_model(row) if row else None

    def list_jobs(self, limit: int = 100, project_id: Optional[str] = None):
        """
        Recent jobs, newest first.

        When project_id is set, prefer the persisted ``project_id`` column (v1).
        If the column is absent on legacy rows, fall back to test_case overlap scan.
        """
        pid = (project_id or "").strip()
        if pid:
            from sqlalchemy import or_

            with get_session() as s:
                rows = (
                    s.query(OrchestratorJobRow)
                    .filter(
                        or_(
                            OrchestratorJobRow.project_id == pid,
                            (OrchestratorJobRow.project_id.is_(None)) & (pid == "default"),
                        )
                    )
                    .order_by(OrchestratorJobRow.created_at.desc())
                    .limit(limit)
                    .all()
                )
            if rows:
                return [_row_to_model(r) for r in rows]

        fetch = limit
        if pid:
            fetch = min(2000, max(limit * 25, limit))

        with get_session() as s:
            rows = (
                s.query(OrchestratorJobRow)
                .order_by(OrchestratorJobRow.created_at.desc())
                .limit(fetch)
                .all()
            )
        models = [_row_to_model(r) for r in rows]
        if not pid:
            return models[:limit]

        from services.db.catalog_repository import catalog_repo

        allowed: Set[str] = set(catalog_repo.list_test_case_ids_for_project(pid))
        if not allowed:
            return []

        out: List = []
        for job in models:
            ids = job.test_case_ids or []
            if any(tcid in allowed for tcid in ids):
                out.append(job)
            if len(out) >= limit:
                break
        return out

    def count_by_status(self, project_id: Optional[str] = None) -> dict:
        """Job counts by status; optional scope to jobs touching a catalog project."""
        from sqlalchemy import func, or_

        pid = (project_id or "").strip()
        if not pid:
            with get_session() as s:
                rows = (
                    s.query(OrchestratorJobRow.status, func.count(OrchestratorJobRow.job_id))
                    .group_by(OrchestratorJobRow.status)
                    .all()
                )
            return {status: count for status, count in rows}

        with get_session() as s:
            rows = (
                s.query(OrchestratorJobRow.status, func.count(OrchestratorJobRow.job_id))
                .filter(
                    or_(
                        OrchestratorJobRow.project_id == pid,
                        (OrchestratorJobRow.project_id.is_(None)) & (pid == "default"),
                    )
                )
                .group_by(OrchestratorJobRow.status)
                .all()
            )
        if rows:
            return {status: count for status, count in rows}

        jobs = self.list_jobs(limit=800, project_id=project_id)
        acc: dict = {}
        for j in jobs:
            acc[j.status] = acc.get(j.status, 0) + 1
        return acc

    def get_last_created_at(self, project_id: Optional[str] = None) -> Optional[str]:
        from sqlalchemy import func

        if not (project_id and str(project_id).strip()):
            with get_session() as s:
                return s.query(func.max(OrchestratorJobRow.created_at)).scalar()

        jobs = self.list_jobs(limit=500, project_id=project_id)
        if not jobs:
            return None
        best = None
        for j in jobs:
            ca = j.created_at.isoformat() if j.created_at else None
            if ca and (best is None or ca > best):
                best = ca
        return best

    def list_job_ids_by_parent_job(self, parent_job_id: str) -> List[str]:
        """Return job_ids of jobs that were created as retries of the given parent."""
        with get_session() as s:
            rows = (
                s.query(OrchestratorJobRow.job_id)
                .filter(OrchestratorJobRow.parent_job_id == parent_job_id)
                .order_by(OrchestratorJobRow.created_at.asc())
                .all()
            )
        return [r[0] for r in rows]

    def clear_all(self) -> None:
        """Wipe all rows — used in tests only."""
        with get_session() as s:
            s.query(OrchestratorJobRow).delete()


# Module-level singleton
orch_job_repo = OrchestratorJobRepository()
