# services/db/pr_analysis_report_repository.py
"""SQLite persistence for ProjectPRAnalysisReport snapshots (read by Incident Investigator)."""
from __future__ import annotations

import json
import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, String, Text, desc

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.pr_analysis_report")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class PRAnalysisReportRow(Base):
    __tablename__ = "pr_analysis_reports"

    id = Column(String, primary_key=True)
    project_id = Column(String, nullable=False, index=True)
    pr_id = Column(String, nullable=False, default="")
    provider = Column(String, nullable=False, default="manual")
    created_at = Column(String, nullable=False)
    report_json = Column(Text, nullable=False, default="{}")


class PRAnalysisReportRepository:
    def upsert(
        self,
        *,
        project_id: str,
        pr_id: str,
        provider: str,
        report: Dict[str, Any],
    ) -> str:
        pid = (project_id or "").strip().lower()
        pr = str(pr_id or "").strip()
        prov = (provider or "manual").strip().lower()
        now = _utc_iso()
        payload = dict(report)
        payload["stored_at"] = now

        with get_session() as session:
            existing = (
                session.query(PRAnalysisReportRow)
                .filter(
                    PRAnalysisReportRow.project_id == pid,
                    PRAnalysisReportRow.pr_id == pr,
                    PRAnalysisReportRow.provider == prov,
                )
                .order_by(desc(PRAnalysisReportRow.created_at))
                .first()
            )
            if existing:
                existing.created_at = now
                existing.report_json = json.dumps(payload, ensure_ascii=False)
                session.commit()
                return existing.id

            row_id = str(uuid.uuid4())
            row = PRAnalysisReportRow(
                id=row_id,
                project_id=pid,
                pr_id=pr,
                provider=prov,
                created_at=now,
                report_json=json.dumps(payload, ensure_ascii=False),
            )
            session.add(row)
            session.commit()
            return row_id

    def list_for_project(
        self,
        project_id: str,
        *,
        limit: int = 50,
    ) -> List[Dict[str, Any]]:
        pid = (project_id or "").strip().lower()
        if not pid:
            return []
        limit = max(1, min(int(limit), 200))
        with get_session() as session:
            rows = (
                session.query(PRAnalysisReportRow)
                .filter(PRAnalysisReportRow.project_id == pid)
                .order_by(desc(PRAnalysisReportRow.created_at))
                .limit(limit)
                .all()
            )
            out: List[Dict[str, Any]] = []
            for row in rows:
                try:
                    report = json.loads(row.report_json or "{}")
                except Exception:
                    continue
                out.append({
                    "id": row.id,
                    "project_id": row.project_id,
                    "pr_id": row.pr_id,
                    "provider": row.provider,
                    "created_at": row.created_at,
                    "report": report,
                })
            return out

    def get(
        self,
        project_id: str,
        pr_id: str,
        *,
        provider: Optional[str] = None,
    ) -> Optional[Dict[str, Any]]:
        pid = (project_id or "").strip().lower()
        pr = str(pr_id or "").strip()
        if not pid or not pr:
            return None
        with get_session() as session:
            q = session.query(PRAnalysisReportRow).filter(
                PRAnalysisReportRow.project_id == pid,
                PRAnalysisReportRow.pr_id == pr,
            )
            if provider:
                q = q.filter(PRAnalysisReportRow.provider == provider.strip().lower())
            row = q.order_by(desc(PRAnalysisReportRow.created_at)).first()
            if not row:
                return None
            try:
                report = json.loads(row.report_json or "{}")
            except Exception:
                return None
            return {
                "id": row.id,
                "project_id": row.project_id,
                "pr_id": row.pr_id,
                "provider": row.provider,
                "created_at": row.created_at,
                "report": report,
            }


pr_analysis_report_repo = PRAnalysisReportRepository()
