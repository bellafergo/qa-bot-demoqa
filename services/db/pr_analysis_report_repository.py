# services/db/pr_analysis_report_repository.py
"""SQLite + Supabase persistence for ProjectPRAnalysisReport snapshots."""
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
    def _upsert_sqlite(
        self,
        *,
        row_id: str,
        project_id: str,
        pr_id: str,
        provider: str,
        created_at: str,
        payload: Dict[str, Any],
    ) -> str:
        pid = (project_id or "").strip().lower()
        pr = str(pr_id or "").strip()
        prov = (provider or "manual").strip().lower()

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
                existing.created_at = created_at
                existing.report_json = json.dumps(payload, ensure_ascii=False)
                session.commit()
                return existing.id

            row = PRAnalysisReportRow(
                id=row_id,
                project_id=pid,
                pr_id=pr,
                provider=prov,
                created_at=created_at,
                report_json=json.dumps(payload, ensure_ascii=False),
            )
            session.add(row)
            session.commit()
            return row_id

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

        row_id = str(uuid.uuid4())
        saved_id = self._upsert_sqlite(
            row_id=row_id,
            project_id=pid,
            pr_id=pr,
            provider=prov,
            created_at=now,
            payload=payload,
        )

        try:
            from services.pr_analysis_report_supabase import persist_pr_analysis_report_supabase

            persist_pr_analysis_report_supabase(
                row_id=saved_id,
                project_id=pid,
                pr_id=pr,
                provider=prov,
                created_at=now,
                report_json=payload,
            )
        except Exception as e:
            logger.warning(
                "pr_analysis_report: supabase mirror failed id=%s project_id=%s: %s",
                saved_id,
                pid,
                e,
            )

        return saved_id

    def _list_sqlite(self, project_id: str, *, limit: int) -> List[Dict[str, Any]]:
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

    def _get_sqlite(
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
        sqlite_rows = self._list_sqlite(pid, limit=limit)
        try:
            from services.pr_analysis_report_supabase import (
                list_pr_analysis_reports_supabase,
                supabase_pr_analysis_reports_enabled,
            )
            from services.report_history_merge import merge_sqlite_supabase_records

            if supabase_pr_analysis_reports_enabled():
                supa_rows = list_pr_analysis_reports_supabase(pid, limit=limit)
                return merge_sqlite_supabase_records(
                    supabase_rows=supa_rows,
                    sqlite_rows=sqlite_rows,
                    limit=limit,
                )
        except Exception as e:
            logger.debug("pr_analysis_report: supabase list fallback project_id=%s: %s", pid, e)
        return sqlite_rows

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
        sqlite_doc = self._get_sqlite(pid, pr, provider=provider)
        supabase_doc: Optional[Dict[str, Any]] = None
        try:
            from services.pr_analysis_report_supabase import (
                fetch_pr_analysis_report_supabase,
                supabase_pr_analysis_reports_enabled,
            )
            from services.report_history_merge import pick_newer_record

            if supabase_pr_analysis_reports_enabled():
                supabase_doc = fetch_pr_analysis_report_supabase(pid, pr, provider=provider)
            return pick_newer_record(sqlite_row=sqlite_doc, supabase_row=supabase_doc)
        except Exception as e:
            logger.debug(
                "pr_analysis_report: supabase get compare failed project_id=%s pr_id=%s: %s",
                pid,
                pr,
                e,
            )
        return sqlite_doc


pr_analysis_report_repo = PRAnalysisReportRepository()
