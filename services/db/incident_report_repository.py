# services/db/incident_report_repository.py
"""SQLite persistence for project-scoped Incident Investigator QA reports (v1.1)."""
from __future__ import annotations

import json
import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, String, Text, desc

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.incident_report")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class IncidentReportRow(Base):
    __tablename__ = "incident_investigation_reports"

    id = Column(String, primary_key=True)
    project_id = Column(String, nullable=False, index=True)
    description = Column(Text, nullable=False, default="")
    severity = Column(String, nullable=False, default="medium")
    summary = Column(Text, nullable=False, default="")
    confidence = Column(String, nullable=False, default="0")
    created_at = Column(String, nullable=False)
    report_json = Column(Text, nullable=False, default="{}")


class IncidentReportRepository:
    def save(
        self,
        *,
        project_id: str,
        description: str,
        severity: str,
        summary: str,
        confidence: float,
        report: Dict[str, Any],
    ) -> str:
        report_id = str(uuid.uuid4())
        now = _utc_iso()
        payload = dict(report)
        payload["id"] = report_id
        payload["created_at"] = now
        row = IncidentReportRow(
            id=report_id,
            project_id=(project_id or "").strip().lower(),
            description=(description or "").strip(),
            severity=(severity or "medium").strip(),
            summary=(summary or "").strip(),
            confidence=str(round(float(confidence), 4)),
            created_at=now,
            report_json=json.dumps(payload, ensure_ascii=False),
        )
        with get_session() as session:
            session.add(row)
            session.commit()
        return report_id

    def get(self, report_id: str) -> Optional[Dict[str, Any]]:
        rid = (report_id or "").strip()
        if not rid:
            return None
        with get_session() as session:
            row = session.get(IncidentReportRow, rid)
            if not row:
                return None
            try:
                data = json.loads(row.report_json or "{}")
            except Exception:
                logger.warning("incident_report: corrupt payload for %s", rid)
                return None
            data.setdefault("id", row.id)
            data.setdefault("project_id", row.project_id)
            data.setdefault("description", row.description)
            data.setdefault("severity", row.severity)
            data.setdefault("summary", row.summary)
            data.setdefault("created_at", row.created_at)
            try:
                data.setdefault("confidence", float(row.confidence or 0))
            except (TypeError, ValueError):
                data.setdefault("confidence", 0.0)
            return data

    def list_reports(
        self,
        *,
        project_id: str,
        limit: int = 50,
    ) -> List[Dict[str, Any]]:
        pid = (project_id or "").strip().lower()
        if not pid:
            return []
        limit = max(1, min(int(limit), 200))
        with get_session() as session:
            rows = (
                session.query(IncidentReportRow)
                .filter(IncidentReportRow.project_id == pid)
                .order_by(desc(IncidentReportRow.created_at))
                .limit(limit)
                .all()
            )
            out: List[Dict[str, Any]] = []
            for row in rows:
                try:
                    conf = float(row.confidence or 0)
                except (TypeError, ValueError):
                    conf = 0.0
                out.append({
                    "id": row.id,
                    "project_id": row.project_id,
                    "description": row.description,
                    "severity": row.severity,
                    "summary": row.summary,
                    "confidence": conf,
                    "created_at": row.created_at,
                })
            return out

    def get_for_project(self, project_id: str, report_id: str) -> Optional[Dict[str, Any]]:
        data = self.get(report_id)
        if not data:
            return None
        pid = (project_id or "").strip().lower()
        if pid and str(data.get("project_id") or "").strip().lower() != pid:
            return None
        return data


incident_report_repo = IncidentReportRepository()
