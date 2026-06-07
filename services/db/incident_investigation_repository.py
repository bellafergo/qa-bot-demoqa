# services/db/incident_investigation_repository.py
"""SQLite persistence for Autonomous Incident Investigator runs (MVP)."""
from __future__ import annotations

import json
import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, Integer, String, Text, desc

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.incident_investigation")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class IncidentInvestigationRow(Base):
    __tablename__ = "incident_investigation_runs"

    id = Column(String, primary_key=True)
    created_at = Column(String, nullable=False)
    updated_at = Column(String, nullable=False)
    status = Column(String, nullable=False, default="running")
    project_id = Column(String, nullable=True)
    payload_json = Column(Text, nullable=False, default="{}")


class IncidentInvestigationRepository:
    def create_running(
        self,
        *,
        incident_description: str,
        project_id: Optional[str] = None,
        module: Optional[str] = None,
        target_url: Optional[str] = None,
    ) -> str:
        run_id = str(uuid.uuid4())
        now = _utc_iso()
        payload: Dict[str, Any] = {
            "id": run_id,
            "created_at": now,
            "updated_at": now,
            "status": "running",
            "incident_description": incident_description,
            "target_url": target_url,
            "project_id": project_id,
            "module": module,
            "severity": "info",
            "reproduced": "unknown",
            "suspected_area": "unknown",
            "console_errors": [],
            "network_errors": [],
            "http_errors": [],
            "steps_executed": ["investigation_started"],
            "diagnosis_summary": "",
            "recommendations": [],
            "reproduction_steps": [],
            "raw_evidence": {},
            "meta": {},
        }
        row = IncidentInvestigationRow(
            id=run_id,
            created_at=now,
            updated_at=now,
            status="running",
            project_id=(project_id or "").strip() or None,
            payload_json=json.dumps(payload, ensure_ascii=False),
        )
        with get_session() as session:
            session.add(row)
            session.commit()
        return run_id

    def save_payload(self, run_id: str, payload: Dict[str, Any]) -> None:
        now = _utc_iso()
        payload = dict(payload)
        payload["updated_at"] = now
        status = str(payload.get("status") or "completed")
        with get_session() as session:
            row = session.get(IncidentInvestigationRow, run_id)
            if not row:
                raise KeyError(f"incident run not found: {run_id}")
            row.updated_at = now
            row.status = status
            row.payload_json = json.dumps(payload, ensure_ascii=False)
            session.commit()

    def get(self, run_id: str) -> Optional[Dict[str, Any]]:
        with get_session() as session:
            row = session.get(IncidentInvestigationRow, run_id)
            if not row:
                return None
            try:
                return json.loads(row.payload_json or "{}")
            except Exception:
                logger.warning("incident_investigation: corrupt payload for %s", run_id)
                return None

    def list_runs(
        self,
        *,
        limit: int = 50,
        project_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        limit = max(1, min(int(limit), 200))
        with get_session() as session:
            q = session.query(IncidentInvestigationRow).order_by(
                desc(IncidentInvestigationRow.created_at)
            )
            pid = (project_id or "").strip()
            if pid:
                q = q.filter(IncidentInvestigationRow.project_id == pid)
            rows = q.limit(limit).all()
            out: List[Dict[str, Any]] = []
            for row in rows:
                try:
                    out.append(json.loads(row.payload_json or "{}"))
                except Exception:
                    continue
            return out


incident_investigation_repo = IncidentInvestigationRepository()
