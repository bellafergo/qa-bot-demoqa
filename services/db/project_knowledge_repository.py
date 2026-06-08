# services/db/project_knowledge_repository.py
"""SQLite persistence for per-project System Memory (Phase 1)."""
from __future__ import annotations

import json
import logging
from datetime import datetime, timezone
from typing import Any, Dict, Optional

from sqlalchemy import Column, String, Text

from models.project_knowledge_models import ProjectKnowledge
from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.project_knowledge")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class ProjectKnowledgeRow(Base):
    __tablename__ = "project_knowledge"

    project_id = Column(String, primary_key=True)
    project_name = Column(String, nullable=False, default="")
    payload_json = Column(Text, nullable=False, default="{}")
    updated_at = Column(String, nullable=False)


class ProjectKnowledgeRepository:
    def get(self, project_id: str) -> Optional[ProjectKnowledge]:
        pid = (project_id or "").strip()
        if not pid:
            return None
        with get_session() as session:
            row = session.get(ProjectKnowledgeRow, pid)
            if not row:
                return None
            try:
                data = json.loads(row.payload_json or "{}")
            except json.JSONDecodeError:
                data = {}
            data.setdefault("project_id", pid)
            data["project_name"] = row.project_name or data.get("project_name") or ""
            data["updated_at"] = row.updated_at or data.get("updated_at") or _utc_iso()
            return ProjectKnowledge.model_validate(data)

    def upsert(self, knowledge: ProjectKnowledge) -> ProjectKnowledge:
        pid = (knowledge.project_id or "").strip()
        if not pid:
            raise ValueError("project_id is required")
        now = _utc_iso()
        payload = knowledge.model_copy(update={"updated_at": now})
        blob = payload.model_dump()
        with get_session() as session:
            row = session.get(ProjectKnowledgeRow, pid)
            if row:
                row.project_name = payload.project_name or row.project_name or pid
                row.payload_json = json.dumps(blob, ensure_ascii=False)
                row.updated_at = now
            else:
                session.add(ProjectKnowledgeRow(
                    project_id=pid,
                    project_name=payload.project_name or pid,
                    payload_json=json.dumps(blob, ensure_ascii=False),
                    updated_at=now,
                ))
            session.commit()
        return payload

    def delete(self, project_id: str) -> bool:
        pid = (project_id or "").strip()
        if not pid:
            return False
        with get_session() as session:
            row = session.get(ProjectKnowledgeRow, pid)
            if not row:
                return False
            session.delete(row)
            session.commit()
            return True

    def list_project_ids(self) -> list[str]:
        with get_session() as session:
            rows = session.query(ProjectKnowledgeRow.project_id).all()
            return [r[0] for r in rows if r and r[0]]


project_knowledge_repo = ProjectKnowledgeRepository()
