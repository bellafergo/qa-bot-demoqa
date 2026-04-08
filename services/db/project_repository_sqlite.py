# services/db/project_repository_sqlite.py
"""SQLite persistence for projects table (local dev / fallback when Supabase is not configured)."""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, String, Text

from services.db.sqlite_db import Base, get_session
from services.project_settings_service import dump_settings_json, parse_settings_json

logger = logging.getLogger("vanya.db.project.sqlite")


class ProjectRow(Base):
    __tablename__ = "projects"

    id            = Column(String, primary_key=True)
    name          = Column(String, nullable=False)
    description   = Column(Text, default="")
    color         = Column(String, default="#6366f1")
    base_url      = Column(String, nullable=True)
    settings_json = Column(Text, nullable=True)
    created_at    = Column(String, nullable=False)
    updated_at    = Column(String, nullable=False)


def _row_to_model(row: ProjectRow):
    from models.project import Project

    return Project(
        id=row.id,
        name=row.name,
        description=row.description or "",
        color=row.color or "#6366f1",
        base_url=row.base_url,
        settings=parse_settings_json(getattr(row, "settings_json", None)),
        created_at=datetime.fromisoformat(row.created_at.replace("Z", "+00:00")),
        updated_at=datetime.fromisoformat(row.updated_at.replace("Z", "+00:00")),
    )


class ProjectRepositorySqlite:

    def list_projects(self) -> List[Any]:
        with get_session() as s:
            rows = s.query(ProjectRow).order_by(ProjectRow.id.asc()).all()
        out = [_row_to_model(r) for r in rows]
        logger.info("project_repo.sqlite: list_projects count=%s", len(out))
        return out

    def get_project(self, project_id: str) -> Optional[Any]:
        pid = (project_id or "").strip().lower()
        if not pid:
            return None
        with get_session() as s:
            row = s.query(ProjectRow).filter_by(id=pid).first()
        return _row_to_model(row) if row else None

    def create_project(self, p) -> Any:
        now = datetime.now(timezone.utc).isoformat()
        settings_dict: Dict[str, Any] = {}
        if hasattr(p, "model_dump"):
            sd = p.model_dump().get("settings")
            if isinstance(sd, dict):
                settings_dict = sd
        elif getattr(p, "settings", None) is not None and isinstance(p.settings, dict):
            settings_dict = p.settings
        row = ProjectRow(
            id=p.id,
            name=p.name,
            description=p.description or "",
            color=p.color or "#6366f1",
            base_url=p.base_url,
            settings_json=dump_settings_json(settings_dict or {}),
            created_at=now,
            updated_at=now,
        )
        with get_session() as s:
            s.add(row)
        logger.info("project_repo.sqlite: created project id=%s", p.id)
        return _row_to_model(row)

    def update_project(self, project_id: str, data: Dict[str, Any]) -> Optional[Any]:
        pid = (project_id or "").strip().lower()
        now = datetime.now(timezone.utc).isoformat()
        with get_session() as s:
            row = s.query(ProjectRow).filter_by(id=pid).first()
            if row is None:
                return None
            if "name" in data and data["name"] is not None:
                row.name = data["name"]
            if "description" in data and data["description"] is not None:
                row.description = data["description"]
            if "color" in data and data["color"] is not None:
                row.color = data["color"]
            if "base_url" in data:
                row.base_url = data["base_url"]
            if "settings" in data and data["settings"] is not None:
                row.settings_json = dump_settings_json(data["settings"])
            row.updated_at = now
            s.flush()
            return _row_to_model(row)

    def delete_project(self, project_id: str) -> bool:
        pid = (project_id or "").strip().lower()
        with get_session() as s:
            row = s.query(ProjectRow).filter_by(id=pid).first()
            if row is None:
                return False
            s.delete(row)
        logger.info("project_repo.sqlite: deleted project id=%s", pid)
        return True
