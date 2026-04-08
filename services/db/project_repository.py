# services/db/project_repository.py
"""
Project persistence facade.

- **Supabase (Postgres via PostgREST)** when `VANYA_PROJECTS_BACKEND=supabase`, or when
  `VANYA_PROJECTS_BACKEND=auto` (default) and `SUPABASE_URL` + `SUPABASE_SERVICE_ROLE_KEY` are set.
- **SQLite** (`./data/vanya.db` via SQLAlchemy) when `VANYA_PROJECTS_BACKEND=sqlite`, or in auto
  mode if Supabase credentials are missing (local dev).

Production: set Supabase env vars so auto selects Supabase; optional explicit `VANYA_PROJECTS_BACKEND=supabase`.
"""
from __future__ import annotations

import logging
import os
from typing import Any, Dict, List, Optional

from services.db.project_repository_sqlite import ProjectRepositorySqlite
from services.db.project_repository_supabase import ProjectRepositorySupabase
from services.supabase_store import is_supabase_configured

logger = logging.getLogger("vanya.db.project")


def _backend_name() -> str:
    raw = (os.getenv("VANYA_PROJECTS_BACKEND") or "auto").strip().lower()
    if raw in ("supabase", "sqlite"):
        return raw
    if raw != "auto":
        logger.warning("VANYA_PROJECTS_BACKEND=%r invalid — using auto", raw)
    return "supabase" if is_supabase_configured() else "sqlite"


def _make_impl(name: str):
    if name == "supabase":
        return ProjectRepositorySupabase()
    return ProjectRepositorySqlite()


class ProjectRepository:
    """Delegates to SQLite or Supabase implementation (chosen once at construction)."""

    def __init__(self) -> None:
        self._backend = _backend_name()
        self._impl = _make_impl(self._backend)
        logger.info(
            "project_repo: active_backend=%s (VANYA_PROJECTS_BACKEND=%s, supabase_configured=%s)",
            self._backend,
            (os.getenv("VANYA_PROJECTS_BACKEND") or "auto").strip() or "auto",
            is_supabase_configured(),
        )

    def list_projects(self) -> List[Any]:
        return self._impl.list_projects()

    def get_project(self, project_id: str) -> Optional[Any]:
        return self._impl.get_project(project_id)

    def create_project(self, p) -> Any:
        return self._impl.create_project(p)

    def update_project(self, project_id: str, data: Dict[str, Any]) -> Optional[Any]:
        return self._impl.update_project(project_id, data)

    def delete_project(self, project_id: str) -> bool:
        return self._impl.delete_project(project_id)


project_repo = ProjectRepository()
