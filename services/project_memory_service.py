# services/project_memory_service.py
"""CRUD facade for per-project System Memory."""
from __future__ import annotations

import logging
from typing import Any, Dict, Optional

from models.project_knowledge_models import ProjectKnowledge, _utc_now_iso
from services.app_knowledge_graph import apply_patch
from services.db.project_knowledge_repository import project_knowledge_repo

logger = logging.getLogger("vanya.project_memory")


def get_or_create(project_id: str, *, project_name: str = "") -> ProjectKnowledge:
    pid = (project_id or "").strip()
    if not pid:
        raise ValueError("project_id is required")
    existing = project_knowledge_repo.get(pid)
    if existing:
        if project_name and not existing.project_name:
            existing = existing.model_copy(update={"project_name": project_name})
            project_knowledge_repo.upsert(existing)
        return existing
    knowledge = ProjectKnowledge(
        project_id=pid,
        project_name=project_name or pid,
        updated_at=_utc_now_iso(),
        metadata={"created_at": _utc_now_iso()},
    )
    return project_knowledge_repo.upsert(knowledge)


def get_memory(project_id: str) -> Optional[ProjectKnowledge]:
    return project_knowledge_repo.get((project_id or "").strip())


def save_memory(knowledge: ProjectKnowledge) -> ProjectKnowledge:
    return project_knowledge_repo.upsert(knowledge)


def merge_memory_patch(project_id: str, patch: Dict[str, Any], *, project_name: str = "") -> ProjectKnowledge:
    base = get_or_create(project_id, project_name=project_name)
    merged = apply_patch(base, patch)
    return project_knowledge_repo.upsert(merged)
