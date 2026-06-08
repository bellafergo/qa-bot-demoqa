# api/routes/project_knowledge_routes.py
"""System Memory / App Knowledge Graph API (Phase 1)."""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, HTTPException, Query

from models.project_knowledge_models import (
    KnowledgeRefreshMode,
    ProjectKnowledge,
    ProjectKnowledgeRefreshRequest,
)
from services.project_knowledge_service import (
    get_project_knowledge,
    refresh_project_knowledge,
)

logger = logging.getLogger("vanya.project_knowledge_routes")

router = APIRouter(prefix="/projects", tags=["project-knowledge"])


@router.get("/{project_id}/knowledge", response_model=ProjectKnowledge)
def get_knowledge(project_id: str):
    row = get_project_knowledge(project_id)
    if not row:
        raise HTTPException(status_code=404, detail=f"No knowledge stored for project: {project_id}")
    return row


@router.post("/{project_id}/knowledge/refresh", response_model=ProjectKnowledge)
def refresh_knowledge(
    project_id: str,
    mode: KnowledgeRefreshMode = Query(default="replace"),
    include_catalog: bool = Query(default=True),
    include_runs: bool = Query(default=True),
    include_failures: bool = Query(default=True),
    include_incidents: bool = Query(default=True),
    include_discovery: bool = Query(default=True),
):
    try:
        return refresh_project_knowledge(
            project_id,
            ProjectKnowledgeRefreshRequest(
                mode=mode,
                include_catalog=include_catalog,
                include_runs=include_runs,
                include_failures=include_failures,
                include_incidents=include_incidents,
                include_discovery=include_discovery,
            ),
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("POST /projects/%s/knowledge/refresh failed", project_id)
        raise HTTPException(status_code=500, detail=f"Knowledge refresh failed: {exc}") from exc
