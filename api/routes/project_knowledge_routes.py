# api/routes/project_knowledge_routes.py
"""System Memory / App Knowledge Graph API (Phase 1)."""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, HTTPException, Query

from models.project_knowledge_models import (
    KnowledgeRefreshMode,
    ProjectKnowledge,
    ProjectKnowledgeExplorer,
    ProjectKnowledgeRefreshRequest,
)
from models.pr_analysis_models import ProjectPRAnalysisReport, ProjectPRAnalysisRequest
from models.risk_engine_models import RiskAssessment
from services.pr_analysis_service import pr_analysis_service
from services.project_knowledge_service import (
    get_project_knowledge,
    refresh_project_knowledge,
)
from services.project_risk_service import assess_project_risk

logger = logging.getLogger("vanya.project_knowledge_routes")

router = APIRouter(prefix="/projects", tags=["project-knowledge"])


@router.post("/{project_id}/pr-analysis", response_model=ProjectPRAnalysisReport)
def analyze_project_pr(project_id: str, req: ProjectPRAnalysisRequest):
    """
    PR Analysis v1 — map changed files to modules via System Memory,
    consume Risk Engine ``module_risks`` and ``recommended_tests`` (no risk recalc).
    """
    try:
        report = pr_analysis_service.analyze_for_project(project_id, req)
        from services.pr_analysis_report_store import persist_pr_analysis_report

        persist_pr_analysis_report(
            project_id,
            report,
            pr_id=req.pr_id,
            provider="manual",
        )
        return report
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("POST /projects/%s/pr-analysis failed", project_id)
        raise HTTPException(status_code=500, detail=f"PR analysis failed: {exc}") from exc


@router.get("/{project_id}/risk", response_model=RiskAssessment)
def get_project_risk(project_id: str):
    """On-demand risk assessment (no separate persistence)."""
    try:
        from services.project_memory_service import get_memory

        mem = get_memory(project_id) or get_project_knowledge(project_id)
        return assess_project_risk(project_id, knowledge=mem)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("GET /projects/%s/risk failed", project_id)
        raise HTTPException(status_code=500, detail=f"Risk assessment failed: {exc}") from exc


@router.get("/{project_id}/knowledge", response_model=ProjectKnowledge)
def get_knowledge(project_id: str):
    row = get_project_knowledge(project_id)
    if not row:
        raise HTTPException(status_code=404, detail=f"No knowledge stored for project: {project_id}")
    return row


@router.get("/{project_id}/knowledge/explorer", response_model=ProjectKnowledgeExplorer)
def get_knowledge_explorer(project_id: str):
    """Navigable module graph: routes, APIs, tests, failure clusters per module."""
    try:
        from services.knowledge_explorer_service import get_project_knowledge_explorer
        return get_project_knowledge_explorer(project_id)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("GET /projects/%s/knowledge/explorer failed", project_id)
        raise HTTPException(status_code=500, detail=f"Knowledge explorer failed: {exc}") from exc


@router.post("/{project_id}/knowledge/refresh", response_model=ProjectKnowledge)
def refresh_knowledge(
    project_id: str,
    mode: KnowledgeRefreshMode = Query(default="replace"),
    include_catalog: bool = Query(default=True),
    include_runs: bool = Query(default=True),
    include_failures: bool = Query(default=True),
    include_incidents: bool = Query(default=True),
    include_discovery: bool = Query(default=True),
    include_repository: bool = Query(default=False),
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
                include_repository=include_repository,
            ),
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("POST /projects/%s/knowledge/refresh failed", project_id)
        raise HTTPException(status_code=500, detail=f"Knowledge refresh failed: {exc}") from exc
