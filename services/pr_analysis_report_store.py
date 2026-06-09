# services/pr_analysis_report_store.py
"""Thin persistence facade for stored PR Analysis reports (no re-analysis)."""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

from models.pr_analysis_models import ProjectPRAnalysisReport

logger = logging.getLogger("vanya.pr_analysis_report_store")


def persist_pr_analysis_report(
    project_id: str,
    report: ProjectPRAnalysisReport,
    *,
    pr_id: Optional[str] = None,
    provider: str = "manual",
) -> Optional[str]:
    """Store a report snapshot after analysis. Best-effort — never raises."""
    try:
        from services.db.pr_analysis_report_repository import pr_analysis_report_repo

        pid = (project_id or report.project_id or "").strip().lower()
        if not pid:
            return None
        pr = str(pr_id or "manual").strip()
        row_id = pr_analysis_report_repo.upsert(
            project_id=pid,
            pr_id=pr,
            provider=provider,
            report=report.model_dump(),
        )
        return row_id
    except Exception as e:
        logger.warning("pr_analysis_report_store: persist failed project_id=%s: %s", project_id, e)
        return None


def list_stored_pr_reports(project_id: str, *, limit: int = 50) -> List[Dict[str, Any]]:
    try:
        from services.db.pr_analysis_report_repository import pr_analysis_report_repo

        return pr_analysis_report_repo.list_for_project(project_id, limit=limit)
    except Exception as e:
        logger.debug("pr_analysis_report_store: list failed: %s", e)
        return []


def get_stored_pr_report(
    project_id: str,
    pr_id: str,
    *,
    provider: Optional[str] = None,
) -> Optional[Dict[str, Any]]:
    try:
        from services.db.pr_analysis_report_repository import pr_analysis_report_repo

        return pr_analysis_report_repo.get(project_id, pr_id, provider=provider)
    except Exception as e:
        logger.debug("pr_analysis_report_store: get failed: %s", e)
        return None
