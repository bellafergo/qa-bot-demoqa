# services/pr_analysis_project_debug.py
"""Temporary PR Analysis project_id lookup diagnostics."""
from __future__ import annotations

import logging
from typing import Optional

logger = logging.getLogger("vanya.pr_analysis.project_debug")

_DEBUG_PREFIX = "PR_ANALYSIS_PROJECT_DEBUG"


def _available_project_ids() -> list[str]:
    try:
        from services.db.project_knowledge_repository import project_knowledge_repo

        return project_knowledge_repo.list_project_ids()
    except Exception:
        return []


def log_project_id_lookup(
    *,
    project_id: str,
    memory_found: Optional[bool] = None,
    include_available_ids: bool = True,
) -> None:
    if not logger.isEnabledFor(logging.DEBUG):
        return
    normalized = (project_id or "").strip()
    lines = [
        f"project_id={project_id!r}",
        f"normalized_project_id={normalized!r}",
    ]
    if memory_found is not None:
        lines.append(f"memory_found={'true' if memory_found else 'false'}")
    if include_available_ids:
        lines.append(f"available_project_ids={_available_project_ids()!r}")
    logger.debug("%s\n%s", _DEBUG_PREFIX, "\n".join(lines))


def log_memory_refresh_event(
    event: str,
    *,
    project_id: str,
    memory_found: Optional[bool] = None,
) -> None:
    """Operational logs for PR Analysis automatic memory refresh fallback."""
    normalized = (project_id or "").strip()
    parts = [
        "PR_ANALYSIS_MEMORY_REFRESH",
        f"event={event}",
        f"project_id={project_id!r}",
        f"normalized_project_id={normalized!r}",
    ]
    if memory_found is not None:
        parts.append(f"memory_found={'true' if memory_found else 'false'}")
    logger.info(" ".join(parts))
