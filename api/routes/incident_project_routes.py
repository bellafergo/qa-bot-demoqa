# api/routes/incident_project_routes.py
"""Project-scoped Incident Investigator — QA Intelligence correlation."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.incident_models import (
    ProjectIncidentInvestigationReport,
    ProjectInvestigateIncidentRequest,
)
from services.incident_qa_investigator_service import investigate_project_incident

logger = logging.getLogger("vanya.incident_project_routes")

router = APIRouter(prefix="/projects", tags=["incidents"])


@router.post(
    "/{project_id}/incidents/investigate",
    response_model=ProjectIncidentInvestigationReport,
)
def post_project_investigate_incident(project_id: str, body: ProjectInvestigateIncidentRequest):
    """
    Correlate a user-reported incident with real QA data for the project:
    failed runs, evidence, failure intelligence, system memory, and open PRs.

    Deterministic heuristics — does not invent evidence. Optional browser probe
    when ``include_browser_probe=true`` and ``target_url`` is set.
    """
    try:
        return investigate_project_incident(project_id, body)
    except LookupError as e:
        raise HTTPException(status_code=404, detail=str(e)) from None
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("POST /projects/%s/incidents/investigate failed", project_id)
        raise HTTPException(status_code=500, detail=f"Investigation failed: {exc}") from exc
