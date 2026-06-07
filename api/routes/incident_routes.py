# api/routes/incident_routes.py
"""Autonomous Incident Investigator API (MVP)."""
from __future__ import annotations

import logging
from typing import Optional

from fastapi import APIRouter, HTTPException, Query

from models.incident_models import (
    IncidentInvestigationListResponse,
    IncidentInvestigationRun,
    InvestigateIncidentRequest,
)
from services.incident_investigator_service import (
    get_incident_run,
    investigate_incident,
    list_incident_runs,
)

logger = logging.getLogger("vanya.incident_routes")

router = APIRouter(prefix="/incidents", tags=["incidents"])


@router.post("/investigate", response_model=IncidentInvestigationRun)
def post_investigate_incident(body: InvestigateIncidentRequest):
    """
    Investigate a user-reported incident with Playwright (passive observation).

    Navigates to target_url (or inferred URL), captures console/network/screenshot,
    and returns a heuristic diagnosis. Non-destructive — no form submits.
    """
    try:
        return investigate_incident(body)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("POST /incidents/investigate failed")
        raise HTTPException(status_code=500, detail=f"Investigation failed: {exc}") from exc


@router.get("/runs", response_model=IncidentInvestigationListResponse)
def get_incident_runs(
    limit: int = Query(default=50, ge=1, le=200),
    project_id: Optional[str] = Query(default=None),
):
    items = list_incident_runs(limit=limit, project_id=project_id)
    return IncidentInvestigationListResponse(items=items, total=len(items))


@router.get("/runs/{run_id}", response_model=IncidentInvestigationRun)
def get_incident_run_by_id(run_id: str):
    row = get_incident_run(run_id)
    if not row:
        raise HTTPException(status_code=404, detail=f"Incident investigation not found: {run_id}")
    return row
