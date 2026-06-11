# api/routes/incident_project_routes.py
"""Project-scoped Incident Investigator — QA Intelligence correlation (v1.1)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException, Query, Request

from models.incident_models import (
    ProjectIncidentInvestigationListResponse,
    ProjectIncidentInvestigationReport,
    ProjectInvestigateIncidentRequest,
)
from services.incident_qa_investigator_service import (
    get_project_incident_report,
    investigate_project_incident,
    list_project_incident_history,
)

logger = logging.getLogger("vanya.incident_project_routes")

router = APIRouter(prefix="/projects", tags=["incidents"])


@router.post(
    "/{project_id}/incidents/investigate",
    response_model=ProjectIncidentInvestigationReport,
)
def post_project_investigate_incident(
    project_id: str,
    body: ProjectInvestigateIncidentRequest,
    request: Request,
):
    """
    Correlate a user-reported incident with real QA data for the project.
    Persists the generated report automatically (v1.1).
    """
    from services.audit_event_service import set_audit_actor
    from services.auth_identity_service import auth_context_from_state

    from services.authorization_service import require_permission_from_request

    require_permission_from_request(
        request,
        "VIEW_INCIDENTS",
        resource_type="INCIDENTS",
        resource_id=project_id,
    )
    ctx = auth_context_from_state(request.state)
    set_audit_actor(user_id=ctx.get("user_id"), user_email=ctx.get("email"))
    try:
        return investigate_project_incident(project_id, body)
    except LookupError as e:
        raise HTTPException(status_code=404, detail=str(e)) from None
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("POST /projects/%s/incidents/investigate failed", project_id)
        raise HTTPException(status_code=500, detail=f"Investigation failed: {exc}") from exc


@router.get(
    "/{project_id}/incidents/history",
    response_model=ProjectIncidentInvestigationListResponse,
)
def get_project_incident_history(
    project_id: str,
    request: Request,
    limit: int = Query(default=50, ge=1, le=200),
):
    """List persisted project-scoped incident investigation reports."""
    from services.authorization_service import require_permission_from_request

    require_permission_from_request(
        request,
        "VIEW_INCIDENTS",
        resource_type="INCIDENTS",
        resource_id=project_id,
    )
    try:
        return list_project_incident_history(project_id, limit=limit)
    except LookupError as e:
        raise HTTPException(status_code=404, detail=str(e)) from None
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception("GET /projects/%s/incidents/history failed", project_id)
        raise HTTPException(status_code=500, detail=f"History failed: {exc}") from exc


@router.get(
    "/{project_id}/incidents/{incident_id}",
    response_model=ProjectIncidentInvestigationReport,
)
def get_project_incident_by_id(project_id: str, incident_id: str, request: Request):
    """Retrieve a persisted project-scoped incident investigation report."""
    from services.authorization_service import require_permission_from_request

    require_permission_from_request(
        request,
        "VIEW_INCIDENTS",
        resource_type="INCIDENTS",
        resource_id=project_id,
    )
    try:
        return get_project_incident_report(project_id, incident_id)
    except LookupError as e:
        raise HTTPException(status_code=404, detail=str(e)) from None
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        logger.exception(
            "GET /projects/%s/incidents/%s failed", project_id, incident_id,
        )
        raise HTTPException(status_code=500, detail=f"Get incident failed: {exc}") from exc
