# api/routes/azure_devops_oauth_routes.py
"""Fixed Azure DevOps OAuth callback — register this exact URI in Azure Portal."""
from __future__ import annotations

import logging

from fastapi import APIRouter, Query
from fastapi.responses import RedirectResponse

from api.routes.azure_devops_project_routes import _frontend_integrations_url
from services.project_azure_devops_settings_service import complete_oauth_callback

logger = logging.getLogger("vanya.azure_devops_oauth_routes")

router = APIRouter(tags=["azure-devops-oauth"])


@router.get("/azure-devops/callback")
def azure_devops_oauth_callback(
    code: str = Query(default=""),
    state: str = Query(default=""),
    error: str = Query(default=""),
    error_description: str = Query(default=""),
):
    """
    Microsoft OAuth redirect target (fixed path).

    ``state`` must be the Vanya ``project_id``. Azure App Registration must list:
    ``AZURE_REDIRECT_URI=https://qa-bot-demoqa.onrender.com/azure-devops/callback``
    """
    pid = (state or "").strip().lower()
    if error:
        return RedirectResponse(
            _frontend_integrations_url(project_id=pid or "unknown", ok=False, message=error_description or error),
            status_code=302,
        )
    try:
        if not pid:
            raise ValueError("state (project_id) is required")
        complete_oauth_callback(code=code, state=pid)
        return RedirectResponse(_frontend_integrations_url(project_id=pid, ok=True), status_code=302)
    except Exception as exc:
        logger.warning("azure oauth callback failed project_id=%s: %s", pid, type(exc).__name__)
        return RedirectResponse(
            _frontend_integrations_url(project_id=pid or "unknown", ok=False, message=str(exc)),
            status_code=302,
        )
