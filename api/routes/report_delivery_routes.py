# api/routes/report_delivery_routes.py
"""Enterprise ENT-02C — Executive Report Delivery (user-initiated)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException, Request

from models.report_delivery_models import (
    ReportDeliveryPreview,
    ReportDeliveryPreviewRequest,
    ReportDeliveryResult,
    ReportDeliverySendRequest,
)
from services.report_delivery_service import (
    build_report_delivery_preview,
    send_report_delivery,
)

logger = logging.getLogger("vanya.report_delivery_routes")

router = APIRouter(prefix="/projects", tags=["report-delivery"])


def _ensure_project(project_id: str) -> str:
    pid = (project_id or "").strip().lower()
    if not pid:
        raise HTTPException(status_code=400, detail="project_id is required")
    from services.db.project_repository import project_repo

    if project_repo.get_project(pid) is None:
        raise HTTPException(status_code=404, detail=f"project not found: {pid}")
    return pid


@router.post("/{project_id}/reports/preview", response_model=ReportDeliveryPreview)
def post_report_delivery_preview(project_id: str, body: ReportDeliveryPreviewRequest, request: Request):
    """Preview how an existing executive report will be delivered."""
    pid = _ensure_project(project_id)
    from services.audit_event_service import set_audit_actor
    from services.auth_identity_service import auth_context_from_state

    ctx = auth_context_from_state(request.state)
    set_audit_actor(user_id=ctx.get("user_id"), user_email=ctx.get("email"))
    try:
        return build_report_delivery_preview(
            project_id=pid,
            report_type=body.report_type,
            channel=body.channel,
            recipient=body.recipient,
        )
    except ValueError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from None
    except LookupError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from None
    except Exception as exc:
        logger.exception("POST /projects/%s/reports/preview failed", pid)
        raise HTTPException(status_code=500, detail=f"Preview failed: {exc}") from exc


@router.post("/{project_id}/reports/send", response_model=ReportDeliveryResult)
def post_report_delivery_send(project_id: str, body: ReportDeliverySendRequest, request: Request):
    """Send an executive report through a configured delivery channel (explicit approval required)."""
    pid = _ensure_project(project_id)
    from services.audit_event_service import set_audit_actor
    from services.auth_identity_service import auth_context_from_state

    ctx = auth_context_from_state(request.state)
    set_audit_actor(user_id=ctx.get("user_id"), user_email=ctx.get("email"))
    if not body.requires_user_approval:
        raise HTTPException(
            status_code=400,
            detail="requires_user_approval must be true — automatic delivery is not allowed",
        )
    try:
        return send_report_delivery(
            project_id=pid,
            report_type=body.report_type,
            channel=body.channel,
            recipient=body.recipient,
            recipients=body.recipients,
            channel_override=body.channel_override,
            requires_user_approval=body.requires_user_approval,
        )
    except ValueError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from None
    except LookupError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from None
    except Exception as exc:
        logger.exception("POST /projects/%s/reports/send failed", pid)
        raise HTTPException(status_code=500, detail=f"Send failed: {exc}") from exc
