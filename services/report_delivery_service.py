# services/report_delivery_service.py
"""
Enterprise ENT-02C — Executive Report Delivery.

Transforms existing ExecutiveReportPreview data into channel payloads and sends
via IntegrationDispatcher only. No new intelligence generation.
"""
from __future__ import annotations

import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from models.report_delivery_models import (
    ReportDeliveryPreview,
    ReportDeliveryRequest,
    ReportDeliveryResult,
)
from models.scheduled_report_models import ExecutiveReportPreview
from services.integration_dispatcher import integration_dispatcher

logger = logging.getLogger("vanya.report_delivery")

_ALLOWED_REPORT_TYPES = frozenset({
    "EXECUTIVE_QUALITY",
    "RELEASE_READINESS",
    "WEEKLY_QUALITY_BRIEF",
    "INCIDENT_REVIEW",
})
_ALLOWED_CHANNELS = frozenset({"email", "slack", "teams"})

_SCHEDULED_TYPE_MAP = {
    "EXECUTIVE_QUALITY": "EXECUTIVE_SUMMARY",
    "RELEASE_READINESS": "RELEASE_READINESS",
    "WEEKLY_QUALITY_BRIEF": "QUALITY_BRIEF",
    "INCIDENT_REVIEW": "INCIDENT_REVIEW",
}

_REPORT_LABELS = {
    "EXECUTIVE_QUALITY": "Executive Quality Report",
    "RELEASE_READINESS": "Release Readiness Report",
    "WEEKLY_QUALITY_BRIEF": "Weekly Quality Brief",
    "INCIDENT_REVIEW": "Incident Review",
}


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _normalize_report_type(report_type: str) -> str:
    rtype = str(report_type or "").strip().upper()
    if rtype not in _ALLOWED_REPORT_TYPES:
        raise ValueError(
            f"Invalid report_type '{report_type}'. "
            f"Allowed: {', '.join(sorted(_ALLOWED_REPORT_TYPES))}"
        )
    return rtype


def _normalize_channel(channel: str) -> str:
    ch = str(channel or "").strip().lower()
    if ch not in _ALLOWED_CHANNELS:
        raise ValueError(
            f"Invalid channel '{channel}'. Allowed: {', '.join(sorted(_ALLOWED_CHANNELS))}"
        )
    return ch


def _load_executive_preview(project_id: str, report_type: str) -> ExecutiveReportPreview:
    from services.scheduled_report_service import (
        _has_preview_inputs,
        _load_latest_incident_report,
        build_executive_report_preview,
    )

    pid = (project_id or "").strip().lower()
    scheduled_type = _SCHEDULED_TYPE_MAP[report_type]
    latest = _load_latest_incident_report(pid)
    if not latest or not _has_preview_inputs(latest):
        label = _REPORT_LABELS.get(report_type, report_type)
        raise LookupError(
            f"No stored executive intelligence available for {label}. "
            "Run an incident investigation first."
        )
    return build_executive_report_preview(pid, latest, report_type=scheduled_type)


def _format_body(preview: ExecutiveReportPreview, *, project_id: str) -> str:
    lines = [
        preview.title,
        f"Project: {project_id}",
        f"Generated: {preview.generated_at}",
        "",
        f"Quality Score: {preview.quality_score}/100",
        f"Trend: {preview.quality_trend}",
        f"Risk Level: {preview.risk_level}",
        "",
        preview.executive_summary or "No executive summary available.",
    ]
    if preview.top_risks:
        lines.extend(["", "Top Risks:"])
        lines.extend(f"• {risk}" for risk in preview.top_risks[:5])
    if preview.top_recommendations:
        lines.extend(["", "Top Recommendations:"])
        lines.extend(f"• {rec}" for rec in preview.top_recommendations[:5])
    lines.extend([
        "",
        f"Incidents: {preview.incident_count}",
        f"Critical Contracts: {preview.critical_contract_count}",
        f"Broken Journeys: {preview.broken_journey_count}",
    ])
    if preview.jira_blocker_count and preview.jira_blocker_count > 0:
        lines.append(f"Jira Blockers: {preview.jira_blocker_count}")
        for key in (preview.jira_blocker_keys or [])[:5]:
            lines.append(f"• {key}")
    return "\n".join(lines)


def _build_subject(preview: ExecutiveReportPreview, *, project_id: str) -> str:
    return f"{preview.title} — {project_id}"


def _build_slack_payload(preview: ExecutiveReportPreview, *, body: str) -> Dict[str, Any]:
    blocks: List[Dict[str, Any]] = [
        {
            "type": "header",
            "text": {"type": "plain_text", "text": preview.title[:150]},
        },
        {
            "type": "section",
            "fields": [
                {"type": "mrkdwn", "text": f"*Quality:*\n{preview.quality_score}/100"},
                {"type": "mrkdwn", "text": f"*Trend:*\n{preview.quality_trend}"},
                {"type": "mrkdwn", "text": f"*Risk:*\n{preview.risk_level}"},
            ],
        },
        {
            "type": "section",
            "text": {"type": "mrkdwn", "text": preview.executive_summary or "_No summary._"},
        },
    ]
    if preview.top_risks:
        blocks.append({
            "type": "section",
            "text": {
                "type": "mrkdwn",
                "text": "*Top Risks:*\n" + "\n".join(f"• {r}" for r in preview.top_risks[:5]),
            },
        })
    if preview.top_recommendations:
        blocks.append({
            "type": "section",
            "text": {
                "type": "mrkdwn",
                "text": "*Recommendations:*\n" + "\n".join(f"• {r}" for r in preview.top_recommendations[:5]),
            },
        })
    return {
        "text": body[:3000],
        "blocks": blocks,
    }


def _build_teams_payload(preview: ExecutiveReportPreview, *, subject: str, body: str) -> Dict[str, Any]:
    return {
        "@type": "MessageCard",
        "@context": "https://schema.org/extensions",
        "summary": subject,
        "themeColor": "0078D4",
        "title": preview.title,
        "sections": [
            {
                "activityTitle": preview.title,
                "facts": [
                    {"name": "Quality Score", "value": str(preview.quality_score)},
                    {"name": "Trend", "value": preview.quality_trend},
                    {"name": "Risk Level", "value": preview.risk_level},
                ],
                "text": preview.executive_summary or body,
            },
        ],
    }


def _build_email_payload(*, subject: str, body: str, recipient: Optional[str]) -> Dict[str, Any]:
    return {
        "subject": subject,
        "body": body,
        "recipient": recipient,
    }


def channel_is_ready(channel: str) -> Tuple[bool, str]:
    ch = _normalize_channel(channel)
    readiness = integration_dispatcher.readiness().get(ch) or {}
    if readiness.get("ready"):
        return True, ""
    return False, str(readiness.get("message") or f"{ch} connector is not ready")


def build_report_delivery_preview(
    *,
    project_id: str,
    report_type: str,
    channel: str,
    recipient: Optional[str] = None,
) -> ReportDeliveryPreview:
    """Build deliverable preview from existing executive report intelligence."""
    pid = (project_id or "").strip().lower()
    rtype = _normalize_report_type(report_type)
    ch = _normalize_channel(channel)

    preview = _load_executive_preview(pid, rtype)
    subject = _build_subject(preview, project_id=pid)
    body = _format_body(preview, project_id=pid)
    summary = preview.executive_summary or preview.title

    if ch == "email":
        payload = _build_email_payload(subject=subject, body=body, recipient=recipient)
    elif ch == "slack":
        payload = _build_slack_payload(preview, body=body)
    else:
        payload = _build_teams_payload(preview, subject=subject, body=body)

    preview = ReportDeliveryPreview(
        channel=ch,
        report_type=rtype,
        subject=subject,
        summary=summary,
        payload=payload,
    )
    from services.audit_event_service import safe_record_event

    safe_record_event(
        event_type="REPORT_PREVIEWED",
        resource_type="REPORTS",
        resource_id=f"{pid}:{rtype}",
        action="preview",
        result="SUCCESS",
        metadata={"channel": ch, "report_type": rtype},
    )
    return preview


def send_report_delivery(
    *,
    project_id: str,
    report_type: str,
    channel: str,
    recipient: Optional[str] = None,
    recipients: Optional[List[str]] = None,
    channel_override: Optional[str] = None,
    requires_user_approval: bool = False,
) -> ReportDeliveryResult:
    """Send an executive report through IntegrationDispatcher (user-initiated only)."""
    if not requires_user_approval:
        raise ValueError("requires_user_approval must be true — automatic delivery is not allowed")

    pid = (project_id or "").strip().lower()
    rtype = _normalize_report_type(report_type)
    ch = _normalize_channel(channel)

    ready, reason = channel_is_ready(ch)
    if not ready:
        return ReportDeliveryResult(
            success=False,
            channel=ch,
            report_type=rtype,
            error=reason or f"Channel '{ch}' is not configured",
        )

    delivery_preview = build_report_delivery_preview(
        project_id=pid,
        report_type=rtype,
        channel=ch,
        recipient=recipient,
    )
    body = str(delivery_preview.payload.get("body") or delivery_preview.summary)
    if ch == "slack":
        body = str(delivery_preview.payload.get("text") or body)
    elif ch == "teams":
        body = delivery_preview.summary

    email_recipients: Optional[List[str]] = None
    if ch == "email":
        email_recipients = list(recipients or [])
        if recipient and recipient not in email_recipients:
            email_recipients.insert(0, recipient)
        if not email_recipients:
            return ReportDeliveryResult(
                success=False,
                channel=ch,
                report_type=rtype,
                error="At least one email recipient is required",
            )

    request = ReportDeliveryRequest(
        request_id=f"report_delivery:{uuid.uuid4().hex[:12]}",
        project_id=pid,
        report_type=rtype,
        channel=ch,
        recipient=recipient or (email_recipients[0] if email_recipients else None),
        created_at=_utc_now_iso(),
    )

    ok, detail = integration_dispatcher.send_alert(
        connector_id=ch,
        subject=delivery_preview.subject,
        message=body,
        recipients=email_recipients,
        channel=channel_override,
        context={"report_type": rtype, "request_id": request.request_id},
    )

    from services.audit_event_service import safe_record_event

    if ok:
        safe_record_event(
            event_type="REPORT_SENT",
            resource_type="REPORTS",
            resource_id=f"{pid}:{rtype}",
            action="send",
            result="SUCCESS",
            metadata={"channel": ch, "report_type": rtype, "request_id": request.request_id},
        )
        return ReportDeliveryResult(
            success=True,
            channel=ch,
            report_type=rtype,
            delivered_at=_utc_now_iso(),
            request=request,
        )

    error = str(detail.get("error") or detail.get("message") or "Delivery failed")
    safe_record_event(
        event_type="REPORT_SENT",
        resource_type="REPORTS",
        resource_id=f"{pid}:{rtype}",
        action="send",
        result="FAILURE",
        metadata={"channel": ch, "report_type": rtype, "error": error},
    )
    return ReportDeliveryResult(
        success=False,
        channel=ch,
        report_type=rtype,
        error=error,
        request=request,
    )
