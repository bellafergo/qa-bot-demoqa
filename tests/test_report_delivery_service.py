# tests/test_report_delivery_service.py
"""ENT-02C — Executive Report Delivery."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

import pytest

from models.scheduled_report_models import ExecutiveReportPreview
from services.report_delivery_service import (
    build_report_delivery_preview,
    channel_is_ready,
    send_report_delivery,
)


def _preview() -> ExecutiveReportPreview:
    return ExecutiveReportPreview(
        preview_id="exec_preview:demo:quality_brief",
        title="Weekly Quality Brief",
        generated_at="2026-06-10T08:00:00+00:00",
        quality_score=82,
        quality_trend="DEGRADING",
        risk_level="HIGH",
        executive_summary="Payments require executive attention.",
        top_risks=["Checkout latency spike"],
        top_recommendations=["Run Payments Regression Suite"],
        incident_count=2,
        critical_contract_count=1,
        broken_journey_count=1,
    )


@patch("services.report_delivery_service._load_executive_preview")
def test_email_preview_includes_jira_blockers(mock_load):
    mock_load.return_value = ExecutiveReportPreview(
        preview_id="exec_preview:demo:quality_brief",
        title="Weekly Quality Brief",
        generated_at="2026-06-10T08:00:00+00:00",
        quality_score=82,
        quality_trend="DEGRADING",
        risk_level="HIGH",
        executive_summary="Payments require executive attention.",
        top_risks=["Jira blocker PAY-451 (Payments): timeout in staging"],
        top_recommendations=["Run Payments Regression Suite"],
        incident_count=2,
        critical_contract_count=1,
        broken_journey_count=1,
        jira_blocker_count=2,
        jira_blocker_keys=["PAY-451", "AUTH-228"],
    )
    result = build_report_delivery_preview(
        project_id="demo",
        report_type="WEEKLY_QUALITY_BRIEF",
        channel="email",
        recipient="cto@example.com",
    )
    assert "Jira Blockers: 2" in result.payload["body"]
    assert "• PAY-451" in result.payload["body"]
    assert "• AUTH-228" in result.payload["body"]


@patch("services.report_delivery_service._load_executive_preview")
def test_email_preview(mock_load):
    mock_load.return_value = _preview()
    result = build_report_delivery_preview(
        project_id="demo",
        report_type="WEEKLY_QUALITY_BRIEF",
        channel="email",
        recipient="cto@example.com",
    )
    assert result.channel == "email"
    assert result.report_type == "WEEKLY_QUALITY_BRIEF"
    assert "Weekly Quality Brief" in result.subject
    assert result.payload["subject"] == result.subject
    assert "Payments require executive attention." in result.payload["body"]
    assert result.payload["recipient"] == "cto@example.com"


@patch("services.report_delivery_service._load_executive_preview")
def test_slack_preview(mock_load):
    mock_load.return_value = _preview()
    result = build_report_delivery_preview(
        project_id="demo",
        report_type="EXECUTIVE_QUALITY",
        channel="slack",
    )
    assert result.channel == "slack"
    assert result.report_type == "EXECUTIVE_QUALITY"
    assert "blocks" in result.payload
    assert result.payload["text"]


@patch("services.report_delivery_service._load_executive_preview")
def test_teams_preview(mock_load):
    mock_load.return_value = _preview()
    result = build_report_delivery_preview(
        project_id="demo",
        report_type="RELEASE_READINESS",
        channel="teams",
    )
    assert result.channel == "teams"
    assert result.report_type == "RELEASE_READINESS"
    assert result.payload["@type"] == "MessageCard"
    assert result.payload["title"] == "Weekly Quality Brief"


@patch("services.report_delivery_service.integration_dispatcher")
@patch("services.report_delivery_service.build_report_delivery_preview")
def test_dispatcher_send(mock_preview, mock_dispatcher):
    from models.report_delivery_models import ReportDeliveryPreview

    mock_preview.return_value = ReportDeliveryPreview(
        channel="slack",
        report_type="WEEKLY_QUALITY_BRIEF",
        subject="Weekly Quality Brief — demo",
        summary="Payments require executive attention.",
        payload={"text": "Payments require executive attention.", "blocks": []},
    )
    mock_dispatcher.readiness.return_value = {
        "slack": {"ready": True, "enabled": True, "health": "ok"},
    }
    mock_dispatcher.send_alert.return_value = (True, {"message": "sent"})

    result = send_report_delivery(
        project_id="demo",
        report_type="WEEKLY_QUALITY_BRIEF",
        channel="slack",
        requires_user_approval=True,
    )
    assert result.success is True
    assert result.channel == "slack"
    assert result.delivered_at
    mock_dispatcher.send_alert.assert_called_once()


@patch("services.report_delivery_service.integration_dispatcher")
def test_send_requires_user_approval(mock_dispatcher):
    with pytest.raises(ValueError, match="requires_user_approval"):
        send_report_delivery(
            project_id="demo",
            report_type="INCIDENT_REVIEW",
            channel="email",
            recipients=["a@example.com"],
            requires_user_approval=False,
        )
    mock_dispatcher.send_alert.assert_not_called()


def test_invalid_channel():
    with pytest.raises(ValueError, match="Invalid channel"):
        build_report_delivery_preview(
            project_id="demo",
            report_type="WEEKLY_QUALITY_BRIEF",
            channel="jira",
        )


def test_invalid_report_type():
    with pytest.raises(ValueError, match="Invalid report_type"):
        build_report_delivery_preview(
            project_id="demo",
            report_type="UNKNOWN_REPORT",
            channel="email",
        )


@patch("services.report_delivery_service.integration_dispatcher")
def test_channel_is_ready(mock_dispatcher):
    mock_dispatcher.readiness.return_value = {
        "email": {"ready": False, "message": "EMAIL_ENABLED is false"},
    }
    ready, reason = channel_is_ready("email")
    assert ready is False
    assert "EMAIL_ENABLED" in reason
