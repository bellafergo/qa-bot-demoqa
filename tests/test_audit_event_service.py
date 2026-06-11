# tests/test_audit_event_service.py
"""SEC-01E — centralized audit event tests."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from services import audit_event_service as svc
from services.sso_service import validate_provider_config


@pytest.fixture(autouse=True)
def isolated_sso_config(tmp_path, monkeypatch):
    from services import sso_service as sso_svc

    config_file = tmp_path / "sso_config.json"
    monkeypatch.setattr(sso_svc, "_CONFIG_PATH", config_file)
    with sso_svc._LOCK:
        sso_svc._CONFIGS.clear()
    yield
    with sso_svc._LOCK:
        sso_svc._CONFIGS.clear()


def test_record_and_list_events():
    event = svc.record_event(
        event_type="REPORT_PREVIEWED",
        resource_type="REPORTS",
        resource_id="demo:executive_quality",
        action="preview",
        result="SUCCESS",
        user_id="user-1",
        user_email="admin@example.com",
        metadata={"channel": "email"},
    )
    assert event.event_id
    assert event.user_id == "user-1"

    listed = svc.list_events(event_type="REPORT_PREVIEWED")
    assert listed.total == 1
    assert listed.events[0].event_type == "REPORT_PREVIEWED"
    assert listed.events[0].metadata["channel"] == "email"


def test_list_events_by_resource():
    svc.record_event(
        event_type="INTEGRATION_CONFIG_UPDATED",
        resource_type="INTEGRATIONS",
        resource_id="slack",
        action="update_config",
        result="SUCCESS",
    )
    listed = svc.list_events_by_resource(resource_type="INTEGRATIONS", resource_id="slack")
    assert listed.total == 1
    assert listed.events[0].resource_id == "slack"


def test_build_audit_summary():
    svc.record_event(
        event_type="REPORT_SENT",
        resource_type="REPORTS",
        resource_id="demo:release_readiness",
        action="send",
        result="SUCCESS",
    )
    svc.record_event(
        event_type="REPORT_PREVIEWED",
        resource_type="REPORTS",
        resource_id="demo:release_readiness",
        action="preview",
        result="SUCCESS",
    )

    summary = svc.build_audit_summary()
    assert summary.total_events == 2
    assert summary.event_types["REPORT_SENT"] == 1
    assert summary.event_types["REPORT_PREVIEWED"] == 1
    assert summary.latest_event is not None


def test_sso_instrumentation_records_validation():
    validate_provider_config(
        provider="GOOGLE",
        client_id="google-client-id.apps.googleusercontent.com",
    )
    events = svc.list_events(event_type="SSO_PROVIDER_VALIDATED")
    assert events.total == 1
    assert events.events[0].result == "SUCCESS"


@patch("services.report_delivery_service._load_executive_preview")
def test_report_preview_instrumentation(mock_preview):
    from models.scheduled_report_models import ExecutiveReportPreview
    from services.report_delivery_service import build_report_delivery_preview

    mock_preview.return_value = ExecutiveReportPreview(
        preview_id="preview-1",
        title="Executive Quality",
        generated_at="2026-06-11T10:00:00+00:00",
        executive_summary="Summary",
        top_risks=[],
        top_recommendations=[],
    )

    build_report_delivery_preview(
        project_id="demo",
        report_type="executive_quality",
        channel="email",
        recipient="cto@example.com",
    )
    events = svc.list_events(event_type="REPORT_PREVIEWED")
    assert events.total == 1
    assert events.events[0].resource_type == "REPORTS"


def test_filtering_by_user_id():
    svc.record_event(
        event_type="RELEASE_READINESS_VIEWED",
        resource_type="RELEASES",
        resource_id="demo",
        action="view",
        user_id="alice",
    )
    svc.record_event(
        event_type="RELEASE_READINESS_VIEWED",
        resource_type="RELEASES",
        resource_id="demo",
        action="view",
        user_id="bob",
    )
    filtered = svc.list_events(user_id="alice")
    assert filtered.total == 1
    assert filtered.events[0].user_id == "alice"
