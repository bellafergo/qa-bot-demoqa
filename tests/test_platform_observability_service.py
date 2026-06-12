# tests/test_platform_observability_service.py
"""OBS-02A — platform self observability service tests."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.connector import ConnectorSummary
from services import audit_event_service as audit_svc
from services.platform_observability_service import build_platform_observability_report


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


def _record_auth_events() -> None:
    audit_svc.record_event(
        event_type="SSO_PROVIDER_VALIDATED",
        resource_type="SECURITY",
        resource_id="google",
        action="validate",
        result="SUCCESS",
    )
    audit_svc.record_event(
        event_type="SSO_LOGIN_URL_GENERATED",
        resource_type="SECURITY",
        resource_id="google",
        action="login_url",
        result="SUCCESS",
    )


def _record_report_delivery_events() -> None:
    audit_svc.record_event(
        event_type="REPORT_SENT",
        resource_type="REPORTS",
        resource_id="demo:executive_quality",
        action="send",
        result="SUCCESS",
    )
    audit_svc.record_event(
        event_type="REPORT_SENT",
        resource_type="REPORTS",
        resource_id="demo:release_readiness",
        action="send",
        result="FAILURE",
    )


def _record_incident_events(*, started: int = 2, completed: int = 1) -> None:
    for _ in range(started):
        audit_svc.record_event(
            event_type="INCIDENT_INVESTIGATION_STARTED",
            resource_type="INCIDENTS",
            resource_id="inc-1",
            action="investigate",
            result="SUCCESS",
        )
    for _ in range(completed):
        audit_svc.record_event(
            event_type="INCIDENT_INVESTIGATION_COMPLETED",
            resource_type="INCIDENTS",
            resource_id="inc-1",
            action="investigate",
            result="SUCCESS",
        )


@patch("services.platform_observability_service.integration_service")
def test_authentication_health_from_audit_events(mock_integration_service):
    mock_integration_service.list_connectors.return_value = []
    _record_auth_events()

    report = build_platform_observability_report()

    assert report.authentication_health.status in {"HEALTHY", "DEGRADED"}
    assert report.authentication_health.metrics
    assert any(metric.metric_name == "sso_validations" for metric in report.authentication_health.metrics)


@patch("services.platform_observability_service.integration_service")
def test_integration_health_counts_connector_status(mock_integration_service):
    mock_integration_service.list_connectors.return_value = [
        ConnectorSummary(
            connector_id="github",
            connector_name="GitHub",
            description="GitHub connector",
            enabled=True,
            health="unreachable",
            supported_actions=[],
        ),
        ConnectorSummary(
            connector_id="qmetry",
            connector_name="QMetry",
            description="QMetry connector",
            enabled=True,
            health="degraded",
            supported_actions=[],
        ),
        ConnectorSummary(
            connector_id="slack",
            connector_name="Slack",
            description="Slack connector",
            enabled=True,
            health="ok",
            supported_actions=[],
        ),
    ]
    audit_svc.record_event(
        event_type="INTEGRATION_VALIDATED",
        resource_type="INTEGRATIONS",
        resource_id="qmetry",
        action="health_check",
        result="FAILURE",
        metadata={"health": "degraded"},
    )

    report = build_platform_observability_report()

    assert report.integration_health.status in {"DEGRADED", "UNHEALTHY"}
    assert report.integration_health.integration_summary is not None
    assert report.integration_health.integration_summary.healthy == 1
    assert report.integration_health.integration_summary.degraded == 1
    assert report.integration_health.integration_summary.disconnected == 1
    assert any("GitHub" in risk for risk in report.top_platform_risks)
    assert any("QMetry" in risk for risk in report.top_platform_risks)


@patch("services.platform_observability_service.integration_service")
def test_report_delivery_health_detects_failures(mock_integration_service):
    mock_integration_service.list_connectors.return_value = []
    _record_report_delivery_events()

    report = build_platform_observability_report()

    assert report.report_delivery_health.status == "DEGRADED"
    assert "Report delivery failures observed" in report.top_platform_risks


@patch("services.platform_observability_service.integration_service")
def test_incident_investigation_health_pending(mock_integration_service):
    mock_integration_service.list_connectors.return_value = []
    _record_incident_events(started=2, completed=1)

    report = build_platform_observability_report()

    assert report.incident_investigation_health.status == "DEGRADED"
    assert "1" in report.incident_investigation_health.metrics[1].value


@patch("services.platform_observability_service.integration_service")
def test_executive_summary_reflects_overall_health(mock_integration_service):
    mock_integration_service.list_connectors.return_value = []
    _record_auth_events()
    _record_report_delivery_events()
    _record_incident_events(started=2, completed=1)

    report = build_platform_observability_report()

    assert report.executive_summary
    assert report.generated_at
    assert report.top_platform_risks
