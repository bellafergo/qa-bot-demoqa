# tests/test_alerting.py
"""Post-run Slack auto-alert (services/alerting.py)."""
from __future__ import annotations

import asyncio
from datetime import datetime, timezone
from unittest.mock import patch

from models.connector import ConnectorConfig


def make_test_run(**kwargs):
    from models import test_run as test_run_module

    defaults = dict(
        run_id="run-1",
        test_case_id="TC-1",
        test_name="Demo",
        executed_at=datetime.now(timezone.utc),
        environment="default",
        status="fail",
        duration_ms=100,
        evidence_url="https://ex/ev",
        logs=[],
        steps_result=[],
        meta={},
    )
    defaults.update(kwargs)
    return test_run_module.TestRun(**defaults)


def test_send_skips_when_pass():
    from services.alerting import _send_slack_alert_if_configured_for_test_run

    with patch("connectors.registry.registry.get") as reg_get:
        _send_slack_alert_if_configured_for_test_run(make_test_run(status="pass"))
        reg_get.assert_not_called()


def test_send_skips_when_slack_disabled():
    from connectors.slack_connector import SlackConnector
    from services.alerting import _send_slack_alert_if_configured_for_test_run

    cfg = ConnectorConfig(connector_id="slack", enabled=False, channel="#q", token_present=True)
    with patch("services.integration_service.integration_service.get_config", return_value=cfg), patch(
        "connectors.registry.registry.get", return_value=SlackConnector()
    ), patch.object(SlackConnector, "send_alert") as mock_send:
        _send_slack_alert_if_configured_for_test_run(make_test_run())
        mock_send.assert_not_called()


def test_send_calls_slack_when_enabled_and_valid():
    from connectors.slack_connector import SlackConnector
    from services.alerting import _send_slack_alert_if_configured_for_test_run

    cfg = ConnectorConfig(connector_id="slack", enabled=True, channel="#qa", token_present=True)
    slack = SlackConnector()
    with patch("services.integration_service.integration_service.get_config", return_value=cfg), patch(
        "connectors.registry.registry.get", return_value=slack
    ), patch.object(SlackConnector, "send_alert", return_value=(True, "ok")) as mock_send:
        _send_slack_alert_if_configured_for_test_run(
            make_test_run(
                test_name="Verificar opción Login en Berel",
                status="fail",
                duration_ms=1842,
                meta={"runner_reason": "No se encontró el botón de login"},
                evidence_url="https://example.com/evidence/123",
            )
        )
        mock_send.assert_called_once()
        kwargs = mock_send.call_args.kwargs
        assert "Verificar opción Login" in kwargs.get("text", "")
        assert kwargs.get("run_id") == "run-1"
        assert kwargs.get("evidence_url") == "https://example.com/evidence/123"


def test_accepts_failed_alias_string_on_generic_run():
    """Runner payloads may use 'failed'; TestRun model uses 'fail'. Accept both."""
    from types import SimpleNamespace

    from connectors.slack_connector import SlackConnector
    from services.alerting import _send_slack_alert_if_configured_for_test_run

    cfg = ConnectorConfig(connector_id="slack", enabled=True, channel="#qa", token_present=True)
    slack = SlackConnector()
    run = SimpleNamespace(
        status="failed",
        run_id="r1",
        test_case_id="TC",
        test_name="T",
        duration_ms=1,
        evidence_url=None,
        logs=[],
        steps_result=[],
        meta={},
    )
    with patch("services.integration_service.integration_service.get_config", return_value=cfg), patch(
        "connectors.registry.registry.get", return_value=slack
    ), patch.object(SlackConnector, "send_alert", return_value=(True, "ok")) as mock_send:
        _send_slack_alert_if_configured_for_test_run(run)
        mock_send.assert_called_once()


def test_async_send_alert_if_configured():
    from services import alerting

    async def _go() -> None:
        with patch.object(alerting, "_send_slack_alert_if_configured_for_test_run") as mock_sync:
            await alerting.send_alert_if_configured(make_test_run())
            mock_sync.assert_called_once()

    asyncio.run(_go())
