# tests/test_integrations.py
"""
Integration connector framework tests.

Covers:
  - ConnectorRegistry — all 4 connectors present, ids unique
  - BaseConnector contract — each connector implements required methods
  - ConnectorConfig model — defaults, secret masking
  - Per-connector validate_config logic
  - Per-connector health_check determinism
  - IntegrationService — list, enable/disable, config update, health check
  - Config persistence round-trip (in-memory)
  - API routes via FastAPI test client

No real network calls are made — all health checks are stubs.
"""
from __future__ import annotations

import pytest
from unittest.mock import patch


# ── Registry tests ─────────────────────────────────────────────────────────────

class TestConnectorRegistry:
    def test_all_four_connectors_present(self):
        from connectors.registry import registry
        ids = registry.ids()
        assert "jira"    in ids
        assert "github"  in ids
        assert "slack"   in ids
        assert "qmetry"  in ids

    def test_no_duplicate_ids(self):
        from connectors.registry import registry
        ids = registry.ids()
        assert len(ids) == len(set(ids))

    def test_get_known_connector(self):
        from connectors.registry import registry
        c = registry.get("jira")
        assert c is not None
        assert c.connector_id == "jira"

    def test_get_unknown_connector_returns_none(self):
        from connectors.registry import registry
        assert registry.get("nonexistent") is None

    def test_all_returns_list(self):
        from connectors.registry import registry
        assert isinstance(registry.all(), list)
        assert len(registry.all()) == 4


# ── BaseConnector contract ─────────────────────────────────────────────────────

class TestBaseConnectorContract:
    def _all_connectors(self):
        from connectors.registry import registry
        return registry.all()

    def test_all_have_connector_id(self):
        for c in self._all_connectors():
            assert isinstance(c.connector_id, str) and c.connector_id

    def test_all_have_connector_name(self):
        for c in self._all_connectors():
            assert isinstance(c.connector_name, str) and c.connector_name

    def test_all_have_description(self):
        for c in self._all_connectors():
            assert isinstance(c.description, str) and c.description

    def test_all_supported_actions_return_nonempty_list(self):
        from models.connector import ConnectorConfig
        for c in self._all_connectors():
            actions = c.supported_actions()
            assert isinstance(actions, list)
            assert len(actions) > 0

    def test_all_get_status_returns_connector_status(self):
        from models.connector import ConnectorConfig, ConnectorStatus
        for c in self._all_connectors():
            cfg = ConnectorConfig(connector_id=c.connector_id)
            status = c.get_status(cfg)
            assert isinstance(status, ConnectorStatus)
            assert status.connector_id == c.connector_id

    def test_all_to_summary_returns_connector_summary(self):
        from models.connector import ConnectorConfig, ConnectorSummary
        for c in self._all_connectors():
            cfg = ConnectorConfig(connector_id=c.connector_id)
            summary = c.to_summary(cfg)
            assert isinstance(summary, ConnectorSummary)
            assert summary.connector_id == c.connector_id


# ── ConnectorConfig model ──────────────────────────────────────────────────────

class TestConnectorConfigModel:
    def test_defaults(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="jira")
        assert cfg.enabled is False
        assert cfg.base_url is None
        assert cfg.token_present is False
        assert cfg.api_key_present is False

    def test_secrets_not_in_model_fields(self):
        from models.connector import ConnectorConfig
        # ConnectorConfig must NOT have a 'token' or 'api_key' field
        fields = ConnectorConfig.model_fields
        assert "token" not in fields
        assert "api_key" not in fields

    def test_config_update_has_write_only_token(self):
        from models.connector import ConnectorConfigUpdate
        upd = ConnectorConfigUpdate(token="xoxb-secret")
        assert upd.token == "xoxb-secret"   # present in update object
        # But the ConnectorConfig model (the stored form) must not have it
        from models.connector import ConnectorConfig
        assert "token" not in ConnectorConfig.model_fields


# ── Jira connector ─────────────────────────────────────────────────────────────

class TestJiraConnector:
    def _connector(self):
        from connectors.jira_connector import JiraConnector
        return JiraConnector()

    def _full_config(self):
        from models.connector import ConnectorConfig
        return ConnectorConfig(
            connector_id="jira",
            enabled=True,
            base_url="https://example.atlassian.net",
            token_present=True,
            workspace="user@example.com",
        )

    def test_validate_config_missing_base_url(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="jira", token_present=True, workspace="u@e.com")
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "base_url" in msg

    def test_validate_config_missing_token(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="jira", base_url="https://x.atlassian.net", workspace="u@e.com")
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "token" in msg.lower()

    def test_validate_config_missing_workspace(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="jira", base_url="https://x.atlassian.net", token_present=True)
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "workspace" in msg.lower()

    def test_validate_config_valid(self):
        ok, msg = self._connector().validate_config(self._full_config())
        assert ok is True

    def test_health_check_disabled(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="jira")
        health, _ = self._connector().health_check(cfg)
        assert health == "unconfigured"

    def test_health_check_enabled_full_config(self):
        health, msg = self._connector().health_check(self._full_config())
        assert health == "ok"
        assert "jira" in msg.lower()

    def test_list_projects_stub(self):
        projects = self._connector().list_projects_stub()
        assert isinstance(projects, list)
        assert all("key" in p for p in projects)

    def test_push_result_stub(self):
        result = self._connector().push_result_stub("run-123", "pass")
        assert result["pushed"] is True
        assert result["stub"] is True
        assert "run-123" in result["run_id"]


# ── GitHub connector ───────────────────────────────────────────────────────────

class TestGitHubConnector:
    def _connector(self):
        from connectors.github_connector import GitHubConnector
        return GitHubConnector()

    def _full_config(self):
        from models.connector import ConnectorConfig
        return ConnectorConfig(
            connector_id="github",
            enabled=True,
            token_present=True,
            workspace="myorg",
        )

    def test_validate_missing_token(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="github", workspace="myorg")
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "token" in msg.lower()

    def test_validate_missing_workspace(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="github", token_present=True)
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "workspace" in msg.lower()

    def test_validate_valid(self):
        ok, _ = self._connector().validate_config(self._full_config())
        assert ok is True

    def test_health_check_ok(self):
        health, msg = self._connector().health_check(self._full_config())
        assert health == "ok"
        assert "myorg" in msg

    def test_build_status_check_payload(self):
        payload = self._connector().build_status_check_payload(
            state="success",
            description="All tests passed",
        )
        assert payload["state"] == "success"
        assert payload["context"] == "vanya/qa-bot"

    def test_post_pr_comment_stub(self):
        result = self._connector().post_pr_comment_stub(42, "Tests passed!")
        assert result["posted"] is True
        assert result["pr_number"] == 42


# ── Slack connector ────────────────────────────────────────────────────────────

class TestSlackConnector:
    def _connector(self):
        from connectors.slack_connector import SlackConnector
        return SlackConnector()

    def _full_config(self):
        from models.connector import ConnectorConfig
        return ConnectorConfig(
            connector_id="slack",
            enabled=True,
            token_present=True,
            channel="#qa-alerts",
        )

    def test_validate_missing_token(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="slack", channel="#qa")
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False

    def test_validate_missing_channel(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="slack", token_present=True)
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "channel" in msg.lower()

    def test_validate_valid(self):
        ok, _ = self._connector().validate_config(self._full_config())
        assert ok is True

    def test_health_check_ok(self):
        health, msg = self._connector().health_check(self._full_config())
        assert health == "ok"
        assert "#qa-alerts" in msg

    def test_build_notification_payload(self):
        payload = self._connector().build_notification_payload(
            channel="#qa-alerts",
            text="Tests passed",
            status="pass",
        )
        assert payload["channel"] == "#qa-alerts"
        assert "attachments" in payload

    def test_send_notification_stub(self):
        result = self._connector().send_notification_stub("#qa-alerts", "hello", "pass")
        assert result["sent"] is True
        assert result["stub"] is True


# ── QMetry connector ──────────────────────────────────────────────────────────

class TestQMetryConnector:
    def _connector(self):
        from connectors.qmetry_connector import QMetryConnector
        return QMetryConnector()

    def _full_config(self):
        from models.connector import ConnectorConfig
        return ConnectorConfig(
            connector_id="qmetry",
            enabled=True,
            api_key_present=True,
            base_url="https://jira.example.com",
        )

    def test_validate_missing_api_key(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="qmetry", base_url="https://x.com")
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "api key" in msg.lower()

    def test_validate_missing_base_url(self):
        from models.connector import ConnectorConfig
        cfg = ConnectorConfig(connector_id="qmetry", api_key_present=True)
        ok, msg = self._connector().validate_config(cfg)
        assert ok is False
        assert "base_url" in msg.lower()

    def test_validate_valid(self):
        ok, _ = self._connector().validate_config(self._full_config())
        assert ok is True

    def test_health_check_ok(self):
        health, msg = self._connector().health_check(self._full_config())
        assert health == "ok"

    def test_sync_test_cases_stub(self):
        result = self._connector().sync_test_cases_stub(["TC-001", "TC-002"])
        assert result["synced"] == 2

    def test_push_execution_result_stub(self):
        result = self._connector().push_execution_result_stub("run-1", "TC-001", "pass")
        assert result["qmetry_status"] == "PASS"
        assert result["stub"] is True

    def test_status_mapping_fail(self):
        from connectors.qmetry_connector import _map_status
        assert _map_status("fail") == "FAIL"

    def test_fetch_test_cycles_stub(self):
        cycles = self._connector().fetch_test_cycles_stub()
        assert isinstance(cycles, list)
        assert len(cycles) > 0


# ── IntegrationService ─────────────────────────────────────────────────────────

def _fresh_service():
    """Return a fresh IntegrationService with clean in-memory state."""
    import services.integration_service as svc_module
    svc_module._CONFIGS.clear()
    svc_module._LAST_HEALTH.clear()
    from services.integration_service import IntegrationService
    return IntegrationService()


class TestIntegrationService:
    def test_list_connectors_returns_all_four(self):
        svc = _fresh_service()
        summaries = svc.list_connectors()
        ids = [s.connector_id for s in summaries]
        assert "jira" in ids
        assert "github" in ids
        assert "slack" in ids
        assert "qmetry" in ids

    def test_get_connector_status_known(self):
        svc = _fresh_service()
        status = svc.get_connector_status("jira")
        assert status.connector_id == "jira"

    def test_get_connector_status_unknown_raises(self):
        svc = _fresh_service()
        with pytest.raises(KeyError):
            svc.get_connector_status("unknown_tool")

    def test_enable_sets_enabled_true(self):
        svc = _fresh_service()
        cfg = svc.enable("slack")
        assert cfg.enabled is True

    def test_disable_sets_enabled_false(self):
        svc = _fresh_service()
        svc.enable("slack")
        cfg = svc.disable("slack")
        assert cfg.enabled is False

    def test_update_config_masks_token(self):
        from models.connector import ConnectorConfigUpdate
        svc = _fresh_service()
        update = ConnectorConfigUpdate(
            token="super-secret-token",
            base_url="https://example.atlassian.net",
            workspace="myorg",
        )
        cfg = svc.update_config("jira", update)
        # Token must never appear in stored config
        assert cfg.token_present is True
        cfg_dict = cfg.model_dump()
        assert "super-secret-token" not in str(cfg_dict)
        assert "token" not in cfg_dict  # no 'token' key at all

    def test_update_config_masks_api_key(self):
        from models.connector import ConnectorConfigUpdate
        svc = _fresh_service()
        update = ConnectorConfigUpdate(api_key="qmetry-secret-key")
        cfg = svc.update_config("qmetry", update)
        assert cfg.api_key_present is True
        cfg_dict = cfg.model_dump()
        assert "qmetry-secret-key" not in str(cfg_dict)

    def test_update_config_base_url_persisted(self):
        from models.connector import ConnectorConfigUpdate
        svc = _fresh_service()
        svc.update_config("jira", ConnectorConfigUpdate(base_url="https://myorg.atlassian.net"))
        cfg = svc.get_config("jira")
        assert cfg.base_url == "https://myorg.atlassian.net"

    def test_health_check_returns_status(self):
        from models.connector import ConnectorStatus
        svc = _fresh_service()
        status = svc.run_health_check("jira")
        assert isinstance(status, ConnectorStatus)
        assert status.connector_id == "jira"

    def test_health_check_disabled_connector_unconfigured(self):
        svc = _fresh_service()
        # Default config has enabled=False
        status = svc.run_health_check("github")
        assert status.health == "unconfigured"

    def test_health_check_enabled_and_configured(self):
        from models.connector import ConnectorConfigUpdate
        svc = _fresh_service()
        svc.update_config("slack", ConnectorConfigUpdate(
            enabled=True, token="xoxb-test", channel="#qa"
        ))
        status = svc.run_health_check("slack")
        assert status.health == "ok"

    def test_get_actions_known_connector(self):
        svc = _fresh_service()
        actions = svc.get_actions("jira")
        assert isinstance(actions, list)
        assert len(actions) > 0

    def test_get_actions_unknown_raises(self):
        svc = _fresh_service()
        with pytest.raises(KeyError):
            svc.get_actions("unknown_tool")

    def test_enable_unknown_raises(self):
        svc = _fresh_service()
        with pytest.raises(KeyError):
            svc.enable("unknown_tool")


# ── API routes (FastAPI test client) ──────────────────────────────────────────

class TestIntegrationRoutes:
    def _client(self):
        from fastapi.testclient import TestClient
        from app import app
        return TestClient(app)

    def test_list_integrations_200(self):
        res = self._client().get("/integrations")
        assert res.status_code == 200
        data = res.json()
        assert isinstance(data, list)
        ids = [d["connector_id"] for d in data]
        assert "jira" in ids
        assert "slack" in ids

    def test_get_integration_known(self):
        res = self._client().get("/integrations/jira")
        assert res.status_code == 200
        data = res.json()
        assert data["connector_id"] == "jira"

    def test_get_integration_unknown_404(self):
        res = self._client().get("/integrations/nonexistent")
        assert res.status_code == 404

    def test_health_check_route(self):
        res = self._client().post("/integrations/github/health-check")
        assert res.status_code == 200
        data = res.json()
        assert "health" in data
        assert "connector_id" in data

    def test_enable_route(self):
        res = self._client().post("/integrations/slack/enable")
        assert res.status_code == 200
        data = res.json()
        assert data["enabled"] is True

    def test_disable_route(self):
        res = self._client().post("/integrations/slack/disable")
        assert res.status_code == 200
        data = res.json()
        assert data["enabled"] is False

    def test_config_update_route_no_secrets_in_response(self):
        res = self._client().post("/integrations/jira/config", json={
            "base_url":  "https://test.atlassian.net",
            "token":     "secret-value",
            "workspace": "testorg",
        })
        assert res.status_code == 200
        body = res.text
        assert "secret-value" not in body
        data = res.json()
        assert data["token_present"] is True
        assert "token" not in data

    def test_get_actions_route(self):
        res = self._client().get("/integrations/qmetry/actions")
        assert res.status_code == 200
        data = res.json()
        assert "actions" in data
        assert isinstance(data["actions"], list)

    def test_actions_unknown_connector_404(self):
        res = self._client().get("/integrations/notaconnector/actions")
        assert res.status_code == 404

    def test_health_check_unknown_connector_404(self):
        res = self._client().post("/integrations/bogus/health-check")
        assert res.status_code == 404
