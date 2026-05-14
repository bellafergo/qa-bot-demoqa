# tests/test_local_agent_cli.py
from __future__ import annotations

import json

import httpx
import pytest

from local_agent.client import AgentAuthError, VanyaAgentClient, build_dry_run_result_ref
from local_agent.config import AgentConfig, build_config
from local_agent.runner import run, run_cycle
from local_agent.security import token_log_label


def test_show_agent_version_exits_zero():
    from local_agent.cli import main

    with pytest.raises(SystemExit) as ei:
        main(["--show-agent-version"])
    assert ei.value.code == 0


def test_build_config_from_env(monkeypatch):
    monkeypatch.setenv("VANYA_CLOUD_URL", "https://cloud.example.com")
    monkeypatch.setenv("VANYA_AGENT_ID", "agent-1")
    monkeypatch.setenv("VANYA_AGENT_TOKEN", "vla_testtoken_secret_value")
    monkeypatch.setenv("VANYA_AGENT_POLL_INTERVAL", "15")
    monkeypatch.setenv("VANYA_AGENT_HTTP_TIMEOUT", "12")
    cfg = build_config(
        base_url=None,
        agent_id=None,
        agent_token=None,
        poll_interval_s=None,
        http_timeout_s=None,
        dry_run=False,
        once=False,
        agent_version=None,
    )
    assert cfg.base_url == "https://cloud.example.com"
    assert cfg.agent_id == "agent-1"
    assert cfg.agent_token == "vla_testtoken_secret_value"
    assert cfg.poll_interval_s == 15.0
    assert cfg.http_timeout_s == 12.0


def test_build_config_cli_overrides_env(monkeypatch):
    monkeypatch.setenv("VANYA_CLOUD_URL", "https://wrong.example.com")
    monkeypatch.setenv("VANYA_AGENT_ID", "wrong")
    monkeypatch.setenv("VANYA_AGENT_TOKEN", "vla_wrong_wrong_wrong")
    cfg = build_config(
        base_url="https://right.example.com",
        agent_id="right-id",
        agent_token="vla_right_right_right",
        poll_interval_s=7.0,
        http_timeout_s=20.0,
        dry_run=True,
        once=True,
        agent_version="custom/1",
    )
    assert cfg.base_url == "https://right.example.com"
    assert cfg.agent_id == "right-id"
    assert cfg.agent_token == "vla_right_right_right"
    assert cfg.poll_interval_s == 7.0
    assert cfg.dry_run is True
    assert cfg.once is True
    assert cfg.agent_version == "custom/1"


def test_token_log_label_never_includes_full_secret():
    secret = "vla_" + "x" * 40
    label = token_log_label(secret)
    assert secret not in label
    assert label.endswith(secret[-4:])
    assert "sha256:" in label


def test_heartbeat_request_shape_and_auth():
    captured: list[httpx.Request] = []

    def handler(request: httpx.Request) -> httpx.Response:
        captured.append(request)
        return httpx.Response(
            200,
            json={
                "agent_id": "a1",
                "project_id": "p1",
                "name": "n",
                "status": "online",
                "capabilities": ["browser_inspection"],
                "enabled": True,
                "token_fingerprint": "abc",
                "metadata": {},
                "created_at": "t",
                "updated_at": "t",
            },
        )

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    token = "vla_heartbeat_test_token_xx"
    c = VanyaAgentClient("https://api.example", "a1", token, client=http, timeout_s=5.0, max_retries=0)
    try:
        c.heartbeat(agent_version="test-agent/1")
    finally:
        c.close()
    assert len(captured) == 1
    req = captured[0]
    assert req.method == "POST"
    assert req.url.path == "/agent-api/a1/heartbeat"
    assert req.headers.get("authorization") == f"Bearer {token}"
    body = json.loads(req.content.decode("utf-8"))
    assert body.get("agent_version") == "test-agent/1"


def test_poll_no_jobs():
    def handler(request: httpx.Request) -> httpx.Response:
        if "/heartbeat" in str(request.url):
            return httpx.Response(200, json={"agent_id": "a1", "status": "online"})
        if "/poll" in str(request.url):
            return httpx.Response(200, json={"jobs": []})
        return httpx.Response(404, json={"detail": "unexpected"})

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    c = VanyaAgentClient("https://api.example", "a1", "vla_poll_no_jobs_token_xxx", client=http, max_retries=0)
    try:
        out = c.poll(limit=5)
    finally:
        c.close()
    assert out == {"jobs": []}


def test_poll_with_job_submits_result():
    calls: list[tuple[str, str]] = []

    def handler(request: httpx.Request) -> httpx.Response:
        u = str(request.url)
        calls.append((request.method, u))
        if "/heartbeat" in u:
            return httpx.Response(200, json={"agent_id": "a1", "status": "online"})
        if "/poll" in u:
            return httpx.Response(
                200,
                json={
                    "jobs": [
                        {
                            "job_id": "job-99",
                            "project_id": "p1",
                            "job_type": "browser_inspection",
                            "target_url": "https://example.com",
                            "payload": {},
                            "status": "queued",
                            "created_at": "t",
                        }
                    ]
                },
            )
        if "/jobs/job-99/result" in u:
            body = json.loads(request.content.decode("utf-8"))
            assert body["status"] == "succeeded"
            assert "result_ref" in body
            return httpx.Response(
                200,
                json={
                    "job_id": "job-99",
                    "project_id": "p1",
                    "job_type": "browser_inspection",
                    "target_url": "https://example.com",
                    "payload": {},
                    "status": "succeeded",
                    "created_at": "t",
                    "completed_at": "t2",
                    "result_ref": body["result_ref"],
                },
            )
        return httpx.Response(500, json={"detail": "unexpected"})

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    token = "vla_jobflow_token_secret_xxx"
    c = VanyaAgentClient("https://api.example", "a1", token, client=http, max_retries=0)
    try:
        cfg = AgentConfig(
            base_url="https://api.example",
            agent_id="a1",
            agent_token=token,
            poll_interval_s=10.0,
            http_timeout_s=10.0,
            dry_run=False,
            once=True,
            agent_version="vanya-local-agent/4b",
        )
        n = run_cycle(c, cfg)
        assert n == 1
        assert any("/result" in u for _, u in calls)
    finally:
        c.close()


def test_once_exits_zero():
    def handler(request: httpx.Request) -> httpx.Response:
        u = str(request.url)
        if "/heartbeat" in u:
            return httpx.Response(200, json={"agent_id": "a1", "status": "online"})
        if "/poll" in u:
            return httpx.Response(200, json={"jobs": []})
        return httpx.Response(404)

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    c = VanyaAgentClient("https://api.example", "a1", "vla_once_exit_token_secret", client=http, max_retries=0)
    cfg = AgentConfig(
        base_url="https://api.example",
        agent_id="a1",
        agent_token="vla_once_exit_token_secret",
        poll_interval_s=10.0,
        http_timeout_s=10.0,
        dry_run=False,
        once=True,
        agent_version="vanya-local-agent/4b",
    )
    try:
        code = run(cfg, client=c)
    finally:
        c.close()
    assert code == 0


def test_401_exit_code_2():
    def handler(request: httpx.Request) -> httpx.Response:
        return httpx.Response(401, json={"detail": "nope"})

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    c = VanyaAgentClient("https://api.example", "a1", "vla_bad_token________", client=http, max_retries=0)
    cfg = AgentConfig(
        base_url="https://api.example",
        agent_id="a1",
        agent_token="vla_bad_token________",
        poll_interval_s=10.0,
        http_timeout_s=10.0,
        dry_run=False,
        once=True,
        agent_version="vanya-local-agent/4b",
    )
    try:
        code = run(cfg, client=c)
    finally:
        c.close()
    assert code == 2


def test_403_exit_code_2():
    def handler(request: httpx.Request) -> httpx.Response:
        return httpx.Response(403, json={"detail": "agent is disabled"})

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    c = VanyaAgentClient("https://api.example", "a1", "vla_disabled_token____", client=http, max_retries=0)
    cfg = AgentConfig(
        base_url="https://api.example",
        agent_id="a1",
        agent_token="vla_disabled_token____",
        poll_interval_s=10.0,
        http_timeout_s=10.0,
        dry_run=False,
        once=True,
        agent_version="vanya-local-agent/4b",
    )
    try:
        code = run(cfg, client=c)
    finally:
        c.close()
    assert code == 2


def test_dry_run_skips_result_post():
    calls: list[str] = []

    def handler(request: httpx.Request) -> httpx.Response:
        calls.append(str(request.url))
        u = str(request.url)
        if "/heartbeat" in u:
            return httpx.Response(200, json={"agent_id": "a1", "status": "online"})
        if "/poll" in u:
            return httpx.Response(
                200,
                json={
                    "jobs": [
                        {
                            "job_id": "job-dry",
                            "project_id": "p1",
                            "job_type": "browser_inspection",
                            "target_url": "https://example.com",
                            "payload": {},
                            "status": "queued",
                            "created_at": "t",
                        }
                    ]
                },
            )
        return httpx.Response(500, json={"detail": "unexpected"})

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    token = "vla_dry_run_skip_result_token"
    c = VanyaAgentClient("https://api.example", "a1", token, client=http, max_retries=0)
    try:
        cfg = AgentConfig(
            base_url="https://api.example",
            agent_id="a1",
            agent_token=token,
            poll_interval_s=10.0,
            http_timeout_s=10.0,
            dry_run=True,
            once=True,
            agent_version="vanya-local-agent/4b",
        )
        run_cycle(c, cfg)
        assert not any("/jobs/" in u and "/result" in u for u in calls)
    finally:
        c.close()


def test_log_line_uses_redacted_token_only():
    secret = "vla_" + "Q" * 36
    cfg = build_config(
        base_url="https://x.com",
        agent_id="id1",
        agent_token=secret,
        poll_interval_s=10.0,
        http_timeout_s=10.0,
        dry_run=False,
        once=True,
        agent_version=None,
    )
    line = f"starting token={cfg.token_log_label()}"
    assert secret not in line
    assert line.endswith(secret[-4:])
    s = build_dry_run_result_ref()
    d = json.loads(s)
    assert d["message"] == "local agent dry-run result"
    assert d["artifacts"] == []


def test_agent_auth_error_message():
    def handler(request: httpx.Request) -> httpx.Response:
        return httpx.Response(401, json={"detail": "x"})

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    c = VanyaAgentClient("https://api.example", "a1", "vla_auth_err_token____", client=http, max_retries=0)
    try:
        with pytest.raises(AgentAuthError) as ei:
            c.heartbeat(agent_version="v")
        assert "401" in str(ei.value)
    finally:
        c.close()
