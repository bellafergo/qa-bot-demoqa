# tests/test_local_agent_browser.py
"""Phase 4C — local browser job validation and lightweight result packing (mocked Playwright)."""
from __future__ import annotations

import json
from unittest.mock import patch

import pytest

from local_agent.browser_job import execute_browser_inspection_job
from local_agent.config import AgentConfig
from local_agent.result_packager import pack_browser_inspection_result_ref
from local_agent.runner import run_cycle
from local_agent.url_guard import LocalAgentURLRejected, validate_job_navigation_url
from models.browser_inspection_models import BrowserInspectionResult


def _cfg(**kwargs) -> AgentConfig:
    base = dict(
        base_url="https://x.com",
        agent_id="a",
        agent_token="vla_token_minimum_len_ok________",
        poll_interval_s=10.0,
        http_timeout_s=30.0,
        dry_run=False,
        once=True,
        agent_version="vanya-local-agent/4c",
        browser_enabled=True,
        allow_localhost=False,
        allow_private_ips=False,
        browser_headless=True,
        browser_timeout_ms_default=15_000,
    )
    base.update(kwargs)
    return AgentConfig(**base)


def test_unsupported_job_type_fails():
    job = {"job_id": "j1", "job_type": "future_type", "target_url": "https://a.com", "payload": {}}
    st, ref, err = execute_browser_inspection_job(job, _cfg(), agent_capabilities=[])
    assert st == "failed"
    assert ref is None
    assert "unsupported" in (err or "").lower()


def test_browser_inspection_disabled_fails():
    job = {
        "job_id": "j1",
        "job_type": "browser_inspection",
        "target_url": "https://a.com",
        "payload": {"url": "https://a.com", "execution_mode": "local_agent"},
    }
    st, ref, err = execute_browser_inspection_job(job, _cfg(browser_enabled=False), agent_capabilities=[])
    assert st == "failed"
    assert "disabled" in (err or "").lower()


def test_localhost_blocked_without_flag():
    with pytest.raises(LocalAgentURLRejected):
        validate_job_navigation_url(
            "http://localhost:3000/",
            allow_localhost=False,
            allow_private_ips=False,
            agent_capabilities=frozenset(),
        )


def test_localhost_allowed_with_flag():
    u = validate_job_navigation_url(
        "http://localhost:3000/",
        allow_localhost=True,
        allow_private_ips=False,
        agent_capabilities=frozenset(),
    )
    assert u.startswith("http://localhost")


def test_localhost_allowed_with_capability():
    u = validate_job_navigation_url(
        "http://127.0.0.1:8080/x",
        allow_localhost=False,
        allow_private_ips=False,
        agent_capabilities=frozenset({"localhost_access"}),
    )
    assert "127.0.0.1" in u


def test_private_ip_blocked_without_flag():
    with pytest.raises(LocalAgentURLRejected):
        validate_job_navigation_url(
            "http://192.168.1.10/",
            allow_localhost=False,
            allow_private_ips=False,
            agent_capabilities=frozenset(),
        )


def test_private_ip_allowed_with_env_flag():
    u = validate_job_navigation_url(
        "http://10.0.0.1/",
        allow_localhost=False,
        allow_private_ips=True,
        agent_capabilities=frozenset(),
    )
    assert u.startswith("http://10.0.0.1")


def test_wrong_execution_mode_when_browser_enabled():
    job = {
        "job_id": "j1",
        "job_type": "browser_inspection",
        "target_url": "https://a.com",
        "payload": {"url": "https://a.com", "execution_mode": "cloud"},
    }
    st, ref, err = execute_browser_inspection_job(job, _cfg(browser_enabled=True), agent_capabilities=[])
    assert st == "failed"
    assert "local_agent" in (err or "").lower()


def test_file_scheme_blocked():
    with pytest.raises(LocalAgentURLRejected):
        validate_job_navigation_url(
            "file:///etc/passwd",
            allow_localhost=True,
            allow_private_ips=True,
            agent_capabilities=frozenset(),
        )


def test_pack_result_has_no_screenshot_b64():
    r = BrowserInspectionResult(
        inspection_id="i1",
        url="https://ex.com",
        final_url="https://ex.com/",
        title="T",
        status_code=200,
        headings=[{"tag": "h1", "text": "Hi"}],
        inspection_succeeded=True,
    )
    raw = {"screenshot_b64": "AAAABBBBCCCC"}
    ref = pack_browser_inspection_result_ref(r, raw_runner=raw)
    assert "screenshot_b64" not in ref
    d = json.loads(ref)
    assert d.get("screenshot_present") is True
    assert d.get("screenshot_sha256_24")


def test_pack_result_lightweight_length():
    r = BrowserInspectionResult(
        inspection_id="i2",
        url="https://ex.com",
        final_url="https://ex.com/",
        title="T",
        headings=[{"tag": "h1", "text": "x"}],
        links=[{"text": "a", "href": "https://ex.com/a"}],
        inspection_succeeded=True,
    )
    ref = pack_browser_inspection_result_ref(r, raw_runner={})
    assert len(ref) <= 512


@patch("local_agent.browser_job.run_local_browser_inspection")
def test_browser_job_uses_mocked_playwright(mock_run):
    mock_run.return_value = (
        BrowserInspectionResult(
            inspection_id="rid",
            url="https://example.com",
            final_url="https://example.com/",
            title="OK",
            status_code=200,
            inspection_succeeded=True,
        ),
        {"screenshot_b64": None},
    )
    job = {
        "job_id": "j99",
        "job_type": "browser_inspection",
        "target_url": "https://example.com",
        "payload": {
            "url": "https://example.com",
            "timeout_ms": 5000,
            "execution_mode": "local_agent",
        },
    }
    st, ref, err = execute_browser_inspection_job(job, _cfg(), agent_capabilities=["localhost_access"])
    assert st == "succeeded"
    assert err is None
    assert ref and "vanya_local_agent_browser_inspection" in ref
    mock_run.assert_called_once()


def test_cli_once_browser_job_mocked_playwright():
    import httpx

    from local_agent.client import VanyaAgentClient
    from local_agent.config import build_config
    from local_agent.runner import run_cycle

    calls: list[str] = []

    def handler(request: httpx.Request) -> httpx.Response:
        u = str(request.url)
        calls.append(u)
        if "/heartbeat" in u:
            return httpx.Response(200, json={"agent_id": "a1", "status": "online"})
        if "/poll" in u:
            return httpx.Response(
                200,
                json={
                    "jobs": [
                        {
                            "job_id": "jb1",
                            "project_id": "p1",
                            "job_type": "browser_inspection",
                            "target_url": "https://example.com",
                            "payload": {
                                "url": "https://example.com",
                                "execution_mode": "local_agent",
                                "timeout_ms": 8000,
                            },
                            "status": "queued",
                            "created_at": "t",
                        }
                    ],
                    "agent_capabilities": ["browser_inspection"],
                },
            )
        if "/jobs/jb1/result" in u:
            return httpx.Response(
                200,
                json={
                    "job_id": "jb1",
                    "project_id": "p1",
                    "job_type": "browser_inspection",
                    "target_url": "https://example.com",
                    "payload": {},
                    "status": "succeeded",
                    "created_at": "t",
                    "completed_at": "t2",
                },
            )
        return httpx.Response(404)

    transport = httpx.MockTransport(handler)
    http = httpx.Client(transport=transport)
    token = "vla_cli_browser_token_________"
    c = VanyaAgentClient("https://api.example", "a1", token, client=http, max_retries=0)
    cfg = build_config(
        base_url="https://api.example",
        agent_id="a1",
        agent_token=token,
        poll_interval_s=10.0,
        http_timeout_s=30.0,
        dry_run=False,
        once=True,
        agent_version="t",
        browser_enabled=True,
    )
    fake = (
        BrowserInspectionResult(
            inspection_id="cli-ins",
            url="https://example.com",
            final_url="https://example.com/",
            title="CLI",
            status_code=200,
            inspection_succeeded=True,
        ),
        {"screenshot_b64": None},
    )
    with patch("local_agent.browser_job.run_local_browser_inspection", return_value=fake):
        try:
            n = run_cycle(c, cfg)
        finally:
            c.close()
    assert n == 1
    assert any("/result" in u for u in calls)


def test_token_not_logged_full_string():
    """Regression: logs must use redaction (checked via token_log_label)."""
    from local_agent.security import token_log_label

    tok = "vla_" + "M" * 40
    lab = token_log_label(tok)
    assert tok not in lab
