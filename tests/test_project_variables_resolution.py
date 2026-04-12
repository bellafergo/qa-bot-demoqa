# tests/test_project_variables_resolution.py
from __future__ import annotations

import pytest

from core.step_compiler import CompileError, resolve_interpolated_credentials
from services.project_execution_context import (
    api_runner_credential_interpolation,
    execution_context_from_project,
)


class _FakeProject:
    def __init__(self, settings):
        self.settings = settings


def test_project_variable_wins_over_env(monkeypatch):
    monkeypatch.setenv("VANYA_TEST_EMAIL", "env@example.com")
    ctx = {
        "project_variables": {"EMAIL": "project@example.com"},
        "credentials": {"email": "creds@example.com"},
    }
    out = resolve_interpolated_credentials("{EMAIL}", ctx, purpose="t")
    assert out == "project@example.com"


def test_credentials_used_when_no_project_var():
    ctx = {
        "credentials": {"email": "only@prompt.com"},
    }
    out = resolve_interpolated_credentials("{EMAIL}", ctx, purpose="t")
    assert out == "only@prompt.com"


def test_api_runner_credential_interpolation_from_project():
    p = _FakeProject({"variables": {"EMAIL": "p@q.co", "PASSWORD": "sec"}})
    d = api_runner_credential_interpolation(p)
    assert d["project_email"] == "p@q.co"
    assert d["project_password"] == "sec"


def test_api_runner_credential_interpolation_env_fallback(monkeypatch):
    monkeypatch.setenv("TOS_TEST_EMAIL", "tos@x.co")
    monkeypatch.setenv("VANYA_TEST_PASSWORD", "vanya-pw")
    d = api_runner_credential_interpolation(None)
    assert d["project_email"] == "tos@x.co"
    assert d["project_password"] == "vanya-pw"


def test_api_runner_credential_interpolation_project_wins_over_env(monkeypatch):
    monkeypatch.setenv("VANYA_TEST_EMAIL", "env@e.co")
    monkeypatch.setenv("VANYA_TEST_PASSWORD", "envpw")
    p = _FakeProject({"variables": {"EMAIL": "proj@e.co", "PASSWORD": "projpw"}})
    d = api_runner_credential_interpolation(p)
    assert d["project_email"] == "proj@e.co"
    assert d["project_password"] == "projpw"


def test_api_runner_reads_variables_json_string():
    raw = '{"EMAIL": "js@string.io", "PASSWORD": "pw1"}'
    p = _FakeProject({"variables": raw})
    d = api_runner_credential_interpolation(p)
    assert d["project_email"] == "js@string.io"
    assert d["project_password"] == "pw1"


def test_api_runner_reads_top_level_email_password():
    p = _FakeProject({"EMAIL": "top@level.test", "PASSWORD": "toppw"})
    d = api_runner_credential_interpolation(p)
    assert d["project_email"] == "top@level.test"
    assert d["project_password"] == "toppw"


def test_api_runner_reads_credentials_subdoc():
    p = _FakeProject(
        {
            "credentials": {"email": "sub@doc.test", "password": "subpw"},
        },
    )
    d = api_runner_credential_interpolation(p)
    assert d["project_email"] == "sub@doc.test"
    assert d["project_password"] == "subpw"


def test_execution_context_from_project_variables_json_string():
    raw = '{"EMAIL": "x@y.z", "PASSWORD": "sec"}'
    p = _FakeProject({"variables": raw})
    ctx = execution_context_from_project(p)
    assert ctx["project_variables"]["EMAIL"] == "x@y.z"
    assert ctx["credentials"]["email"] == "x@y.z"


def test_execution_context_from_project_builds_maps():
    p = _FakeProject(
        {
            "login_profile": {
                "email_selector": "input[type='email']",
                "password_selector": "input[type='password']",
                "submit_selector": "button[type='submit']",
                "success_text": "Hi",
            },
            "variables": {"EMAIL": "a@b.co", "PASSWORD": "secret"},
        },
    )
    ctx = execution_context_from_project(p)
    assert ctx["project_variables"]["EMAIL"] == "a@b.co"
    assert ctx["login_profile"]["success_text"] == "Hi"
    assert ctx["credentials"]["email"] == "a@b.co"


def test_unresolved_without_project_or_env():
    with pytest.raises(CompileError) as ei:
        resolve_interpolated_credentials("{EMAIL}", {"credentials": {}}, purpose="t")
    assert "Unresolved variable" in str(ei.value)


def test_compile_login_dsl_with_project_profile_and_success_assert():
    from core.step_compiler import compile_to_runner_steps

    ctx = {
        "project_variables": {"EMAIL": "u@x.com", "PASSWORD": "pw"},
        "login_profile": {
            "login_url": "/login",
            "email_selector": "input#e",
            "password_selector": "input#p",
            "submit_selector": "button#s",
            "success_text": "Dashboard ready",
        },
    }
    base = "https://example.com"
    out = compile_to_runner_steps([{"action": "login"}], base_url=base, context=ctx)
    actions = [s.get("action") for s in out]
    assert actions[0] == "goto"
    assert "fill" in actions and "click" in actions
    assert any(s.get("action") == "assert_text_contains" for s in out)
    assert any(
        s.get("action") == "assert_text_contains" and "Dashboard ready" in str(s.get("text", ""))
        for s in out
    )
