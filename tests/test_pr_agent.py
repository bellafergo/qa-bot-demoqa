# tests/test_pr_agent.py
"""Direct unit tests for pr_agent webhook adapter (no HTTP app startup)."""
from __future__ import annotations

import hashlib
import hmac
from types import SimpleNamespace
from unittest.mock import patch

import pytest

from models.pr_analysis_models import PRAnalysisResult
from services.pr_agent import (
    PRContext,
    format_pr_qa_analysis_comment,
    handle_pull_request_event,
    parse_github_pull_request_event,
    select_suites,
    verify_github_signature,
)

WEBHOOK_SECRET = "test-webhook-secret-for-unit-tests"


def _signed_body(raw_body: bytes, secret: str = WEBHOOK_SECRET) -> str:
    digest = hmac.new(secret.encode("utf-8"), raw_body, hashlib.sha256).hexdigest()
    return f"sha256={digest}"


def _pr_payload(
    *,
    action: str = "opened",
    number: int = 42,
    title: str = "Fix login",
    full_name: str = "acme/demo",
    sha: str = "abc123def456",
    body: str = "PR body",
    draft: bool = False,
) -> dict:
    return {
        "action": action,
        "pull_request": {
            "number": number,
            "title": title,
            "html_url": f"https://github.com/{full_name}/pull/{number}",
            "url": f"https://api.github.com/repos/{full_name}/pulls/{number}",
            "head": {"sha": sha},
            "body": body,
            "draft": draft,
        },
        "repository": {"full_name": full_name},
    }


def _ctx(**kwargs) -> PRContext:
    defaults = dict(
        provider="github",
        owner="acme",
        repo="demo",
        pr_number=42,
        sha="abc123def456",
        title="Fix login",
        html_url="https://github.com/acme/demo/pull/42",
        api_url="https://api.github.com/repos/acme/demo/pulls/42",
    )
    defaults.update(kwargs)
    return PRContext(**defaults)


@pytest.fixture(autouse=True)
def _pr_agent_settings(monkeypatch):
    monkeypatch.setattr(
        "services.pr_agent.settings",
        SimpleNamespace(
            GITHUB_WEBHOOK_SECRET=WEBHOOK_SECRET,
            PR_AGENT_EXECUTE_RUNS=False,
            ENV="dev",
        ),
    )


def _settings_no_secret(monkeypatch, env: str):
    monkeypatch.setattr(
        "services.pr_agent.settings",
        SimpleNamespace(
            GITHUB_WEBHOOK_SECRET="",
            PR_AGENT_EXECUTE_RUNS=False,
            ENV=env,
        ),
    )


class TestVerifyGithubSignature:
    def test_valid_hmac_sha256_returns_true(self):
        body = b'{"action":"opened"}'
        assert verify_github_signature(body, _signed_body(body)) is True

    def test_invalid_signature_returns_false(self):
        body = b'{"action":"opened"}'
        assert verify_github_signature(body, "sha256=deadbeef") is False

    def test_empty_or_missing_signature_returns_false(self):
        body = b'{"action":"opened"}'
        assert verify_github_signature(body, "") is False
        assert verify_github_signature(body, "not-sha256=abc") is False

    def test_empty_secret_production_returns_false(self, monkeypatch):
        _settings_no_secret(monkeypatch, "production")
        body = b'{"action":"opened"}'
        assert verify_github_signature(body, "") is False

    def test_empty_secret_prod_returns_false(self, monkeypatch):
        _settings_no_secret(monkeypatch, "prod")
        body = b'{"action":"opened"}'
        assert verify_github_signature(body, "sha256=anything") is False

    def test_empty_secret_dev_returns_true(self, monkeypatch):
        _settings_no_secret(monkeypatch, "dev")
        body = b'{"action":"opened"}'
        assert verify_github_signature(body, "") is True

    def test_empty_secret_test_returns_true(self, monkeypatch):
        _settings_no_secret(monkeypatch, "test")
        body = b'{"action":"opened"}'
        assert verify_github_signature(body, "bad-signature") is True


class TestParseGithubPullRequestEvent:
    def test_opened_pr_returns_pr_context(self):
        ctx = parse_github_pull_request_event(_pr_payload(action="opened"))
        assert ctx is not None
        assert ctx.provider == "github"
        assert ctx.owner == "acme"
        assert ctx.repo == "demo"
        assert ctx.pr_number == 42
        assert ctx.title == "Fix login"

    def test_synchronize_pr_returns_pr_context(self):
        ctx = parse_github_pull_request_event(_pr_payload(action="synchronize"))
        assert ctx is not None
        assert ctx.pr_number == 42

    def test_closed_action_still_parses_context(self):
        """parse_github_pull_request_event does not filter by action; handle_pull_request_event does."""
        ctx = parse_github_pull_request_event(_pr_payload(action="closed"))
        assert ctx is not None
        assert ctx.pr_number == 42

    def test_missing_fields_return_none_without_exception(self):
        assert parse_github_pull_request_event({}) is None
        assert parse_github_pull_request_event({"pull_request": {}, "repository": {}}) is None
        assert parse_github_pull_request_event({"pull_request": {"number": 1}, "repository": {}}) is None


class TestSelectSuites:
    _DB_TAGS = frozenset({"database", "db", "migration", "high_risk"})

    def _assert_database_suite(self, sel):
        assert any(tag in self._DB_TAGS for tag in sel.tags), sel.tags

    def test_auth_login_files_select_login_suite(self):
        sel = select_suites([{"filename": "src/auth/login.py"}])
        assert "login" in sel.tags

    def test_empty_file_list_returns_smoke_default(self):
        sel = select_suites([])
        assert sel.tags == ["smoke"]
        assert sel.reason

    def test_test_only_files_handled_safely(self):
        sel = select_suites([{"filename": "tests/test_checkout.py"}])
        assert sel.tags
        assert isinstance(sel.reason, str)

    @pytest.mark.parametrize(
        "filename",
        [
            "db/migrations/001_create_users.sql",
            "schema/auth.sql",
            "alembic/versions/123_update.py",
            "src/db/schema.py",
        ],
    )
    def test_sql_schema_files_select_database_suite(self, filename):
        sel = select_suites([{"filename": filename}])
        self._assert_database_suite(sel)


class TestFormatPrQaAnalysisComment:
    def test_includes_pr_number_and_risk_level(self):
        analysis = PRAnalysisResult(
            inferred_modules=["auth"],
            inferred_risk_level="high",
            matched_test_case_ids=["TC-1"],
            recommended_api_tests=["TC-1"],
            recommended_ui_tests=[],
        )
        comment = format_pr_qa_analysis_comment(_ctx(), analysis, file_count=3)
        assert "42" in comment
        assert "high" in comment.lower() or "HIGH" in comment or "**high**" in comment.lower()

    def test_minimal_data_does_not_raise(self):
        analysis = PRAnalysisResult()
        comment = format_pr_qa_analysis_comment(_ctx(), analysis, file_count=0)
        assert isinstance(comment, str)
        assert len(comment) > 0


class TestHandlePullRequestEvent:
    @patch("services.pr_agent.resolve_github_http_for_repository", return_value=None)
    def test_unknown_repo_returns_safe_dict(self, _resolve):
        result = handle_pull_request_event(_pr_payload())
        assert result["ok"] is True
        assert result.get("commented") is False
        assert "reason" in result

    @patch("services.pr_agent.resolve_github_http_for_repository")
    def test_closed_pr_is_ignored(self, mock_resolve):
        result = handle_pull_request_event(_pr_payload(action="closed"))
        assert result["ok"] is True
        assert result.get("ignored") is True
        mock_resolve.assert_not_called()

    @patch("services.pr_agent.post_pr_comment", return_value=1001)
    @patch("services.pr_agent.list_changed_files", return_value=[{"filename": "src/auth/login.py"}])
    @patch("services.pr_agent.resolve_github_http_for_repository")
    @patch("services.pr_analysis_service.pr_analysis_service")
    def test_opened_pr_delegates_to_pr_analysis_service(
        self,
        mock_analysis_svc,
        mock_resolve,
        _list_files,
        _post_comment,
    ):
        from services.github_project_context import GitHubHttpConfig

        mock_resolve.return_value = GitHubHttpConfig(token="ghp_test")
        mock_analysis_svc.analyze.return_value = PRAnalysisResult(
            inferred_modules=["auth"],
            inferred_risk_level="medium",
            matched_test_case_ids=["TC-AUTH"],
            recommended_api_tests=["TC-AUTH"],
        )

        result = handle_pull_request_event(_pr_payload(action="opened"))

        assert result["ok"] is True
        assert result.get("commented") is True
        mock_analysis_svc.analyze.assert_called_once()
        req = mock_analysis_svc.analyze.call_args[0][0]
        assert req.pr_id == "42"
        assert req.auto_enqueue is False
        assert "src/auth/login.py" in req.changed_files
