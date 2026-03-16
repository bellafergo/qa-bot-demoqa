# tests/test_jobs_guardrails.py
"""
Tests for workers/jobs.py:
  _redact secret masking, _is_transient, _run_with_retry backoff.
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


class TestRedact:
    def _redact(self, text: str) -> str:
        from workers.jobs import _redact
        return _redact(text)

    def test_password_redacted(self):
        result = self._redact("Error: password=supersecret123 was wrong")
        assert "supersecret123" not in result
        assert "***REDACTED***" in result

    def test_token_redacted(self):
        result = self._redact("token: abc123xyz in header")
        assert "abc123xyz" not in result
        assert "***REDACTED***" in result

    def test_api_key_redacted(self):
        result = self._redact('api_key="sk-proj-very-secret"')
        assert "sk-proj-very-secret" not in result
        assert "***REDACTED***" in result

    def test_secret_redacted(self):
        result = self._redact("secret=mysecretvalue error occurred")
        assert "mysecretvalue" not in result

    def test_no_false_positive_plain_text(self):
        msg = "TimeoutError: page load timed out after 30s"
        assert self._redact(msg) == msg

    def test_multiple_secrets_all_redacted(self):
        msg = "password=abc token=xyz api_key=qrs"
        result = self._redact(msg)
        assert "abc" not in result
        assert "xyz" not in result
        assert "qrs" not in result
        assert result.count("***REDACTED***") == 3

    def test_redact_applied_to_retry_log(self):
        """
        The warning logged inside _run_with_retry must also be redacted.
        Verifies that _redact(str(e)) is called on the exception message.
        """
        from unittest.mock import patch, call
        import workers.jobs as jobs

        calls_log: list = []

        def fake_execute(**kwargs):
            raise TimeoutError("connection password=hunter2 timed out")

        with patch.object(jobs, "execute_test", side_effect=fake_execute):
            with patch.object(jobs, "time") as mock_time:
                mock_time.sleep = lambda _: None
                with patch.object(jobs.logger, "warning", side_effect=lambda *a, **kw: calls_log.append(a)):
                    with pytest.raises(TimeoutError):
                        jobs._run_with_retry({})

        # At least one warning must have been logged (retry attempts)
        assert calls_log, "Expected at least one logger.warning call"
        # None of the logged strings should contain the raw secret
        for logged_args in calls_log:
            combined = " ".join(str(a) for a in logged_args)
            assert "hunter2" not in combined, f"Secret leaked in log: {combined}"


class TestTransientRetry:
    def test_timeout_error_is_transient(self):
        from workers.jobs import _is_transient
        assert _is_transient(TimeoutError("page load timed out"))

    def test_connection_error_is_transient(self):
        from workers.jobs import _is_transient
        assert _is_transient(ConnectionError("connection refused"))

    def test_os_error_is_transient(self):
        from workers.jobs import _is_transient
        assert _is_transient(OSError("network unreachable"))

    def test_keyword_timeout_is_transient(self):
        from workers.jobs import _is_transient
        assert _is_transient(Exception("ERR: timeout waiting for selector"))

    def test_value_error_not_transient(self):
        from workers.jobs import _is_transient
        assert not _is_transient(ValueError("invalid selector syntax"))

    def test_attribute_error_not_transient(self):
        from workers.jobs import _is_transient
        assert not _is_transient(AttributeError("NoneType has no attribute 'click'"))

    def test_retry_succeeds_on_second_attempt(self):
        from unittest.mock import patch
        import workers.jobs as jobs

        calls = {"n": 0}

        def fake_execute(**kwargs):
            calls["n"] += 1
            if calls["n"] < 2:
                raise TimeoutError("transient")
            return {"status": "passed"}

        with patch.object(jobs, "execute_test", side_effect=fake_execute):
            with patch.object(jobs, "time") as mock_time:
                mock_time.sleep = lambda _: None
                result = jobs._run_with_retry({})

        assert result == {"status": "passed"}
        assert calls["n"] == 2

    def test_non_transient_error_not_retried(self):
        from unittest.mock import patch
        import workers.jobs as jobs

        calls = {"n": 0}

        def fake_execute(**kwargs):
            calls["n"] += 1
            raise ValueError("bad step config")

        with patch.object(jobs, "execute_test", side_effect=fake_execute):
            with pytest.raises(ValueError, match="bad step config"):
                jobs._run_with_retry({})

        assert calls["n"] == 1

    def test_transient_exhausts_all_retries(self):
        from unittest.mock import patch
        import workers.jobs as jobs

        calls = {"n": 0}

        def fake_execute(**kwargs):
            calls["n"] += 1
            raise TimeoutError("always times out")

        with patch.object(jobs, "execute_test", side_effect=fake_execute):
            with patch.object(jobs, "time") as mock_time:
                mock_time.sleep = lambda _: None
                with pytest.raises(TimeoutError):
                    jobs._run_with_retry({})

        assert calls["n"] == jobs._MAX_RETRIES
