# tests/test_retry_policy.py
"""Unit tests for services/retry_policy.py."""
import pytest
from services.retry_policy import RetryPolicy, classify_runner_failure

# ── Fixture helpers ────────────────────────────────────────────────────────────

def _result(status="failed", reason="", logs=None, failure_context=None):
    return {
        "ok": status == "passed",
        "status": status,
        "reason": reason,
        "logs": logs or [],
        "failure_context": failure_context or {},
        "steps": [],
    }


# ── classify_runner_failure ────────────────────────────────────────────────────

class TestClassifyRunnerFailure:
    def test_passed_result_is_none(self):
        assert classify_runner_failure(_result("passed")) == "none"

    def test_timeout_in_reason_is_transient(self):
        r = _result(reason="Timeout exceeded waiting for element")
        assert classify_runner_failure(r) == "transient"

    def test_navigation_in_reason_is_transient(self):
        r = _result(reason="Navigation failed: net::ERR_CONNECTION_REFUSED")
        assert classify_runner_failure(r) == "transient"

    def test_not_found_in_reason_is_terminal(self):
        r = _result(reason="locator_not_found: could not find #username")
        assert classify_runner_failure(r) == "terminal"

    def test_assertion_in_reason_is_terminal(self):
        r = _result(reason="assertion_failed: expected 'Secure Area' not found in body")
        assert classify_runner_failure(r) == "terminal"

    def test_invalid_target_is_terminal(self):
        r = _result(reason="invalid_target: no primary selector provided")
        assert classify_runner_failure(r) == "terminal"

    def test_unknown_reason_is_unknown(self):
        r = _result(reason="Something unexpected happened")
        assert classify_runner_failure(r) == "unknown"

    def test_transient_in_logs(self):
        r = _result(logs=["[step 2] timed out after 15000ms"])
        assert classify_runner_failure(r) == "transient"

    def test_terminal_in_failure_context(self):
        r = _result(failure_context={"classification": {"failure_type": "locator_not_found"}})
        assert classify_runner_failure(r) == "terminal"

    def test_empty_result_is_unknown(self):
        r = {"ok": False, "status": "failed"}
        assert classify_runner_failure(r) == "unknown"

    def test_terminal_wins_over_transient(self):
        # If both patterns appear, terminal pattern wins (checked first in code)
        r = _result(reason="timeout AND locator_not_found combined")
        # terminal_re matches first
        assert classify_runner_failure(r) == "terminal"


# ── RetryPolicy.should_retry ───────────────────────────────────────────────────

class TestRetryPolicyDefault:
    def setup_method(self):
        self.policy = RetryPolicy.default()

    def test_does_not_retry_passed(self):
        r = _result("passed")
        assert self.policy.should_retry(r, attempt=1) is False

    def test_retries_transient_on_attempt_1(self):
        r = _result(reason="timeout occurred")
        assert self.policy.should_retry(r, attempt=1) is True

    def test_retries_transient_on_attempt_2(self):
        r = _result(reason="timeout occurred")
        assert self.policy.should_retry(r, attempt=2) is True

    def test_does_not_retry_after_max(self):
        r = _result(reason="timeout occurred")
        # default max_retries=2, so attempt 3+ should not retry
        assert self.policy.should_retry(r, attempt=3) is False

    def test_does_not_retry_terminal(self):
        r = _result(reason="locator_not_found: #missing-button")
        assert self.policy.should_retry(r, attempt=1) is False

    def test_retries_unknown_by_default(self):
        r = _result(reason="Something weird happened")
        assert self.policy.should_retry(r, attempt=1) is True


class TestRetryPolicyStrict:
    def setup_method(self):
        self.policy = RetryPolicy.strict()

    def test_does_not_retry_anything(self):
        r = _result(reason="timeout occurred")
        assert self.policy.should_retry(r, attempt=1) is False

    def test_does_not_retry_unknown(self):
        r = _result(reason="something unknown")
        assert self.policy.should_retry(r, attempt=1) is False


class TestRetryPolicyAggressive:
    def setup_method(self):
        self.policy = RetryPolicy.aggressive()

    def test_retries_terminal(self):
        r = _result(reason="locator_not_found: #btn")
        assert self.policy.should_retry(r, attempt=1) is True

    def test_retries_up_to_max_3(self):
        r = _result(reason="timeout")
        assert self.policy.should_retry(r, attempt=3) is True
        assert self.policy.should_retry(r, attempt=4) is False


class TestRetryPolicyCustom:
    def test_no_retry_unknown_when_disabled(self):
        policy = RetryPolicy(max_retries=2, retry_unknown=False)
        r = _result(reason="Something unknown")
        assert policy.should_retry(r, attempt=1) is False

    def test_retry_terminal_when_enabled(self):
        policy = RetryPolicy(max_retries=1, retry_terminal=True)
        r = _result(reason="locator_not_found")
        assert policy.should_retry(r, attempt=1) is True
