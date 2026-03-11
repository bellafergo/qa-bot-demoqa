# tests/test_orchestrator.py
"""
Unit tests for services/test_orchestrator.py.
execute_test() is mocked — no real browser is launched.
"""
import time
import pytest
from unittest.mock import patch, MagicMock

from core.suite_models import (
    SuiteStatus,
    TestCase,
    TestStatus,
    TestSuite,
)
from core.exec_env import ExecEnv
from services.retry_policy import RetryPolicy
from services.test_orchestrator import _build_jobs, _skipped_result, run_suite

STEPS = [
    {"action": "goto", "url": "https://example.com"},
    {"action": "assert_visible", "selector": "body"},
]

def _passed_raw(duration_ms=500):
    return {"ok": True, "status": "passed", "steps": [], "logs": [], "duration_ms": duration_ms,
            "evidence_id": None, "evidence_url": None, "report_url": None, "reason": ""}

def _failed_raw(reason="timeout", duration_ms=300):
    return {"ok": False, "status": "failed", "steps": [], "logs": [], "duration_ms": duration_ms,
            "evidence_id": None, "evidence_url": None, "report_url": None, "reason": reason}

def _suite(n=3, concurrency=2):
    cases = [TestCase(name=f"tc{i}", steps=STEPS) for i in range(n)]
    return TestSuite(name="smoke", cases=cases, max_concurrency=concurrency)


# ── _build_jobs ────────────────────────────────────────────────────────────────

class TestBuildJobs:
    def test_one_job_per_case(self):
        suite = _suite(3)
        env = ExecEnv(base_url="https://example.com")
        jobs = _build_jobs(suite, env)
        assert len(jobs) == 3

    def test_job_inherits_env_base_url(self):
        suite = _suite(1)
        env = ExecEnv(base_url="https://staging.example.com")
        jobs = _build_jobs(suite, env)
        assert jobs[0].base_url == "https://staging.example.com"

    def test_case_base_url_overrides_env(self):
        case = TestCase(name="t", steps=STEPS, base_url="https://override.example.com")
        suite = TestSuite(name="s", cases=[case])
        env = ExecEnv(base_url="https://env.example.com")
        jobs = _build_jobs(suite, env)
        assert jobs[0].base_url == "https://override.example.com"

    def test_job_ids_are_unique(self):
        suite = _suite(5)
        jobs = _build_jobs(suite, ExecEnv())
        ids = [j.id for j in jobs]
        assert len(ids) == len(set(ids))

    def test_attempt_starts_at_1(self):
        suite = _suite(2)
        jobs = _build_jobs(suite, ExecEnv())
        assert all(j.attempt == 1 for j in jobs)

    def test_suite_tags_merged_with_case_tags(self):
        case = TestCase(name="t", steps=STEPS, tags=["smoke"])
        suite = TestSuite(name="s", cases=[case], tags=["regression"])
        jobs = _build_jobs(suite, ExecEnv())
        assert "smoke" in jobs[0].tags
        assert "regression" in jobs[0].tags


# ── _skipped_result ────────────────────────────────────────────────────────────

class TestSkippedResult:
    def test_status_is_skipped(self):
        suite = _suite(1)
        job = _build_jobs(suite, ExecEnv())[0]
        result = _skipped_result(job)
        assert result.status == TestStatus.skipped

    def test_duration_is_zero(self):
        suite = _suite(1)
        job = _build_jobs(suite, ExecEnv())[0]
        result = _skipped_result(job)
        assert result.duration_ms == 0


# ── run_suite — integration with mocked execute_test ──────────────────────────

class TestRunSuiteAllPass:
    @patch("services.test_orchestrator.execute_test")
    def test_all_passed(self, mock_exec):
        mock_exec.return_value = _passed_raw()
        suite = _suite(3, concurrency=2)
        result = run_suite(suite, policy=RetryPolicy.strict())
        assert result.status == SuiteStatus.passed
        assert result.passed == 3
        assert result.failed == 0

    @patch("services.test_orchestrator.execute_test")
    def test_execute_test_called_once_per_case(self, mock_exec):
        mock_exec.return_value = _passed_raw()
        suite = _suite(4, concurrency=2)
        run_suite(suite, policy=RetryPolicy.strict())
        assert mock_exec.call_count == 4

    @patch("services.test_orchestrator.execute_test")
    def test_result_contains_all_job_results(self, mock_exec):
        mock_exec.return_value = _passed_raw()
        suite = _suite(3)
        result = run_suite(suite, policy=RetryPolicy.strict())
        assert len(result.job_results) == 3


class TestRunSuiteAllFail:
    @patch("services.test_orchestrator.execute_test")
    def test_all_failed_no_retry(self, mock_exec):
        mock_exec.return_value = _failed_raw("locator_not_found: #btn")
        suite = _suite(3)
        result = run_suite(suite, policy=RetryPolicy.strict())
        assert result.status == SuiteStatus.failed
        assert result.failed == 3

    @patch("services.test_orchestrator.execute_test")
    def test_failed_cases_listed(self, mock_exec):
        mock_exec.return_value = _failed_raw("locator_not_found")
        suite = _suite(2)
        result = run_suite(suite, policy=RetryPolicy.strict())
        assert len(result.failed_cases) == 2


class TestRetryBehavior:
    @patch("services.test_orchestrator.execute_test")
    def test_transient_failure_triggers_retry(self, mock_exec):
        # Fails first (transient), passes on second attempt → flaky
        mock_exec.side_effect = [_failed_raw("timeout occurred"), _passed_raw()]
        suite = TestSuite(name="s", cases=[TestCase(name="tc0", steps=STEPS)])
        policy = RetryPolicy(max_retries=1)
        result = run_suite(suite, policy=policy)
        assert result.flaky == 1
        assert mock_exec.call_count == 2

    @patch("services.test_orchestrator.execute_test")
    def test_terminal_failure_not_retried(self, mock_exec):
        # Terminal failure — should only call execute_test once
        mock_exec.return_value = _failed_raw("locator_not_found: #missing")
        suite = TestSuite(name="s", cases=[TestCase(name="tc0", steps=STEPS)])
        policy = RetryPolicy(max_retries=2, retry_terminal=False)
        result = run_suite(suite, policy=policy)
        assert result.failed == 1
        assert mock_exec.call_count == 1

    @patch("services.test_orchestrator.execute_test")
    def test_retry_exhausted_marks_failed(self, mock_exec):
        # Always fails (transient) → exhausts retries → failed
        mock_exec.return_value = _failed_raw("timeout")
        suite = TestSuite(name="s", cases=[TestCase(name="tc0", steps=STEPS)])
        policy = RetryPolicy(max_retries=2)
        result = run_suite(suite, policy=policy)
        assert result.failed == 1
        assert mock_exec.call_count == 3  # attempt 1, 2, 3


class TestStopOnCriticalFailure:
    @patch("services.test_orchestrator.execute_test")
    def test_stop_triggers_skips(self, mock_exec):
        # Always fails — with threshold 0.0 (stop after first failure) + concurrency=1
        mock_exec.return_value = _failed_raw("locator_not_found")
        suite = TestSuite(
            name="s",
            cases=[TestCase(name=f"tc{i}", steps=STEPS) for i in range(5)],
            max_concurrency=1,  # sequential so threshold triggers predictably
            stop_on_critical_failure=True,
            critical_failure_threshold=0.0,  # any failure triggers stop
        )
        result = run_suite(suite, policy=RetryPolicy.strict())
        # At least some cases should be skipped
        assert result.skipped > 0
        assert result.skipped + result.failed == 5


class TestBackwardCompatibility:
    """Single execute_test() usage is unaffected by this module."""
    @patch("services.test_orchestrator.execute_test")
    def test_single_case_suite_works(self, mock_exec):
        mock_exec.return_value = _passed_raw()
        suite = TestSuite(name="single", cases=[TestCase(name="login", steps=STEPS)])
        result = run_suite(suite)
        assert result.total == 1
        assert result.passed == 1

    @patch("services.test_orchestrator.execute_test")
    def test_callback_invoked_per_job(self, mock_exec):
        mock_exec.return_value = _passed_raw()
        suite = _suite(3)
        callbacks = []
        run_suite(suite, on_job_complete=callbacks.append, policy=RetryPolicy.strict())
        assert len(callbacks) == 3

    @patch("services.test_orchestrator.execute_test")
    def test_env_base_url_passed_to_runner(self, mock_exec):
        mock_exec.return_value = _passed_raw()
        suite = TestSuite(name="s", cases=[TestCase(name="t", steps=STEPS)])
        env = ExecEnv(base_url="https://env.example.com", headless=False)
        run_suite(suite, env=env, policy=RetryPolicy.strict())
        call_kwargs = mock_exec.call_args[1]
        assert call_kwargs.get("base_url") == "https://env.example.com"
