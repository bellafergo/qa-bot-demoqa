# tests/test_suite_aggregator.py
"""Unit tests for services/suite_aggregator.py."""
import pytest
from core.suite_models import (
    BatchRun,
    JobResult,
    SuiteStatus,
    TestCase,
    TestStatus,
    TestSuite,
)
from services.suite_aggregator import aggregate_results

STEPS = [{"action": "goto", "url": "https://example.com"}]


def _suite(n=3):
    cases = [TestCase(name=f"tc{i}", steps=STEPS) for i in range(n)]
    return TestSuite(name="smoke", cases=cases)


def _batch(suite):
    return BatchRun(suite_id=suite.id, suite_name=suite.name, jobs=[], env_name="test")


def _result(case_name, status: TestStatus, duration_ms=1000, reason=None):
    return JobResult(
        job_id=f"job-{case_name}",
        case_id=f"id-{case_name}",
        case_name=case_name,
        status=status,
        attempt=1,
        duration_ms=duration_ms,
        failure_reason=reason,
    )


class TestAggregateAllPassed:
    def test_status_is_passed(self):
        suite = _suite(3)
        batch = _batch(suite)
        results = [_result(f"tc{i}", TestStatus.passed, 1000) for i in range(3)]
        r = aggregate_results(results, suite, batch)
        assert r.status == SuiteStatus.passed

    def test_totals(self):
        suite = _suite(3)
        batch = _batch(suite)
        results = [_result(f"tc{i}", TestStatus.passed) for i in range(3)]
        r = aggregate_results(results, suite, batch)
        assert r.total == 3
        assert r.passed == 3
        assert r.failed == 0
        assert r.flaky == 0

    def test_pass_rate_is_1(self):
        suite = _suite(3)
        results = [_result(f"tc{i}", TestStatus.passed) for i in range(3)]
        r = aggregate_results(results, suite, _batch(suite))
        assert r.pass_rate == 1.0


class TestAggregateAllFailed:
    def test_status_is_failed(self):
        suite = _suite(3)
        results = [_result(f"tc{i}", TestStatus.failed, reason="not_found") for i in range(3)]
        r = aggregate_results(results, suite, _batch(suite))
        assert r.status == SuiteStatus.failed

    def test_pass_rate_is_0(self):
        suite = _suite(3)
        results = [_result(f"tc{i}", TestStatus.failed) for i in range(3)]
        r = aggregate_results(results, suite, _batch(suite))
        assert r.pass_rate == 0.0

    def test_failed_cases_list(self):
        suite = _suite(2)
        results = [_result("tc0", TestStatus.failed), _result("tc1", TestStatus.failed)]
        r = aggregate_results(results, suite, _batch(suite))
        assert "tc0" in r.failed_cases
        assert "tc1" in r.failed_cases


class TestAggregatePartial:
    def test_status_is_partial(self):
        suite = _suite(4)
        results = [
            _result("tc0", TestStatus.passed),
            _result("tc1", TestStatus.passed),
            _result("tc2", TestStatus.failed, reason="timeout"),
            _result("tc3", TestStatus.failed, reason="timeout"),
        ]
        r = aggregate_results(results, suite, _batch(suite))
        assert r.status == SuiteStatus.partial

    def test_counts_correct(self):
        suite = _suite(4)
        results = [
            _result("tc0", TestStatus.passed),
            _result("tc1", TestStatus.flaky),
            _result("tc2", TestStatus.failed),
            _result("tc3", TestStatus.skipped),
        ]
        r = aggregate_results(results, suite, _batch(suite))
        assert r.passed == 1
        assert r.flaky == 1
        assert r.failed == 1
        assert r.skipped == 1

    def test_pass_rate_includes_flaky(self):
        suite = _suite(4)
        results = [
            _result("tc0", TestStatus.passed),
            _result("tc1", TestStatus.flaky),
            _result("tc2", TestStatus.failed),
            _result("tc3", TestStatus.failed),
        ]
        r = aggregate_results(results, suite, _batch(suite))
        # (1 passed + 1 flaky) / 4 = 0.5
        assert r.pass_rate == 0.5


class TestAvgDuration:
    def test_avg_duration_excludes_skipped(self):
        suite = _suite(3)
        results = [
            _result("tc0", TestStatus.passed, duration_ms=1000),
            _result("tc1", TestStatus.passed, duration_ms=2000),
            _result("tc2", TestStatus.skipped, duration_ms=0),
        ]
        r = aggregate_results(results, suite, _batch(suite))
        # Only 2 non-skipped: avg = (1000 + 2000) / 2 = 1500
        assert r.avg_duration_ms == 1500.0

    def test_avg_duration_all_skipped_is_zero(self):
        suite = _suite(2)
        results = [
            _result("tc0", TestStatus.skipped, duration_ms=0),
            _result("tc1", TestStatus.skipped, duration_ms=0),
        ]
        r = aggregate_results(results, suite, _batch(suite))
        assert r.avg_duration_ms == 0.0


class TestTopFailureReasons:
    def test_reasons_counted(self):
        suite = _suite(3)
        results = [
            _result("tc0", TestStatus.failed, reason="timeout: element not visible"),
            _result("tc1", TestStatus.failed, reason="timeout: element not visible"),
            _result("tc2", TestStatus.failed, reason="locator_not_found: #btn"),
        ]
        r = aggregate_results(results, suite, _batch(suite))
        assert len(r.top_failure_reasons) >= 1
        # Most common reason should be first
        assert "timeout" in r.top_failure_reasons[0]

    def test_no_reasons_for_passed(self):
        suite = _suite(2)
        results = [
            _result("tc0", TestStatus.passed),
            _result("tc1", TestStatus.passed),
        ]
        r = aggregate_results(results, suite, _batch(suite))
        assert r.top_failure_reasons == []


class TestEdgeCases:
    def test_empty_results(self):
        suite = TestSuite(name="empty", cases=[])
        r = aggregate_results([], suite, _batch(suite))
        assert r.status == SuiteStatus.error
        assert r.total == 0
        assert r.pass_rate == 0.0

    def test_duration_ms_computed(self):
        suite = _suite(1)
        results = [_result("tc0", TestStatus.passed)]
        r = aggregate_results(results, suite, _batch(suite),
                              started_at=1000, finished_at=5000)
        assert r.duration_ms == 4000
