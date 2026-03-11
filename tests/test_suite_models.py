# tests/test_suite_models.py
"""Unit tests for core/suite_models.py — model creation, defaults, validation."""
import pytest
from core.suite_models import (
    BatchRun,
    ExecutionJob,
    JobResult,
    SuiteRunResult,
    SuiteStatus,
    TestCase,
    TestStatus,
    TestSuite,
)

SAMPLE_STEPS = [
    {"action": "goto",  "url": "https://example.com"},
    {"action": "assert_visible", "selector": "body"},
]


class TestTestCase:
    def test_default_id_generated(self):
        tc = TestCase(name="login", steps=SAMPLE_STEPS)
        assert tc.id.startswith("tc-")
        assert len(tc.id) > 4

    def test_unique_ids(self):
        a = TestCase(name="a", steps=SAMPLE_STEPS)
        b = TestCase(name="b", steps=SAMPLE_STEPS)
        assert a.id != b.id

    def test_default_expected_pass(self):
        tc = TestCase(name="t", steps=SAMPLE_STEPS)
        assert tc.expected == "pass"

    def test_default_headless_true(self):
        tc = TestCase(name="t", steps=SAMPLE_STEPS)
        assert tc.headless is True

    def test_custom_base_url(self):
        tc = TestCase(name="t", steps=SAMPLE_STEPS, base_url="https://staging.example.com")
        assert tc.base_url == "https://staging.example.com"

    def test_tags_default_empty(self):
        tc = TestCase(name="t", steps=SAMPLE_STEPS)
        assert tc.tags == []

    def test_tags_list_not_shared(self):
        a = TestCase(name="a", steps=SAMPLE_STEPS)
        b = TestCase(name="b", steps=SAMPLE_STEPS)
        a.tags.append("smoke")
        assert b.tags == []


class TestTestSuite:
    def test_default_id_generated(self):
        suite = TestSuite(name="smoke", cases=[])
        assert suite.id.startswith("suite-")

    def test_default_concurrency(self):
        suite = TestSuite(name="s", cases=[])
        assert suite.max_concurrency == 2

    def test_default_stop_on_critical_false(self):
        suite = TestSuite(name="s", cases=[])
        assert suite.stop_on_critical_failure is False

    def test_threshold_default(self):
        suite = TestSuite(name="s", cases=[])
        assert suite.critical_failure_threshold == 0.8

    def test_cases_stored(self):
        cases = [TestCase(name=f"tc{i}", steps=SAMPLE_STEPS) for i in range(3)]
        suite = TestSuite(name="s", cases=cases)
        assert len(suite.cases) == 3


class TestExecutionJob:
    def test_default_attempt_is_1(self):
        job = ExecutionJob(suite_id="s1", case_id="c1", case_name="login", steps=SAMPLE_STEPS)
        assert job.attempt == 1

    def test_default_id_generated(self):
        job = ExecutionJob(suite_id="s1", case_id="c1", case_name="t", steps=SAMPLE_STEPS)
        assert job.id.startswith("job-")


class TestJobResult:
    def test_passed_status(self):
        r = JobResult(job_id="j1", case_id="c1", case_name="login",
                      status=TestStatus.passed, attempt=1, duration_ms=1200)
        assert r.status == TestStatus.passed

    def test_flaky_status(self):
        r = JobResult(job_id="j1", case_id="c1", case_name="login",
                      status=TestStatus.flaky, attempt=2, duration_ms=2000)
        assert r.status == TestStatus.flaky

    def test_optional_fields_default_none(self):
        r = JobResult(job_id="j1", case_id="c1", case_name="t",
                      status=TestStatus.failed, attempt=1, duration_ms=0)
        assert r.evidence_url is None
        assert r.failure_reason is None


class TestSuiteRunResult:
    def test_pass_rate_field(self):
        r = SuiteRunResult(
            suite_id="s1", suite_name="s", batch_id="b1",
            status=SuiteStatus.passed,
            total=5, passed=4, failed=0, flaky=1, skipped=0,
            pass_rate=1.0, avg_duration_ms=1500.0,
            started_at=0, finished_at=1000, duration_ms=1000,
        )
        assert r.pass_rate == 1.0

    def test_suite_status_enum(self):
        assert SuiteStatus.passed == "passed"
        assert SuiteStatus.failed == "failed"
        assert SuiteStatus.partial == "partial"

    def test_test_status_enum(self):
        assert TestStatus.flaky == "flaky"
        assert TestStatus.skipped == "skipped"
