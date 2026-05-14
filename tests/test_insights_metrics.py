# tests/test_insights_metrics.py
from __future__ import annotations

from models.test_run import TestRun as TestRunRecord
from services.insights_metrics import compute_blast_radius


def test_blast_radius_isolated_single_test():
    r = TestRunRecord(test_case_id="TC-1", test_name="a", status="fail", meta={})
    br = compute_blast_radius(
        module="checkout",
        affected_test_case_ids=["TC-1"],
        total_failures=1,
        root_cause_category="selector_issue",
        runs=[r],
    )
    assert br.affected_tests_count == 1
    assert br.impact_scope == "isolated_failure"
    assert br.estimated_severity == "low"
    assert br.affected_suites_count is None


def test_blast_radius_localized_multi_test():
    runs = [
        TestRunRecord(test_case_id="TC-A", test_name="a", status="fail", meta={}),
        TestRunRecord(test_case_id="TC-B", test_name="b", status="fail", meta={}),
    ]
    br = compute_blast_radius(
        module="auth",
        affected_test_case_ids=["TC-A", "TC-B"],
        total_failures=5,
        root_cause_category="auth_issue",
        runs=runs,
    )
    assert br.impact_scope == "localized_regression"
    assert br.estimated_severity == "critical"


def test_blast_radius_counts_suites_when_meta_present():
    runs = [
        TestRunRecord(
            test_case_id="TC-A",
            test_name="a",
            status="fail",
            meta={"suite_run_id": "suite-1"},
        ),
        TestRunRecord(
            test_case_id="TC-B",
            test_name="b",
            status="fail",
            meta={"suite_run_id": "suite-1"},
        ),
    ]
    br = compute_blast_radius(
        module="cart",
        affected_test_case_ids=["TC-A", "TC-B"],
        total_failures=2,
        root_cause_category="api_failure",
        runs=runs,
    )
    assert br.affected_suites_count == 1
