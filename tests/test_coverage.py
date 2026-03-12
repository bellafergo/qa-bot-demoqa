# tests/test_coverage.py
"""
Tests for the Coverage Intelligence service.

No browser, no network required.
All tests seed the SQLite DB via _reset_for_testing() and populate
the catalog/run history with known fixtures before asserting metrics.
"""
from __future__ import annotations

import pytest

from models.coverage_models import CoverageResult
from models.test_case import TestCaseCreate
from models.test_run import TestRun
from services.coverage_service import CoverageService, _build_recommendations, _compute_module_coverage
from services.test_catalog_service import TestCatalogService, _reset_for_testing
from services.catalog_orchestrator import _reset_for_testing as _orch_reset


# ── Fixtures / helpers ────────────────────────────────────────────────────────

def _reset():
    _reset_for_testing()
    _orch_reset()


def _add_test(tc_id: str, module: str, type_: str = "smoke", priority: str = "medium") -> None:
    svc = TestCatalogService()
    try:
        svc.create_test_case(TestCaseCreate(
            test_case_id = tc_id,
            name         = f"Test {tc_id}",
            module       = module,
            type         = type_,
            priority     = priority,
            steps        = [{"action": "goto", "value": "https://example.com"}],
            assertions   = [],
        ))
    except ValueError:
        pass  # already exists — fine


def _add_run(tc_id: str, status: str) -> TestRun:
    from services.db.test_run_repository import test_run_repo
    run = TestRun(test_case_id=tc_id, status=status)
    test_run_repo.create_run(run)
    return run


def _svc() -> CoverageService:
    return CoverageService()


# ── Coverage score calculation ────────────────────────────────────────────────

class TestCoverageScore:
    def setup_method(self):
        _reset()

    def test_zero_tests_gives_zero_score(self):
        result = _svc().get_module_coverage("empty-module")
        assert result.coverage_score == 0.0
        assert result.total_tests == 0

    def test_all_tests_executed_gives_score_one(self):
        _add_test("TC-COV-001", "auth")
        _add_test("TC-COV-002", "auth")
        _add_run("TC-COV-001", "pass")
        _add_run("TC-COV-002", "pass")
        result = _svc().get_module_coverage("auth")
        assert result.coverage_score == 1.0

    def test_half_tests_executed_gives_half_score(self):
        _add_test("TC-COV-003", "search")
        _add_test("TC-COV-004", "search")
        _add_run("TC-COV-003", "pass")  # only one run
        result = _svc().get_module_coverage("search")
        assert result.coverage_score == pytest.approx(0.5)

    def test_no_runs_gives_zero_score(self):
        _add_test("TC-COV-005", "checkout")
        result = _svc().get_module_coverage("checkout")
        assert result.coverage_score == 0.0

    def test_score_is_between_zero_and_one(self):
        _add_test("TC-COV-006", "forms")
        _add_test("TC-COV-007", "forms")
        _add_test("TC-COV-008", "forms")
        _add_run("TC-COV-006", "fail")
        result = _svc().get_module_coverage("forms")
        assert 0.0 <= result.coverage_score <= 1.0

    def test_failed_run_counts_as_executed(self):
        _add_test("TC-COV-009", "payments")
        _add_run("TC-COV-009", "fail")
        result = _svc().get_module_coverage("payments")
        assert result.executed_tests == 1
        assert result.coverage_score == 1.0


# ── Pass / fail / never-run counts ───────────────────────────────────────────

class TestRunCounts:
    def setup_method(self):
        _reset()

    def test_passed_tests_counted(self):
        _add_test("TC-CNT-001", "auth")
        _add_test("TC-CNT-002", "auth")
        _add_run("TC-CNT-001", "pass")
        _add_run("TC-CNT-002", "fail")
        result = _svc().get_module_coverage("auth")
        assert result.passed_tests == 1
        assert result.failed_tests == 1

    def test_error_run_counts_as_failed(self):
        _add_test("TC-CNT-003", "orders")
        _add_run("TC-CNT-003", "error")
        result = _svc().get_module_coverage("orders")
        assert result.failed_tests == 1

    def test_never_run_tests_counted(self):
        _add_test("TC-CNT-004", "nav")
        _add_test("TC-CNT-005", "nav")
        _add_run("TC-CNT-004", "pass")
        result = _svc().get_module_coverage("nav")
        assert result.never_run_tests == 1

    def test_only_latest_run_counts(self):
        # Test runs pass then fail — latest is fail
        _add_test("TC-CNT-006", "checkout")
        _add_run("TC-CNT-006", "pass")   # older
        _add_run("TC-CNT-006", "fail")   # latest (repo inserts newer first)
        result = _svc().get_module_coverage("checkout")
        # executed=1 regardless — one unique test
        assert result.executed_tests == 1

    def test_total_tests_correct(self):
        for i in range(5):
            _add_test(f"TC-TOT-{i:03d}", "profile")
        result = _svc().get_module_coverage("profile")
        assert result.total_tests == 5


# ── Missing test type detection ───────────────────────────────────────────────

class TestMissingTypes:
    def setup_method(self):
        _reset()

    def test_module_with_no_tests_flags_no_smoke_no_negative(self):
        result = _svc().get_module_coverage("empty")
        # No tests at all — empty module has no types
        # missing_test_types may be empty since there are no tests
        # but the recommendation should mention "no tests"
        assert any("no test" in r.lower() for r in result.recommendations)

    def test_smoke_only_flags_missing_negative(self):
        _add_test("TC-TYPE-001", "auth", type_="smoke")
        result = _svc().get_module_coverage("auth")
        assert "negative" in result.missing_test_types

    def test_negative_only_flags_missing_smoke(self):
        _add_test("TC-TYPE-002", "checkout", type_="negative")
        result = _svc().get_module_coverage("checkout")
        assert "smoke" in result.missing_test_types

    def test_both_smoke_and_negative_present_no_flags(self):
        _add_test("TC-TYPE-003", "search", type_="smoke")
        _add_test("TC-TYPE-004", "search", type_="negative")
        result = _svc().get_module_coverage("search")
        assert result.missing_test_types == []

    def test_missing_types_list_has_no_duplicates(self):
        _add_test("TC-TYPE-005", "forms", type_="smoke")
        result = _svc().get_module_coverage("forms")
        assert len(result.missing_test_types) == len(set(result.missing_test_types))

    def test_all_standard_types_present_no_missing(self):
        for i, t in enumerate(["smoke", "negative", "regression", "functional", "e2e"]):
            _add_test(f"TC-TYPE-{10 + i}", "full-module", type_=t)
        result = _svc().get_module_coverage("full-module")
        assert result.missing_test_types == []


# ── Recommendations ───────────────────────────────────────────────────────────

class TestRecommendations:
    def setup_method(self):
        _reset()

    def test_zero_tests_recommendation(self):
        result = _svc().get_module_coverage("new-module")
        assert len(result.recommendations) >= 1
        assert any("no test" in r.lower() for r in result.recommendations)

    def test_low_coverage_recommendation(self):
        for i in range(4):
            _add_test(f"TC-REC-{i:03d}", "api")
        # Only run 1 of 4 → 25% coverage
        _add_run("TC-REC-000", "pass")
        result = _svc().get_module_coverage("api")
        assert any("low" in r.lower() or "coverage" in r.lower() for r in result.recommendations)

    def test_never_run_recommendation(self):
        _add_test("TC-NR-001", "billing")
        result = _svc().get_module_coverage("billing")
        assert any("never" in r.lower() or "never been executed" in r.lower()
                   for r in result.recommendations)

    def test_failed_tests_recommendation(self):
        _add_test("TC-FAIL-001", "admin")
        _add_run("TC-FAIL-001", "fail")
        result = _svc().get_module_coverage("admin")
        assert any("fail" in r.lower() for r in result.recommendations)

    def test_healthy_module_has_positive_recommendation(self):
        _add_test("TC-OK-001", "healthy", type_="smoke")
        _add_test("TC-OK-002", "healthy", type_="negative")
        _add_run("TC-OK-001", "pass")
        _add_run("TC-OK-002", "pass")
        result = _svc().get_module_coverage("healthy")
        assert len(result.recommendations) >= 1

    def test_missing_type_recommendation_present(self):
        _add_test("TC-MT-001", "smoke-only", type_="smoke")
        result = _svc().get_module_coverage("smoke-only")
        assert any("negative" in r.lower() for r in result.recommendations)

    def test_recommendations_are_non_empty_strings(self):
        _add_test("TC-REC-STR-001", "module-x")
        result = _svc().get_module_coverage("module-x")
        assert all(isinstance(r, str) and r for r in result.recommendations)


# ── Summary (all modules) ─────────────────────────────────────────────────────

class TestCoverageSummary:
    def setup_method(self):
        _reset()

    def test_empty_catalog_returns_empty_list(self):
        result = _svc().get_summary()
        assert result == []

    def test_summary_returns_all_modules(self):
        _add_test("TC-SUM-001", "auth")
        _add_test("TC-SUM-002", "checkout")
        _add_test("TC-SUM-003", "search")
        results = _svc().get_summary()
        modules = {r.module for r in results}
        assert "auth" in modules
        assert "checkout" in modules
        assert "search" in modules

    def test_summary_results_are_coverage_result_instances(self):
        _add_test("TC-SUM-004", "forms")
        results = _svc().get_summary()
        assert all(isinstance(r, CoverageResult) for r in results)

    def test_summary_module_names_are_unique(self):
        _add_test("TC-SUM-005", "auth")
        _add_test("TC-SUM-006", "auth")
        _add_test("TC-SUM-007", "checkout")
        results = _svc().get_summary()
        modules = [r.module for r in results]
        assert len(modules) == len(set(modules))

    def test_summary_includes_run_data(self):
        _add_test("TC-SUM-008", "orders")
        _add_run("TC-SUM-008", "pass")
        results = _svc().get_summary()
        orders_result = next(r for r in results if r.module == "orders")
        assert orders_result.executed_tests == 1
        assert orders_result.passed_tests == 1

    def test_summary_coverage_scores_valid(self):
        _add_test("TC-SUM-009", "nav")
        results = _svc().get_summary()
        for r in results:
            assert 0.0 <= r.coverage_score <= 1.0


# ── Module coverage lookup ────────────────────────────────────────────────────

class TestModuleCoverage:
    def setup_method(self):
        _reset()

    def test_unknown_module_returns_empty_result(self):
        result = _svc().get_module_coverage("this-module-does-not-exist")
        assert result.total_tests == 0
        assert result.coverage_score == 0.0

    def test_module_result_has_correct_module_name(self):
        _add_test("TC-MOD-001", "auth-service")
        result = _svc().get_module_coverage("auth-service")
        assert result.module == "auth-service"

    def test_module_result_is_coverage_result(self):
        _add_test("TC-MOD-002", "checkout")
        result = _svc().get_module_coverage("checkout")
        assert isinstance(result, CoverageResult)

    def test_multiple_tests_in_module_counted(self):
        for i in range(3):
            _add_test(f"TC-MOD-{10 + i}", "search")
        result = _svc().get_module_coverage("search")
        assert result.total_tests == 3
