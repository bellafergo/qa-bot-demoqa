# tests/test_risk_selection.py
"""
Tests for the Risk-Based Test Selection service.

No browser, no network required.
All tests reset the SQLite DB and populate the catalog / run history
with known fixtures before asserting selection behaviour.
"""
from __future__ import annotations

import pytest

from models.risk_selection_models import RiskSelectionRequest, RiskSelectionResult, SelectedTest
from models.test_case import TestCaseCreate
from models.test_run import TestRun
from services.risk_selection_service import RiskSelectionService, _is_high_risk_module
from services.test_catalog_service import TestCatalogService, _reset_for_testing
from services.catalog_orchestrator import _reset_for_testing as _orch_reset


# ── Fixtures / helpers ─────────────────────────────────────────────────────────

def _reset():
    _reset_for_testing()
    _orch_reset()


def _add_test(
    tc_id: str,
    module: str,
    type_: str = "smoke",
    priority: str = "medium",
) -> None:
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
        pass


def _add_run(tc_id: str, status: str) -> None:
    from services.db.test_run_repository import test_run_repo
    run = TestRun(test_case_id=tc_id, status=status)
    test_run_repo.create_run(run)


def _svc() -> RiskSelectionService:
    return RiskSelectionService()


# ── High-risk module helper ────────────────────────────────────────────────────

class TestHighRiskModuleHelper:
    def test_checkout_is_high_risk(self):
        assert _is_high_risk_module("checkout") is True

    def test_auth_is_high_risk(self):
        assert _is_high_risk_module("auth") is True

    def test_login_is_high_risk(self):
        assert _is_high_risk_module("login") is True

    def test_payment_is_high_risk(self):
        assert _is_high_risk_module("payment-service") is True

    def test_order_is_high_risk(self):
        assert _is_high_risk_module("order-management") is True

    def test_search_is_not_high_risk(self):
        assert _is_high_risk_module("search") is False

    def test_nav_is_not_high_risk(self):
        assert _is_high_risk_module("navigation") is False


# ── Basic selection ────────────────────────────────────────────────────────────

class TestBasicSelection:
    def setup_method(self):
        _reset()

    def test_empty_catalog_returns_empty(self):
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert result.total_selected == 0
        assert result.selected_tests == []

    def test_module_match_selects_test(self):
        _add_test("TC-RS-001", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert "TC-RS-001" in ids

    def test_no_module_match_excludes_test(self):
        _add_test("TC-RS-002", "search")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert "TC-RS-002" not in ids

    def test_no_changed_modules_selects_all(self):
        _add_test("TC-RS-003", "auth")
        _add_test("TC-RS-004", "search")
        _add_test("TC-RS-005", "checkout")
        result = _svc().select(RiskSelectionRequest())
        assert result.total_selected == 3

    def test_result_is_risk_selection_result(self):
        _add_test("TC-RS-006", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert isinstance(result, RiskSelectionResult)

    def test_selected_tests_are_selected_test_instances(self):
        _add_test("TC-RS-007", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        for t in result.selected_tests:
            assert isinstance(t, SelectedTest)

    def test_total_selected_matches_list_length(self):
        _add_test("TC-RS-008", "auth")
        _add_test("TC-RS-009", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert result.total_selected == len(result.selected_tests)

    def test_reasoning_is_non_empty_string(self):
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert isinstance(result.reasoning, str) and result.reasoning


# ── Priority ordering ─────────────────────────────────────────────────────────

class TestPriorityOrdering:
    def setup_method(self):
        _reset()

    def test_critical_before_low(self):
        _add_test("TC-PO-001", "auth", priority="low")
        _add_test("TC-PO-002", "auth", priority="critical")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert ids.index("TC-PO-002") < ids.index("TC-PO-001")

    def test_critical_before_high(self):
        _add_test("TC-PO-003", "checkout", priority="high")
        _add_test("TC-PO-004", "checkout", priority="critical")
        result = _svc().select(RiskSelectionRequest(changed_modules=["checkout"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert ids.index("TC-PO-004") < ids.index("TC-PO-003")

    def test_high_before_medium(self):
        _add_test("TC-PO-005", "auth", priority="medium")
        _add_test("TC-PO-006", "auth", priority="high")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert ids.index("TC-PO-006") < ids.index("TC-PO-005")

    def test_medium_before_low(self):
        _add_test("TC-PO-007", "search")
        _add_test("TC-PO-008", "search", priority="low")
        result = _svc().select(RiskSelectionRequest())  # no module filter → all
        ids = [t.test_case_id for t in result.selected_tests]
        assert ids.index("TC-PO-007") < ids.index("TC-PO-008")

    def test_selected_test_carries_priority(self):
        _add_test("TC-PO-009", "auth", priority="critical")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        tc = result.selected_tests[0]
        assert tc.priority == "critical"


# ── Selection score ───────────────────────────────────────────────────────────

class TestSelectionScore:
    def setup_method(self):
        _reset()

    def test_module_match_increases_score(self):
        _add_test("TC-SC-001", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert result.selected_tests[0].selection_score >= 10  # MODULE_MATCH_BONUS

    def test_critical_priority_adds_score(self):
        _add_test("TC-SC-002", "auth", priority="critical")
        _add_test("TC-SC-003", "auth", priority="low")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        scores = {t.test_case_id: t.selection_score for t in result.selected_tests}
        assert scores["TC-SC-002"] > scores["TC-SC-003"]

    def test_recent_failure_increases_score(self):
        _add_test("TC-SC-004", "auth")
        _add_test("TC-SC-005", "auth")
        _add_run("TC-SC-004", "fail")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        scores = {t.test_case_id: t.selection_score for t in result.selected_tests}
        assert scores["TC-SC-004"] > scores["TC-SC-005"]

    def test_error_run_counts_as_failure(self):
        _add_test("TC-SC-006", "auth")
        _add_run("TC-SC-006", "error")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        tc = next(t for t in result.selected_tests if t.test_case_id == "TC-SC-006")
        # Should have failure bonus
        assert tc.selection_score > 10

    def test_passed_run_does_not_add_failure_bonus(self):
        _add_test("TC-SC-007", "auth")
        _add_test("TC-SC-008", "auth")
        _add_run("TC-SC-007", "pass")
        # no run for SC-008
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        scores = {t.test_case_id: t.selection_score for t in result.selected_tests}
        # Both module-matched, both should have equal score (no failure bonus for either)
        assert scores["TC-SC-007"] == scores["TC-SC-008"]

    def test_checkout_module_gets_high_risk_bonus(self):
        _add_test("TC-SC-009", "checkout")
        _add_test("TC-SC-010", "search")
        result = _svc().select(RiskSelectionRequest())
        scores = {t.test_case_id: t.selection_score for t in result.selected_tests}
        assert scores["TC-SC-009"] > scores["TC-SC-010"]

    def test_smoke_type_gets_type_bonus(self):
        _add_test("TC-SC-011", "auth", type_="smoke")
        _add_test("TC-SC-012", "auth", type_="e2e")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        scores = {t.test_case_id: t.selection_score for t in result.selected_tests}
        assert scores["TC-SC-011"] > scores["TC-SC-012"]

    def test_score_is_non_negative(self):
        _add_test("TC-SC-013", "nav")
        result = _svc().select(RiskSelectionRequest())
        for t in result.selected_tests:
            assert t.selection_score >= 0


# ── Max tests limit ───────────────────────────────────────────────────────────

class TestMaxTestsLimit:
    def setup_method(self):
        _reset()

    def test_max_tests_limits_results(self):
        for i in range(10):
            _add_test(f"TC-MAX-{i:03d}", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"], max_tests=3))
        assert result.total_selected == 3
        assert len(result.selected_tests) == 3

    def test_default_max_is_100(self):
        req = RiskSelectionRequest()
        assert req.max_tests == 100

    def test_returns_all_when_fewer_than_max(self):
        for i in range(5):
            _add_test(f"TC-FEW-{i:03d}", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"], max_tests=20))
        assert result.total_selected == 5

    def test_max_tests_one_returns_highest_scored(self):
        _add_test("TC-ONE-001", "auth", priority="low")
        _add_test("TC-ONE-002", "auth", priority="critical")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"], max_tests=1))
        assert result.total_selected == 1
        assert result.selected_tests[0].test_case_id == "TC-ONE-002"


# ── Priority filter ───────────────────────────────────────────────────────────

class TestPriorityFilter:
    def setup_method(self):
        _reset()

    def test_priority_filter_excludes_other_priorities(self):
        _add_test("TC-PF-001", "auth", priority="critical")
        _add_test("TC-PF-002", "auth", priority="low")
        result = _svc().select(RiskSelectionRequest(
            changed_modules=["auth"], priority="critical"
        ))
        ids = [t.test_case_id for t in result.selected_tests]
        assert "TC-PF-001" in ids
        assert "TC-PF-002" not in ids

    def test_priority_filter_high_only(self):
        _add_test("TC-PF-003", "search", priority="high")
        _add_test("TC-PF-004", "search", priority="medium")
        result = _svc().select(RiskSelectionRequest(priority="high"))
        ids = [t.test_case_id for t in result.selected_tests]
        assert "TC-PF-003" in ids
        assert "TC-PF-004" not in ids

    def test_no_priority_filter_includes_all_priorities(self):
        _add_test("TC-PF-005", "auth", priority="critical")
        _add_test("TC-PF-006", "auth", priority="low")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert "TC-PF-005" in ids
        assert "TC-PF-006" in ids

    def test_priority_filter_no_match_returns_empty(self):
        _add_test("TC-PF-007", "auth", priority="low")
        result = _svc().select(RiskSelectionRequest(
            changed_modules=["auth"], priority="critical"
        ))
        assert result.total_selected == 0


# ── Selection reasons ─────────────────────────────────────────────────────────

class TestSelectionReasons:
    def setup_method(self):
        _reset()

    def test_module_match_appears_in_reason(self):
        _add_test("TC-RN-001", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        tc = result.selected_tests[0]
        assert "auth" in tc.selection_reason.lower() or "match" in tc.selection_reason.lower()

    def test_recent_failure_appears_in_reason(self):
        _add_test("TC-RN-002", "auth")
        _add_run("TC-RN-002", "fail")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        tc = result.selected_tests[0]
        assert "fail" in tc.selection_reason.lower()

    def test_reason_is_non_empty_string(self):
        _add_test("TC-RN-003", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        for t in result.selected_tests:
            assert isinstance(t.selection_reason, str) and t.selection_reason

    def test_no_module_match_uses_general_reason(self):
        _add_test("TC-RN-004", "search")
        result = _svc().select(RiskSelectionRequest())  # no module filter
        tc = next(t for t in result.selected_tests if t.test_case_id == "TC-RN-004")
        assert tc.selection_reason  # non-empty


# ── SelectedTest fields ───────────────────────────────────────────────────────

class TestSelectedTestFields:
    def setup_method(self):
        _reset()

    def test_selected_test_has_test_case_id(self):
        _add_test("TC-FLD-001", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert result.selected_tests[0].test_case_id == "TC-FLD-001"

    def test_selected_test_has_module(self):
        _add_test("TC-FLD-002", "checkout")
        result = _svc().select(RiskSelectionRequest(changed_modules=["checkout"]))
        assert result.selected_tests[0].module == "checkout"

    def test_selected_test_has_name(self):
        _add_test("TC-FLD-003", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert result.selected_tests[0].name == "Test TC-FLD-003"

    def test_selected_test_has_type(self):
        _add_test("TC-FLD-004", "auth", type_="regression")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert result.selected_tests[0].type == "regression"

    def test_selected_test_has_priority(self):
        _add_test("TC-FLD-005", "auth", priority="high")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert result.selected_tests[0].priority == "high"


# ── Reasoning summary ─────────────────────────────────────────────────────────

class TestReasoningSummary:
    def setup_method(self):
        _reset()

    def test_empty_selection_has_no_match_message(self):
        result = _svc().select(RiskSelectionRequest(changed_modules=["nonexistent-module"]))
        assert "no test" in result.reasoning.lower() or "no" in result.reasoning.lower()

    def test_reasoning_mentions_selected_count(self):
        _add_test("TC-RS2-001", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        assert "1" in result.reasoning

    def test_reasoning_mentions_changed_modules(self):
        _add_test("TC-RS2-002", "checkout")
        result = _svc().select(RiskSelectionRequest(changed_modules=["checkout"]))
        assert "checkout" in result.reasoning.lower()

    def test_reasoning_mentions_cap_when_limited(self):
        for i in range(10):
            _add_test(f"TC-CAP-{i:03d}", "auth")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"], max_tests=2))
        assert "cap" in result.reasoning.lower() or "2" in result.reasoning


# ── Multiple changed modules ───────────────────────────────────────────────────

class TestMultipleChangedModules:
    def setup_method(self):
        _reset()

    def test_two_changed_modules_both_selected(self):
        _add_test("TC-MM-001", "auth")
        _add_test("TC-MM-002", "checkout")
        _add_test("TC-MM-003", "search")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth", "checkout"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert "TC-MM-001" in ids
        assert "TC-MM-002" in ids
        assert "TC-MM-003" not in ids

    def test_empty_changed_modules_selects_all(self):
        _add_test("TC-MM-004", "auth")
        _add_test("TC-MM-005", "search")
        result = _svc().select(RiskSelectionRequest(changed_modules=[]))
        assert result.total_selected == 2

    def test_partial_module_name_matches(self):
        # "auth" in changed_modules should match "auth-service" module
        _add_test("TC-MM-006", "auth-service")
        result = _svc().select(RiskSelectionRequest(changed_modules=["auth"]))
        ids = [t.test_case_id for t in result.selected_tests]
        assert "TC-MM-006" in ids
