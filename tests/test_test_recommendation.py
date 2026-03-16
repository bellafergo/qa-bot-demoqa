# tests/test_test_recommendation.py
"""
Tests for services/test_recommendation.py

Run: .venv/bin/python -m pytest tests/test_test_recommendation.py -v
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.test_recommendation import recommend_tests, FAILURE_THRESHOLD


# ── Builders ───────────────────────────────────────────────────────────────────

def _test(test_id, module="checkout", priority="medium"):
    return {"test_id": test_id, "module": module, "priority": priority}


def _run(test_id, status="failed", module=None):
    r = {"test_case_id": test_id, "status": status}
    if module:
        r["module"] = module
    return r


def _failed_runs(test_id, count, module=None):
    return [_run(test_id, module=module) for _ in range(count)]


# ══════════════════════════════════════════════════════════════
# 1. Output contract
# ══════════════════════════════════════════════════════════════

class TestContract:

    def test_returns_list(self):
        assert isinstance(recommend_tests([], [], []), list)

    def test_empty_inputs_returns_empty(self):
        assert recommend_tests([], [], []) == []

    def test_empty_catalog_returns_empty(self):
        runs = [_run("checkout_flow")]
        assert recommend_tests(["checkout"], runs, []) == []

    def test_empty_runs_no_historical(self):
        catalog = [_test("payment_flow", module="payment")]
        assert recommend_tests(["checkout"], [], catalog) == []

    def test_rec_has_required_keys(self):
        catalog = [_test("checkout_flow", module="checkout")]
        result = recommend_tests(["checkout"], [], catalog)
        assert len(result) == 1
        for key in ("test_id", "reason", "priority"):
            assert key in result[0]

    def test_none_inputs_safe(self):
        assert recommend_tests(None, None, None) == []


# ══════════════════════════════════════════════════════════════
# 2. module_changed rule
# ══════════════════════════════════════════════════════════════

class TestModuleChanged:

    def test_test_in_changed_module_recommended(self):
        catalog = [_test("checkout_flow", module="checkout")]
        result = recommend_tests(["checkout"], [], catalog)
        assert len(result) == 1
        assert result[0]["test_id"] == "checkout_flow"
        assert result[0]["reason"] == "module_changed"

    def test_test_not_in_changed_module_excluded(self):
        catalog = [_test("payment_flow", module="payment")]
        assert recommend_tests(["checkout"], [], catalog) == []

    def test_multiple_tests_same_module_all_included(self):
        catalog = [
            _test("checkout_flow", module="checkout"),
            _test("checkout_edge", module="checkout"),
        ]
        result = recommend_tests(["checkout"], [], catalog)
        assert len(result) == 2
        assert all(r["reason"] == "module_changed" for r in result)

    def test_multiple_changed_modules(self):
        catalog = [
            _test("checkout_flow", module="checkout"),
            _test("payment_flow",  module="payment"),
            _test("login_flow",    module="auth"),
        ]
        result = recommend_tests(["checkout", "payment"], [], catalog)
        ids = {r["test_id"] for r in result}
        assert ids == {"checkout_flow", "payment_flow"}

    def test_module_match_is_case_insensitive(self):
        catalog = [_test("checkout_flow", module="Checkout")]
        result = recommend_tests(["checkout"], [], catalog)
        assert len(result) == 1

    def test_priority_preserved_when_no_module_failures(self):
        catalog = [_test("checkout_flow", module="checkout", priority="high")]
        result = recommend_tests(["checkout"], [], catalog)
        assert result[0]["priority"] == "high"


# ══════════════════════════════════════════════════════════════
# 3. historical_failure rule
# ══════════════════════════════════════════════════════════════

class TestHistoricalFailure:

    def test_frequent_failure_recommended(self):
        catalog = [_test("payment_flow", module="payment")]
        runs = _failed_runs("payment_flow", FAILURE_THRESHOLD)
        result = recommend_tests([], runs, catalog)
        assert len(result) == 1
        assert result[0]["test_id"] == "payment_flow"
        assert result[0]["reason"] == "historical_failure"

    def test_below_threshold_not_recommended(self):
        catalog = [_test("payment_flow", module="payment")]
        runs = _failed_runs("payment_flow", FAILURE_THRESHOLD - 1)
        assert recommend_tests([], runs, catalog) == []

    def test_exactly_threshold_recommended(self):
        catalog = [_test("t1", module="mod")]
        runs = _failed_runs("t1", FAILURE_THRESHOLD)
        assert len(recommend_tests([], runs, catalog)) == 1

    def test_passing_runs_not_counted(self):
        catalog = [_test("t1", module="mod")]
        runs = [_run("t1", status="passed")] * FAILURE_THRESHOLD
        assert recommend_tests([], runs, catalog) == []

    def test_historical_test_not_in_catalog_excluded(self):
        runs = _failed_runs("ghost_test", FAILURE_THRESHOLD)
        assert recommend_tests([], runs, []) == []

    def test_priority_preserved_for_historical_failure(self):
        catalog = [_test("t1", module="mod", priority="low")]
        runs = _failed_runs("t1", FAILURE_THRESHOLD)
        result = recommend_tests([], runs, catalog)
        assert result[0]["priority"] == "low"


# ══════════════════════════════════════════════════════════════
# 4. Priority bump (module_changed + failed module)
# ══════════════════════════════════════════════════════════════

class TestPriorityBump:

    def _bumped(self, base_priority):
        catalog = [_test("checkout_flow", module="checkout", priority=base_priority)]
        runs = [_run("checkout_flow", status="failed")]
        return recommend_tests(["checkout"], runs, catalog)[0]["priority"]

    def test_low_bumped_to_medium(self):
        assert self._bumped("low") == "medium"

    def test_medium_bumped_to_high(self):
        assert self._bumped("medium") == "high"

    def test_high_stays_high(self):
        assert self._bumped("high") == "high"

    def test_no_bump_without_module_failures(self):
        catalog = [_test("checkout_flow", module="checkout", priority="low")]
        result = recommend_tests(["checkout"], [], catalog)
        assert result[0]["priority"] == "low"

    def test_historical_failure_not_bumped(self):
        """historical_failure reason does not trigger priority bump."""
        catalog = [_test("t1", module="payment", priority="low")]
        runs = _failed_runs("t1", FAILURE_THRESHOLD)
        result = recommend_tests([], runs, catalog)
        assert result[0]["priority"] == "low"

    def test_bump_resolves_module_from_catalog_when_run_lacks_module_field(self):
        """Run has no module field — module resolved via catalog index."""
        catalog = [_test("checkout_flow", module="checkout", priority="low")]
        runs = [_run("checkout_flow", status="failed")]   # no module key
        result = recommend_tests(["checkout"], runs, catalog)
        assert result[0]["priority"] == "medium"


# ══════════════════════════════════════════════════════════════
# 5. Both rules apply — deduplication and precedence
# ══════════════════════════════════════════════════════════════

class TestBothRules:

    def test_module_changed_takes_precedence(self):
        catalog = [_test("checkout_flow", module="checkout")]
        runs = _failed_runs("checkout_flow", FAILURE_THRESHOLD)
        result = recommend_tests(["checkout"], runs, catalog)
        assert len(result) == 1
        assert result[0]["reason"] == "module_changed"

    def test_no_duplicate_when_both_rules_match(self):
        catalog = [_test("checkout_flow", module="checkout")]
        runs = _failed_runs("checkout_flow", FAILURE_THRESHOLD)
        assert len(recommend_tests(["checkout"], runs, catalog)) == 1

    def test_duplicate_catalog_entries_deduplicated(self):
        catalog = [
            _test("checkout_flow", module="checkout"),
            _test("checkout_flow", module="checkout"),   # duplicate
        ]
        assert len(recommend_tests(["checkout"], [], catalog)) == 1


# ══════════════════════════════════════════════════════════════
# 6. Priority ordering
# ══════════════════════════════════════════════════════════════

class TestPriorityOrdering:

    def test_sorted_high_medium_low(self):
        catalog = [
            _test("low_test",    module="checkout", priority="low"),
            _test("medium_test", module="checkout", priority="medium"),
            _test("high_test",   module="checkout", priority="high"),
        ]
        result = recommend_tests(["checkout"], [], catalog)
        assert [r["priority"] for r in result] == ["high", "medium", "low"]

    def test_all_high_preserved(self):
        catalog = [_test(f"t{i}", module="checkout", priority="high") for i in range(3)]
        result = recommend_tests(["checkout"], [], catalog)
        assert all(r["priority"] == "high" for r in result)

    def test_missing_priority_defaults_to_medium(self):
        catalog = [{"test_id": "t1", "module": "checkout"}]
        result = recommend_tests(["checkout"], [], catalog)
        assert result[0]["priority"] == "medium"

    def test_unknown_priority_defaults_to_medium(self):
        catalog = [_test("t1", module="checkout", priority="critical")]
        result = recommend_tests(["checkout"], [], catalog)
        assert result[0]["priority"] == "medium"

    def test_mixed_reasons_sorted_by_priority(self):
        catalog = [
            _test("hist_low",   module="other",    priority="low"),
            _test("mod_medium", module="checkout", priority="medium"),
            _test("hist_high",  module="other2",   priority="high"),
        ]
        runs = (
            _failed_runs("hist_low",  FAILURE_THRESHOLD) +
            _failed_runs("hist_high", FAILURE_THRESHOLD)
        )
        result = recommend_tests(["checkout"], runs, catalog)
        priorities = [r["priority"] for r in result]
        order = {"high": 0, "medium": 1, "low": 2}
        assert priorities == sorted(priorities, key=lambda p: order[p])


# ══════════════════════════════════════════════════════════════
# 7. Edge cases / robustness
# ══════════════════════════════════════════════════════════════

class TestEdgeCases:

    def test_non_dict_catalog_items_skipped(self):
        catalog = [None, "bad", 42, _test("checkout_flow", module="checkout")]
        result = recommend_tests(["checkout"], [], catalog)
        assert len(result) == 1

    def test_non_dict_run_items_skipped(self):
        catalog = [_test("t1", module="mod")]
        runs = [None, "bad", _run("t1")] * FAILURE_THRESHOLD
        result = recommend_tests([], runs, catalog)
        assert len(result) == 1

    def test_empty_changed_modules_no_module_changed_recs(self):
        catalog = [_test("checkout_flow", module="checkout")]
        assert recommend_tests([], [], catalog) == []

    def test_run_with_test_id_field_instead_of_test_case_id(self):
        catalog = [_test("t1", module="mod")]
        runs = [{"test_id": "t1", "status": "failed"} for _ in range(FAILURE_THRESHOLD)]
        assert len(recommend_tests([], runs, catalog)) == 1

    def test_catalog_item_without_module_not_matched(self):
        catalog = [{"test_id": "t1"}]   # no module field
        assert recommend_tests(["checkout"], [], catalog) == []
