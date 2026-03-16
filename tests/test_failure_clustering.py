# tests/test_failure_clustering.py
"""
Tests for services/failure_clustering.py

Run: .venv/bin/python -m pytest tests/test_failure_clustering.py -v
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.failure_clustering import cluster_failures


# ── Builders ──────────────────────────────────────────────────────────────────

def _run(evidence_id, failure_type, target=None, status="failed"):
    fa = {"failure_type": failure_type, "target": target, "layer": "resolver", "confidence": "high"}
    return {"evidence_id": evidence_id, "status": status, "failure_analysis": fa}


def _run_no_fa(evidence_id, status="failed"):
    return {"evidence_id": evidence_id, "status": status}


# ══════════════════════════════════════════════════════════════
# 1. Output contract
# ══════════════════════════════════════════════════════════════

class TestContract:

    def test_returns_list(self):
        assert isinstance(cluster_failures([]), list)

    def test_empty_input_returns_empty(self):
        assert cluster_failures([]) == []

    def test_cluster_has_required_keys(self):
        runs = [_run("EV-1", "selector_not_found", "button(login)")]
        cluster = cluster_failures(runs)[0]
        for key in ("cluster_id", "failure_type", "target", "count", "runs"):
            assert key in cluster, f"missing key: {key}"

    def test_cluster_id_format_with_target(self):
        runs = [_run("EV-1", "selector_not_found", "button(login)")]
        c = cluster_failures(runs)[0]
        assert c["cluster_id"] == "selector_not_found:button(login)"

    def test_cluster_id_format_without_target(self):
        runs = [_run("EV-1", "timeout", target=None)]
        c = cluster_failures(runs)[0]
        assert c["cluster_id"] == "timeout"

    def test_count_matches_runs_length(self):
        runs = [_run(f"EV-{i}", "timeout") for i in range(4)]
        c = cluster_failures(runs)[0]
        assert c["count"] == len(c["runs"])


# ══════════════════════════════════════════════════════════════
# 2. Multiple selector_not_found — same target grouped together
# ══════════════════════════════════════════════════════════════

class TestSelectorNotFoundClustering:

    def test_three_same_target_one_cluster(self):
        runs = [_run(f"EV-{i}", "selector_not_found", "button(login)") for i in range(3)]
        clusters = cluster_failures(runs)
        assert len(clusters) == 1
        assert clusters[0]["count"] == 3

    def test_cluster_contains_all_evidence_ids(self):
        runs = [_run(f"EV-{i}", "selector_not_found", "button(login)") for i in range(3)]
        clusters = cluster_failures(runs)
        assert sorted(clusters[0]["runs"]) == ["EV-0", "EV-1", "EV-2"]

    def test_different_targets_different_clusters(self):
        runs = [
            _run("EV-1", "selector_not_found", "button(login)"),
            _run("EV-2", "selector_not_found", "input(username)"),
            _run("EV-3", "selector_not_found", "button(login)"),
        ]
        clusters = cluster_failures(runs)
        assert len(clusters) == 2

    def test_cluster_failure_type_and_target_preserved(self):
        runs = [_run(f"EV-{i}", "selector_not_found", "button(login)") for i in range(2)]
        c = cluster_failures(runs)[0]
        assert c["failure_type"] == "selector_not_found"
        assert c["target"] == "button(login)"


# ══════════════════════════════════════════════════════════════
# 3. assertion_failed — distinct targets produce distinct clusters
# ══════════════════════════════════════════════════════════════

class TestAssertionFailedClustering:

    def test_distinct_targets_distinct_clusters(self):
        runs = [
            _run("EV-1", "assertion_failed", "assert_text_contains(body)"),
            _run("EV-2", "assertion_failed", "assert_visible(#user-name)"),
            _run("EV-3", "assertion_failed", "assert_text_contains(body)"),
        ]
        clusters = cluster_failures(runs)
        assert len(clusters) == 2

    def test_majority_cluster_first_after_sort(self):
        runs = [
            _run("EV-1", "assertion_failed", "assert_text_contains(body)"),
            _run("EV-2", "assertion_failed", "assert_text_contains(body)"),
            _run("EV-3", "assertion_failed", "assert_visible(#user-name)"),
        ]
        clusters = cluster_failures(runs)
        assert clusters[0]["count"] == 2
        assert clusters[0]["target"] == "assert_text_contains(body)"


# ══════════════════════════════════════════════════════════════
# 4. Mixed failure types
# ══════════════════════════════════════════════════════════════

class TestMixedTypes:

    def test_mixed_types_separate_clusters(self):
        runs = [
            _run("EV-1", "selector_not_found", "button(login)"),
            _run("EV-2", "assertion_failed",   "assert_visible(#user-name)"),
            _run("EV-3", "timeout"),
            _run("EV-4", "navigation_failed"),
            _run("EV-5", "selector_not_found", "button(login)"),
        ]
        clusters = cluster_failures(runs)
        # selector_not_found:button(login)=2, others=1 each
        assert len(clusters) == 4

    def test_largest_cluster_first(self):
        runs = [
            _run("EV-1", "selector_not_found", "button(login)"),
            _run("EV-2", "selector_not_found", "button(login)"),
            _run("EV-3", "selector_not_found", "button(login)"),
            _run("EV-4", "assertion_failed",   "assert_visible(#user-name)"),
            _run("EV-5", "timeout"),
        ]
        clusters = cluster_failures(runs)
        assert clusters[0]["failure_type"] == "selector_not_found"
        assert clusters[0]["count"] == 3

    def test_all_unique_types_all_separate(self):
        types = ["selector_not_found", "assertion_failed", "timeout", "navigation_failed", "unknown"]
        runs = [_run(f"EV-{i}", t) for i, t in enumerate(types)]
        clusters = cluster_failures(runs)
        assert len(clusters) == len(types)


# ══════════════════════════════════════════════════════════════
# 5. Runs without target
# ══════════════════════════════════════════════════════════════

class TestNoTarget:

    def test_no_target_cluster_id_is_type_only(self):
        runs = [_run("EV-1", "timeout", target=None)]
        c = cluster_failures(runs)[0]
        assert c["cluster_id"] == "timeout"
        assert c["target"] is None

    def test_no_target_multiple_grouped_together(self):
        runs = [_run(f"EV-{i}", "timeout", target=None) for i in range(4)]
        clusters = cluster_failures(runs)
        assert len(clusters) == 1
        assert clusters[0]["count"] == 4

    def test_same_type_with_and_without_target_separate_clusters(self):
        runs = [
            _run("EV-1", "selector_not_found", "button(login)"),
            _run("EV-2", "selector_not_found", target=None),
        ]
        clusters = cluster_failures(runs)
        assert len(clusters) == 2

    def test_empty_string_target_treated_as_none(self):
        """An empty-string target should be treated as no target."""
        fa = {"failure_type": "timeout", "target": "", "layer": "runner", "confidence": "high"}
        run = {"evidence_id": "EV-1", "status": "failed", "failure_analysis": fa}
        c = cluster_failures([run])[0]
        assert c["target"] is None
        assert c["cluster_id"] == "timeout"


# ══════════════════════════════════════════════════════════════
# 6. Ordering by frequency
# ══════════════════════════════════════════════════════════════

class TestOrdering:

    def test_sorted_by_count_descending(self):
        runs = (
            [_run(f"EV-A{i}", "selector_not_found", "button(login)") for i in range(5)]
            + [_run(f"EV-B{i}", "assertion_failed",  "assert_visible(#user-name)") for i in range(3)]
            + [_run(f"EV-C{i}", "timeout") for i in range(1)]
        )
        clusters = cluster_failures(runs)
        counts = [c["count"] for c in clusters]
        assert counts == sorted(counts, reverse=True)

    def test_single_run_cluster_count_one(self):
        runs = [_run("EV-1", "navigation_failed", "goto(https://example.com)")]
        c = cluster_failures(runs)[0]
        assert c["count"] == 1
        assert c["runs"] == ["EV-1"]

    def test_tie_both_appear_in_result(self):
        runs = [
            _run("EV-1", "timeout"),
            _run("EV-2", "navigation_failed"),
        ]
        clusters = cluster_failures(runs)
        assert len(clusters) == 2
        assert all(c["count"] == 1 for c in clusters)


# ══════════════════════════════════════════════════════════════
# 7. Edge cases / robustness
# ══════════════════════════════════════════════════════════════

class TestEdgeCases:

    def test_runs_without_failure_analysis_skipped(self):
        runs = [
            _run_no_fa("EV-1"),
            _run("EV-2", "timeout"),
        ]
        clusters = cluster_failures(runs)
        assert len(clusters) == 1
        assert clusters[0]["runs"] == ["EV-2"]

    def test_all_runs_without_failure_analysis_empty_result(self):
        runs = [_run_no_fa(f"EV-{i}") for i in range(5)]
        assert cluster_failures(runs) == []

    def test_non_dict_items_in_list_skipped(self):
        runs = [None, "bad", 42, _run("EV-1", "timeout")]
        clusters = cluster_failures(runs)
        assert len(clusters) == 1

    def test_none_input_returns_empty(self):
        assert cluster_failures(None) == []

    def test_evidence_id_missing_run_not_added_to_runs_list(self):
        """A run with no evidence_id contributes nothing to the runs list."""
        fa = {"failure_type": "timeout", "target": None, "layer": "runner", "confidence": "high"}
        run = {"status": "failed", "failure_analysis": fa}   # no evidence_id
        c = cluster_failures([run])[0]
        assert c["runs"] == []   # nothing to record
        assert c["count"] == 0   # count reflects only identifiable runs

    def test_duplicate_evidence_ids_both_recorded(self):
        """Same evidence_id appearing twice (re-runs) are both listed."""
        runs = [
            _run("EV-1", "timeout"),
            _run("EV-1", "timeout"),
        ]
        c = cluster_failures(runs)[0]
        assert c["count"] == 2
        assert c["runs"] == ["EV-1", "EV-1"]


# ══════════════════════════════════════════════════════════════
# 8. get_recent_failure_clusters smoke test
# ══════════════════════════════════════════════════════════════

class TestGetRecentFailureClusters:

    def test_importable_and_callable(self):
        from services.failure_clustering import get_recent_failure_clusters
        result = get_recent_failure_clusters(limit=1)
        assert isinstance(result, list)

    def test_returns_list_even_with_no_failed_runs(self):
        from services.failure_clustering import get_recent_failure_clusters
        result = get_recent_failure_clusters(limit=0)
        assert isinstance(result, list)
