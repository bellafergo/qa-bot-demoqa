# tests/test_module_canonical.py
"""QA-03A — canonical module keys for aggregation (no persistence changes)."""
from __future__ import annotations

import pytest

from services.module_canonical import (
    build_module_label_map,
    canonical_module_key,
    canonical_module_keys,
    merge_module_counts,
    merge_module_stats_dicts,
    module_display_label,
)


class TestCanonicalModuleKey:
    def test_auth_variants_same_key(self):
        assert canonical_module_key("AUTH") == canonical_module_key("auth") == canonical_module_key("Auth")

    def test_empty_becomes_unknown(self):
        assert canonical_module_key("") == "unknown"
        assert canonical_module_key(None) == "unknown"

    def test_strips_and_folds_accents(self):
        assert canonical_module_key("  Autenticación  ") == "autenticacion"

    def test_canonical_module_keys_dedupes_preserving_order(self):
        assert canonical_module_keys(["AUTH", "checkout", "auth", "Auth"]) == ["auth", "checkout"]


class TestModuleDisplayLabel:
    def test_prefers_first_catalog_spelling(self):
        labels = build_module_label_map(["AUTH", "auth", "Auth"])
        assert module_display_label("auth", labels_by_key=labels) == "AUTH"
        assert module_display_label("AUTH", labels_by_key=labels) == "AUTH"

    def test_no_upper_for_visible_labels(self):
        labels = build_module_label_map(["Auth"])
        assert module_display_label("auth", labels_by_key=labels) == "Auth"
        assert module_display_label("auth", labels_by_key=labels) != "AUTH"

    def test_fallback_returns_raw_when_no_catalog(self):
        assert module_display_label("user_profile") == "user_profile"


class TestMergeHelpers:
    def test_merge_module_counts_sums_variants(self):
        merged, label_map = merge_module_counts({"AUTH": 2, "auth": 1, "Auth": 3})
        assert merged == {"auth": 6}
        assert label_map["auth"] == "AUTH"

    def test_merge_module_stats_dicts_combines_numeric_fields(self):
        stats = {
            "AUTH": {"test_count": 2, "failure_count": 1, "regression_count": 1},
            "auth": {"test_count": 1, "failure_count": 2},
            "Auth": {"test_count": 3, "flaky_count": 1},
        }
        merged, label_map = merge_module_stats_dicts(stats)
        assert len(merged) == 1
        bucket = merged["auth"]
        assert bucket["test_count"] == 6
        assert bucket["failure_count"] == 3
        assert bucket["regression_count"] == 1
        assert bucket["flaky_count"] == 1
        assert label_map["auth"] == "AUTH"
