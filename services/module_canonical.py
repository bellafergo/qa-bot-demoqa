# services/module_canonical.py
"""
Canonical module keys for aggregation and metrics (QA-03A).

Separates stable lowercase keys (``auth``) from display labels (``Auth``).
Does not mutate persisted catalog or report snapshots.
"""
from __future__ import annotations

from collections import defaultdict
from typing import Any, Dict, Iterable, List, Tuple

from services.module_aliases import _strip_accents

UNKNOWN_MODULE = "unknown"

_STAT_SUM_KEYS = (
    "regression_count",
    "failure_count",
    "flaky_count",
    "incident_count",
    "recent_failure_count",
    "pass_total",
    "run_total",
)


def canonical_module_key(name: str | None) -> str:
    """Stable lowercase slug for aggregation keys and joins."""
    raw = (name or "").strip()
    if not raw:
        return UNKNOWN_MODULE
    folded = _strip_accents(raw.lower())
    return folded or UNKNOWN_MODULE


def canonical_module_keys(names: Iterable[str | None]) -> List[str]:
    """Dedupe module names to canonical keys, preserving first-seen order."""
    out: List[str] = []
    seen: set[str] = set()
    for name in names:
        key = canonical_module_key(name)
        if key not in seen:
            seen.add(key)
            out.append(key)
    return out


def build_module_label_map(catalog_modules: Iterable[str | None]) -> Dict[str, str]:
    """Map canonical key → preferred display label (stable first spelling wins)."""
    out: Dict[str, str] = {}
    raw_list = [
        str(raw).strip()
        for raw in catalog_modules
        if raw is not None and str(raw).strip()
    ]
    for raw_s in sorted(raw_list, key=lambda m: (canonical_module_key(m), m.lower(), m)):
        key = canonical_module_key(raw_s)
        if key not in out:
            out[key] = raw_s
    return out


def module_display_label(
    name: str | None,
    *,
    labels_by_key: Dict[str, str] | None = None,
) -> str:
    """Human-facing label; prefers catalog spelling when available."""
    key = canonical_module_key(name)
    if labels_by_key and key in labels_by_key:
        return labels_by_key[key]
    raw = (name or "").strip()
    if raw and canonical_module_key(raw) == key:
        return raw
    if key == UNKNOWN_MODULE:
        return UNKNOWN_MODULE
    return key.replace("_", " ").replace("-", " ").title()


def merge_module_counts(raw_counts: Dict[str, int]) -> Tuple[Dict[str, int], Dict[str, str]]:
    """Sum integer counts by canonical module key."""
    label_map = build_module_label_map(raw_counts.keys())
    merged: Dict[str, int] = defaultdict(int)
    for raw_mod, count in raw_counts.items():
        merged[canonical_module_key(raw_mod)] += int(count or 0)
    return dict(merged), label_map


def merge_module_stats_dicts(
    stats_by_module: Dict[str, Dict[str, Any]],
    *,
    extra_labels: Iterable[str | None] | None = None,
) -> Tuple[Dict[str, Dict[str, Any]], Dict[str, str]]:
    """Merge per-module stat buckets by canonical key (numeric fields combined)."""
    label_sources: List[str] = list(stats_by_module.keys())
    if extra_labels:
        label_sources.extend(str(x) for x in extra_labels if x is not None)
    label_map = build_module_label_map(label_sources)

    merged: Dict[str, Dict[str, Any]] = {}
    for raw_mod, stats in stats_by_module.items():
        key = canonical_module_key(raw_mod)
        bucket = merged.setdefault(key, _empty_module_stats())
        bucket["test_count"] = int(bucket.get("test_count") or 0) + int(stats.get("test_count") or 0)
        for field in _STAT_SUM_KEYS:
            bucket[field] = int(bucket.get(field) or 0) + int(stats.get(field) or 0)
        bucket["incident_severity_sum"] = float(bucket.get("incident_severity_sum") or 0.0) + float(
            stats.get("incident_severity_sum") or 0.0
        )
        if stats.get("pass_rate") is not None and bucket.get("run_total", 0) == 0:
            bucket["pass_rate"] = stats.get("pass_rate")

    for bucket in merged.values():
        total = int(bucket.get("run_total") or 0)
        if total > 0:
            bucket["pass_rate"] = round(int(bucket.get("pass_total") or 0) / total * 100.0, 1)

    return merged, label_map


def _empty_module_stats() -> Dict[str, Any]:
    return {
        "test_count": 0,
        "regression_count": 0,
        "failure_count": 0,
        "flaky_count": 0,
        "incident_count": 0,
        "incident_severity_sum": 0.0,
        "recent_failure_count": 0,
        "pass_total": 0,
        "run_total": 0,
        "pass_rate": None,
    }
