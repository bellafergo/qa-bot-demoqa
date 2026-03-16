# services/failure_clustering.py
"""
Failure Clustering for Vanya.

    cluster_failures(runs: list[dict]) -> list[dict]

Groups failed runs by probable cause using the failure_analysis dict
already present in each run (produced by services/failure_intelligence.py).

Clustering key:  "{failure_type}:{target}"  when target is present,
                 "{failure_type}"            otherwise.

Output per cluster:
    {
        "cluster_id":    str,           # e.g. "selector_not_found:button(login)"
        "failure_type":  str,           # e.g. "selector_not_found"
        "target":        str | None,    # e.g. "button(login)"  or None
        "count":         int,           # number of runs in the cluster
        "runs":          list[str],     # evidence_ids, insertion order
    }

Clusters are returned sorted by count descending.

    get_recent_failure_clusters(limit=50) -> list[dict]

Convenience helper: reads the last `limit` runs from the in-memory run store,
filters to those that are failed and carry a failure_analysis, and clusters them.

Pure module — no I/O of its own except the run_store read in the helper.
"""
from __future__ import annotations

from collections import defaultdict
from typing import Any, Dict, List, Optional


# ── Public API ─────────────────────────────────────────────────────────────────

def cluster_failures(runs: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """
    Group failed runs by failure_type + target.

    Only runs that carry a 'failure_analysis' dict are clustered;
    others are silently skipped.

    Returns a list of cluster dicts sorted by count descending.
    """
    # cluster_key -> {"failure_type", "target", "runs": [evidence_id, ...]}
    buckets: Dict[str, Dict[str, Any]] = {}
    order:   List[str]                 = []   # insertion order for determinism

    for run in (runs or []):
        if not isinstance(run, dict):
            continue

        fa = run.get("failure_analysis")
        if not isinstance(fa, dict):
            continue

        failure_type = _s(fa.get("failure_type") or "unknown")
        target       = _s(fa.get("target") or "") or None   # None when empty/missing

        cluster_key  = f"{failure_type}:{target}" if target else failure_type
        evidence_id  = _s(run.get("evidence_id") or "")

        if cluster_key not in buckets:
            buckets[cluster_key] = {
                "cluster_id":   cluster_key,
                "failure_type": failure_type,
                "target":       target,
                "runs":         [],
            }
            order.append(cluster_key)

        if evidence_id:
            buckets[cluster_key]["runs"].append(evidence_id)

    result = []
    for key in order:
        b = buckets[key]
        result.append({
            "cluster_id":   b["cluster_id"],
            "failure_type": b["failure_type"],
            "target":       b["target"],
            "count":        len(b["runs"]),
            "runs":         list(b["runs"]),
        })

    result.sort(key=lambda c: c["count"], reverse=True)
    return result


def get_recent_failure_clusters(limit: int = 50) -> List[Dict[str, Any]]:
    """
    Read the most recent `limit` runs from the in-memory run store,
    keep only the failed ones that carry a failure_analysis, and cluster them.
    """
    from services.run_store import list_runs

    all_runs = list_runs(limit=max(1, int(limit or 50)))
    failed = [
        r for r in all_runs
        if isinstance(r, dict)
        and _s(r.get("status")).lower() == "failed"
        and isinstance(r.get("failure_analysis"), dict)
    ]
    return cluster_failures(failed)


# ── Helpers ────────────────────────────────────────────────────────────────────

def _s(v: Any) -> str:
    try:
        return str(v) if v is not None else ""
    except Exception:
        return ""
