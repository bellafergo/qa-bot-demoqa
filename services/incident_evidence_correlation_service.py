# services/incident_evidence_correlation_service.py
"""
Incident Investigator II-02A/II-02B — Evidence Correlation Engine (read-only).

Correlates operational signals already present in Vanya without executing
actions, writing data, or modifying scoring/confidence/ranking.
"""
from __future__ import annotations

from collections import defaultdict
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Set

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentInvestigationRun,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
)


def _parse_iso(ts: Optional[str]) -> Optional[datetime]:
    if not ts or not str(ts).strip():
        return None
    s = str(ts).strip().replace("Z", "+00:00")
    try:
        dt = datetime.fromisoformat(s)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return dt
    except Exception:
        return None


def _minutes_before(event_ts: Optional[str], reference_ts: Optional[str]) -> Optional[int]:
    ev, ref = _parse_iso(event_ts), _parse_iso(reference_ts)
    if ev is None or ref is None:
        return None
    return max(0, int((ref - ev).total_seconds() / 60))


def _minutes_apart(a: Optional[str], b: Optional[str]) -> Optional[int]:
    da, db = _parse_iso(a), _parse_iso(b)
    if da is None or db is None:
        return None
    return int(abs((db - da).total_seconds()) / 60)


_RUN_DEDUPE_SOURCES = frozenset({"failed_run", "api_evidence"})
_BROWSER_DEDUPE_WINDOW_MINUTES = 15


def dedupe_correlated_evidence(evidence: List[CorrelatedEvidence]) -> List[CorrelatedEvidence]:
    """Remove overlapping correlated evidence; keep the item with higher evidence weight."""
    if len(evidence) < 2:
        return list(evidence)

    to_remove: Set[int] = set()

    by_run: Dict[str, List[tuple[int, CorrelatedEvidence]]] = defaultdict(list)
    for idx, item in enumerate(evidence):
        if item.source in _RUN_DEDUPE_SOURCES:
            rid = (item.related_run_id or "").strip()
            if rid:
                by_run[rid].append((idx, item))

    for group in by_run.values():
        sources = {item.source for _, item in group}
        if not ({"failed_run", "api_evidence"} <= sources):
            continue
        best_idx = max(group, key=lambda pair: (pair[1].confidence, pair[1].source))[0]
        for idx, _ in group:
            if idx != best_idx:
                to_remove.add(idx)

    probes = [
        (idx, item)
        for idx, item in enumerate(evidence)
        if item.source == "browser_probe" and idx not in to_remove
    ]
    watches = [
        (idx, item)
        for idx, item in enumerate(evidence)
        if item.source == "browser_watch" and idx not in to_remove
    ]

    for probe_idx, probe in probes:
        if probe_idx in to_remove:
            continue
        for watch_idx, watch in watches:
            if watch_idx in to_remove:
                continue
            gap = _minutes_apart(probe.timestamp, watch.timestamp)
            if gap is None or gap >= _BROWSER_DEDUPE_WINDOW_MINUTES:
                continue
            if probe.confidence >= watch.confidence:
                to_remove.add(watch_idx)
            else:
                to_remove.add(probe_idx)
                break

    return [item for idx, item in enumerate(evidence) if idx not in to_remove]


def correlate_failed_runs(
    related_runs: List[RelatedRunSummary],
    *,
    incident_reported_at: Optional[str] = None,
    limit: int = 8,
) -> List[CorrelatedEvidence]:
    items: List[CorrelatedEvidence] = []
    for run in related_runs[:limit]:
        mins = _minutes_before(run.started_at, incident_reported_at)
        time_part = f" {mins} minutes before incident report" if mins is not None else " in time window"
        detail = f"Run {run.run_id} failed{time_part}"
        if run.test_name or run.test_id:
            detail += f" ({run.test_name or run.test_id})"
        if run.error_summary:
            detail += f" — {run.error_summary[:120]}"
        items.append(CorrelatedEvidence(
            source="failed_run",
            confidence=0.85,
            title="Correlated failed run",
            detail=detail,
            timestamp=run.started_at,
            related_run_id=run.run_id,
        ))
    return items


def correlate_browser_alerts(
    browser_events: List[Dict[str, Any]],
    *,
    limit: int = 6,
) -> List[CorrelatedEvidence]:
    items: List[CorrelatedEvidence] = []
    for ev in browser_events[:limit]:
        watch_id = str(ev.get("watch_id") or "watch")
        summary = str(ev.get("summary") or "Browser Watch alert").strip()
        detail = f"Watch {watch_id} reported {summary}"
        items.append(CorrelatedEvidence(
            source="browser_watch",
            confidence=0.75,
            title="Browser alert detected",
            detail=detail,
            timestamp=str(ev.get("timestamp") or "") or None,
        ))
    return items


def correlate_browser_investigations(
    browser_investigation: Optional[IncidentInvestigationRun],
) -> List[CorrelatedEvidence]:
    if browser_investigation is None:
        return []
    detail = "Passive browser observation captured evidence"
    if browser_investigation.probable_cause:
        detail = f"{detail} — {browser_investigation.probable_cause[:160]}"
    return [CorrelatedEvidence(
        source="browser_probe",
        confidence=0.80,
        title="Browser investigation available",
        detail=detail,
        timestamp=browser_investigation.created_at,
    )]


def correlate_api_failures(
    related_runs: List[RelatedRunSummary],
    *,
    limit_runs: int = 5,
) -> List[CorrelatedEvidence]:
    """Read step.evidence from persisted runs — summaries only, no full payloads."""
    items: List[CorrelatedEvidence] = []
    seen_runs: Set[str] = set()

    try:
        from services.run_history_service import run_history_service
    except Exception:
        return items

    for run in related_runs[:limit_runs]:
        rid = (run.run_id or "").strip()
        if not rid or rid in seen_runs:
            continue
        seen_runs.add(rid)
        try:
            canonical = run_history_service.get_run_unified(rid)
        except Exception:
            canonical = None
        if canonical is None:
            continue
        steps = getattr(canonical, "steps", None) or []
        has_api_evidence = False
        failure_hint = ""
        for step in steps:
            if not isinstance(step, dict):
                continue
            ev = step.get("evidence")
            if not isinstance(ev, dict) or not ev:
                continue
            has_api_evidence = True
            failure = ev.get("failure") if isinstance(ev.get("failure"), dict) else {}
            ftype = str(failure.get("type") or failure.get("kind") or "").strip()
            if ftype:
                failure_hint = ftype.replace("_", " ")
                break
        if not has_api_evidence and run.error_summary:
            blob = (run.error_summary or "").lower()
            if any(k in blob for k in ("http", "api", "status", "5xx", "4xx", "request")):
                has_api_evidence = True
                failure_hint = "HTTP-related failure summary"
        if has_api_evidence:
            detail = "HTTP failure evidence detected"
            if failure_hint:
                detail = f"HTTP failure evidence detected ({failure_hint})"
            items.append(CorrelatedEvidence(
                source="api_evidence",
                confidence=0.70,
                title="API step evidence",
                detail=detail,
                timestamp=run.started_at,
                related_run_id=rid,
            ))
    return items


def correlate_failure_clusters(
    clusters: List[Dict[str, Any]],
    *,
    impacted_modules: Optional[List[str]] = None,
    hints: Optional[Set[str]] = None,
    limit: int = 5,
) -> List[CorrelatedEvidence]:
    items: List[CorrelatedEvidence] = []
    mod_set = {m.lower() for m in (impacted_modules or []) if m}
    hint_set = {h.lower() for h in (hints or set()) if h}

    for cluster in clusters[:limit]:
        mod = str(cluster.get("module") or "").strip()
        if not mod:
            continue
        mod_low = mod.lower()
        if mod_set and mod_low not in mod_set:
            if hint_set and mod_low not in hint_set and not any(h in mod_low for h in hint_set):
                continue
        fails = int(cluster.get("total_failures") or 0)
        cat = str(cluster.get("root_cause_category") or "unknown")
        detail = f"Failure cluster overlaps incident modules ({mod}, {fails} failure(s), {cat})"
        items.append(CorrelatedEvidence(
            source="failure_cluster",
            confidence=min(0.88, 0.65 + fails * 0.03),
            title="Failure cluster overlap",
            detail=detail,
        ))
    return items


def _correlate_pr_analysis(
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    *,
    limit: int = 4,
) -> List[CorrelatedEvidence]:
    items: List[CorrelatedEvidence] = []
    for pra in related_pr_analysis[:limit]:
        mods = ", ".join(pra.impacted_modules[:4]) or "unknown module"
        detail = f"PR Analysis references impacted module {mods}"
        if pra.reason:
            detail = f"{detail} — {pra.reason}"
        items.append(CorrelatedEvidence(
            source="pr_analysis",
            confidence=min(0.82, 0.55 + pra.pr_risk_score / 200.0),
            title="PR Analysis snapshot",
            detail=detail,
            timestamp=pra.analyzed_at,
        ))
    return items


def _correlate_system_memory(
    knowledge_hints: Optional[List[str]],
    *,
    limit: int = 3,
) -> List[CorrelatedEvidence]:
    items: List[CorrelatedEvidence] = []
    for hint in (knowledge_hints or [])[:limit]:
        text = str(hint or "").strip()
        if not text:
            continue
        items.append(CorrelatedEvidence(
            source="system_memory",
            confidence=0.55,
            title="System Memory hint",
            detail=text[:240],
        ))
    return items


def build_evidence_correlation(
    *,
    related_runs: List[RelatedRunSummary],
    browser_events: List[Dict[str, Any]],
    browser_investigation: Optional[IncidentInvestigationRun],
    clusters: List[Dict[str, Any]],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    impacted_modules: Optional[List[str]] = None,
    hints: Optional[Set[str]] = None,
    knowledge_hints: Optional[List[str]] = None,
    incident_reported_at: Optional[str] = None,
) -> EvidenceCorrelationSummary:
    """Aggregate read-only correlated evidence from all operational sources."""
    evidence: List[CorrelatedEvidence] = []
    evidence.extend(correlate_failed_runs(
        related_runs, incident_reported_at=incident_reported_at,
    ))
    evidence.extend(correlate_browser_alerts(browser_events))
    evidence.extend(correlate_browser_investigations(browser_investigation))
    evidence.extend(correlate_api_failures(related_runs))
    evidence.extend(correlate_failure_clusters(
        clusters, impacted_modules=impacted_modules, hints=hints,
    ))
    evidence.extend(_correlate_pr_analysis(related_pr_analysis))
    evidence.extend(_correlate_system_memory(knowledge_hints))

    if not evidence:
        return EvidenceCorrelationSummary(
            total_correlations=0,
            strongest_source=None,
            evidence=[],
        )

    evidence = dedupe_correlated_evidence(evidence)
    if not evidence:
        return EvidenceCorrelationSummary(
            total_correlations=0,
            strongest_source=None,
            evidence=[],
        )

    ordered = sorted(evidence, key=lambda e: (-e.confidence, e.source))
    strongest = ordered[0].source if ordered else None
    return EvidenceCorrelationSummary(
        total_correlations=len(ordered),
        strongest_source=strongest,
        evidence=ordered,
    )
