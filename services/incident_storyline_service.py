# services/incident_storyline_service.py
"""
Incident Investigator II-03B — Incident Storyline (read-only).

Reconstructs the most likely causal sequence from existing investigation data.
No scanners, background jobs, or external calls.
"""
from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional, Set, Tuple

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentStorylineStep,
    IncidentTimelineEvent,
    RelatedPRAnalysisSummary,
    RelatedPRSummary,
    RelatedRunSummary,
    TemporalCorrelationSummary,
)

_CATEGORY_ORDER = {
    "pr_analysis": 10,
    "browser_watch": 20,
    "browser_probe": 25,
    "run": 30,
    "failure_cluster": 40,
    "conclusion": 50,
}

_SYNTHETIC_OFFSET_MINUTES = {
    "pr_analysis": 180,
    "browser_watch": 120,
    "browser_probe": 110,
    "run": 60,
    "failure_cluster": 30,
    "conclusion": 0,
}


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


def _story_key(
    entity_type: Optional[str],
    entity_id: Optional[str],
    title: str,
) -> str:
    et = (entity_type or "").strip()
    eid = (entity_id or "").strip()
    if et and eid:
        return f"{et}:{eid}"
    return f"title:{title.strip().lower()}"


@dataclass
class _StoryCandidate:
    category: str
    title: str
    description: str
    confidence: float
    related_entity_type: Optional[str]
    related_entity_id: Optional[str]
    timestamp: Optional[str]


def _effective_sort_time(
    candidate: _StoryCandidate,
    *,
    anchor: datetime,
) -> datetime:
    parsed = _parse_iso(candidate.timestamp)
    if parsed is not None:
        return parsed
    offset = _SYNTHETIC_OFFSET_MINUTES.get(candidate.category, 90)
    return anchor - timedelta(minutes=offset)


def _upsert_candidate(
    pool: Dict[str, _StoryCandidate],
    candidate: _StoryCandidate,
) -> None:
    key = _story_key(
        candidate.related_entity_type,
        candidate.related_entity_id,
        candidate.title,
    )
    existing = pool.get(key)
    if existing is None or candidate.confidence > existing.confidence:
        pool[key] = candidate


def _from_pr_analysis(pra: RelatedPRAnalysisSummary) -> _StoryCandidate:
    entity_id = f"{pra.provider}:{pra.pr_number}"
    modules = ", ".join(pra.impacted_modules or []) or "impacted modules"
    return _StoryCandidate(
        category="pr_analysis",
        title=f"PR #{pra.pr_number} merged",
        description=(
            pra.reason
            or f"Recent code change correlated with incident evidence ({pra.risk_level} risk, {modules})."
        ),
        confidence=min(0.95, 0.55 + float(pra.pr_risk_score) / 200.0),
        related_entity_type="pr_analysis",
        related_entity_id=entity_id,
        timestamp=pra.analyzed_at,
    )


def _from_open_pr(pr: RelatedPRSummary) -> _StoryCandidate:
    entity_id = f"{pr.provider}:{pr.pr_id}"
    detail = pr.title or "open pull request"
    return _StoryCandidate(
        category="pr_analysis",
        title=f"PR #{pr.pr_id} merged",
        description=pr.match_reason or f"Recent code change ({detail}) correlated with incident evidence.",
        confidence=0.72,
        related_entity_type="pr_analysis",
        related_entity_id=entity_id,
        timestamp=pr.updated_at or None,
    )


def _from_browser_event(ev: Dict[str, Any]) -> _StoryCandidate:
    watch_id = str(ev.get("watch_id") or "").strip()
    summary = str(ev.get("summary") or "").strip()
    change_level = str(ev.get("change_level") or "").strip()
    if summary and len(summary) <= 72:
        title = summary
    elif change_level:
        title = f"{change_level.title()} UI change detected"
    else:
        title = "Browser Watch alert detected"
    return _StoryCandidate(
        category="browser_watch",
        title=title,
        description="Browser Watch detected a visual or behavioral change correlated with the incident.",
        confidence=0.78 if change_level else 0.72,
        related_entity_type="browser_watch" if watch_id else None,
        related_entity_id=watch_id or None,
        timestamp=str(ev.get("timestamp") or "") or None,
    )


def _from_failed_run(run: RelatedRunSummary) -> _StoryCandidate:
    label = run.test_name or run.test_id or run.run_id
    return _StoryCandidate(
        category="run",
        title=f"Failed run: {label}",
        description=(
            run.error_summary
            or run.rca_summary
            or f"Run {run.run_id} failed during the investigation window."
        ),
        confidence=0.82 if run.error_summary else 0.74,
        related_entity_type="run",
        related_entity_id=run.run_id,
        timestamp=run.started_at,
    )


def _from_cluster(cluster: Dict[str, Any]) -> _StoryCandidate:
    mod = str(cluster.get("module") or "unknown")
    fails = int(cluster.get("total_failures") or 0)
    cid = str(cluster.get("cluster_id") or cluster.get("module") or "").strip()
    return _StoryCandidate(
        category="failure_cluster",
        title=f"Failure cluster expanded in {mod}",
        description=(
            f"{fails} failure(s) clustered around "
            f"{cluster.get('root_cause_category', 'unknown')} root cause."
        ),
        confidence=min(0.92, 0.62 + fails * 0.05),
        related_entity_type="failure_cluster" if cid else None,
        related_entity_id=cid or None,
        timestamp=None,
    )


def _from_correlated_evidence(ev: CorrelatedEvidence) -> Optional[_StoryCandidate]:
    source = (ev.source or "").strip()
    entity_type = ev.related_entity_type
    entity_id = ev.related_entity_id
    ts = ev.timestamp

    if source == "pr_analysis":
        raw = (entity_id or "").strip()
        pr_num = raw.split(":")[-1] if ":" in raw else raw
        return _StoryCandidate(
            category="pr_analysis",
            title=f"PR #{pr_num or 'unknown'} merged",
            description=ev.reason or ev.detail or "Recent code change correlated with incident evidence.",
            confidence=float(ev.confidence),
            related_entity_type="pr_analysis",
            related_entity_id=entity_id,
            timestamp=ts,
        )
    if source == "browser_watch":
        return _StoryCandidate(
            category="browser_watch",
            title=ev.title or "Browser Watch alert detected",
            description=ev.reason or ev.detail or "Browser Watch detected a change correlated with the incident.",
            confidence=float(ev.confidence),
            related_entity_type="browser_watch",
            related_entity_id=entity_id,
            timestamp=ts,
        )
    if source in ("failed_run", "api_evidence"):
        rid = (entity_id or ev.related_run_id or "").strip()
        return _StoryCandidate(
            category="run",
            title=ev.title or "Failed run appeared",
            description=ev.reason or ev.detail or "A failed test run appeared in the investigation window.",
            confidence=float(ev.confidence),
            related_entity_type="run" if rid else None,
            related_entity_id=rid or None,
            timestamp=ts,
        )
    if source == "failure_cluster":
        return _StoryCandidate(
            category="failure_cluster",
            title=ev.title or "Failure cluster expanded",
            description=ev.reason or ev.detail or "Multiple failures clustered in an impacted module.",
            confidence=float(ev.confidence),
            related_entity_type="failure_cluster",
            related_entity_id=entity_id,
            timestamp=ts,
        )
    if source == "browser_probe":
        return _StoryCandidate(
            category="browser_probe",
            title=ev.title or "Browser Probe evidence recorded",
            description=ev.reason or ev.detail or "Passive browser observation supports the incident narrative.",
            confidence=float(ev.confidence),
            related_entity_type="browser_probe",
            related_entity_id=entity_id,
            timestamp=ts,
        )
    return None


def _from_timeline_event(ev: IncidentTimelineEvent) -> Optional[_StoryCandidate]:
    source = (ev.source or "").strip()
    entity_type: Optional[str] = None
    entity_id: Optional[str] = None

    if ev.event_type == "run_failed" and source.startswith("run:"):
        entity_type, entity_id = "run", source[4:].strip()
        category = "run"
    elif ev.event_type == "browser_watch_alert" and source.startswith("browser_watch:"):
        entity_type, entity_id = "browser_watch", source.split(":", 1)[-1].strip()
        category = "browser_watch"
    elif ev.event_type == "pr_analyzed" and source.startswith("pr_analysis:"):
        provider = source.split(":", 1)[-1].strip()
        pr_num = ""
        if "#" in ev.title:
            pr_num = ev.title.split("#", 1)[-1].split()[0].strip()
        entity_type = "pr_analysis"
        entity_id = f"{provider}:{pr_num}" if provider and pr_num else None
        category = "pr_analysis"
    elif ev.event_type == "failure_cluster":
        category = "failure_cluster"
    else:
        return None

    return _StoryCandidate(
        category=category,
        title=ev.title,
        description=ev.details or ev.title,
        confidence=0.7,
        related_entity_type=entity_type,
        related_entity_id=entity_id,
        timestamp=ev.timestamp,
    )


def _conclusion_candidate(h: IncidentHypothesis) -> _StoryCandidate:
    return _StoryCandidate(
        category="conclusion",
        title="Investigation conclusion",
        description=h.statement,
        confidence=float(h.confidence),
        related_entity_type=None,
        related_entity_id=None,
        timestamp=None,
    )


def _has_actionable_signals(pool: Dict[str, _StoryCandidate]) -> bool:
    return any(c.category != "conclusion" for c in pool.values())


def build_incident_storyline(
    *,
    hypotheses: List[IncidentHypothesis],
    evidence_correlation: Optional[EvidenceCorrelationSummary],
    related_runs: List[RelatedRunSummary],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    related_prs: List[RelatedPRSummary],
    browser_events: List[Dict[str, Any]],
    clusters: List[Dict[str, Any]],
    timeline: List[IncidentTimelineEvent],
    temporal_correlation: Optional[TemporalCorrelationSummary],
    incident_reported_at: Optional[str] = None,
    limit: int = 10,
) -> List[IncidentStorylineStep]:
    """Build read-only causal storyline from existing investigation signals."""
    pool: Dict[str, _StoryCandidate] = {}
    pr_numbers_seen: Set[str] = set()

    for pra in related_pr_analysis[:3]:
        pr_numbers_seen.add(str(pra.pr_number))
        _upsert_candidate(pool, _from_pr_analysis(pra))

    for pr in related_prs[:2]:
        if str(pr.pr_id) in pr_numbers_seen:
            continue
        _upsert_candidate(pool, _from_open_pr(pr))

    for ev in browser_events[:4]:
        _upsert_candidate(pool, _from_browser_event(ev))

    for run in related_runs[:4]:
        if (run.status or "").lower() == "failed" or run.error_summary or run.rca_summary:
            _upsert_candidate(pool, _from_failed_run(run))

    for cluster in clusters[:3]:
        if int(cluster.get("total_failures") or 0) > 0:
            _upsert_candidate(pool, _from_cluster(cluster))

    for ev in (evidence_correlation.evidence if evidence_correlation else []):
        candidate = _from_correlated_evidence(ev)
        if candidate:
            _upsert_candidate(pool, candidate)

    for ev in timeline:
        if ev.event_type == "incident_reported":
            continue
        candidate = _from_timeline_event(ev)
        if candidate:
            _upsert_candidate(pool, candidate)

    if not _has_actionable_signals(pool):
        return []

    anchor = _parse_iso(incident_reported_at) or datetime.now(timezone.utc)
    ordered = sorted(
        pool.values(),
        key=lambda c: (
            _effective_sort_time(c, anchor=anchor),
            _CATEGORY_ORDER.get(c.category, 99),
            -c.confidence,
        ),
    )

    top = hypotheses[0] if hypotheses else None
    if top and top.confidence >= 0.45 and top.basis != "assumption":
        conclusion = _conclusion_candidate(top)
        ordered = [c for c in ordered if c.category != "conclusion"]
        ordered.append(conclusion)

    if temporal_correlation and temporal_correlation.signal in ("strong", "medium"):
        chain_hint = temporal_correlation.reason
        if chain_hint and ordered:
            first = ordered[0]
            if not first.description.endswith("."):
                first.description = f"{first.description}."
            ordered[0] = _StoryCandidate(
                category=first.category,
                title=first.title,
                description=f"{first.description} Temporal correlation: {chain_hint}",
                confidence=min(0.98, first.confidence + 0.05),
                related_entity_type=first.related_entity_type,
                related_entity_id=first.related_entity_id,
                timestamp=first.timestamp,
            )

    steps: List[IncidentStorylineStep] = []
    for idx, candidate in enumerate(ordered[: max(1, int(limit))], start=1):
        steps.append(
            IncidentStorylineStep(
                step_number=idx,
                title=candidate.title,
                description=candidate.description,
                confidence=round(max(0.0, min(1.0, candidate.confidence)), 2),
                related_entity_type=candidate.related_entity_type,
                related_entity_id=candidate.related_entity_id,
                timestamp=candidate.timestamp,
            )
        )
    return steps
