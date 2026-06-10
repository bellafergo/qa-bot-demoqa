# services/incident_investigation_plan_service.py
"""
Incident Investigator II-03A — Recommended Investigation Plan (read-only).

Suggests next investigation steps from existing correlations and hypotheses.
No execution, workflows, or background jobs.
"""
from __future__ import annotations

from typing import Any, Dict, List, Optional, Set

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    InvestigationPlanItem,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
)


def _plan_key(entity_type: Optional[str], entity_id: Optional[str], title: str) -> str:
    et = (entity_type or "").strip()
    eid = (entity_id or "").strip()
    if et and eid:
        return f"{et}:{eid}"
    return f"title:{title.strip().lower()}"


def _title_for_correlated(ev: CorrelatedEvidence) -> str:
    source = (ev.source or "").strip()
    if source == "pr_analysis":
        raw = (ev.related_entity_id or "").strip()
        pr_num = raw.split(":")[-1] if ":" in raw else raw
        return f"Review PR #{pr_num or 'unknown'}"
    if source == "browser_watch":
        return "Inspect Browser Watch Alert"
    if source == "failure_cluster":
        return "Review Failure Cluster"
    if source in ("failed_run", "api_evidence"):
        return "Inspect Latest Failed Run"
    if source == "browser_probe":
        return "Review Browser Probe Evidence"
    if source == "system_memory":
        return "Review System Memory Hints"
    return (ev.title or "Review correlated evidence").strip()


def _reason_for_item(
    *,
    default: str,
    hypotheses: List[IncidentHypothesis],
    entity_type: Optional[str],
    entity_id: Optional[str],
) -> str:
    if default.strip():
        return default.strip()
    for h in hypotheses:
        for ref in h.supporting_refs or []:
            ref_low = ref.lower()
            if entity_type == "run" and ref_low.startswith("run:") and entity_id and entity_id in ref:
                return h.statement
            if entity_type == "pr_analysis" and ref_low.startswith("pr_analysis:") and entity_id and entity_id in ref:
                return h.statement
            if entity_type == "failure_cluster" and ref_low.startswith("cluster:") and entity_id and entity_id in ref:
                return h.statement
    return default or "Correlated evidence suggests this as a next investigation step."


def _hypothesis_boosts(hypotheses: List[IncidentHypothesis]) -> Dict[str, int]:
    boosts: Dict[str, int] = {}
    for h in hypotheses:
        extra = int(round(float(h.confidence) * 25))
        for ref in h.supporting_refs or []:
            if ref.startswith("run:"):
                rid = ref[4:].strip()
                if rid:
                    boosts[_plan_key("run", rid, "")] = max(boosts.get(_plan_key("run", rid, ""), 0), extra)
            elif ref.startswith("pr_analysis:"):
                parts = ref.split(":", 2)
                if len(parts) >= 3:
                    key = _plan_key("pr_analysis", f"{parts[1]}:{parts[2]}", "")
                    boosts[key] = max(boosts.get(key, 0), extra)
            elif ref.startswith("cluster:"):
                cid = ref[8:].strip() or ref.split(":", 1)[-1]
                if cid:
                    key = _plan_key("failure_cluster", cid, "")
                    boosts[key] = max(boosts.get(key, 0), extra)
    return boosts


def _upsert_item(
    items: Dict[str, InvestigationPlanItem],
    *,
    title: str,
    reason: str,
    priority: int,
    related_entity_type: Optional[str] = None,
    related_entity_id: Optional[str] = None,
) -> None:
    key = _plan_key(related_entity_type, related_entity_id, title)
    prio = max(0, min(100, int(priority)))
    existing = items.get(key)
    if existing is None or prio > existing.priority:
        items[key] = InvestigationPlanItem(
            title=title.strip(),
            reason=reason.strip(),
            priority=prio,
            related_entity_type=related_entity_type,
            related_entity_id=related_entity_id,
        )


def build_investigation_plan(
    *,
    hypotheses: List[IncidentHypothesis],
    evidence_correlation: Optional[EvidenceCorrelationSummary],
    related_runs: List[RelatedRunSummary],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    browser_events: List[Dict[str, Any]],
    clusters: List[Dict[str, Any]],
    limit: int = 8,
) -> List[InvestigationPlanItem]:
    """Build read-only investigation recommendations from gathered signals."""
    boosts = _hypothesis_boosts(hypotheses)
    top_hypothesis_conf = float(hypotheses[0].confidence) if hypotheses else 0.0
    items: Dict[str, InvestigationPlanItem] = {}
    seen_sources: Set[str] = set()

    for ev in (evidence_correlation.evidence if evidence_correlation else []):
        title = _title_for_correlated(ev)
        entity_type = ev.related_entity_type
        entity_id = ev.related_entity_id
        if not entity_type and ev.source == "failed_run" and ev.related_run_id:
            entity_type, entity_id = "run", ev.related_run_id
        if not entity_type and ev.source == "api_evidence" and ev.related_run_id:
            entity_type, entity_id = "run", ev.related_run_id
        if not entity_type and ev.source == "browser_watch" and ev.related_entity_id:
            entity_type, entity_id = "browser_watch", ev.related_entity_id

        base = int(round(float(ev.confidence) * 100))
        key = _plan_key(entity_type, entity_id, title)
        boost = boosts.get(key, 0)
        if top_hypothesis_conf >= 0.7 and ev.source in ("failed_run", "pr_analysis", "failure_cluster"):
            boost += 5

        _upsert_item(
            items,
            title=title,
            reason=_reason_for_item(
                default=ev.reason or ev.detail,
                hypotheses=hypotheses,
                entity_type=entity_type,
                entity_id=entity_id,
            ),
            priority=base + boost,
            related_entity_type=entity_type,
            related_entity_id=entity_id,
        )
        seen_sources.add(ev.source)

    if related_runs:
        top = related_runs[0]
        rid = (top.run_id or "").strip()
        if rid:
            base = 82 if top.error_summary or top.rca_summary else 70
            key = _plan_key("run", rid, "")
            _upsert_item(
                items,
                title="Inspect Latest Failed Run",
                reason=_reason_for_item(
                    default=(
                        f"Most recent failed run {rid} "
                        f"({top.test_name or top.test_id or 'unknown test'}) in the investigation window."
                    ),
                    hypotheses=hypotheses,
                    entity_type="run",
                    entity_id=rid,
                ),
                priority=base + boosts.get(key, 0),
                related_entity_type="run",
                related_entity_id=rid,
            )

    for pra in related_pr_analysis[:2]:
        entity_id = f"{pra.provider}:{pra.pr_number}"
        key = _plan_key("pr_analysis", entity_id, "")
        _upsert_item(
            items,
            title=f"Review PR #{pra.pr_number}",
            reason=_reason_for_item(
                default=(
                    f"Stored PR Analysis shows {pra.risk_level} risk "
                    f"({pra.pr_risk_score:.0f}/100) — {pra.reason or 'module overlap'}."
                ),
                hypotheses=hypotheses,
                entity_type="pr_analysis",
                entity_id=entity_id,
            ),
            priority=int(round(min(0.82, 0.55 + pra.pr_risk_score / 200.0) * 100)) + boosts.get(key, 0),
            related_entity_type="pr_analysis",
            related_entity_id=entity_id,
        )

    for ev in browser_events[:2]:
        watch_id = str(ev.get("watch_id") or "").strip()
        if not watch_id:
            continue
        key = _plan_key("browser_watch", watch_id, "")
        _upsert_item(
            items,
            title="Inspect Browser Watch Alert",
            reason=str(ev.get("summary") or "Browser Watch alert in the investigation window."),
            priority=75 + boosts.get(key, 0),
            related_entity_type="browser_watch",
            related_entity_id=watch_id,
        )

    for cluster in clusters[:3]:
        cid = str(cluster.get("cluster_id") or cluster.get("module") or "").strip()
        mod = str(cluster.get("module") or "unknown")
        if not cid:
            continue
        fails = int(cluster.get("total_failures") or 0)
        key = _plan_key("failure_cluster", cid, "")
        _upsert_item(
            items,
            title="Review Failure Cluster",
            reason=(
                f"Failure cluster in module '{mod}' ({fails} failure(s), "
                f"{cluster.get('root_cause_category', 'unknown')}) overlaps the incident."
            ),
            priority=min(88, 65 + fails * 3) + boosts.get(key, 0),
            related_entity_type="failure_cluster",
            related_entity_id=cid,
        )

    for h in hypotheses[:3]:
        if h.confidence < 0.55 or h.basis == "assumption":
            continue
        _upsert_item(
            items,
            title=f"Validate hypothesis {h.id or h.rank}",
            reason=h.statement,
            priority=int(round(float(h.confidence) * 100)),
            related_entity_type=None,
            related_entity_id=None,
        )

    ordered = sorted(items.values(), key=lambda i: (-i.priority, i.title))
    return ordered[: max(1, int(limit))] if ordered else []
