# services/incident_recommended_actions_service.py
"""
Incident Investigator II-04A — Human Approved Actions (read-only recommendations).

Suggests follow-up actions from existing incident evidence.
Never executes actions, workflows, or external calls.
"""
from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Set

from models.incident_models import (
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentImpactNode,
    IncidentStorylineStep,
    IncidentTimelineEvent,
    InvestigationPlanItem,
    RecommendedAction,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
    TemporalCorrelationSummary,
)

_ALLOWED_ACTION_TYPES = frozenset({
    "run_test_suite",
    "run_browser_probe",
    "analyze_pr",
    "inspect_failure_cluster",
    "inspect_failed_run",
    "review_browser_watch",
    "review_impacted_area",
    "review_hypothesis",
    "review_timeline",
})

_ACTION_TYPE_PRIORITY = {
    "analyze_pr": 0,
    "run_browser_probe": 1,
    "inspect_failed_run": 2,
    "inspect_failure_cluster": 3,
    "review_browser_watch": 4,
    "run_test_suite": 5,
    "review_impacted_area": 6,
    "review_hypothesis": 7,
    "review_timeline": 8,
}


@dataclass
class _ActionDraft:
    action_id: str
    title: str
    description: str
    reason: str
    priority: int
    confidence: float
    action_type: str
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


def build_action_id(
    action_type: str,
    related_entity_type: Optional[str] = None,
    related_entity_id: Optional[str] = None,
    title: str = "",
) -> str:
    """Deterministic action id: type:entity_type:entity_id or type:title:hash."""
    at = (action_type or "").strip()
    et = (related_entity_type or "").strip()
    eid = (related_entity_id or "").strip()
    if et and eid:
        return f"{at}:{et}:{eid}"
    norm = re.sub(r"\s+", " ", (title or at).strip().lower())
    digest = hashlib.sha256(norm.encode("utf-8")).hexdigest()[:12]
    return f"{at}:title:{digest}"


def _priority_from_confidence(confidence: float, *, boost: int = 0) -> int:
    """Lower priority number = higher urgency (sorted ascending)."""
    base = int(round((1.0 - max(0.0, min(1.0, confidence))) * 40))
    return max(1, min(99, base - boost))


def _upsert(pool: Dict[str, _ActionDraft], draft: _ActionDraft) -> None:
    if draft.action_type not in _ALLOWED_ACTION_TYPES:
        return
    key = draft.action_id
    existing = pool.get(key)
    if existing is None or draft.priority < existing.priority or (
        draft.priority == existing.priority and draft.confidence > existing.confidence
    ):
        pool[key] = draft


def _to_action(draft: _ActionDraft) -> RecommendedAction:
    return RecommendedAction(
        action_id=draft.action_id,
        title=draft.title,
        description=draft.description,
        reason=draft.reason,
        priority=draft.priority,
        confidence=round(max(0.0, min(1.0, draft.confidence)), 2),
        action_type=draft.action_type,
        requires_user_approval=True,
        related_entity_type=draft.related_entity_type,
        related_entity_id=draft.related_entity_id,
    )


def _from_plan_item(item: InvestigationPlanItem) -> Optional[_ActionDraft]:
    entity_type = item.related_entity_type
    entity_id = item.related_entity_id
    conf = max(0.0, min(1.0, float(item.priority) / 100.0))
    priority = _priority_from_confidence(conf)

    if entity_type == "pr_analysis" and entity_id:
        pr_num = entity_id.split(":")[-1] if ":" in entity_id else entity_id
        return _ActionDraft(
            action_id=build_action_id("analyze_pr", entity_type, entity_id),
            title=f"Analyze PR #{pr_num}",
            description="Review stored PR Analysis and confirm whether this change introduced the incident.",
            reason=item.reason,
            priority=priority,
            confidence=conf,
            action_type="analyze_pr",
            related_entity_type=entity_type,
            related_entity_id=entity_id,
        )
    if entity_type == "run" and entity_id:
        return _ActionDraft(
            action_id=build_action_id("inspect_failed_run", entity_type, entity_id),
            title=item.title or "Inspect Failed Run",
            description="Open the correlated failed run and review error output before approving further action.",
            reason=item.reason,
            priority=priority,
            confidence=conf,
            action_type="inspect_failed_run",
            related_entity_type=entity_type,
            related_entity_id=entity_id,
        )
    if entity_type == "browser_watch" and entity_id:
        return _ActionDraft(
            action_id=build_action_id("review_browser_watch", entity_type, entity_id),
            title=item.title or "Review Browser Watch Alert",
            description="Inspect the Browser Watch alert that correlates with this incident.",
            reason=item.reason,
            priority=priority,
            confidence=conf,
            action_type="review_browser_watch",
            related_entity_type=entity_type,
            related_entity_id=entity_id,
        )
    if entity_type == "failure_cluster" and entity_id:
        return _ActionDraft(
            action_id=build_action_id("inspect_failure_cluster", entity_type, entity_id),
            title=item.title or "Inspect Failure Cluster",
            description="Review the failure cluster overlapping this incident before approving remediation.",
            reason=item.reason,
            priority=priority,
            confidence=conf,
            action_type="inspect_failure_cluster",
            related_entity_type=entity_type,
            related_entity_id=entity_id,
        )
    return None


def _from_storyline_step(step: IncidentStorylineStep) -> Optional[_ActionDraft]:
    if not step.related_entity_type or not step.related_entity_id:
        return None
    conf = float(step.confidence)
    priority = _priority_from_confidence(conf)
    et, eid = step.related_entity_type, step.related_entity_id

    if et == "pr_analysis":
        pr_num = eid.split(":")[-1] if ":" in eid else eid
        return _ActionDraft(
            action_id=build_action_id("analyze_pr", et, eid),
            title=f"Analyze PR #{pr_num}",
            description=step.description,
            reason=step.description,
            priority=priority,
            confidence=conf,
            action_type="analyze_pr",
            related_entity_type=et,
            related_entity_id=eid,
        )
    if et == "run":
        return _ActionDraft(
            action_id=build_action_id("inspect_failed_run", et, eid),
            title=step.title,
            description=step.description,
            reason=step.description,
            priority=priority,
            confidence=conf,
            action_type="inspect_failed_run",
            related_entity_type=et,
            related_entity_id=eid,
        )
    if et == "browser_watch":
        return _ActionDraft(
            action_id=build_action_id("review_browser_watch", et, eid),
            title=step.title,
            description=step.description,
            reason=step.description,
            priority=priority,
            confidence=conf,
            action_type="review_browser_watch",
            related_entity_type=et,
            related_entity_id=eid,
        )
    if et == "failure_cluster":
        return _ActionDraft(
            action_id=build_action_id("inspect_failure_cluster", et, eid),
            title=step.title,
            description=step.description,
            reason=step.description,
            priority=priority,
            confidence=conf,
            action_type="inspect_failure_cluster",
            related_entity_type=et,
            related_entity_id=eid,
        )
    return None


def _from_impact_node(node: IncidentImpactNode) -> Optional[_ActionDraft]:
    conf = float(node.confidence)
    priority = _priority_from_confidence(conf, boost=1 if node.severity == "high" else 0)
    title = f"Review Impacted Area: {node.title}"
    return _ActionDraft(
        action_id=build_action_id(
            "review_impacted_area",
            node.related_entity_type,
            node.related_entity_id,
            title=title,
        ),
        title=title,
        description=node.description,
        reason=node.description,
        priority=priority,
        confidence=conf,
        action_type="review_impacted_area",
        related_entity_type=node.related_entity_type,
        related_entity_id=node.related_entity_id,
    )


def _from_correlated(ev: CorrelatedEvidence) -> Optional[_ActionDraft]:
    conf = float(ev.confidence)
    priority = _priority_from_confidence(conf)
    source = (ev.source or "").strip()
    et, eid = ev.related_entity_type, ev.related_entity_id

    if source == "pr_analysis" and eid:
        pr_num = eid.split(":")[-1] if ":" in eid else eid
        return _ActionDraft(
            action_id=build_action_id("analyze_pr", et or "pr_analysis", eid),
            title=f"Analyze PR #{pr_num}",
            description=ev.detail or ev.reason,
            reason=ev.reason or ev.detail,
            priority=priority,
            confidence=conf,
            action_type="analyze_pr",
            related_entity_type=et or "pr_analysis",
            related_entity_id=eid,
        )
    if source in ("failed_run", "api_evidence"):
        rid = (eid or ev.related_run_id or "").strip()
        if not rid:
            return None
        return _ActionDraft(
            action_id=build_action_id("inspect_failed_run", "run", rid),
            title="Inspect Failed Run",
            description=ev.detail or ev.reason,
            reason=ev.reason or ev.detail,
            priority=priority,
            confidence=conf,
            action_type="inspect_failed_run",
            related_entity_type="run",
            related_entity_id=rid,
        )
    if source == "browser_watch" and eid:
        return _ActionDraft(
            action_id=build_action_id("review_browser_watch", "browser_watch", eid),
            title="Review Browser Watch Alert",
            description=ev.detail or ev.reason,
            reason=ev.reason or ev.detail,
            priority=priority,
            confidence=conf,
            action_type="review_browser_watch",
            related_entity_type="browser_watch",
            related_entity_id=eid,
        )
    if source == "failure_cluster" and eid:
        return _ActionDraft(
            action_id=build_action_id("inspect_failure_cluster", "failure_cluster", eid),
            title="Inspect Failure Cluster",
            description=ev.detail or ev.reason,
            reason=ev.reason or ev.detail,
            priority=priority,
            confidence=conf,
            action_type="inspect_failure_cluster",
            related_entity_type="failure_cluster",
            related_entity_id=eid,
        )
    if source == "browser_probe":
        probe_title = "Run Browser Probe"
        return _ActionDraft(
            action_id=build_action_id("run_browser_probe", et, eid, title=probe_title),
            title=probe_title,
            description=ev.detail or ev.reason,
            reason=ev.reason or "Browser Probe evidence suggests live UI validation.",
            priority=priority,
            confidence=conf,
            action_type="run_browser_probe",
            related_entity_type=et,
            related_entity_id=eid,
        )
    return None


def build_recommended_actions(
    *,
    hypotheses: List[IncidentHypothesis],
    evidence_correlation: Optional[EvidenceCorrelationSummary],
    investigation_plan: List[InvestigationPlanItem],
    storyline: List[IncidentStorylineStep],
    impact_map: List[IncidentImpactNode],
    related_runs: List[RelatedRunSummary],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    browser_events: List[Dict[str, Any]],
    clusters: List[Dict[str, Any]],
    timeline: List[IncidentTimelineEvent],
    temporal_correlation: Optional[TemporalCorrelationSummary] = None,
    limit: int = 10,
) -> List[RecommendedAction]:
    """Build read-only human-approved action recommendations."""
    pool: Dict[str, _ActionDraft] = {}
    modules_with_failures: Set[str] = set()

    for item in investigation_plan:
        draft = _from_plan_item(item)
        if draft:
            _upsert(pool, draft)

    for step in storyline:
        draft = _from_storyline_step(step)
        if draft:
            _upsert(pool, draft)

    for node in impact_map:
        draft = _from_impact_node(node)
        if draft:
            _upsert(pool, draft)
        if node.severity in ("high", "medium") and node.title:
            mod = node.title.strip()
            modules_with_failures.add(mod.lower())

    for ev in (evidence_correlation.evidence if evidence_correlation else []):
        draft = _from_correlated(ev)
        if draft:
            _upsert(pool, draft)

    for pra in related_pr_analysis[:3]:
        entity_id = f"{pra.provider}:{pra.pr_number}"
        conf = min(0.95, 0.55 + float(pra.pr_risk_score) / 200.0)
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("analyze_pr", "pr_analysis", entity_id),
            title=f"Analyze PR #{pra.pr_number}",
            description=(
                f"Stored PR Analysis shows {pra.risk_level} risk "
                f"({pra.pr_risk_score:.0f}/100) — confirm before approving changes."
            ),
            reason=pra.reason or "PR Analysis correlates with incident modules.",
            priority=_priority_from_confidence(conf, boost=2),
            confidence=conf,
            action_type="analyze_pr",
            related_entity_type="pr_analysis",
            related_entity_id=entity_id,
        ))

    for run in related_runs[:4]:
        mod = (run.module or run.test_name or run.test_id or "").strip()
        if mod:
            modules_with_failures.add(mod.lower())
        conf = 0.82 if run.error_summary else 0.72
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("inspect_failed_run", "run", run.run_id),
            title=f"Inspect Failed Run: {run.test_name or run.test_id or run.run_id}",
            description=run.error_summary or run.rca_summary or "Review failed run output before approving remediation.",
            reason=f"Failed run {run.run_id} correlates with the incident window.",
            priority=_priority_from_confidence(conf),
            confidence=conf,
            action_type="inspect_failed_run",
            related_entity_type="run",
            related_entity_id=run.run_id,
        ))

    for ev in browser_events[:3]:
        watch_id = str(ev.get("watch_id") or "").strip()
        if not watch_id:
            continue
        summary = str(ev.get("summary") or "Browser Watch alert in investigation window.")
        conf = 0.76
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("review_browser_watch", "browser_watch", watch_id),
            title="Review Browser Watch Alert",
            description=summary,
            reason=summary,
            priority=_priority_from_confidence(conf),
            confidence=conf,
            action_type="review_browser_watch",
            related_entity_type="browser_watch",
            related_entity_id=watch_id,
        ))
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("run_browser_probe", "browser_watch", watch_id),
            title="Run Browser Probe",
            description="Collect live UI evidence on the route referenced by the Browser Watch alert.",
            reason="Browser Watch alert detected — probe can validate current UI behavior.",
            priority=_priority_from_confidence(conf, boost=-2),
            confidence=min(0.88, conf + 0.08),
            action_type="run_browser_probe",
            related_entity_type="browser_watch",
            related_entity_id=watch_id,
        ))

    for cluster in clusters[:3]:
        mod = str(cluster.get("module") or "").strip()
        cid = str(cluster.get("cluster_id") or mod).strip()
        if not cid:
            continue
        fails = int(cluster.get("total_failures") or 0)
        if mod:
            modules_with_failures.add(mod.lower())
        conf = min(0.92, 0.62 + fails * 0.04)
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("inspect_failure_cluster", "failure_cluster", cid),
            title=f"Inspect Failure Cluster in {mod or 'unknown'}",
            description=f"{fails} failure(s) clustered — review before approving test reruns.",
            reason=f"Failure cluster overlaps incident evidence in module '{mod}'.",
            priority=_priority_from_confidence(conf),
            confidence=conf,
            action_type="inspect_failure_cluster",
            related_entity_type="failure_cluster",
            related_entity_id=cid,
        ))

    for h in hypotheses[:3]:
        if h.confidence < 0.45 or h.basis == "assumption":
            continue
        hid = (h.id or f"H{h.rank}" or "hypothesis").strip()
        conf = float(h.confidence)
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("review_hypothesis", "hypothesis", hid),
            title=f"Review Hypothesis {hid}",
            description=h.statement,
            reason=h.statement,
            priority=_priority_from_confidence(conf, boost=-1),
            confidence=conf,
            action_type="review_hypothesis",
            related_entity_type="hypothesis",
            related_entity_id=hid,
        ))

    if temporal_correlation and temporal_correlation.signal in ("strong", "medium"):
        conf = 0.8 if temporal_correlation.signal == "strong" else 0.68
        timeline_title = "Review Incident Timeline"
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("review_timeline", title=timeline_title),
            title=timeline_title,
            description=temporal_correlation.reason or "Temporal correlation suggests a causal sequence.",
            reason=temporal_correlation.reason or "Events in the investigation window form a plausible chain.",
            priority=_priority_from_confidence(conf),
            confidence=conf,
            action_type="review_timeline",
            related_entity_type=None,
            related_entity_id=None,
        ))

    for mod in sorted(modules_with_failures):
        if not mod or mod == "unknown":
            continue
        display = mod.title() if mod.islower() else mod
        conf = 0.7
        suite_title = f"Run {display} Smoke Suite"
        _upsert(pool, _ActionDraft(
            action_id=build_action_id("run_test_suite", "module", mod),
            title=suite_title,
            description=f"Validate {display} with a focused smoke suite after reviewing incident evidence.",
            reason=f"Multiple failed runs or clusters reference {display}.",
            priority=_priority_from_confidence(conf, boost=3),
            confidence=conf,
            action_type="run_test_suite",
            related_entity_type="module",
            related_entity_id=mod,
        ))

    if not pool:
        return []

    ordered = sorted(
        pool.values(),
        key=lambda d: (
            d.priority,
            -d.confidence,
            _ACTION_TYPE_PRIORITY.get(d.action_type, 99),
            d.title.lower(),
        ),
    )
    return [_to_action(d) for d in ordered[: max(1, int(limit))]]
