# services/deployment_risk_assessment_service.py
"""
Incident Investigator II-05A — Deployment Risk Assessment (read-only).

Estimates deployment risk from existing incident intelligence.
No scanners, LLM calls, execution paths, or external calls.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Set

from models.incident_models import (
    DeploymentRiskAssessment,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentImpactNode,
    IncidentStorylineStep,
    InvestigationPlanItem,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
    RiskFactor,
    TemporalCorrelationSummary,
)


@dataclass
class _FactorDraft:
    title: str
    description: str
    weight: float
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


def _risk_level(score: int) -> str:
    if score <= 24:
        return "low"
    if score <= 49:
        return "medium"
    if score <= 74:
        return "high"
    return "critical"


def _summary_for_level(level: str, score: int) -> str:
    if level == "critical":
        return (
            f"Multiple correlated indicators suggest critical deployment risk ({score}/100). "
            "Validate changes carefully before release."
        )
    if level == "high":
        return (
            f"Several correlated indicators suggest elevated deployment risk ({score}/100). "
            "Additional validation is recommended before release."
        )
    if level == "medium":
        return (
            f"Some incident signals suggest moderate deployment risk ({score}/100). "
            "Review contributing factors before proceeding."
        )
    return (
        f"Limited correlated indicators suggest low deployment risk ({score}/100). "
        "Standard validation may be sufficient."
    )


def _upsert_factor(pool: Dict[str, _FactorDraft], draft: _FactorDraft) -> None:
    key = draft.title.strip().lower()
    if not key:
        return
    w = max(0.0, min(1.0, float(draft.weight)))
    existing = pool.get(key)
    if existing is None or w > existing.weight:
        pool[key] = _FactorDraft(
            title=draft.title.strip(),
            description=draft.description.strip(),
            weight=w,
            related_entity_type=draft.related_entity_type,
            related_entity_id=draft.related_entity_id,
        )


def _module_overlap(modules_a: Set[str], modules_b: Set[str]) -> bool:
    if not modules_a or not modules_b:
        return False
    return bool(modules_a & modules_b)


def _confidence_from_factors(
    factors: List[_FactorDraft],
    *,
    evidence_correlation: Optional[EvidenceCorrelationSummary],
    hypotheses: List[IncidentHypothesis],
) -> float:
    score = 0.45 + 0.06 * len(factors)
    if evidence_correlation and evidence_correlation.evidence:
        strongest = max(float(e.confidence) for e in evidence_correlation.evidence)
        score = max(score, 0.5 + strongest * 0.35)
    if hypotheses:
        score = max(score, 0.45 + float(hypotheses[0].confidence) * 0.4)
    if len(factors) >= 4:
        score += 0.05
    return round(min(1.0, score), 2)


def build_deployment_risk_assessment(
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
    temporal_correlation: Optional[TemporalCorrelationSummary] = None,
) -> Optional[DeploymentRiskAssessment]:
    """Build read-only deployment risk assessment from gathered signals."""
    pool: Dict[str, _FactorDraft] = {}

    impacted_modules: Set[str] = {
        (n.title or "").strip().lower()
        for n in impact_map
        if (n.title or "").strip()
    }
    failed_modules: Set[str] = {
        (r.module or r.test_name or r.test_id or "").strip().lower()
        for r in related_runs
        if (r.module or r.test_name or r.test_id or "").strip()
    }

    for pra in related_pr_analysis[:3]:
        pra_modules = {m.strip().lower() for m in (pra.impacted_modules or []) if m.strip()}
        overlap = _module_overlap(pra_modules, impacted_modules | failed_modules)
        if float(pra.pr_risk_score) >= 55 or overlap:
            entity_id = f"{pra.provider}:{pra.pr_number}"
            mod_label = ", ".join(sorted(pra_modules)) or "impacted modules"
            _upsert_factor(pool, _FactorDraft(
                title="PR Correlated With Failure Cluster" if overlap else "High-Risk PR Analysis",
                description=(
                    f"PR #{pra.pr_number} ({pra.risk_level}, {pra.pr_risk_score:.0f}/100) "
                    f"overlaps unstable areas: {mod_label}."
                    if overlap
                    else f"Stored PR Analysis for #{pra.pr_number} shows elevated risk ({pra.pr_risk_score:.0f}/100)."
                ),
                weight=0.22 if overlap else 0.16,
                related_entity_type="pr_analysis",
                related_entity_id=entity_id,
            ))

    for cluster in clusters[:3]:
        mod = str(cluster.get("module") or "").strip()
        fails = int(cluster.get("total_failures") or 0)
        cid = str(cluster.get("cluster_id") or mod).strip()
        if fails < 2 and not mod:
            continue
        label = mod.title() if mod else "unknown"
        _upsert_factor(pool, _FactorDraft(
            title=f"Repeated {label} Failures" if fails >= 3 else f"Failure Cluster in {label}",
            description=(
                f"{fails} recent failure(s) clustered in '{mod or 'unknown'}' "
                f"({cluster.get('root_cause_category', 'unknown')})."
            ),
            weight=min(0.28, 0.14 + fails * 0.03),
            related_entity_type="failure_cluster" if cid else None,
            related_entity_id=cid or None,
        ))

    for h in hypotheses[:2]:
        if h.confidence < 0.55 or h.basis == "assumption":
            continue
        hid = (h.id or f"H{h.rank}" or "hypothesis").strip()
        _upsert_factor(pool, _FactorDraft(
            title="High Confidence Incident Hypothesis",
            description=h.statement,
            weight=min(0.22, 0.12 + float(h.confidence) * 0.12),
            related_entity_type="hypothesis",
            related_entity_id=hid,
        ))

    if browser_events:
        watch_id = str(browser_events[0].get("watch_id") or "").strip()
        count = len(browser_events)
        _upsert_factor(pool, _FactorDraft(
            title="Browser Alerts Detected",
            description=f"{count} Browser Watch alert(s) correlate with the incident window.",
            weight=min(0.22, 0.12 + count * 0.04),
            related_entity_type="browser_watch" if watch_id else None,
            related_entity_id=watch_id or None,
        ))

    high_impact = [n for n in impact_map if n.severity in ("high", "medium")]
    if len(high_impact) >= 2:
        top = high_impact[0]
        _upsert_factor(pool, _FactorDraft(
            title="Large Impacted Area Count",
            description=f"{len(high_impact)} impacted areas show medium or high severity signals.",
            weight=min(0.20, 0.10 + len(high_impact) * 0.03),
            related_entity_type=top.related_entity_type,
            related_entity_id=top.related_entity_id,
        ))
    elif len(impact_map) == 1 and impact_map[0].severity == "high":
        node = impact_map[0]
        _upsert_factor(pool, _FactorDraft(
            title=f"Impacted Area: {node.title}",
            description=node.description,
            weight=0.12,
            related_entity_type=node.related_entity_type,
            related_entity_id=node.related_entity_id,
        ))

    if temporal_correlation and temporal_correlation.signal in ("strong", "medium"):
        _upsert_factor(pool, _FactorDraft(
            title="Strong Temporal Correlation",
            description=temporal_correlation.reason or "Events form a plausible causal sequence.",
            weight=0.20 if temporal_correlation.signal == "strong" else 0.12,
        ))

    corr_count = int(evidence_correlation.total_correlations if evidence_correlation else 0)
    if corr_count >= 2:
        top_ev = max(
            (evidence_correlation.evidence if evidence_correlation else []),
            key=lambda e: float(e.confidence),
            default=None,
        )
        _upsert_factor(pool, _FactorDraft(
            title="Multiple Correlated Signals",
            description=f"{corr_count} evidence correlation(s) reinforce deployment risk.",
            weight=min(0.20, 0.10 + corr_count * 0.03),
            related_entity_type=getattr(top_ev, "related_entity_type", None) if top_ev else None,
            related_entity_id=getattr(top_ev, "related_entity_id", None) if top_ev else None,
        ))

    if len(related_runs) >= 2:
        top_run = related_runs[0]
        _upsert_factor(pool, _FactorDraft(
            title="Historical Failed Runs",
            description=f"{len(related_runs)} failed run(s) in the investigation window.",
            weight=min(0.18, 0.08 + len(related_runs) * 0.03),
            related_entity_type="run",
            related_entity_id=top_run.run_id,
        ))
    elif len(related_runs) == 1:
        run = related_runs[0]
        _upsert_factor(pool, _FactorDraft(
            title="Recent Failed Run",
            description=f"Run {run.run_id} failed during the investigation window.",
            weight=0.08,
            related_entity_type="run",
            related_entity_id=run.run_id,
        ))

    for item in investigation_plan[:3]:
        if item.priority < 70:
            continue
        if item.related_entity_type and item.related_entity_id:
            _upsert_factor(pool, _FactorDraft(
                title=f"Investigation Priority: {item.title}",
                description=item.reason or item.title,
                weight=min(0.14, float(item.priority) / 600.0),
                related_entity_type=item.related_entity_type,
                related_entity_id=item.related_entity_id,
            ))

    for step in storyline[:3]:
        if step.confidence < 0.7 or not step.related_entity_type:
            continue
        _upsert_factor(pool, _FactorDraft(
            title=f"Storyline Signal: {step.title}",
            description=step.description,
            weight=min(0.14, float(step.confidence) * 0.16),
            related_entity_type=step.related_entity_type,
            related_entity_id=step.related_entity_id,
        ))

    if not pool:
        return None

    ordered = sorted(pool.values(), key=lambda f: (-f.weight, f.title.lower()))
    raw_score = sum(f.weight for f in ordered)
    risk_score = max(0, min(100, int(round(raw_score * 100))))
    if risk_score < 8 and len(ordered) < 2:
        return None

    level = _risk_level(risk_score)
    confidence = _confidence_from_factors(ordered, evidence_correlation=evidence_correlation, hypotheses=hypotheses)

    factors = [
        RiskFactor(
            title=f.title,
            description=f.description,
            weight=round(f.weight, 2),
            related_entity_type=f.related_entity_type,
            related_entity_id=f.related_entity_id,
        )
        for f in ordered
    ]

    return DeploymentRiskAssessment(
        risk_score=risk_score,
        risk_level=level,
        confidence=confidence,
        summary=_summary_for_level(level, risk_score),
        contributing_factors=factors,
    )
