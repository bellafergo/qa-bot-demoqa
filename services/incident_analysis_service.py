# services/incident_analysis_service.py
"""
Incident Investigator v1.3B — evidence strength, blast radius, temporal correlation,
and explainable test recommendations (deterministic, analyze-only).
"""
from __future__ import annotations

from collections import defaultdict
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Set, Tuple

from models.incident_models import (
    BlastRadiusModule,
    IncidentEvidenceItem,
    IncidentEvidenceStrength,
    IncidentHypothesis,
    IncidentTimelineEvent,
    RecommendedTestRecommendation,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
    TemporalCorrelationSummary,
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


def _minutes_between(a: Optional[str], b: Optional[str]) -> Optional[int]:
    da, db = _parse_iso(a), _parse_iso(b)
    if da is None or db is None:
        return None
    return int(abs((db - da).total_seconds()) / 60)


def build_evidence_strength(
    *,
    related_runs: List[RelatedRunSummary],
    related_evidence: List[Any],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    browser_events: List[Dict[str, Any]],
    clusters: List[Dict[str, Any]],
    related_prs: List[Any],
    hints: Set[str],
    hypotheses: List[IncidentHypothesis],
) -> IncidentEvidenceStrength:
    """Aggregate report-level evidence / inference / assumption buckets."""
    evidence: List[IncidentEvidenceItem] = []
    inference: List[IncidentEvidenceItem] = []
    assumptions: List[IncidentEvidenceItem] = []

    if related_runs:
        evidence.append(IncidentEvidenceItem(
            kind="evidence",
            label="Failed runs",
            detail=f"{len(related_runs)} correlated failed run(s) in time window",
            ref="runs:correlated",
        ))
    for ev in related_evidence[:8]:
        evidence.append(IncidentEvidenceItem(
            kind="evidence",
            label="Run evidence",
            detail=f"{getattr(ev, 'test_name', None) or getattr(ev, 'run_id', 'run')} — evidence URL or error summary",
            ref=f"evidence:{getattr(ev, 'run_id', '')}",
        ))
    for pra in related_pr_analysis[:5]:
        evidence.append(IncidentEvidenceItem(
            kind="evidence",
            label="PR Analysis snapshot",
            detail=f"PR #{pra.pr_number} stored analysis ({pra.risk_level}, {pra.pr_risk_score:.0f}/100)",
            ref=f"pr_analysis:{pra.provider}:{pra.pr_number}",
        ))
    for c in clusters[:5]:
        mod = str(c.get("module") or "unknown")
        fails = int(c.get("total_failures") or 0)
        evidence.append(IncidentEvidenceItem(
            kind="evidence",
            label="Failure cluster",
            detail=f"{fails} failure(s) in module '{mod}' ({c.get('root_cause_category', 'unknown')})",
            ref=f"cluster:{c.get('cluster_id', mod)}",
        ))
    for be in browser_events[:5]:
        ts = str(be.get("timestamp") or "")
        evidence.append(IncidentEvidenceItem(
            kind="evidence",
            label="Browser Watch alert",
            detail=f"Alert at {ts[:16] if ts else 'unknown'}: {be.get('summary', 'Browser Watch alert')}",
            ref=f"browser_watch:{be.get('watch_id', '')}",
        ))

    if hints:
        hint_list = ", ".join(sorted(h for h in hints if len(h) >= 3)[:6])
        if hint_list:
            inference.append(IncidentEvidenceItem(
                kind="inference",
                label="Topic keywords",
                detail=f"Incident description keywords suggest: {hint_list}",
                ref="hints:description",
            ))
    for pr in related_prs[:3]:
        if not any(p.pr_id == pr.pr_id for p in related_pr_analysis):
            inference.append(IncidentEvidenceItem(
                kind="inference",
                label="Open PR (no snapshot)",
                detail=f"PR #{pr.pr_id} '{pr.title}' matched but no stored PR Analysis",
                ref=f"pr:{pr.provider}:{pr.pr_id}",
            ))

    for h in hypotheses:
        if h.basis == "assumption":
            assumptions.append(IncidentEvidenceItem(
                kind="assumption",
                label=h.id or "hypothesis",
                detail=h.statement,
                ref=(h.supporting_refs[0] if h.supporting_refs else f"hypothesis:{h.id}"),
            ))

    return IncidentEvidenceStrength(
        evidence=evidence,
        inference=inference,
        assumptions=assumptions,
    )


def attach_hypothesis_classifications(
    hypotheses: List[IncidentHypothesis],
    evidence_strength: IncidentEvidenceStrength,
) -> List[IncidentHypothesis]:
    """Ensure each hypothesis basis matches evidence/inference/assumption taxonomy."""
    updated: List[IncidentHypothesis] = []
    for h in hypotheses:
        basis = h.basis
        refs = h.supporting_refs or []
        if refs and refs[0].startswith("keyword:"):
            basis = "assumption"
        elif refs and refs[0].startswith("pr:") and not refs[0].startswith("pr_analysis:"):
            basis = "inference"
        elif basis == "inference" and any(r.startswith(("run:", "cluster:", "pr_analysis:", "regression:", "browser:")) for r in refs):
            basis = "evidence"
        updated.append(h.model_copy(update={"basis": basis}))
    return updated


def build_blast_radius(
    *,
    related_runs: List[RelatedRunSummary],
    clusters: List[Dict[str, Any]],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    related_prs: List[Any],
    primary_module: Optional[str] = None,
    project_id: Optional[str] = None,
) -> List[BlastRadiusModule]:
    """Score modules from observed QA signals only — no invented dependencies."""
    from services.module_canonical import build_module_label_map, canonical_module_key, module_display_label

    raw_names: List[str] = []
    for r in related_runs:
        if r.module:
            raw_names.append(r.module)
    for c in clusters:
        if c.get("module"):
            raw_names.append(str(c["module"]))
    for pra in related_pr_analysis:
        raw_names.extend(pra.impacted_modules)
    label_map = build_module_label_map(raw_names)

    buckets: Dict[str, Dict[str, Any]] = defaultdict(lambda: {"score": 0.0, "reasons": []})

    def _bump(raw_mod: str, points: float, reason: str) -> None:
        key = canonical_module_key(raw_mod)
        if key == "unknown":
            return
        buckets[key]["score"] += points
        if reason not in buckets[key]["reasons"]:
            buckets[key]["reasons"].append(reason)
        buckets[key]["display"] = module_display_label(raw_mod, labels_by_key=label_map)

    for r in related_runs:
        if r.module:
            _bump(r.module, 28.0, "failed run(s) in window")

    for c in clusters:
        mod = str(c.get("module") or "")
        if mod:
            fails = int(c.get("total_failures") or 0)
            _bump(mod, min(24.0, 8.0 + fails * 2.0), "failure cluster overlap")

    for pra in related_pr_analysis:
        for mod in pra.impacted_modules:
            _bump(mod, 18.0, "stored PR Analysis impacted module")

    pr_mods_from_titles: Set[str] = set()
    for pr in related_prs:
        blob = f"{pr.title} {pr.branch}".lower()
        for raw in raw_names:
            if raw.lower() in blob or canonical_module_key(raw) in blob:
                pr_mods_from_titles.add(raw)
    for mod in pr_mods_from_titles:
        _bump(mod, 10.0, "related open PR reference")

    if primary_module:
        pk = canonical_module_key(primary_module)
        if pk in buckets:
            buckets[pk]["score"] += 8.0
            if "primary hypothesis module" not in buckets[pk]["reasons"]:
                buckets[pk]["reasons"].append("primary hypothesis module")

    coverage_reasons = _coverage_adjacency_reasons(project_id, set(buckets.keys()), label_map)
    for key, reason in coverage_reasons.items():
        if key in buckets:
            buckets[key]["score"] += 12.0
            buckets[key]["reasons"].append(reason)

    ranked: List[BlastRadiusModule] = []
    for key, data in sorted(buckets.items(), key=lambda x: -x[1]["score"]):
        score = min(100.0, round(float(data["score"]), 1))
        if score < 8:
            continue
        display = data.get("display") or module_display_label(key, labels_by_key=label_map)
        reason = " + ".join(data["reasons"][:4])
        ranked.append(BlastRadiusModule(module=display, score=score, reason=reason))

    return ranked[:12]


def _coverage_adjacency_reasons(
    project_id: Optional[str],
    module_keys: Set[str],
    label_map: Dict[str, str],
) -> Dict[str, str]:
    """Modules that share low coverage with affected modules (read-only)."""
    if not module_keys:
        return {}
    try:
        from services.coverage_service import CoverageService
        from services.module_canonical import canonical_module_key, module_display_label

        svc = CoverageService()
        summary = svc.get_summary()
        cov_by_key: Dict[str, float] = {}
        for row in summary:
            key = canonical_module_key(row.module)
            cov_by_key[key] = float(row.coverage_score or 0.0)

        out: Dict[str, str] = {}
        for key in module_keys:
            cov = cov_by_key.get(key)
            if cov is not None and cov < 0.5:
                label = module_display_label(key, labels_by_key=label_map)
                out[key] = f"low coverage in module '{label}' ({cov:.0%})"
        return out
    except Exception:
        return {}


def enrich_timeline_temporal(
    events: List[IncidentTimelineEvent],
) -> Tuple[List[IncidentTimelineEvent], TemporalCorrelationSummary]:
    """Add time_distance_minutes between consecutive events and overall correlation signal."""
    if not events:
        return [], TemporalCorrelationSummary(signal="none", reason="No timeline events")

    sorted_ev = sorted(
        events,
        key=lambda e: _parse_iso(e.timestamp) or datetime.min.replace(tzinfo=timezone.utc),
    )

    enriched: List[IncidentTimelineEvent] = []
    prev: Optional[IncidentTimelineEvent] = None
    gaps: List[int] = []

    for ev in sorted_ev:
        delta: Optional[int] = None
        rel: Optional[str] = None
        if prev and ev.timestamp and prev.timestamp:
            delta = _minutes_between(prev.timestamp, ev.timestamp)
            rel = prev.event_type
            if delta is not None and prev.event_type != "incident_reported":
                gaps.append(delta)
        enriched.append(ev.model_copy(update={
            "time_distance_minutes": delta,
            "relative_to_previous": rel,
        }))
        prev = ev

    signal, reason, chain = _temporal_correlation_signal(enriched, gaps)
    return enriched, TemporalCorrelationSummary(signal=signal, reason=reason, event_chain=chain)


def _temporal_correlation_signal(
    events: List[IncidentTimelineEvent],
    gaps: List[int],
) -> Tuple[str, str, List[str]]:
    """Detect PR → alert → failure chains without LLM."""
    chain_labels: List[str] = []
    typed = [e for e in events if e.event_type != "incident_reported"]

    pr_times = [_parse_iso(e.timestamp) for e in typed if e.event_type == "pr_analyzed"]
    alert_times = [_parse_iso(e.timestamp) for e in typed if e.event_type == "browser_watch_alert"]
    run_times = [_parse_iso(e.timestamp) for e in typed if e.event_type == "run_failed"]

    if pr_times and alert_times and run_times:
        pr_t, alert_t, run_t = pr_times[0], alert_times[0], run_times[0]
        if pr_t and alert_t and run_t and pr_t <= alert_t <= run_t:
            gap1 = int((alert_t - pr_t).total_seconds() / 60)
            gap2 = int((run_t - alert_t).total_seconds() / 60)
            chain_labels = [
                f"PR analyzed",
                f"↓ {gap1} min",
                f"Browser alert",
                f"↓ {gap2} min",
                f"Failed run(s)",
            ]
            if gap1 <= 30 and gap2 <= 30:
                return "strong", "PR analysis, browser alert, and failed runs within 30 min windows", chain_labels
            if gap1 <= 120 and gap2 <= 120:
                return "medium", "PR → alert → failure sequence within 2 hours", chain_labels
            return "weak", "PR → alert → failure sequence detected with wide gaps", chain_labels

    if gaps:
        avg_gap = sum(gaps) / len(gaps)
        if avg_gap <= 30:
            return "strong", f"Timeline events average {avg_gap:.0f} min apart", chain_labels
        if avg_gap <= 120:
            return "medium", f"Timeline events average {avg_gap:.0f} min apart", chain_labels
        return "weak", f"Timeline events spread over {avg_gap:.0f} min average gaps", chain_labels

    if len(typed) >= 2:
        return "weak", "Multiple events in window but no clear causal chain", chain_labels

    return "none", "Insufficient temporal events for correlation", chain_labels


def build_recommended_tests_v2(
    *,
    related_runs: List[RelatedRunSummary],
    regressions: List[Dict[str, Any]],
    pr_analysis_tests: List[str],
    primary_module: Optional[str],
    impacted_modules_ranked: List[BlastRadiusModule],
    time_window_hours: int,
) -> List[RecommendedTestRecommendation]:
    """Explainable test recommendations with strength — no execution."""
    from services.module_canonical import canonical_module_key

    primary_key = canonical_module_key(primary_module) if primary_module else ""
    high_modules = {
        canonical_module_key(m.module)
        for m in impacted_modules_ranked[:3]
        if m.score >= 50
    }

    items: List[RecommendedTestRecommendation] = []
    seen: Set[str] = set()

    def _add(
        tid: str,
        *,
        name: str = "",
        module: str = "",
        reason: str,
        strength: str,
    ) -> None:
        t = tid.strip()
        if not t or t in seen:
            return
        seen.add(t)
        items.append(RecommendedTestRecommendation(
            test_case_id=t,
            name=name,
            module=module,
            reason=reason,
            recommendation_strength=strength,  # type: ignore[arg-type]
        ))

    for r in related_runs:
        tid = (r.test_id or "").strip()
        mod_key = canonical_module_key(r.module)
        if not tid:
            continue
        if primary_key and mod_key == primary_key:
            _add(
                tid,
                name=r.test_name or "",
                module=r.module or "",
                reason=f"Touches primary hypothesis module {r.module or primary_module}",
                strength="high",
            )
        elif mod_key in high_modules:
            _add(
                tid,
                name=r.test_name or "",
                module=r.module or "",
                reason=f"Failed run in high blast-radius module {r.module}",
                strength="high",
            )
        else:
            _add(
                tid,
                name=r.test_name or "",
                module=r.module or "",
                reason="Correlated failed run in investigation window",
                strength="medium",
            )

    for reg in regressions[:8]:
        tid = str(reg.get("test_case_id") or "").strip()
        if not tid:
            continue
        count = int(reg.get("repeated_failures") or reg.get("count") or 2)
        mod = str(reg.get("module") or "")
        days_approx = max(1, time_window_hours // 24)
        _add(
            tid,
            name=str(reg.get("summary") or reg.get("test_name") or ""),
            module=mod,
            reason=f"Failed {count} time(s) in last {days_approx} day(s) regression pattern",
            strength="high" if count >= 2 else "medium",
        )

    for tid in pr_analysis_tests[:8]:
        _add(
            tid,
            reason="Recommended by matched PR Analysis snapshot",
            strength="medium",
        )

    return items[:15]
