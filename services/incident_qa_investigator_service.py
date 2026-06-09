# services/incident_qa_investigator_service.py
"""
Project-scoped Incident Investigator — QA Intelligence correlation (deterministic).

v1.1: persistence, PR Analysis correlation, timeline, confidence breakdown.
v1.3: hypothesis ranking, actions_available (analyze-only), aligned confidence.
v1.3B: evidence strength, blast radius, temporal correlation, recommended tests v2.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Set, Tuple

from models.incident_models import (
    ConfidenceFactor,
    IncidentActionAvailable,
    IncidentHypothesis,
    IncidentInvestigationReportRecord,
    IncidentInvestigationRun,
    InvestigateIncidentRequest,
    ProjectIncidentInvestigationListResponse,
    ProjectIncidentInvestigationReport,
    ProjectInvestigateIncidentRequest,
    RelatedRunSummary,
)
from services.incident_qa_context_service import (
    extract_topic_hints,
    gather_failed_runs,
    gather_failure_clusters,
    gather_knowledge_context,
    gather_open_prs,
    gather_regressions,
    gather_related_evidence,
    gather_related_pr_analysis,
)
from services.incident_timeline_service import (
    build_incident_timeline,
    gather_browser_watch_events,
)

logger = logging.getLogger("vanya.incident_qa_investigator")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _match_runs_by_hints(
    runs: List[RelatedRunSummary],
    hints: Set[str],
) -> List[RelatedRunSummary]:
    if not hints:
        return runs[:10]
    matched = []
    for r in runs:
        blob = f"{r.test_name} {r.test_id} {r.module} {r.error_summary or ''} {r.rca_summary or ''}".lower()
        if any(h in blob for h in hints if len(h) >= 3):
            matched.append(r)
    return matched[:10] if matched else runs[:5]


def _impacted_modules(
    runs: List[RelatedRunSummary],
    clusters: List[Dict[str, Any]],
    knowledge_ctx: Any,
    hints: Set[str],
    pr_analysis_mods: Optional[List[str]] = None,
) -> List[str]:
    mods: List[str] = []
    seen: Set[str] = set()
    for r in runs:
        m = (r.module or "").strip()
        if m and m.lower() not in seen:
            seen.add(m.lower())
            mods.append(m)
    for c in clusters[:5]:
        m = str(c.get("module") or "").strip()
        if m and m.lower() not in seen:
            seen.add(m.lower())
            mods.append(m)
    for m in (pr_analysis_mods or []):
        name = str(m or "").strip()
        if name and name.lower() not in seen:
            seen.add(name.lower())
            mods.append(name)
    if knowledge_ctx and getattr(knowledge_ctx, "modules", None):
        for m in knowledge_ctx.modules:
            name = str(m or "").strip()
            if not name:
                continue
            if hints and name.lower() not in hints and not any(h in name.lower() for h in hints):
                continue
            if name.lower() not in seen:
                seen.add(name.lower())
                mods.append(name)
    return mods[:12]


def _recommended_tests(
    runs: List[RelatedRunSummary],
    regressions: List[Dict[str, Any]],
    knowledge_ctx: Any,
    pr_analysis_tests: Optional[List[str]] = None,
) -> List[str]:
    rec: List[str] = []
    seen: Set[str] = set()
    for tid in (pr_analysis_tests or []):
        t = str(tid or "").strip()
        if t and t not in seen:
            seen.add(t)
            rec.append(t)
    for r in runs:
        tid = (r.test_id or "").strip()
        if tid and tid not in seen:
            seen.add(tid)
            rec.append(tid)
    for reg in regressions[:5]:
        tid = str(reg.get("test_case_id") or "").strip()
        if tid and tid not in seen:
            seen.add(tid)
            rec.append(tid)
    if knowledge_ctx and getattr(knowledge_ctx, "related_tests", None):
        for tid in knowledge_ctx.related_tests:
            t = str(tid or "").strip()
            if t and t not in seen:
                seen.add(t)
                rec.append(t)
    return rec[:15]


def _build_hypotheses(
    *,
    description: str,
    hints: Set[str],
    related_runs: List[RelatedRunSummary],
    clusters: List[Dict[str, Any]],
    regressions: List[Dict[str, Any]],
    knowledge_ctx: Any,
    related_prs: List[Any],
    related_pr_analysis: List[Any],
) -> List[IncidentHypothesis]:
    hypotheses: List[IncidentHypothesis] = []

    if related_runs:
        top = related_runs[0]
        hypotheses.append(IncidentHypothesis(
            statement=(
                f"Recent test failure '{top.test_name or top.test_id}' may be related "
                f"({top.error_summary or top.rca_summary or 'see run evidence'})."
            ),
            confidence=0.75 if top.error_summary or top.rca_summary else 0.55,
            basis="evidence",
            supporting_refs=[f"run:{top.run_id}"],
        ))

    for pra in related_pr_analysis[:2]:
        hypotheses.append(IncidentHypothesis(
            statement=(
                f"Stored PR Analysis for #{pra.pr_number} shows {pra.risk_level} risk "
                f"({pra.pr_risk_score:.0f}/100) — {pra.reason}."
            ),
            confidence=0.72 if pra.reason.startswith("same module") else 0.58,
            basis="evidence",
            supporting_refs=[f"pr_analysis:{pra.provider}:{pra.pr_number}"],
        ))

    for c in clusters[:3]:
        mod = str(c.get("module") or "unknown")
        cat = str(c.get("root_cause_category") or "unknown")
        fails = int(c.get("total_failures") or 0)
        if hints and mod.lower() not in hints and not any(h in mod.lower() for h in hints):
            continue
        hypotheses.append(IncidentHypothesis(
            statement=(
                f"Failure cluster in module '{mod}' ({cat}, {fails} recent failures) "
                f"may explain the incident."
            ),
            confidence=min(0.85, 0.5 + fails * 0.05),
            basis="evidence",
            supporting_refs=[f"cluster:{c.get('cluster_id', mod)}"],
        ))

    for reg in regressions[:2]:
        tid = str(reg.get("test_case_id") or "")
        mod = str(reg.get("module") or "")
        if not tid:
            continue
        hypotheses.append(IncidentHypothesis(
            statement=f"Recurring regression on test '{tid}' in module '{mod}'.",
            confidence=0.7,
            basis="evidence",
            supporting_refs=[f"regression:{tid}"],
        ))

    if knowledge_ctx and getattr(knowledge_ctx, "hints", None):
        for hint in (knowledge_ctx.hints or [])[:2]:
            hypotheses.append(IncidentHypothesis(
                statement=hint,
                confidence=0.6,
                basis="evidence" if "recent" in hint.lower() or "failure" in hint.lower() else "inference",
                supporting_refs=["knowledge:system_memory"],
            ))

    if related_prs and not related_pr_analysis:
        pr = related_prs[0]
        hypotheses.append(IncidentHypothesis(
            statement=(
                f"Recent open PR #{pr.pr_id} '{pr.title}' ({pr.provider}) may have introduced the issue."
            ),
            confidence=0.45,
            basis="inference",
            supporting_refs=[f"pr:{pr.provider}:{pr.pr_id}"],
        ))

    if any(h in hints for h in ("auth", "login")) and not any("auth" in h.statement.lower() for h in hypotheses):
        hypotheses.append(IncidentHypothesis(
            statement="Incident description suggests an authentication or session flow problem.",
            confidence=0.4,
            basis="assumption",
            supporting_refs=["keyword:auth"],
        ))

    if not hypotheses:
        hypotheses.append(IncidentHypothesis(
            statement="No strong correlates found in recent QA data — manual triage recommended.",
            confidence=0.2,
            basis="assumption",
            supporting_refs=[],
        ))

    return hypotheses[:8]


def _rank_hypotheses(hypotheses: List[IncidentHypothesis]) -> List[IncidentHypothesis]:
    """Sort by confidence desc; evidence-backed ties break ahead of inference."""
    ordered = sorted(
        hypotheses,
        key=lambda h: (-h.confidence, {"evidence": 0, "inference": 1, "assumption": 2}.get(h.basis, 3)),
    )
    ranked: List[IncidentHypothesis] = []
    for i, h in enumerate(ordered, start=1):
        ranked.append(h.model_copy(update={"id": f"H{i}", "rank": i}))
    return ranked


def _hypothesis_from_browser_probe(browser_run: IncidentInvestigationRun) -> Optional[IncidentHypothesis]:
    cause = (browser_run.probable_cause or browser_run.diagnosis_summary or "").strip()
    if not cause:
        return None
    basis: str = "evidence" if browser_run.reproduced == "true" else "inference"
    confidence = 0.78 if basis == "evidence" else 0.55
    if browser_run.severity in ("critical", "high"):
        confidence = min(0.88, confidence + 0.05)
    return IncidentHypothesis(
        statement=f"Browser probe observation: {cause}",
        confidence=confidence,
        basis=basis,  # type: ignore[arg-type]
        supporting_refs=[f"browser:{browser_run.id}"],
    )


def _align_global_confidence(
    hypotheses: List[IncidentHypothesis],
    breakdown_score: float,
    factors: List[ConfidenceFactor],
) -> Tuple[float, List[ConfidenceFactor]]:
    """Align report confidence with rank-1 hypothesis; cap when evidence is weak."""
    if not hypotheses:
        aligned = min(breakdown_score, 0.25)
        factors = list(factors) + [ConfidenceFactor(
            label="primary_hypothesis",
            delta=0.0,
            reason="No hypotheses — confidence capped low",
        )]
        return round(max(0.05, aligned), 2), factors

    primary = hypotheses[0]
    aligned = primary.confidence
    if primary.basis != "evidence":
        aligned = min(aligned, breakdown_score)
    elif not primary.supporting_refs:
        aligned = min(aligned, 0.45)
    else:
        aligned = min(max(aligned, breakdown_score * 0.85), 0.95)

    factors = list(factors) + [ConfidenceFactor(
        label="primary_hypothesis",
        delta=0.0,
        reason=f"Global confidence aligned to {primary.id} ({primary.confidence:.0%})",
    )]
    return round(max(0.05, min(0.95, aligned)), 2), factors


def _build_actions_available(
    *,
    req: ProjectInvestigateIncidentRequest,
    related_runs: List[RelatedRunSummary],
    related_prs: List[Any],
    related_pr_analysis: List[Any],
    recommended_tests: List[str],
    browser_run: Optional[IncidentInvestigationRun],
) -> List[IncidentActionAvailable]:
    """Recommend follow-up actions — never executed automatically in v1.3A."""
    actions: List[IncidentActionAvailable] = []

    if not req.include_browser_probe and (req.target_url or "").strip():
        actions.append(IncidentActionAvailable(
            action="run_browser_probe",
            label="Run Browser Probe",
            requires_user_approval=True,
            reason="Could collect live UI evidence",
        ))

    if related_prs and not related_pr_analysis:
        actions.append(IncidentActionAvailable(
            action="analyze_related_pr",
            label="Analyze related PR",
            requires_user_approval=True,
            reason="Relevant PR found but no PR analysis snapshot exists",
        ))

    if related_runs:
        actions.append(IncidentActionAvailable(
            action="generate_rca",
            label="Generate RCA",
            requires_user_approval=True,
            reason="Failed runs found",
        ))

    if recommended_tests:
        actions.append(IncidentActionAvailable(
            action="run_recommended_tests",
            label="Run recommended tests",
            requires_user_approval=True,
            reason="Recommended tests were identified",
        ))

    return actions


def _compute_confidence_v2(
    *,
    related_runs: List[RelatedRunSummary],
    related_evidence: List[Any],
    related_pr_analysis: List[Any],
    browser_events: List[Dict[str, Any]],
    hypotheses: List[IncidentHypothesis],
    clusters: List[Dict[str, Any]],
    knowledge_ctx: Any,
    data_gaps: List[str],
    impacted_modules: List[str],
    module_overlap_with_pr: bool,
) -> Tuple[float, List[ConfidenceFactor]]:
    score = 0.10
    factors: List[ConfidenceFactor] = []

    if related_evidence:
        delta = min(0.18, 0.05 * len(related_evidence))
        score += delta
        factors.append(ConfidenceFactor(
            label="evidence_records",
            delta=round(delta, 3),
            reason=f"{len(related_evidence)} evidence record(s) from failed runs",
        ))

    if related_runs:
        delta = min(0.22, 0.07 * len(related_runs))
        score += delta
        factors.append(ConfidenceFactor(
            label="failed_runs",
            delta=round(delta, 3),
            reason=f"{len(related_runs)} correlated failed run(s)",
        ))

    if related_pr_analysis:
        delta = min(0.16, 0.06 * len(related_pr_analysis))
        score += delta
        factors.append(ConfidenceFactor(
            label="pr_analysis",
            delta=round(delta, 3),
            reason=f"{len(related_pr_analysis)} stored PR Analysis report(s) matched",
        ))

    if module_overlap_with_pr:
        score += 0.10
        factors.append(ConfidenceFactor(
            label="module_overlap",
            delta=0.10,
            reason="PR Analysis impacted modules overlap with incident modules",
        ))

    if browser_events:
        delta = min(0.12, 0.04 * len(browser_events))
        score += delta
        factors.append(ConfidenceFactor(
            label="browser_watch",
            delta=round(delta, 3),
            reason=f"{len(browser_events)} Browser Watch alert(s) in window",
        ))

    if clusters:
        delta = min(0.14, 0.05 * len(clusters))
        score += delta
        factors.append(ConfidenceFactor(
            label="failure_clusters",
            delta=round(delta, 3),
            reason=f"{len(clusters)} failure intelligence cluster(s)",
        ))

    if knowledge_ctx and getattr(knowledge_ctx, "hints", None):
        delta = min(0.10, 0.04 * len(knowledge_ctx.hints))
        score += delta
        factors.append(ConfidenceFactor(
            label="system_memory",
            delta=round(delta, 3),
            reason=f"{len(knowledge_ctx.hints)} System Memory hint(s)",
        ))

    evidence_h = sum(1 for h in hypotheses if h.basis == "evidence")
    if evidence_h:
        delta = min(0.12, 0.04 * evidence_h)
        score += delta
        factors.append(ConfidenceFactor(
            label="evidence_hypotheses",
            delta=round(delta, 3),
            reason=f"{evidence_h} hypothesis(es) backed by evidence",
        ))

    if not related_runs:
        score -= 0.08
        factors.append(ConfidenceFactor(
            label="no_failed_runs",
            delta=-0.08,
            reason="No correlated failed runs in time window",
        ))

    if not related_pr_analysis and not related_runs:
        score -= 0.05
        factors.append(ConfidenceFactor(
            label="no_pr_analysis",
            delta=-0.05,
            reason="No stored PR Analysis reports matched",
        ))

    if not related_evidence:
        score -= 0.04
        factors.append(ConfidenceFactor(
            label="no_evidence",
            delta=-0.04,
            reason="No run evidence URLs or error summaries",
        ))

    gap_penalty = min(0.15, 0.03 * len(data_gaps))
    if gap_penalty:
        score -= gap_penalty
        factors.append(ConfidenceFactor(
            label="data_gaps",
            delta=-round(gap_penalty, 3),
            reason=f"{len(data_gaps)} data gap(s) reported",
        ))

    if not impacted_modules:
        score -= 0.03
        factors.append(ConfidenceFactor(
            label="no_modules",
            delta=-0.03,
            reason="No impacted modules identified",
        ))

    score = max(0.05, min(0.95, score))
    return round(score, 2), factors


def investigate_project_incident(
    project_id: str,
    req: ProjectInvestigateIncidentRequest,
) -> ProjectIncidentInvestigationReport:
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")
    if project_repo.get_project(pid) is None:
        raise LookupError(f"project not found: {pid}")

    desc = req.description.strip()
    hints = extract_topic_hints(desc, module=req.module)
    data_gaps: List[str] = []
    evidence_found: List[str] = []
    now = _utc_iso()

    failed_runs = gather_failed_runs(pid, time_window_hours=req.time_window_hours)
    if not failed_runs:
        data_gaps.append(
            f"No failed test runs found in the last {req.time_window_hours}h for this project."
        )
    else:
        evidence_found.append(f"{len(failed_runs)} failed run(s) in time window")

    related_runs = _match_runs_by_hints(failed_runs, hints)
    related_evidence = gather_related_evidence(related_runs)
    if related_evidence:
        evidence_found.append(f"{len(related_evidence)} evidence record(s) from failed runs")

    clusters = gather_failure_clusters(
        pid,
        time_window_hours=req.time_window_hours,
        in_window_run_ids=[r.run_id for r in failed_runs],
    )
    if not clusters:
        data_gaps.append("No failure intelligence clusters available.")

    regressions = gather_regressions(pid)
    knowledge_ctx = gather_knowledge_context(pid, description=desc, target_url=req.target_url)
    if knowledge_ctx is None:
        data_gaps.append("System Memory (project knowledge) not available — refresh knowledge first.")
    elif knowledge_ctx.hints:
        evidence_found.append(f"{len(knowledge_ctx.hints)} knowledge hint(s)")

    related_prs = gather_open_prs(pid, description=desc)
    if not related_prs:
        data_gaps.append("No matching open PRs from connected GitHub/Azure DevOps (or SCM not connected).")

    prelim_modules = _impacted_modules(related_runs, clusters, knowledge_ctx, hints)
    related_pr_analysis = gather_related_pr_analysis(
        pid,
        related_prs=related_prs,
        impacted_modules=prelim_modules,
        hints=hints,
        time_window_hours=req.time_window_hours,
    )
    if not related_pr_analysis:
        data_gaps.append(
            "No stored PR Analysis reports matched — analyze PRs in PR Intelligence first."
        )
    else:
        evidence_found.append(f"{len(related_pr_analysis)} stored PR Analysis report(s)")

    pr_analysis_mods: List[str] = []
    pr_analysis_tests: List[str] = []
    for pra in related_pr_analysis:
        pr_analysis_mods.extend(pra.impacted_modules)
        pr_analysis_tests.extend(pra.recommended_tests)

    module_overlap_with_pr = bool(
        prelim_modules
        and pr_analysis_mods
        and {m.lower() for m in prelim_modules} & {m.lower() for m in pr_analysis_mods}
    )

    impacted_modules = _impacted_modules(
        related_runs, clusters, knowledge_ctx, hints, pr_analysis_mods=pr_analysis_mods,
    )
    recommended_tests = _recommended_tests(
        related_runs, regressions, knowledge_ctx, pr_analysis_tests=pr_analysis_tests,
    )

    browser_events = gather_browser_watch_events(
        pid, time_window_hours=req.time_window_hours,
    )
    if browser_events:
        evidence_found.append(f"{len(browser_events)} Browser Watch alert(s)")

    hypotheses = _build_hypotheses(
        description=desc,
        hints=hints,
        related_runs=related_runs,
        clusters=clusters,
        regressions=regressions,
        knowledge_ctx=knowledge_ctx,
        related_prs=related_prs,
        related_pr_analysis=related_pr_analysis,
    )
    breakdown_score, confidence_breakdown = _compute_confidence_v2(
        related_runs=related_runs,
        related_evidence=related_evidence,
        related_pr_analysis=related_pr_analysis,
        browser_events=browser_events,
        hypotheses=hypotheses,
        clusters=clusters,
        knowledge_ctx=knowledge_ctx,
        data_gaps=data_gaps,
        impacted_modules=impacted_modules,
        module_overlap_with_pr=module_overlap_with_pr,
    )

    browser_run: Optional[IncidentInvestigationRun] = None
    if req.include_browser_probe and (req.target_url or "").strip():
        try:
            from services.incident_investigator_service import investigate_incident

            browser_run = investigate_incident(InvestigateIncidentRequest(
                incident_description=desc,
                target_url=req.target_url,
                project_id=pid,
                module=req.module,
            ))
            evidence_found.append("browser_probe_completed")
            browser_hyp = _hypothesis_from_browser_probe(browser_run)
            if browser_hyp:
                hypotheses.append(browser_hyp)
        except Exception as e:
            logger.warning("incident_qa: browser probe failed: %s", e)
            data_gaps.append(f"Browser probe failed: {type(e).__name__}")

    hypotheses = _rank_hypotheses(hypotheses)
    primary_hypothesis_id = hypotheses[0].id if hypotheses else None
    confidence, confidence_breakdown = _align_global_confidence(
        hypotheses, breakdown_score, confidence_breakdown,
    )

    summary_parts = [f"Investigated incident for project '{pid}' over {req.time_window_hours}h."]
    if related_runs:
        summary_parts.append(f"Found {len(related_runs)} correlated failed run(s).")
    else:
        summary_parts.append("No correlated failed runs in the time window.")
    if related_pr_analysis:
        top = related_pr_analysis[0]
        summary_parts.append(
            f"Matched PR Analysis #{top.pr_number} (risk {top.pr_risk_score:.0f}/100, {top.risk_level})."
        )
    if impacted_modules:
        summary_parts.append(f"Possibly impacted modules: {', '.join(impacted_modules[:5])}.")

    next_steps: List[str] = []
    if related_runs:
        next_steps.append("Review correlated failed runs and open evidence URLs for stack traces/screenshots.")
    if related_pr_analysis:
        next_steps.append("Review matched PR Analysis reports for risk signals and recommended tests.")
    if recommended_tests:
        next_steps.append(f"Re-run recommended tests: {', '.join(recommended_tests[:5])}.")
    if related_prs and not related_pr_analysis:
        next_steps.append("Run PR Analysis on matched open PRs to enrich incident correlation.")
    if not related_runs and not related_prs:
        next_steps.append("Run catalog tests for the affected module or enable Browser Watch on the target URL.")
    if req.include_browser_probe and req.target_url:
        next_steps.append("Browser probe requested — see browser_investigation section.")
    elif (req.target_url or "").strip():
        next_steps.append("Browser probe may provide additional evidence — enable include_browser_probe to run it.")
    next_steps.append("Validate hypotheses against production logs; treat inference-only items as leads, not proof.")

    timeline = build_incident_timeline(
        time_window_hours=req.time_window_hours,
        related_runs=related_runs,
        related_pr_analysis=related_pr_analysis,
        browser_events=browser_events,
        incident_reported_at=now,
        clusters=clusters,
    )

    from services.incident_analysis_service import (
        attach_hypothesis_classifications,
        build_blast_radius,
        build_evidence_strength,
        build_recommended_tests_v2,
        enrich_timeline_temporal,
    )

    primary_module = impacted_modules[0] if impacted_modules else (req.module or "")
    impacted_modules_ranked = build_blast_radius(
        related_runs=related_runs,
        clusters=clusters,
        related_pr_analysis=related_pr_analysis,
        related_prs=related_prs,
        primary_module=primary_module,
        project_id=pid,
    )
    if impacted_modules_ranked:
        impacted_modules = [m.module for m in impacted_modules_ranked]

    recommended_tests_v2 = build_recommended_tests_v2(
        related_runs=related_runs,
        regressions=regressions,
        pr_analysis_tests=pr_analysis_tests,
        primary_module=primary_module,
        impacted_modules_ranked=impacted_modules_ranked,
        time_window_hours=req.time_window_hours,
    )
    recommended_tests = [t.test_case_id for t in recommended_tests_v2]

    evidence_strength = build_evidence_strength(
        related_runs=related_runs,
        related_evidence=related_evidence,
        related_pr_analysis=related_pr_analysis,
        browser_events=browser_events,
        clusters=clusters,
        related_prs=related_prs,
        hints=hints,
        hypotheses=hypotheses,
        browser_investigation=browser_run,
    )
    hypotheses = attach_hypothesis_classifications(hypotheses, evidence_strength)

    timeline, temporal_correlation = enrich_timeline_temporal(timeline)

    actions_available = _build_actions_available(
        req=req,
        related_runs=related_runs,
        related_prs=related_prs,
        related_pr_analysis=related_pr_analysis,
        recommended_tests=recommended_tests,
        browser_run=browser_run,
    )

    if temporal_correlation.signal in ("strong", "medium"):
        summary_parts.append(f"Temporal correlation: {temporal_correlation.signal} — {temporal_correlation.reason}.")
    if impacted_modules_ranked:
        top_blast = impacted_modules_ranked[0]
        summary_parts.append(
            f"Top blast-radius module: {top_blast.module} ({top_blast.score:.0f}/100)."
        )

    report = ProjectIncidentInvestigationReport(
        project_id=pid,
        description=desc,
        severity=req.severity,
        time_window_hours=req.time_window_hours,
        summary=" ".join(summary_parts),
        hypotheses=hypotheses,
        primary_hypothesis_id=primary_hypothesis_id,
        related_runs=related_runs,
        related_evidence=related_evidence,
        related_prs=related_prs,
        related_pr_analysis=related_pr_analysis,
        timeline=timeline,
        temporal_correlation=temporal_correlation,
        impacted_modules=impacted_modules,
        impacted_modules_ranked=impacted_modules_ranked,
        recommended_tests=recommended_tests,
        recommended_tests_v2=recommended_tests_v2,
        evidence_strength=evidence_strength,
        confidence=confidence,
        confidence_breakdown=confidence_breakdown,
        next_steps=next_steps,
        evidence_found=evidence_found,
        data_gaps=data_gaps,
        browser_investigation=browser_run,
        actions_available=actions_available,
        meta={
            "topic_hints": sorted(hints),
            "failure_clusters_count": len(clusters),
            "regressions_count": len(regressions),
            "knowledge_risk_level": getattr(knowledge_ctx, "risk_level", None) if knowledge_ctx else None,
            "browser_watch_alerts": len(browser_events),
            "engine_version": "incident-v1.3b",
            "analyze_only": True,
        },
    )

    try:
        from services.db.incident_report_repository import incident_report_repo

        report_id = incident_report_repo.save(
            project_id=pid,
            description=desc,
            severity=req.severity,
            summary=report.summary,
            confidence=confidence,
            report=report.model_dump(),
        )
        report.id = report_id
        report.created_at = now
    except Exception as e:
        logger.warning("incident_qa: failed to persist report project_id=%s: %s", pid, e)
        data_gaps.append("Report could not be persisted — results are ephemeral for this request.")
        report.data_gaps = data_gaps

    return report


def list_project_incident_history(
    project_id: str,
    *,
    limit: int = 50,
) -> ProjectIncidentInvestigationListResponse:
    from services.db.incident_report_repository import incident_report_repo
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")
    if project_repo.get_project(pid) is None:
        raise LookupError(f"project not found: {pid}")

    rows = incident_report_repo.list_reports(project_id=pid, limit=limit)
    items = [IncidentInvestigationReportRecord.model_validate(r) for r in rows]
    return ProjectIncidentInvestigationListResponse(items=items, total=len(items))


def get_project_incident_report(project_id: str, incident_id: str) -> ProjectIncidentInvestigationReport:
    from services.db.incident_report_repository import incident_report_repo
    from services.db.project_repository import project_repo

    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")
    if project_repo.get_project(pid) is None:
        raise LookupError(f"project not found: {pid}")

    data = incident_report_repo.get_for_project(pid, incident_id)
    if not data:
        raise LookupError(f"incident report not found: {incident_id}")
    return ProjectIncidentInvestigationReport.model_validate(data)
