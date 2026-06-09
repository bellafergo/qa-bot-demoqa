# services/incident_qa_investigator_service.py
"""
Project-scoped Incident Investigator — QA Intelligence correlation (deterministic).

Complements browser-based ``investigate_incident`` with run/evidence/PR/knowledge signals.
Does not invent evidence; separates findings from hypotheses.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Set

from models.incident_models import (
    IncidentHypothesis,
    IncidentInvestigationRun,
    InvestigateIncidentRequest,
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
)

logger = logging.getLogger("vanya.incident_qa_investigator")


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
) -> List[str]:
    rec: List[str] = []
    seen: Set[str] = set()
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

    if related_prs:
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
            basis="inference",
            supporting_refs=["keyword:auth"],
        ))

    if not hypotheses:
        hypotheses.append(IncidentHypothesis(
            statement="No strong correlates found in recent QA data — manual triage recommended.",
            confidence=0.2,
            basis="inference",
            supporting_refs=[],
        ))

    return hypotheses[:8]


def _compute_confidence(
    *,
    related_runs: List[RelatedRunSummary],
    related_evidence: List[Any],
    hypotheses: List[IncidentHypothesis],
    clusters: List[Dict[str, Any]],
    knowledge_ctx: Any,
) -> float:
    score = 0.15
    if related_runs:
        score += min(0.25, 0.08 * len(related_runs))
    if related_evidence:
        score += min(0.15, 0.05 * len(related_evidence))
    if clusters:
        score += min(0.2, 0.06 * len(clusters))
    if knowledge_ctx and getattr(knowledge_ctx, "hints", None):
        score += min(0.15, 0.05 * len(knowledge_ctx.hints))
    evidence_h = sum(1 for h in hypotheses if h.basis == "evidence")
    score += min(0.2, 0.05 * evidence_h)
    return round(min(0.95, score), 2)


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

    clusters = gather_failure_clusters(pid)
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

    impacted_modules = _impacted_modules(related_runs, clusters, knowledge_ctx, hints)
    recommended_tests = _recommended_tests(related_runs, regressions, knowledge_ctx)
    hypotheses = _build_hypotheses(
        description=desc,
        hints=hints,
        related_runs=related_runs,
        clusters=clusters,
        regressions=regressions,
        knowledge_ctx=knowledge_ctx,
        related_prs=related_prs,
    )
    confidence = _compute_confidence(
        related_runs=related_runs,
        related_evidence=related_evidence,
        hypotheses=hypotheses,
        clusters=clusters,
        knowledge_ctx=knowledge_ctx,
    )

    summary_parts = [f"Investigated incident for project '{pid}' over {req.time_window_hours}h."]
    if related_runs:
        summary_parts.append(f"Found {len(related_runs)} correlated failed run(s).")
    else:
        summary_parts.append("No correlated failed runs in the time window.")
    if impacted_modules:
        summary_parts.append(f"Possibly impacted modules: {', '.join(impacted_modules[:5])}.")
    if related_prs:
        summary_parts.append(f"{len(related_prs)} open PR(s) may be relevant.")

    next_steps: List[str] = []
    if related_runs:
        next_steps.append("Review correlated failed runs and open evidence URLs for stack traces/screenshots.")
    if recommended_tests:
        next_steps.append(f"Re-run recommended tests: {', '.join(recommended_tests[:5])}.")
    if related_prs:
        next_steps.append("Review matched open PRs for recent changes in affected areas.")
    if not related_runs and not related_prs:
        next_steps.append("Run catalog tests for the affected module or enable Browser Watch on the target URL.")
    if req.include_browser_probe and req.target_url:
        next_steps.append("Browser probe requested — see browser_investigation section.")
    next_steps.append("Validate hypotheses against production logs; treat inference-only items as leads, not proof.")

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
        except Exception as e:
            logger.warning("incident_qa: browser probe failed: %s", e)
            data_gaps.append(f"Browser probe failed: {type(e).__name__}")

    return ProjectIncidentInvestigationReport(
        project_id=pid,
        description=desc,
        severity=req.severity,
        time_window_hours=req.time_window_hours,
        summary=" ".join(summary_parts),
        hypotheses=hypotheses,
        related_runs=related_runs,
        related_evidence=related_evidence,
        related_prs=related_prs,
        impacted_modules=impacted_modules,
        recommended_tests=recommended_tests,
        confidence=confidence,
        next_steps=next_steps,
        evidence_found=evidence_found,
        data_gaps=data_gaps,
        browser_investigation=browser_run,
        meta={
            "topic_hints": sorted(hints),
            "failure_clusters_count": len(clusters),
            "regressions_count": len(regressions),
            "knowledge_risk_level": getattr(knowledge_ctx, "risk_level", None) if knowledge_ctx else None,
        },
    )
