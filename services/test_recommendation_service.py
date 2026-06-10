# services/test_recommendation_service.py
"""
Incident Investigator II-05B — Test Recommendation Engine (read-only).

Recommends which tests to run next from existing incident intelligence.
Never executes, schedules, or triggers workflows.
"""
from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Set

from models.incident_models import (
    DeploymentRiskAssessment,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentImpactNode,
    IncidentStorylineStep,
    InvestigationPlanItem,
    RecommendedAction,
    RecommendedTest,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
    TemporalCorrelationSummary,
    TestRecommendationReport,
)

_AREA_SUFFIXES = (" flow", " smoke", " suite", " regression", " ui")
_UI_KEYWORDS = ("ui", "visual", "checkout", "login", "page", "screen", "button")


@dataclass
class _RecDraft:
    test_name: str
    test_type: str
    reason: str
    confidence: float
    estimated_risk_reduction: float
    urgency: float
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


def _normalize_area(name: str) -> str:
    s = (name or "").strip().lower()
    for suffix in _AREA_SUFFIXES:
        if s.endswith(suffix):
            s = s[: -len(suffix)].strip()
    return re.sub(r"\s+", " ", s) or ""


def _display_area(name: str) -> str:
    key = _normalize_area(name)
    if not key:
        return "Unknown"
    if key in ("auth", "authentication", "login"):
        return "Login"
    parts = re.split(r"[\s_-]+", key)
    return " ".join(p.capitalize() for p in parts if p)


def _suite_for_area(area: str, *, kind: str = "smoke") -> str:
    label = _display_area(area)
    if kind == "regression":
        return f"{label} Regression Suite"
    return f"{label} Smoke Suite"


def build_recommendation_id(
    test_type: str,
    test_name: str,
    related_entity_type: Optional[str] = None,
    related_entity_id: Optional[str] = None,
) -> str:
    tt = (test_type or "smoke").strip()
    et = (related_entity_type or "").strip()
    eid = (related_entity_id or "").strip()
    if et and eid:
        return f"{tt}:{et}:{eid}"
    norm = re.sub(r"\s+", " ", (test_name or tt).strip().lower())
    digest = hashlib.sha256(norm.encode("utf-8")).hexdigest()[:12]
    return f"{tt}:title:{digest}"


def _is_ui_signal(text: str) -> bool:
    low = (text or "").lower()
    return any(k in low for k in _UI_KEYWORDS)


def _upsert(pool: Dict[str, _RecDraft], draft: _RecDraft) -> None:
    key = _normalize_area(draft.test_name.replace(" Regression Suite", "").replace(" Smoke Suite", ""))
    if not key:
        key = draft.test_name.strip().lower()
    existing = pool.get(key)
    if existing is None or draft.urgency > existing.urgency or (
        draft.urgency == existing.urgency and draft.confidence > existing.confidence
    ):
        pool[key] = draft


def _risk_boost(deployment_risk: Optional[DeploymentRiskAssessment]) -> float:
    if not deployment_risk:
        return 0.0
    if deployment_risk.risk_level == "critical":
        return 0.18
    if deployment_risk.risk_level == "high":
        return 0.12
    if deployment_risk.risk_level == "medium":
        return 0.06
    return 0.02


def _report_confidence(recommendations: List[RecommendedTest]) -> float:
    if not recommendations:
        return 0.0
    avg = sum(r.confidence for r in recommendations) / len(recommendations)
    return round(min(1.0, avg + min(0.08, len(recommendations) * 0.02)), 2)


def build_test_recommendations(
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
    deployment_risk_assessment: Optional[DeploymentRiskAssessment] = None,
    recommended_actions: List[RecommendedAction],
    limit: int = 8,
) -> Optional[TestRecommendationReport]:
    """Build read-only test recommendations from gathered incident signals."""
    pool: Dict[str, _RecDraft] = {}
    risk_boost = _risk_boost(deployment_risk_assessment)
    corr_strength = 0.0
    if evidence_correlation and evidence_correlation.evidence:
        corr_strength = max(float(e.confidence) for e in evidence_correlation.evidence)

    for node in impact_map:
        area = node.title
        key = _normalize_area(area)
        if not key:
            continue
        kind = "regression" if node.severity == "high" and node.related_entity_count >= 5 else "smoke"
        conf = min(0.95, float(node.confidence) + risk_boost)
        _upsert(pool, _RecDraft(
            test_name=_suite_for_area(area, kind=kind),
            test_type="regression" if kind == "regression" else "smoke",
            reason=f"Impacted area '{node.title}' shows {node.severity} severity with {node.related_entity_count} correlated signal(s).",
            confidence=conf,
            estimated_risk_reduction=min(0.4, 0.18 + node.related_entity_count * 0.02 + risk_boost),
            urgency=conf + (0.1 if node.severity == "high" else 0.05),
            related_entity_type=node.related_entity_type,
            related_entity_id=node.related_entity_id,
        ))

    module_fail_counts: Dict[str, int] = {}
    for run in related_runs:
        mod = (run.module or run.test_name or run.test_id or "").strip()
        if not mod:
            continue
        nk = _normalize_area(mod)
        module_fail_counts[nk] = module_fail_counts.get(nk, 0) + 1
        conf = min(0.92, 0.72 + module_fail_counts[nk] * 0.06 + risk_boost)
        _upsert(pool, _RecDraft(
            test_name=_suite_for_area(mod, kind="smoke"),
            test_type="smoke",
            reason=(
                f"Failed run {run.run_id} ({run.test_name or run.test_id}) "
                f"indicates instability in {mod}."
            ),
            confidence=conf,
            estimated_risk_reduction=min(0.35, 0.16 + module_fail_counts[nk] * 0.05),
            urgency=conf + 0.08,
            related_entity_type="run",
            related_entity_id=run.run_id,
        ))

    for cluster in clusters[:4]:
        mod = str(cluster.get("module") or "").strip()
        if not mod:
            continue
        fails = int(cluster.get("total_failures") or 0)
        cid = str(cluster.get("cluster_id") or mod).strip()
        conf = min(0.94, 0.68 + fails * 0.03 + risk_boost)
        _upsert(pool, _RecDraft(
            test_name=_suite_for_area(mod, kind="regression"),
            test_type="cluster_regression",
            reason=f"Failure cluster in '{mod}' ({fails} failure(s)) suggests regression coverage.",
            confidence=conf,
            estimated_risk_reduction=min(0.38, 0.2 + fails * 0.015),
            urgency=conf + 0.1,
            related_entity_type="failure_cluster",
            related_entity_id=cid,
        ))

    if browser_events:
        ui_hits = [
            ev for ev in browser_events
            if _is_ui_signal(str(ev.get("summary") or ""))
        ]
        watch_id = str((ui_hits[0] if ui_hits else browser_events[0]).get("watch_id") or "").strip()
        conf = min(0.9, 0.74 + len(browser_events) * 0.04 + risk_boost)
        _upsert(pool, _RecDraft(
            test_name="UI Smoke Suite",
            test_type="ui_smoke",
            reason="Browser Watch alerts detected UI or route changes correlated with the incident.",
            confidence=conf,
            estimated_risk_reduction=min(0.3, 0.14 + len(browser_events) * 0.04),
            urgency=conf + 0.06,
            related_entity_type="browser_watch" if watch_id else None,
            related_entity_id=watch_id or None,
        ))
        for ev in browser_events[:2]:
            summary = str(ev.get("summary") or "")
            area = _extract_area_from_summary(summary)
            if area:
                wid = str(ev.get("watch_id") or "").strip()
                _upsert(pool, _RecDraft(
                    test_name=_suite_for_area(area, kind="smoke"),
                    test_type="ui_smoke",
                    reason=f"Browser alert references {area}: {summary}",
                    confidence=min(0.88, conf),
                    estimated_risk_reduction=0.22,
                    urgency=conf,
                    related_entity_type="browser_watch" if wid else None,
                    related_entity_id=wid or None,
                ))

    for pra in related_pr_analysis[:3]:
        entity_id = f"{pra.provider}:{pra.pr_number}"
        for mod in pra.impacted_modules or ["changed functionality"]:
            conf = min(0.93, 0.62 + float(pra.pr_risk_score) / 200.0 + risk_boost)
            _upsert(pool, _RecDraft(
                test_name=_suite_for_area(mod, kind="smoke"),
                test_type="pr_validation",
                reason=(
                    f"PR #{pra.pr_number} ({pra.risk_level}) touches '{mod}' — "
                    "validate changed functionality."
                ),
                confidence=conf,
                estimated_risk_reduction=min(0.32, 0.15 + float(pra.pr_risk_score) / 400.0),
                urgency=conf + 0.07,
                related_entity_type="pr_analysis",
                related_entity_id=entity_id,
            ))

    for h in hypotheses[:2]:
        if h.confidence < 0.5 or h.basis == "assumption":
            continue
        area = _extract_area_from_hypothesis(h.statement)
        suite = _suite_for_area(area or "Incident", kind="smoke")
        conf = min(0.9, float(h.confidence) * 0.85 + risk_boost)
        hid = (h.id or f"H{h.rank}" or "hypothesis").strip()
        _upsert(pool, _RecDraft(
            test_name=suite if area else f"Hypothesis Validation: {hid}",
            test_type="hypothesis_validation",
            reason=f"Confirm or reject hypothesis: {h.statement}",
            confidence=conf,
            estimated_risk_reduction=min(0.28, float(h.confidence) * 0.3),
            urgency=conf + 0.05,
            related_entity_type="hypothesis",
            related_entity_id=hid,
        ))

    for action in recommended_actions:
        if action.action_type != "run_test_suite":
            continue
        area = action.title.replace("Run ", "").replace(" Smoke Suite", "").strip()
        _upsert(pool, _RecDraft(
            test_name=action.title if "Suite" in action.title else _suite_for_area(area),
            test_type="smoke",
            reason=action.reason or action.description,
            confidence=min(0.9, float(action.confidence) + risk_boost),
            estimated_risk_reduction=0.25,
            urgency=float(action.confidence) + 0.1,
            related_entity_type=action.related_entity_type,
            related_entity_id=action.related_entity_id,
        ))

    if temporal_correlation and temporal_correlation.signal in ("strong", "medium"):
        for key, draft in list(pool.items()):
            if draft.urgency < 0.75:
                pool[key] = _RecDraft(
                    test_name=draft.test_name,
                    test_type=draft.test_type,
                    reason=f"{draft.reason} Temporal correlation supports prioritization.",
                    confidence=min(0.95, draft.confidence + 0.04),
                    estimated_risk_reduction=min(0.4, draft.estimated_risk_reduction + 0.03),
                    urgency=draft.urgency + 0.05,
                    related_entity_type=draft.related_entity_type,
                    related_entity_id=draft.related_entity_id,
                )

    if corr_strength > 0 and pool:
        for key, draft in list(pool.items()):
            pool[key] = _RecDraft(
                test_name=draft.test_name,
                test_type=draft.test_type,
                reason=draft.reason,
                confidence=min(0.96, draft.confidence + corr_strength * 0.05),
                estimated_risk_reduction=draft.estimated_risk_reduction,
                urgency=draft.urgency + corr_strength * 0.03,
                related_entity_type=draft.related_entity_type,
                related_entity_id=draft.related_entity_id,
            )

    if not pool:
        return None

    ordered = sorted(
        pool.values(),
        key=lambda d: (-d.urgency, -d.confidence, -d.estimated_risk_reduction, d.test_name.lower()),
    )

    recommendations: List[RecommendedTest] = []
    for idx, draft in enumerate(ordered[: max(1, int(limit))], start=1):
        recommendations.append(
            RecommendedTest(
                recommendation_id=build_recommendation_id(
                    draft.test_type,
                    draft.test_name,
                    draft.related_entity_type,
                    draft.related_entity_id,
                ),
                test_name=draft.test_name,
                test_type=draft.test_type,
                priority=idx,
                confidence=round(max(0.0, min(1.0, draft.confidence)), 2),
                reason=draft.reason.strip(),
                estimated_risk_reduction=round(max(0.0, min(1.0, draft.estimated_risk_reduction)), 2),
                related_entity_type=draft.related_entity_type,
                related_entity_id=draft.related_entity_id,
                requires_user_approval=True,
            )
        )

    top = recommendations[0].test_name if recommendations else "key areas"
    summary = (
        f"Run {len(recommendations)} focused test suite(s) starting with '{top}' "
        "to reduce deployment uncertainty before approval."
    )
    if deployment_risk_assessment and deployment_risk_assessment.risk_level in ("high", "critical"):
        summary = (
            f"Elevated deployment risk ({deployment_risk_assessment.risk_score}/100) — "
            f"prioritize '{top}' and related suites before release."
        )

    return TestRecommendationReport(
        recommendations=recommendations,
        summary=summary,
        recommendation_confidence=_report_confidence(recommendations),
    )


def _extract_area_from_summary(text: str) -> Optional[str]:
    t = (text or "").strip()
    if not t:
        return None
    m = re.match(r"^([A-Za-z][A-Za-z0-9 _-]{1,30}?)\s+UI\b", t, re.I)
    if m:
        return m.group(1).strip()
    for word in ("checkout", "payments", "payment", "auth", "login", "search"):
        if word in t.lower():
            return word
    return None


def _extract_area_from_hypothesis(statement: str) -> Optional[str]:
    low = (statement or "").lower()
    for word in ("checkout", "payments", "payment", "auth", "login", "authentication", "search"):
        if word in low:
            return word
    return None
