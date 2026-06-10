# services/similar_incidents_service.py
"""
Incident Investigator II-06A — Historical Learning & Similar Incidents (read-only).

Deterministic similarity against prior investigation reports only.
No embeddings, LLM calls, execution paths, or new persistence.
"""
from __future__ import annotations

import re
from collections import Counter
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Set, Tuple

from models.incident_models import (
    DeploymentRiskAssessment,
    HistoricalLearningReport,
    IncidentHypothesis,
    IncidentImpactNode,
    IncidentStorylineStep,
    ProjectIncidentInvestigationReport,
    RecommendedAction,
    SimilarIncident,
    TestRecommendationReport,
)

_SIMILARITY_THRESHOLD = 0.40
_MAX_INCIDENTS = 10

_WEIGHT_IMPACTED_AREA = 0.25
_WEIGHT_FAILURE_CLUSTER = 0.25
_WEIGHT_HYPOTHESIS_TYPE = 0.15
_WEIGHT_BROWSER_WATCH = 0.15
_WEIGHT_RECOMMENDED_TESTS = 0.10
_WEIGHT_DEPLOYMENT_RISK = 0.10

_AREA_SUFFIXES = (" flow", " smoke", " suite", " regression", " ui", " module")
_HYPOTHESIS_KEYWORDS = (
    "auth",
    "authentication",
    "login",
    "checkout",
    "payment",
    "payments",
    "regression",
    "browser",
    "ui",
    "search",
)


@dataclass
class _IncidentSignals:
    areas: Set[str]
    cluster_ids: Set[str]
    modules: Set[str]
    pr_areas: Set[str]
    hypothesis_categories: Set[str]
    browser_watch_ids: Set[str]
    test_profiles: Set[str]
    risk_level: Optional[str]


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
    parts = re.split(r"[\s_-]+", key)
    return " ".join(p.capitalize() for p in parts if p)


def _hypothesis_categories(hypotheses: List[IncidentHypothesis]) -> Set[str]:
    cats: Set[str] = set()
    for h in hypotheses:
        if h.basis:
            cats.add(str(h.basis).strip().lower())
        low = (h.statement or "").lower()
        for kw in _HYPOTHESIS_KEYWORDS:
            if kw in low:
                cats.add(kw)
    return cats


def _areas_from_impact_map(impact_map: List[IncidentImpactNode]) -> Set[str]:
    return {_normalize_area(n.title) for n in impact_map if _normalize_area(n.title)}


def _cluster_ids_from_report(
    impact_map: List[IncidentImpactNode],
    storyline: List[IncidentStorylineStep],
    recommended_actions: List[RecommendedAction],
    failure_clusters: List[Dict[str, Any]],
) -> Set[str]:
    ids: Set[str] = set()
    for cluster in failure_clusters or []:
        cid = str(cluster.get("cluster_id") or cluster.get("module") or "").strip()
        if cid:
            ids.add(cid.lower())
    for node in impact_map:
        if node.related_entity_type == "failure_cluster" and (node.related_entity_id or "").strip():
            ids.add(str(node.related_entity_id).strip().lower())
    for step in storyline:
        if step.related_entity_type == "failure_cluster" and (step.related_entity_id or "").strip():
            ids.add(str(step.related_entity_id).strip().lower())
    for action in recommended_actions:
        if action.related_entity_type == "failure_cluster" and (action.related_entity_id or "").strip():
            ids.add(str(action.related_entity_id).strip().lower())
    return ids


def _browser_watch_ids(storyline: List[IncidentStorylineStep]) -> Set[str]:
    return {
        str(step.related_entity_id).strip().lower()
        for step in storyline
        if step.related_entity_type == "browser_watch" and (step.related_entity_id or "").strip()
    }


def _test_profiles(test_recommendations: Optional[TestRecommendationReport]) -> Set[str]:
    profiles: Set[str] = set()
    if not test_recommendations:
        return profiles
    for rec in test_recommendations.recommendations or []:
        area = _normalize_area(rec.test_name.replace(" Regression Suite", "").replace(" Smoke Suite", ""))
        tt = (rec.test_type or "smoke").strip().lower()
        if area:
            profiles.add(f"{tt}:{area}")
        elif tt:
            profiles.add(tt)
    return profiles


def _pr_areas_from_report(report: ProjectIncidentInvestigationReport) -> Set[str]:
    areas: Set[str] = set()
    for pra in report.related_pr_analysis or []:
        for mod in pra.impacted_modules or []:
            key = _normalize_area(mod)
            if key:
                areas.add(key)
    return areas


def _extract_signals(
    *,
    hypotheses: List[IncidentHypothesis],
    impact_map: List[IncidentImpactNode],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    storyline: List[IncidentStorylineStep],
    recommended_actions: List[RecommendedAction],
    test_recommendations: Optional[TestRecommendationReport],
    failure_clusters: List[Dict[str, Any]],
    impacted_modules: Optional[List[str]] = None,
    pr_areas: Optional[Set[str]] = None,
) -> _IncidentSignals:
    areas = _areas_from_impact_map(impact_map)
    for mod in impacted_modules or []:
        key = _normalize_area(mod)
        if key:
            areas.add(key)
    if pr_areas:
        areas |= pr_areas
    modules = {_normalize_area(m) for m in (impacted_modules or []) if _normalize_area(m)}
    return _IncidentSignals(
        areas=areas,
        cluster_ids=_cluster_ids_from_report(impact_map, storyline, recommended_actions, failure_clusters),
        modules=modules,
        pr_areas=pr_areas or set(),
        hypothesis_categories=_hypothesis_categories(hypotheses),
        browser_watch_ids=_browser_watch_ids(storyline),
        test_profiles=_test_profiles(test_recommendations),
        risk_level=(
            str(deployment_risk_assessment.risk_level).strip().lower()
            if deployment_risk_assessment
            else None
        ),
    )


def _signals_from_report(report: ProjectIncidentInvestigationReport) -> _IncidentSignals:
    clusters: List[Dict[str, Any]] = []
    count = int((report.meta or {}).get("failure_clusters_count") or 0)
    if count > 0:
        for node in report.impact_map or []:
            if node.related_entity_type == "failure_cluster" and node.related_entity_id:
                clusters.append({"cluster_id": node.related_entity_id, "module": node.title})
    return _extract_signals(
        hypotheses=report.hypotheses or [],
        impact_map=report.impact_map or [],
        deployment_risk_assessment=report.deployment_risk_assessment,
        storyline=report.storyline or [],
        recommended_actions=report.recommended_actions or [],
        test_recommendations=report.test_recommendations,
        failure_clusters=clusters,
        impacted_modules=report.impacted_modules or [],
        pr_areas=_pr_areas_from_report(report),
    )


def _similarity_score(current: _IncidentSignals, historical: _IncidentSignals) -> float:
    score = 0.0
    if current.areas and historical.areas and (current.areas & historical.areas):
        score += _WEIGHT_IMPACTED_AREA
    elif current.modules and historical.modules and (current.modules & historical.modules):
        score += _WEIGHT_IMPACTED_AREA
    elif current.pr_areas and historical.pr_areas and (current.pr_areas & historical.pr_areas):
        score += _WEIGHT_IMPACTED_AREA
    if current.cluster_ids and historical.cluster_ids and (current.cluster_ids & historical.cluster_ids):
        score += _WEIGHT_FAILURE_CLUSTER
    if (
        current.hypothesis_categories
        and historical.hypothesis_categories
        and (current.hypothesis_categories & historical.hypothesis_categories)
    ):
        score += _WEIGHT_HYPOTHESIS_TYPE
    if current.browser_watch_ids and historical.browser_watch_ids and (
        current.browser_watch_ids & historical.browser_watch_ids
    ):
        score += _WEIGHT_BROWSER_WATCH
    if current.test_profiles and historical.test_profiles and (current.test_profiles & historical.test_profiles):
        score += _WEIGHT_RECOMMENDED_TESTS
    if current.risk_level and historical.risk_level and current.risk_level == historical.risk_level:
        score += _WEIGHT_DEPLOYMENT_RISK
    return round(min(1.0, score), 2)


def _top_area_from_signals(signals: _IncidentSignals, report: ProjectIncidentInvestigationReport) -> str:
    if signals.areas:
        return _display_area(sorted(signals.areas)[0])
    if report.impacted_modules:
        return _display_area(report.impacted_modules[0])
    desc = (report.description or "").strip()
    if desc:
        return _display_area(desc.split()[0])
    return "Incident"


def _incident_title(report: ProjectIncidentInvestigationReport, signals: _IncidentSignals) -> str:
    area = _top_area_from_signals(signals, report)
    short_id = (report.id or "0000")[-4:]
    has_regression = any("regression" in p for p in signals.test_profiles)
    has_browser = bool(signals.browser_watch_ids) or any(
        s.related_entity_type == "browser_watch" for s in (report.storyline or [])
    )
    if has_browser:
        label = "Browser Failure"
    elif has_regression:
        label = "Regression"
    else:
        label = "Incident"
    return f"{area} {label} #{short_id}"


def _best_drilldown(report: ProjectIncidentInvestigationReport) -> Tuple[Optional[str], Optional[str]]:
    for node in report.impact_map or []:
        if node.related_entity_type and node.related_entity_id:
            return node.related_entity_type, node.related_entity_id
    for step in report.storyline or []:
        if step.related_entity_type and step.related_entity_id:
            return step.related_entity_type, step.related_entity_id
    for action in report.recommended_actions or []:
        if action.related_entity_type and action.related_entity_id:
            return action.related_entity_type, action.related_entity_id
    return None, None


def _incident_summary(report: ProjectIncidentInvestigationReport, score: float) -> str:
    base = (report.summary or report.description or "").strip()
    if not base:
        base = "Prior investigation with overlapping quality signals."
    if len(base) > 160:
        base = f"{base[:157]}..."
    return f"{base} (similarity {int(round(score * 100))}%)."


def _pattern_summary(
    similar: List[SimilarIncident],
    *,
    current_signals: _IncidentSignals,
    browser_hits: int,
    top_test_name: Optional[str],
) -> str:
    if not similar:
        return ""

    area_counter: Counter[str] = Counter()
    regression_hits = 0
    for item in similar:
        title = item.title or ""
        area = title.split(" ", 1)[0] if title else ""
        if area:
            area_counter[area] += 1
        if "regression" in title.lower() or "regression" in (item.summary or "").lower():
            regression_hits += 1

    parts: List[str] = []
    n = len(similar)
    if area_counter:
        top_area, count = area_counter.most_common(1)[0]
        if regression_hits >= max(2, n // 2):
            parts.append(
                f"{count} recent incident(s) involved {top_area} and resulted in regression-related failures."
            )
        elif count >= 2:
            parts.append(f"{count} similar incident(s) involved {top_area}.")
        else:
            parts.append(f"{n} similar incident(s) involved {top_area}.")

    primary_area = _display_area(sorted(current_signals.areas)[0]) if current_signals.areas else None
    if top_test_name and primary_area:
        parts.append(f"Most incidents affecting {primary_area} required {top_test_name} validation.")

    if browser_hits >= 2 and n >= 2:
        parts.append(f"Browser alerts appeared in {browser_hits} of the last {n} similar incidents.")

    if not parts:
        return (
            f"{n} similar incident(s) share overlapping quality signals with the current investigation."
        )
    return " ".join(parts)


def _report_confidence(scores: List[float]) -> float:
    if not scores:
        return 0.0
    avg = sum(scores) / len(scores)
    return round(min(1.0, 0.45 + avg * 0.35 + min(0.15, len(scores) * 0.03)), 2)


def _sort_key(item: SimilarIncident) -> Tuple[float, str]:
    ts = item.occurrence_timestamp or ""
    return (-float(item.similarity_score), ts)


def build_historical_learning(
    *,
    hypotheses: List[IncidentHypothesis],
    impact_map: List[IncidentImpactNode],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    storyline: List[IncidentStorylineStep],
    recommended_actions: List[RecommendedAction],
    test_recommendations: Optional[TestRecommendationReport],
    failure_clusters: List[Dict[str, Any]],
    previous_reports: List[ProjectIncidentInvestigationReport],
    impacted_modules: Optional[List[str]] = None,
    limit: int = _MAX_INCIDENTS,
) -> Optional[HistoricalLearningReport]:
    """Build read-only historical learning from prior investigation reports."""
    if not previous_reports:
        return None

    pr_areas: Set[str] = set()
    for mod in impacted_modules or []:
        key = _normalize_area(mod)
        if key:
            pr_areas.add(key)

    current = _extract_signals(
        hypotheses=hypotheses,
        impact_map=impact_map,
        deployment_risk_assessment=deployment_risk_assessment,
        storyline=storyline,
        recommended_actions=recommended_actions,
        test_recommendations=test_recommendations,
        failure_clusters=failure_clusters,
        impacted_modules=impacted_modules,
        pr_areas=pr_areas,
    )

    if not (
        current.areas
        or current.cluster_ids
        or current.hypothesis_categories
        or current.browser_watch_ids
        or current.test_profiles
        or current.risk_level
    ):
        return None

    drafts: List[SimilarIncident] = []
    scores: List[float] = []
    browser_hits = 0

    for report in previous_reports:
        if not report.id:
            continue
        historical = _signals_from_report(report)
        score = _similarity_score(current, historical)
        if score < _SIMILARITY_THRESHOLD:
            continue
        if historical.browser_watch_ids:
            browser_hits += 1
        entity_type, entity_id = _best_drilldown(report)
        drafts.append(
            SimilarIncident(
                incident_id=report.id,
                title=_incident_title(report, historical),
                similarity_score=score,
                summary=_incident_summary(report, score),
                occurrence_timestamp=report.created_at,
                related_entity_type=entity_type,
                related_entity_id=entity_id,
            )
        )
        scores.append(score)

    if not drafts:
        return None

    drafts.sort(key=_sort_key)
    similar = drafts[: max(1, int(limit))]

    top_test_name: Optional[str] = None
    if test_recommendations and test_recommendations.recommendations:
        top_test_name = test_recommendations.recommendations[0].test_name

    return HistoricalLearningReport(
        similar_incidents=similar,
        pattern_summary=_pattern_summary(
            similar,
            current_signals=current,
            browser_hits=browser_hits,
            top_test_name=top_test_name,
        ),
        confidence=_report_confidence([s.similarity_score for s in similar]),
    )
