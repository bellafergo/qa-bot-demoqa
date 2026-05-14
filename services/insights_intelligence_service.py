# services/insights_intelligence_service.py
"""
Bundles deterministic QA intelligence: correlated root causes, timeline, recommendations.

Optional LLM hooks are intentionally absent — callers get rule-based output only.
"""
from __future__ import annotations

import logging
from typing import List, Optional

from models.insights_models import (
    ActionableRecommendation,
    DeepInsightsResponse,
    RootCauseCorrelation,
)
from services.failure_intelligence_service import failure_intelligence_service
from services.insights_timeline_service import get_failure_timeline

logger = logging.getLogger("vanya.insights_intelligence")

_SCOPE_COPY = {
    "isolated_failure": "Single-test or very low blast radius — likely an isolated defect.",
    "localized_regression": "Multiple tests in the same module suggest a localized regression.",
    "widespread_or_systemic": "Many tests or failures — treat as systemic; verify shared dependencies.",
    "unknown": "Impact scope could not be classified confidently from available signals.",
}


def _correlation_title(category: str, module: str) -> str:
    cat = (category or "unknown").replace("_", " ").title()
    mod = module or "unknown"
    return f"{cat} — {mod}"


def clusters_to_root_causes(clusters) -> List[RootCauseCorrelation]:
    out: List[RootCauseCorrelation] = []
    for cl in clusters or []:
        out.append(
            RootCauseCorrelation(
                root_cause_id=cl.cluster_id,
                title=_correlation_title(cl.root_cause_category, cl.module),
                suspected_cause=cl.probable_cause or cl.summary or "insufficient evidence",
                confidence=str(cl.confidence or "low"),
                affected_tests=list(cl.affected_test_case_ids or []),
                affected_modules=(
                    list(cl.blast_radius.affected_modules)
                    if cl.blast_radius and cl.blast_radius.affected_modules
                    else ([cl.module] if cl.module else [])
                ),
                signals_used=list(cl.signals_used or [])[:20],
                recommended_action=cl.recommended_action
                or "Review run logs and evidence; no specific recommendation was derived.",
            )
        )
    return out


def _rec_type_from_category(category: str) -> str:
    return {
        "selector_issue": "selector_change",
        "timeout_issue": "timeout",
        "assertion_issue": "fragile_assertion",
        "api_failure": "api_health",
        "auth_issue": "auth_session",
        "data_issue": "test_data",
        "navigation_issue": "navigation",
        "environment_issue": "environment",
        "unknown": "manual_review",
    }.get(str(category or "unknown"), "manual_review")


def _confidence_rank(conf: str) -> str:
    c = str(conf or "").lower()
    return c if c in ("low", "medium", "high") else "low"


def build_actionable_recommendations(
    *,
    clusters,
    flaky,
    regressions,
    limit: int = 40,
) -> List[ActionableRecommendation]:
    recs: List[ActionableRecommendation] = []

    for cl in clusters or []:
        if not cl.total_failures:
            continue
        rtype = _rec_type_from_category(cl.root_cause_category)
        scope_note = ""
        if cl.blast_radius:
            scope_note = _SCOPE_COPY.get(cl.blast_radius.impact_scope, "")
        recs.append(
            ActionableRecommendation(
                type=rtype,
                title=_correlation_title(cl.root_cause_category, cl.module),
                description=(
                    f"{cl.summary or cl.probable_cause} "
                    f"{(' ' + scope_note) if scope_note else ''}"
                ).strip(),
                confidence=_confidence_rank(cl.confidence),
                safe_to_apply_auto_fix=False,
                suggested_next_step=cl.recommended_action
                or "Open the linked runs, confirm signals in evidence, then apply a targeted fix.",
                related_test_case_ids=list(cl.affected_test_case_ids or [])[:25],
                source_cluster_id=cl.cluster_id,
            )
        )

    for f in flaky or []:
        if not getattr(f, "suspected_flaky", False):
            continue
        recs.append(
            ActionableRecommendation(
                type="flaky_quarantine",
                title=f"Flaky signal: {f.test_case_id}",
                description=f.notes or "Pass/fail alternation detected in recent history.",
                confidence="medium",
                safe_to_apply_auto_fix=False,
                suggested_next_step=(
                    "Stabilize waits/selectors, review environment noise, "
                    "and consider quarantine with explicit retry policy."
                ),
                related_test_case_ids=[f.test_case_id],
                source_cluster_id=None,
            )
        )

    for reg in regressions or []:
        if reg.repeated_failures < 2:
            continue
        recs.append(
            ActionableRecommendation(
                type="recurrent_failure",
                title=f"Repeated failures: {reg.test_case_id}",
                description=reg.summary or "Multiple failures in the recent window.",
                confidence="medium",
                safe_to_apply_auto_fix=False,
                suggested_next_step=(
                    "Inspect the latest failing run, compare with the last passing run, "
                    "and validate assertions against current product behaviour."
                ),
                related_test_case_ids=[reg.test_case_id],
                source_cluster_id=None,
            )
        )

    return recs[:limit]


def build_deep_insights(
    *,
    project_id: Optional[str] = None,
    cluster_limit: int = 200,
) -> DeepInsightsResponse:
    pid = (project_id or "").strip() or None
    try:
        clusters = failure_intelligence_service.get_clusters(
            limit=cluster_limit, project_id=pid
        )
    except Exception:
        logger.exception("deep_insights: get_clusters failed")
        clusters = []

    try:
        flaky = failure_intelligence_service.get_flaky_tests(project_id=pid)
    except Exception:
        logger.exception("deep_insights: get_flaky_tests failed")
        flaky = []

    try:
        regressions = failure_intelligence_service.get_regressions(project_id=pid)
    except Exception:
        logger.exception("deep_insights: get_regressions failed")
        regressions = []

    timeline = get_failure_timeline(project_id=pid)

    return DeepInsightsResponse(
        root_causes=clusters_to_root_causes(clusters),
        timeline=timeline,
        recommendations=build_actionable_recommendations(
            clusters=clusters,
            flaky=flaky,
            regressions=regressions,
        ),
    )
