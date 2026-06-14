# services/executive_risk_brief_service.py
"""
Executive Risk Brief — actionable dashboard intelligence (read-only composition).

Selects the highest-priority failure cluster from existing FI / risk / knowledge
sources. No new scoring engines.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import List, Optional, Tuple

from models.dashboard_models import ExecutiveRiskBrief, RiskEvidenceItem
from models.failure_intelligence_models import BlastRadius, FailureCluster
from services.rca_service import recommendation_for_category

logger = logging.getLogger("vanya.executive_risk_brief")

_CONFIDENCE_RANK = {"high": 3, "medium": 2, "low": 1}
_SEVERITY_RANK = {"critical": 4, "high": 3, "medium": 2, "low": 1}

_IMPACT_BY_CATEGORY = {
    "auth_issue": "Users may be unable to sign in or maintain authenticated sessions.",
    "api_failure": "Backend services may be unavailable, affecting dependent user flows.",
    "selector_issue": "Users may see broken UI flows when interacting with affected screens.",
    "timeout_issue": "Users may experience slow or incomplete journeys on affected paths.",
    "assertion_issue": "Users may encounter incorrect content or broken page states.",
    "navigation_issue": "Users may be unable to reach critical pages in the application.",
    "data_issue": "Users may hit missing or invalid data when using affected features.",
    "environment_issue": "Users may be blocked by unstable or misconfigured environments.",
    "unknown": "User-facing quality may be degraded until the failure pattern is resolved.",
}

_ACTION_BY_CATEGORY = {
    "auth_issue": "Run authentication smoke tests for the affected module.",
    "api_failure": "Validate API health and rerun representative failing tests.",
    "selector_issue": "Inspect UI selectors and rerun smoke for the affected module.",
    "timeout_issue": "Review performance and rerun targeted smoke on slow paths.",
    "assertion_issue": "Verify expected page state and rerun module smoke tests.",
    "navigation_issue": "Validate navigation paths and rerun smoke for affected routes.",
    "data_issue": "Check test data fixtures and rerun data-dependent smoke tests.",
    "environment_issue": "Validate environment readiness before rerunning smoke tests.",
    "unknown": "Investigate the cluster and rerun representative smoke tests.",
}


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _cluster_rank(cluster: FailureCluster) -> Tuple[int, int, int]:
    conf = _CONFIDENCE_RANK.get((cluster.confidence or "low").lower(), 1)
    sev = 1
    if cluster.blast_radius:
        sev = _SEVERITY_RANK.get(
            (cluster.blast_radius.estimated_severity or "low").lower(),
            1,
        )
    return (int(cluster.total_failures or 0), conf, sev)


def select_top_cluster(clusters: List[FailureCluster]) -> Optional[FailureCluster]:
    if not clusters:
        return None
    return max(clusters, key=_cluster_rank)


def _impact_for_cluster(cluster: FailureCluster) -> str:
    cat = (cluster.root_cause_category or "unknown").lower()
    base = _IMPACT_BY_CATEGORY.get(cat, _IMPACT_BY_CATEGORY["unknown"])
    if cluster.probable_cause:
        return f"{base} {cluster.probable_cause[:160]}".strip()
    br: Optional[BlastRadius] = cluster.blast_radius
    if br and br.impact_scope == "widespread_or_systemic":
        return f"{base} Impact appears widespread across multiple tests."
    return base


def _recommendation_for_cluster(
    cluster: FailureCluster,
    *,
    module_recommended_tests: List[str],
) -> str:
    if (cluster.recommended_action or "").strip():
        return cluster.recommended_action.strip()
    cat = (cluster.root_cause_category or "unknown").lower()
    action = _ACTION_BY_CATEGORY.get(cat, _ACTION_BY_CATEGORY["unknown"])
    if module_recommended_tests:
        return f"{action} Priority test: {module_recommended_tests[0]}."
    return action


def _evidence_for_cluster(cluster: FailureCluster) -> List[RiskEvidenceItem]:
    items = [
        RiskEvidenceItem(
            kind="occurrences",
            count=int(cluster.total_failures or 0),
        ),
        RiskEvidenceItem(
            kind="module",
            module=cluster.module or "unknown",
        ),
    ]
    if cluster.representative_test_case_id:
        items.append(RiskEvidenceItem(
            kind="representative_test",
            test_case_id=cluster.representative_test_case_id,
        ))
    if cluster.summary:
        items.append(RiskEvidenceItem(
            kind="summary",
            text=cluster.summary[:240],
        ))
    return items


def _brief_from_module_risk(
    *,
    project_id: str,
    module: str,
    confidence: str,
    evidence: List[RiskEvidenceItem],
    impact: str,
    recommendation: str,
) -> ExecutiveRiskBrief:
    return ExecutiveRiskBrief(
        project_id=project_id,
        generated_at=_utc_now_iso(),
        has_risk=True,
        title="primary_risk_detected",
        module=module,
        confidence=confidence,
        evidence=evidence,
        impact=impact,
        recommendation=recommendation,
    )


def build_executive_risk_brief(project_id: str) -> ExecutiveRiskBrief:
    pid = (project_id or "").strip()
    if not pid:
        raise ValueError("project_id is required")

    from services.failure_intelligence_service import failure_intelligence_service
    from services.project_knowledge_service import get_project_knowledge
    from services.project_risk_service import assess_project_risk

    clusters = failure_intelligence_service.get_clusters(project_id=pid, limit=200)
    top = select_top_cluster(clusters)

    module_recommended: List[str] = []
    try:
        risk = assess_project_risk(pid)
        module_recommended = [
            rt.test_case_id for rt in (risk.recommended_tests or [])[:3]
            if rt.test_case_id
        ]
    except Exception:
        logger.debug("executive_risk_brief: risk assessment unavailable project_id=%s", pid)
        risk = None

    if top:
        mod = top.module or "unknown"
        rec_tests = [
            rt.test_case_id
            for rt in (risk.recommended_tests or [])
            if rt.module and rt.module.lower() == mod.lower() and rt.test_case_id
        ] if risk else []
        return ExecutiveRiskBrief(
            project_id=pid,
            generated_at=_utc_now_iso(),
            has_risk=True,
            title="primary_risk_detected",
            module=mod,
            confidence=(top.confidence or "low").lower(),
            evidence=_evidence_for_cluster(top),
            impact=_impact_for_cluster(top),
            recommendation=_recommendation_for_cluster(
                top,
                module_recommended_tests=rec_tests or module_recommended,
            ),
            cluster_id=top.cluster_id,
            representative_test_case_id=top.representative_test_case_id or "",
            root_cause_category=top.root_cause_category or "",
        )

    # Fallback: highest module risk from risk engine
    if risk and (risk.module_risks or []):
        top_mod = max(risk.module_risks, key=lambda m: m.module_risk_score or 0)
        if (top_mod.module_risk_score or 0) >= 25:
            conf = "high" if top_mod.module_risk_level in ("HIGH", "CRITICAL") else "medium"
            factor_detail = ""
            if top_mod.factors:
                factor_detail = top_mod.factors[0].detail or top_mod.factors[0].label
            evidence = [
                RiskEvidenceItem(kind="module", module=top_mod.module),
                RiskEvidenceItem(
                    kind="summary",
                    text=factor_detail or f"Module risk score {top_mod.module_risk_score:.0f}/100.",
                ),
            ]
            if top_mod.regression_count:
                evidence.insert(0, RiskEvidenceItem(
                    kind="occurrences",
                    count=int(top_mod.regression_count),
                ))
            rec = module_recommended[0] if module_recommended else (
                f"Review regressions and rerun smoke for module '{top_mod.module}'."
            )
            if isinstance(rec, str) and not rec.startswith("TC-"):
                recommendation = rec
            else:
                recommendation = f"Rerun priority test {rec} for module '{top_mod.module}'."
            return _brief_from_module_risk(
                project_id=pid,
                module=top_mod.module,
                confidence=conf,
                evidence=evidence,
                impact=(
                    f"Quality degradation in module '{top_mod.module}' may affect dependent user journeys."
                ),
                recommendation=recommendation,
            )

    # Fallback: project knowledge failure history
    try:
        knowledge = get_project_knowledge(pid)
        if knowledge and (knowledge.failure_history or []):
            fail = knowledge.failure_history[0]
            mod = fail.module or "unknown"
            return _brief_from_module_risk(
                project_id=pid,
                module=mod,
                confidence="medium",
                evidence=[
                    RiskEvidenceItem(kind="occurrences", count=int(fail.count or 1)),
                    RiskEvidenceItem(kind="module", module=mod),
                    RiskEvidenceItem(
                        kind="representative_test",
                        test_case_id=fail.test_case_id or fail.test_name or "",
                    ),
                ],
                impact=f"Recent failures in '{mod}' may indicate an active regression.",
                recommendation=(
                    recommendation_for_category("unknown")
                    if not module_recommended
                    else f"Rerun priority test {module_recommended[0]}."
                ),
            )
    except Exception:
        logger.debug("executive_risk_brief: knowledge lookup failed project_id=%s", pid)

    return ExecutiveRiskBrief(
        project_id=pid,
        generated_at=_utc_now_iso(),
        has_risk=False,
        title="primary_risk_detected",
        module="",
        confidence="low",
        evidence=[],
        impact="No dominant failure cluster or module risk was detected from recent signals.",
        recommendation="Continue monitoring and run smoke tests to build failure intelligence.",
    )
