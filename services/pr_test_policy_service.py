# services/pr_test_policy_service.py
"""
Test Recommendation Policy — filter Risk Engine tests by CCE change class.

Preserves the original recommendation list separately; filtered list drives UI/enqueue.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import List

from models.pr_analysis_models import FileChangeClassification, ImpactedModuleReport, PRRecommendedTest
from services.pr_risk_composer_service import (
    any_critical_module,
    dominant_change_class,
)


@dataclass
class PRTestPolicyResult:
    recommended_tests: List[PRRecommendedTest] = field(default_factory=list)
    recommended_tests_raw: List[PRRecommendedTest] = field(default_factory=list)
    policy_reasons: List[str] = field(default_factory=list)


def _blob(rec: PRRecommendedTest) -> str:
    return f"{rec.name} {rec.reason} {rec.module}".lower()


def _is_smoke(rec: PRRecommendedTest) -> bool:
    return "smoke" in _blob(rec)


def _is_security(rec: PRRecommendedTest) -> bool:
    b = _blob(rec)
    return any(k in b for k in ("auth", "security", "login", "permission", "session", "access"))


def _is_api_or_regression(rec: PRRecommendedTest) -> bool:
    b = _blob(rec)
    return any(k in b for k in ("api", "regression", "graphql", "integration"))


def _is_integration(rec: PRRecommendedTest) -> bool:
    return "integration" in _blob(rec)


def _dedupe(tests: List[PRRecommendedTest]) -> List[PRRecommendedTest]:
    seen = set()
    out: List[PRRecommendedTest] = []
    for t in tests:
        if t.test_case_id in seen:
            continue
        seen.add(t.test_case_id)
        out.append(t)
    return out


def filter_recommended_tests_for_pr(
    recommended_tests: List[PRRecommendedTest],
    file_classifications: List[FileChangeClassification],
    impacted_modules: List[ImpactedModuleReport],
    pr_risk_score: float,
) -> PRTestPolicyResult:
    """
    Apply deterministic test recommendation policy based on change classification.

    ``pr_risk_score`` is accepted for future tiering; current rules are class-driven.
    """
    _ = pr_risk_score
    raw = list(recommended_tests or [])
    if not raw:
        return PRTestPolicyResult(
            recommended_tests=[],
            recommended_tests_raw=[],
            policy_reasons=["Test policy: no Risk Engine recommendations to filter."],
        )

    dominant = dominant_change_class(file_classifications)
    critical = any_critical_module(impacted_modules)
    filtered: List[PRRecommendedTest] = []
    reason = ""

    if dominant in ("comments", "docs"):
        if critical:
            pool = [t for t in raw if _is_smoke(t) or _is_security(t)] or raw[:1]
            filtered = _dedupe(pool)[:2]
            reason = (
                f"Test policy ({dominant} + critical module): max 2 smoke/security tests "
                f"(reduced from {len(raw)})."
            )
        else:
            smoke = [t for t in raw if _is_smoke(t)]
            filtered = smoke[:1]
            reason = (
                f"Test policy ({dominant} + normal module): "
                f"{'1 smoke test' if filtered else '0 tests'} (reduced from {len(raw)})."
            )

    elif dominant == "imports":
        smoke = [t for t in raw if _is_smoke(t)] or raw
        filtered = _dedupe(smoke)[:2]
        reason = f"Test policy (imports): max 2 smoke/module tests (reduced from {len(raw)})."

    elif dominant == "formatting":
        if critical:
            smoke = [t for t in raw if _is_smoke(t)]
            filtered = smoke[:1]
            reason = (
                f"Test policy (formatting + critical module): max 1 smoke "
                f"(reduced from {len(raw)})."
            )
        else:
            filtered = []
            reason = f"Test policy (formatting): no catalog tests (reduced from {len(raw)})."

    elif dominant == "test_only":
        filtered = []
        reason = "Test policy (test_only): no additional catalog tests beyond changed tests."

    elif dominant == "schema":
        api_reg = [t for t in raw if _is_api_or_regression(t)]
        filtered = _dedupe(api_reg)[:6] if api_reg else _dedupe(raw)[:4]
        reason = (
            f"Test policy (schema): prefer API/regression ({len(filtered)} of {len(raw)} retained)."
        )

    elif dominant == "config":
        smoke = [t for t in raw if _is_smoke(t)]
        integration = [t for t in raw if _is_integration(t)]
        filtered = _dedupe(smoke[:1] + integration[:1])
        if not filtered:
            filtered = _dedupe(raw)[:2]
        reason = f"Test policy (config): smoke + integration ({len(filtered)} of {len(raw)})."

    else:
        filtered = raw[:12]
        reason = (
            f"Test policy (functional/unknown): retain module-filtered tests "
            f"({len(filtered)} of {len(raw)})."
        )

    return PRTestPolicyResult(
        recommended_tests=filtered,
        recommended_tests_raw=raw,
        policy_reasons=[reason],
    )
