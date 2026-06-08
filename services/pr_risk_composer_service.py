# services/pr_risk_composer_service.py
"""
PR Risk Composer — deterministic PR-scoped risk from CCE + module context.

Separates:
  project_risk_score  → historical project health (System Memory)
  pr_risk_score       → risk of approving this specific change
"""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import List, Optional

from models.pr_analysis_models import (
    FileChangeClassification,
    ImpactedModuleReport,
    PRRecommendedTest,
    PRRiskSignal,
)
from models.risk_engine_models import ModuleRisk
from services.risk_engine_service import risk_level_from_score

# Severity ordering when multiple files are changed
_CLASS_SEVERITY = {
    "schema": 70,
    "config": 60,
    "functional": 50,
    "imports": 30,
    "test_only": 25,
    "formatting": 10,
    "docs": 5,
    "comments": 5,
}

# Midpoint of product base ranges
_BASE_MID = {
    "comments": 2.5,
    "docs": 2.5,
    "formatting": 4.0,
    "imports": 10.0,
    "test_only": 10.0,
    "config": 32.0,
    "schema": 45.0,
    "functional": 35.0,
}

_CRITICAL_MODULE_TOKENS = frozenset({
    "auth", "authentication", "autenticacion", "autenticación",
    "security", "permission", "permissions", "login", "session",
    "access", "token", "credential", "oauth", "sso",
})

_NORMAL_MODULE_TOKENS = frozenset({
    "candidate", "candidates", "candidato", "candidatos",
    "vacancy", "vacancies", "vacante", "vacantes",
    "proposal", "proposals", "propuesta", "propuestas",
    "dashboard", "tablero",
})

_CRITICAL_PATH_RE = re.compile(
    r"(?:^|/)(?:auth|login|session|permission|permissions|security|access)(?:/|\.|$)",
    re.IGNORECASE,
)

_SCHEMA_PATH_RE = re.compile(r"migration|schema\.(?:prisma|sql)|\.sql$|alembic", re.IGNORECASE)


@dataclass
class PRRiskComposeResult:
    pr_risk_score: float
    pr_risk_level: str
    reasoning: List[str] = field(default_factory=list)
    signals: List[str] = field(default_factory=list)
    risk_signals: List[PRRiskSignal] = field(default_factory=list)
    dominant_change_class: str = "functional"


_CHANGE_TYPE_SIGNAL: dict[str, tuple[str, str]] = {
    "comments": (
        "Comment-only change",
        "All modified lines are comments; functional behavior is unchanged.",
    ),
    "docs": (
        "Documentation change",
        "Documentation-only paths or comment content.",
    ),
    "formatting": (
        "Formatting-only change",
        "Whitespace or formatting adjustments without logic changes.",
    ),
    "imports": (
        "Import-only change",
        "Only import or export statements were modified.",
    ),
    "test_only": (
        "Test-only change",
        "Changes are confined to test files.",
    ),
    "config": (
        "Configuration change",
        "Configuration files were modified.",
    ),
    "schema": (
        "Schema-related change",
        "Schema or migration paths were modified.",
    ),
    "functional": (
        "Functional code change",
        "Non-cosmetic logic changes detected or assumed from the diff.",
    ),
}


def _change_type_signal(dominant: str, impact: float) -> PRRiskSignal:
    title, explanation = _CHANGE_TYPE_SIGNAL.get(
        dominant,
        ("Change type", "Change classification from the Change Classification Engine."),
    )
    return PRRiskSignal(
        category="change_type",
        title=title,
        impact=round(impact, 1),
        explanation=explanation,
    )


def _primary_critical_module(impacted_modules: List[ImpactedModuleReport]) -> Optional[str]:
    for im in impacted_modules:
        if module_is_critical(im.module, im.matched_files):
            return im.module
    return None


def _primary_normal_module(impacted_modules: List[ImpactedModuleReport]) -> Optional[str]:
    for im in impacted_modules:
        if module_is_normal(im.module):
            return im.module
    return None


def risk_signals_total(signals: List[PRRiskSignal]) -> float:
    """Sum of signal impacts — should match pr_risk_score after caps."""
    return round(sum(float(s.impact) for s in signals), 1)


def effective_change_class(entry: FileChangeClassification) -> str:
    """Map CCE output to composer change bucket (includes functional fallback)."""
    if entry.primary_class == "formatting":
        blob = " ".join(entry.signals or []).lower()
        if "functional change assumed" in blob or "no deterministic class matched" in blob:
            return "functional"
    return str(entry.primary_class)


def dominant_change_class(classifications: List[FileChangeClassification]) -> str:
    if not classifications:
        return "functional"
    best = "comments"
    best_sev = -1
    for entry in classifications:
        eff = effective_change_class(entry)
        sev = _CLASS_SEVERITY.get(eff, 40)
        if sev > best_sev:
            best_sev = sev
            best = eff
    return best


def _module_blob(module: str, files: List[str]) -> str:
    parts = [module or ""] + list(files or [])
    return " ".join(parts).lower().replace("\\", "/")


def module_is_critical(module: str, matched_files: Optional[List[str]] = None) -> bool:
    blob = _module_blob(module, matched_files or [])
    if any(tok in blob for tok in _CRITICAL_MODULE_TOKENS):
        return True
    return bool(_CRITICAL_PATH_RE.search(blob))


def module_is_normal(module: str) -> bool:
    blob = (module or "").lower()
    return any(tok in blob for tok in _NORMAL_MODULE_TOKENS)


def any_critical_module(impacted_modules: List[ImpactedModuleReport]) -> bool:
    return any(module_is_critical(im.module, im.matched_files) for im in impacted_modules)


def any_normal_module(impacted_modules: List[ImpactedModuleReport]) -> bool:
    return any(module_is_normal(im.module) for im in impacted_modules)


def _critical_module_boost(dominant: str) -> float:
    if dominant in ("comments", "docs"):
        return 12.0
    if dominant in ("imports", "formatting", "test_only"):
        return 15.0
    if dominant in ("schema", "config"):
        return 10.0
    return 22.0


def _normal_module_boost(dominant: str) -> float:
    if dominant in ("comments", "docs", "formatting"):
        return 2.0
    if dominant in ("imports", "test_only"):
        return 5.0
    return 8.0


def _project_context_boost(project_risk_score: float) -> float:
    """Light historical context — max +15, never dominant."""
    score = float(project_risk_score or 0.0)
    if score <= 25.0:
        return 0.0
    return min(15.0, (score - 25.0) * 15.0 / 75.0)


def _critical_test_boost(
    recommended_tests: List[PRRecommendedTest],
    dominant: str,
) -> float:
    if dominant in ("comments", "docs", "formatting"):
        return 0.0
    boost = 0.0
    for rec in recommended_tests[:8]:
        blob = f"{rec.name} {rec.reason}".lower()
        if any(k in blob for k in ("critical", "regression", "recurrent", "fail", "flaky")):
            boost += 3.0
    return min(boost, 12.0)


def _high_risk_module_boost(
    impacted_modules: List[ImpactedModuleReport],
    dominant: str,
) -> float:
    if dominant in ("comments", "docs"):
        return 0.0
    boost = 0.0
    for im in impacted_modules:
        if im.module_risk_level in ("HIGH", "CRITICAL") and im.module_risk_score >= 50:
            boost += 4.0
    return min(boost, 10.0)


def _unmatched_schema_boost(unmatched_files: List[str], dominant: str) -> float:
    if dominant not in ("schema", "config"):
        return 0.0
    hits = [f for f in unmatched_files if _SCHEMA_PATH_RE.search(f.replace("\\", "/"))]
    if hits:
        return 5.0
    return 0.0


def _apply_caps(score: float, dominant: str, critical_module: bool) -> float:
    capped = score
    if dominant in ("comments", "docs"):
        capped = min(capped, 30.0 if critical_module else 8.0)
    elif dominant == "imports":
        capped = min(capped, 35.0 if critical_module else 20.0)
    elif dominant == "formatting":
        capped = min(capped, 15.0 if critical_module else 8.0)
    elif dominant == "test_only":
        capped = min(capped, 25.0 if critical_module else 18.0)
    elif dominant == "schema":
        capped = max(min(capped, 85.0), 30.0)
    elif dominant == "config":
        capped = max(min(capped, 75.0), 20.0)
    else:
        capped = min(max(capped, 20.0), 75.0)
    return round(min(max(capped, 0.0), 100.0), 1)


def compose_pr_risk(
    file_classifications: List[FileChangeClassification],
    impacted_modules: List[ImpactedModuleReport],
    project_risk_score: float,
    module_risks: List[ModuleRisk],
    recommended_tests: List[PRRecommendedTest],
    unmatched_files: List[str],
) -> PRRiskComposeResult:
    """
    Compose PR-specific risk from CCE classifications, impacted modules, and
    Risk Engine recommendations. Does not mutate project baseline risk.
    """
    _ = module_risks  # reserved for future module-risk weighting; kept for API stability
    dominant = dominant_change_class(file_classifications)
    critical = any_critical_module(impacted_modules)
    normal = any_normal_module(impacted_modules)
    reasoning: List[str] = []
    signals: List[str] = []
    risk_signals: List[PRRiskSignal] = []

    score = _BASE_MID.get(dominant, 35.0)
    risk_signals.append(_change_type_signal(dominant, score))
    reasoning.append(f"PR Risk Composer: dominant change class '{dominant}' (base {score:.0f}/100).")
    signals.append(f"dominant_class={dominant}")

    if critical:
        boost = _critical_module_boost(dominant)
        score += boost
        mod_label = (_primary_critical_module(impacted_modules) or "Critical").upper()
        risk_signals.append(PRRiskSignal(
            category="module",
            title=f"Critical module: {mod_label}",
            impact=round(boost, 1),
            explanation="Authentication or security-related modules require additional scrutiny.",
        ))
        reasoning.append(f"Critical module in scope (+{boost:.0f}).")
        signals.append("critical_module=true")
    elif normal:
        boost = _normal_module_boost(dominant)
        score += boost
        mod_label = _primary_normal_module(impacted_modules) or "Business module"
        risk_signals.append(PRRiskSignal(
            category="module",
            title=f"{mod_label} module",
            impact=round(boost, 1),
            explanation="Standard business module impacted by this change.",
        ))
        reasoning.append(f"Normal business module in scope (+{boost:.0f}).")
        signals.append("normal_module=true")
    elif not impacted_modules:
        if dominant in ("comments", "docs", "imports", "formatting", "test_only"):
            before = score
            score = min(score, _BASE_MID[dominant])
            if score < before:
                risk_signals.append(PRRiskSignal(
                    category="module",
                    title="No catalog module mapped",
                    impact=round(score - before, 1),
                    explanation="Files could not be mapped to a catalog module; cosmetic change stays low.",
                ))
            reasoning.append("No catalog module mapped — low PR risk for cosmetic/import-only change.")
        elif dominant in ("schema", "config"):
            risk_signals.append(PRRiskSignal(
                category="module",
                title="No catalog module mapped",
                impact=0.0,
                explanation="Risk is driven by schema or configuration change type rather than module mapping.",
            ))
            reasoning.append("No catalog module mapped — risk driven by change type (schema/config).")
        signals.append("unmapped_files=true")

    ctx = _project_context_boost(project_risk_score)
    if ctx > 0:
        score += ctx
        risk_signals.append(PRRiskSignal(
            category="history",
            title="Project baseline risk",
            impact=round(ctx, 1),
            explanation="Historical project risk contributes lightly to PR risk (capped contribution).",
        ))
        reasoning.append(
            f"Project historical risk context (+{ctx:.0f}, capped contribution from "
            f"project score {project_risk_score:.0f}/100)."
        )
        signals.append(f"project_context=+{ctx:.0f}")

    test_boost = _critical_test_boost(recommended_tests, dominant)
    if test_boost > 0:
        score += test_boost
        risk_signals.append(PRRiskSignal(
            category="tests",
            title="Critical tests in scope",
            impact=round(test_boost, 1),
            explanation="High-priority or regression-related tests are associated with impacted modules.",
        ))
        reasoning.append(f"Critical/high-priority tests in scope (+{test_boost:.0f}).")
        signals.append(f"critical_tests=+{test_boost:.0f}")

    mod_boost = _high_risk_module_boost(impacted_modules, dominant)
    if mod_boost > 0:
        score += mod_boost
        risk_signals.append(PRRiskSignal(
            category="tests",
            title="High-risk module history",
            impact=round(mod_boost, 1),
            explanation="Impacted modules have elevated historical failure or incident rates.",
        ))
        reasoning.append(f"High-risk impacted module history (+{mod_boost:.0f}).")

    schema_boost = _unmatched_schema_boost(unmatched_files, dominant)
    if schema_boost > 0:
        score += schema_boost
        risk_signals.append(PRRiskSignal(
            category="module",
            title="Unmapped schema/config paths",
            impact=round(schema_boost, 1),
            explanation="Schema or migration files could not be mapped to a catalog module.",
        ))
        reasoning.append(f"Unmapped schema/config paths (+{schema_boost:.0f}).")

    pre_cap = round(score, 1)
    final = _apply_caps(score, dominant, critical)
    if final != pre_cap:
        cap_delta = round(final - pre_cap, 1)
        cap_title = "Policy floor applied" if cap_delta > 0 else "Policy cap applied"
        cap_expl = (
            f"PR risk policy enforces a minimum score for '{dominant}' changes."
            if cap_delta > 0
            else f"PR risk policy caps the score for '{dominant}' changes with this module context."
        )
        risk_signals.append(PRRiskSignal(
            category="cap",
            title=cap_title,
            impact=cap_delta,
            explanation=cap_expl,
        ))
        reasoning.append(f"PR risk capped/adjusted to {final:.0f}/100 for '{dominant}' policy.")

    level = risk_level_from_score(final)
    reasoning.append(
        f"PR risk {final:.0f}/100 ({level}) — change-specific; "
        f"project baseline remains separate."
    )

    return PRRiskComposeResult(
        pr_risk_score=final,
        pr_risk_level=level,
        reasoning=reasoning,
        signals=signals,
        risk_signals=risk_signals,
        dominant_change_class=dominant,
    )
