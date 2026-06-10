# services/data_journey_validation_service.py
"""
Integration Intelligence INT-01B — Data Journey Validation (read-only).

Validates that business data traveled through expected workflow stages using
existing incident intelligence only. No writes, external API calls, or repairs.
"""
from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple

from models.incident_models import (
    ApiContractReport,
    DatabaseValidationCheck,
    DatabaseValidationReport,
    DatabaseValidationResult,
    DataJourney,
    DataJourneyReport,
    DataJourneyResult,
    DeploymentRiskAssessment,
    IncidentHypothesis,
    IncidentImpactNode,
    JourneyStage,
)

_STAGE_STATUS = ("PASS", "FAIL", "UNKNOWN")
_JOURNEY_STATUS = ("HEALTHY", "DEGRADED", "BROKEN")
_AREA_SUFFIXES = (" flow", " smoke", " suite", " regression", " ui", " module")


@dataclass
class _StageTemplate:
    slug: str
    name: str
    stage_type: str
    check_keywords: Tuple[str, ...] = ()
    contract_area: Optional[str] = None


@dataclass
class _JourneyTemplate:
    journey_id: str
    name: str
    description: str
    business_area: str
    stages: Tuple[_StageTemplate, ...]


@dataclass
class _EvaluatedStage:
    stage: JourneyStage
    status: str
    missing: bool = False
    inconsistent: bool = False


def build_journey_id(business_area: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "_", (business_area or "general").strip().lower()).strip("_")
    return f"journey:{slug}"


def build_stage_id(journey_id: str, slug: str) -> str:
    stage_slug = re.sub(r"[^a-z0-9]+", "_", (slug or "stage").strip().lower()).strip("_")
    return f"{journey_id}:stage:{stage_slug}"


def _normalize_area(name: str) -> str:
    s = (name or "").strip().lower()
    for suffix in _AREA_SUFFIXES:
        if s.endswith(suffix):
            s = s[: -len(suffix)].strip()
    return re.sub(r"\s+", " ", s) or ""


def _area_category(area: str) -> str:
    key = _normalize_area(area)
    if any(k in key for k in ("checkout", "cart", "order")):
        return "checkout"
    if any(k in key for k in ("payment", "payments", "billing")):
        return "payments"
    if any(k in key for k in ("auth", "authentication", "login", "session")):
        return "authentication"
    return ""


def _journey_templates() -> Dict[str, _JourneyTemplate]:
    checkout_id = build_journey_id("checkout")
    payments_id = build_journey_id("payments")
    auth_id = build_journey_id("authentication")
    return {
        "checkout": _JourneyTemplate(
            journey_id=checkout_id,
            name="Checkout Journey",
            description="Customer checkout propagation across UI, payments, orders, inventory, and ERP.",
            business_area="checkout",
            stages=(
                _StageTemplate("ui", "Web UI", "ui"),
                _StageTemplate("payments_api", "Payments API", "api", contract_area="payments"),
                _StageTemplate("orders_db", "Orders DB", "database", ("order",)),
                _StageTemplate("inventory_db", "Inventory DB", "database", ("inventory",)),
                _StageTemplate("erp_sync", "ERP Sync", "erp", ("erp",)),
            ),
        ),
        "payments": _JourneyTemplate(
            journey_id=payments_id,
            name="Payments Journey",
            description="Payment transaction propagation across API, store, reconciliation, and settlement.",
            business_area="payments",
            stages=(
                _StageTemplate("api", "Payments API", "api", contract_area="payments"),
                _StageTemplate("transaction_store", "Transaction Store", "database", ("transaction",)),
                _StageTemplate("reconciliation", "Reconciliation", "database", ("reconciliation",)),
                _StageTemplate("settlement", "Settlement", "erp", ("settlement", "erp")),
            ),
        ),
        "authentication": _JourneyTemplate(
            journey_id=auth_id,
            name="Authentication Journey",
            description="User authentication propagation across login, session store, and audit log.",
            business_area="authentication",
            stages=(
                _StageTemplate("login", "User Login", "ui", ("login", "auth", "session")),
                _StageTemplate("session_store", "Session Store", "database", ("session", "token")),
                _StageTemplate("audit_log", "Audit Log", "audit", ("audit",)),
            ),
        ),
    }


def _collect_categories(
    impact_map: List[IncidentImpactNode],
    hypotheses: List[IncidentHypothesis],
) -> Set[str]:
    categories: Set[str] = set()
    for node in impact_map:
        cat = _area_category(node.title)
        if cat:
            categories.add(cat)
    for hyp in hypotheses:
        blob = (hyp.statement or "").lower()
        for word in ("checkout", "payments", "payment", "authentication", "auth", "login"):
            if word in blob:
                categories.add(_area_category(word))
    return categories


def _checks_for_stage(
    stage: _StageTemplate,
    checks: List[DatabaseValidationCheck],
) -> List[DatabaseValidationCheck]:
    if not stage.check_keywords:
        return []
    matched: List[DatabaseValidationCheck] = []
    for check in checks:
        blob = f"{check.check_id} {check.name} {check.description}".lower()
        if any(k in blob for k in stage.check_keywords):
            matched.append(check)
    return matched


def _result_for_check(
    check_id: str,
    results: List[DatabaseValidationResult],
) -> Optional[DatabaseValidationResult]:
    for result in results:
        if result.check_id == check_id:
            return result
    return None


def _contract_for_area(api_contract_intelligence: Optional[ApiContractReport], area: str) -> bool:
    if not api_contract_intelligence:
        return False
    needle = area.lower()
    for contract in api_contract_intelligence.contracts:
        if needle in (contract.service_name or "").lower():
            return True
        if needle in (contract.endpoint or "").lower():
            return True
    return False


def _contract_risk_high(api_contract_intelligence: Optional[ApiContractReport], area: str) -> bool:
    if not api_contract_intelligence:
        return False
    for assessment in api_contract_intelligence.risk_assessments:
        contract = next(
            (c for c in api_contract_intelligence.contracts if c.contract_id in assessment.assessment_id),
            None,
        )
        if contract and area.lower() in (contract.service_name or "").lower():
            if assessment.risk_level in ("HIGH", "CRITICAL"):
                return True
    return False


def _evaluate_stage(
    template: _StageTemplate,
    journey_id: str,
    *,
    checks: List[DatabaseValidationCheck],
    results: List[DatabaseValidationResult],
    api_contract_intelligence: Optional[ApiContractReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    area_active: bool,
) -> _EvaluatedStage:
    check_ids = [c.check_id for c in _checks_for_stage(template, checks)]
    stage = JourneyStage(
        stage_id=build_stage_id(journey_id, template.slug),
        name=template.name,
        stage_type=template.stage_type,
        validation_check_ids=check_ids,
    )
    deploy_level = (deployment_risk_assessment.risk_level or "low").lower() if deployment_risk_assessment else "low"

    if template.stage_type == "ui":
        if area_active:
            return _EvaluatedStage(stage=stage, status="PASS")
        return _EvaluatedStage(stage=stage, status="UNKNOWN")

    if template.stage_type == "api":
        has_contract = _contract_for_area(api_contract_intelligence, template.contract_area or "")
        if not has_contract:
            return _EvaluatedStage(stage=stage, status="FAIL", missing=True)
        if _contract_risk_high(api_contract_intelligence, template.contract_area or ""):
            return _EvaluatedStage(stage=stage, status="FAIL", inconsistent=True)
        return _EvaluatedStage(stage=stage, status="PASS")

    stage_checks = _checks_for_stage(template, checks)
    if not stage_checks:
        return _EvaluatedStage(stage=stage, status="FAIL", missing=True)

    statuses: List[str] = []
    for check in stage_checks:
        result = _result_for_check(check.check_id, results)
        if result and result.status == "SUCCESS":
            statuses.append("PASS")
        elif result and result.status in ("BLOCKED", "FAILED", "REJECTED"):
            statuses.append("FAIL")
        elif deploy_level in ("high", "critical"):
            statuses.append("FAIL")
        else:
            statuses.append("UNKNOWN")

    if any(s == "FAIL" for s in statuses):
        return _EvaluatedStage(stage=stage, status="FAIL", inconsistent=True)
    if all(s == "PASS" for s in statuses):
        return _EvaluatedStage(stage=stage, status="PASS")
    if deploy_level in ("high", "critical") and "UNKNOWN" in statuses:
        return _EvaluatedStage(stage=stage, status="FAIL", missing=True)
    return _EvaluatedStage(stage=stage, status="UNKNOWN")


def _journey_status(evaluated: List[_EvaluatedStage]) -> str:
    missing_count = sum(1 for s in evaluated if s.missing or s.status == "FAIL")
    inconsistent_count = sum(1 for s in evaluated if s.inconsistent)
    fail_count = sum(1 for s in evaluated if s.status == "FAIL")
    if inconsistent_count >= 1 and fail_count >= 2:
        return "BROKEN"
    if missing_count >= 2 or fail_count >= 2:
        return "BROKEN"
    if missing_count == 1 or inconsistent_count == 1 or fail_count == 1:
        return "DEGRADED"
    if all(s.status == "PASS" for s in evaluated):
        return "HEALTHY"
    if any(s.status == "UNKNOWN" for s in evaluated):
        return "DEGRADED"
    return "HEALTHY"


def _apply_consistency_rules(
    evaluated: List[_EvaluatedStage],
    *,
    business_area: str,
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
) -> List[_EvaluatedStage]:
    """Detect cross-stage consistency failures without mutating external systems."""
    if business_area != "checkout":
        return evaluated

    by_slug = {e.stage.stage_id.split(":stage:")[-1]: e for e in evaluated}
    orders = by_slug.get("orders_db")
    inventory = by_slug.get("inventory_db")
    if not orders or not inventory:
        return evaluated

    orders_ok = orders.status == "PASS"
    inventory_ok = inventory.status == "PASS"
    deploy_level = (deployment_risk_assessment.risk_level or "low").lower() if deployment_risk_assessment else "low"

    if orders_ok and not inventory_ok:
        inventory.status = "FAIL"
        inventory.inconsistent = True
    elif inventory_ok and not orders_ok and deploy_level in ("high", "critical"):
        orders.status = "FAIL"
        orders.inconsistent = True
    elif orders.status == "PASS" and inventory.status == "UNKNOWN" and deploy_level == "critical":
        inventory.status = "FAIL"
        inventory.inconsistent = True
        inventory.missing = True

    return evaluated


def _build_journey_result(
    template: _JourneyTemplate,
    evaluated: List[_EvaluatedStage],
) -> DataJourneyResult:
    status = _journey_status(evaluated)
    completed = sum(1 for s in evaluated if s.status == "PASS")
    missing = [s.stage.name for s in evaluated if s.missing or (s.status == "FAIL" and not s.inconsistent)]
    inconsistent = [s.stage.name for s in evaluated if s.inconsistent]
    missing = sorted(set(missing))
    inconsistent = sorted(set(inconsistent))

    if status == "HEALTHY":
        summary = f"{template.name} completed all {len(evaluated)} expected stage(s)."
    elif status == "DEGRADED":
        summary = f"{template.name} degraded with {len(missing) + len(inconsistent)} stage issue(s) detected."
    else:
        summary = f"{template.name} broken — data propagation stopped or became inconsistent."

    confidence = round(min(0.95, 0.55 + (completed / max(len(evaluated), 1)) * 0.35), 2)
    if inconsistent:
        confidence = round(max(0.45, confidence - 0.1 * len(inconsistent)), 2)

    return DataJourneyResult(
        journey_id=template.journey_id,
        status=status,
        completed_stages=completed,
        total_stages=len(evaluated),
        confidence=confidence,
        summary=summary,
        missing_stages=missing,
        inconsistent_stages=inconsistent,
    )


def _build_journey_from_template(
    template: _JourneyTemplate,
    *,
    checks: List[DatabaseValidationCheck],
    results: List[DatabaseValidationResult],
    api_contract_intelligence: Optional[ApiContractReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
) -> Tuple[DataJourney, DataJourneyResult]:
    evaluated = [
        _evaluate_stage(
            stage,
            template.journey_id,
            checks=checks,
            results=results,
            api_contract_intelligence=api_contract_intelligence,
            deployment_risk_assessment=deployment_risk_assessment,
            area_active=True,
        )
        for stage in template.stages
    ]
    evaluated = _apply_consistency_rules(
        evaluated,
        business_area=template.business_area,
        deployment_risk_assessment=deployment_risk_assessment,
    )
    journey = DataJourney(
        journey_id=template.journey_id,
        name=template.name,
        description=template.description,
        business_area=template.business_area,
        stages=[e.stage for e in evaluated],
    )
    result = _build_journey_result(template, evaluated)
    return journey, result


def build_data_journey_validation(
    *,
    impact_map: List[IncidentImpactNode],
    database_validation: Optional[DatabaseValidationReport],
    api_contract_intelligence: Optional[ApiContractReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    hypotheses: List[IncidentHypothesis],
) -> Optional[DataJourneyReport]:
    categories = _collect_categories(impact_map, hypotheses)
    if not categories:
        return None

    templates = _journey_templates()
    checks = list(database_validation.checks) if database_validation else []
    results = list(database_validation.results) if database_validation else []

    journeys: List[DataJourney] = []
    journey_results: List[DataJourneyResult] = []

    for category in sorted(categories):
        template = templates.get(category)
        if not template:
            continue
        journey, result = _build_journey_from_template(
            template,
            checks=checks,
            results=results,
            api_contract_intelligence=api_contract_intelligence,
            deployment_risk_assessment=deployment_risk_assessment,
        )
        journeys.append(journey)
        journey_results.append(result)

    if not journeys:
        return None

    broken = sum(1 for r in journey_results if r.status == "BROKEN")
    degraded = sum(1 for r in journey_results if r.status == "DEGRADED")
    healthy = sum(1 for r in journey_results if r.status == "HEALTHY")

    if broken:
        summary = f"{len(journeys)} data journey(s) modeled; {broken} broken journey(s) detected."
    elif degraded:
        summary = f"{len(journeys)} data journey(s) modeled; {degraded} degraded journey(s) detected."
    else:
        summary = f"{len(journeys)} data journey(s) modeled; {healthy} healthy journey(s)."

    digest = hashlib.sha256("|".join(j.journey_id for j in journeys).encode("utf-8")).hexdigest()[:8]
    confidence = round(
        min(0.95, sum(r.confidence for r in journey_results) / max(len(journey_results), 1)),
        2,
    )

    return DataJourneyReport(
        journeys=journeys,
        results=journey_results,
        summary=f"{summary} Ref: {digest}.",
        confidence=confidence,
    )
