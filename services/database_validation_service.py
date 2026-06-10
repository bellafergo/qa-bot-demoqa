# services/database_validation_service.py
"""
Integration Intelligence INT-01A — Database Validation Checks (read-only).

Suggests safe SELECT-style validation checks from existing incident intelligence.
No database execution, mutation, connectors, or background jobs in this sprint.
"""
from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple

from models.incident_models import (
    DatabaseValidationCheck,
    DatabaseValidationReport,
    DatabaseValidationResult,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    HistoricalLearningReport,
    IncidentHypothesis,
    IncidentImpactNode,
    RecommendedAction,
    TestRecommendationReport,
)

_BLOCKED_KEYWORDS = (
    "INSERT",
    "UPDATE",
    "DELETE",
    "DROP",
    "ALTER",
    "CREATE",
    "TRUNCATE",
    "MERGE",
    "GRANT",
    "REVOKE",
    "CALL",
    "EXEC",
    "EXECUTE",
)

_AREA_SUFFIXES = (" flow", " smoke", " suite", " regression", " ui", " module")


@dataclass
class _CheckDraft:
    check_id: str
    name: str
    description: str
    query: str
    database_type: str
    expected_result_type: str
    expected_value: Optional[str]
    priority: int


def validate_query_safety(query: str) -> Tuple[bool, str]:
    """Return whether a query is safe for read-only validation."""
    q = (query or "").strip()
    if not q:
        return False, "Empty query"

    stripped = q.rstrip().rstrip(";").strip()
    if ";" in stripped:
        return False, "Multiple statements are not allowed"

    upper = re.sub(r"\s+", " ", q).upper()
    for keyword in _BLOCKED_KEYWORDS:
        if re.search(rf"\b{keyword}\b", upper):
            return False, f"Blocked keyword: {keyword}"

    compact = upper.lstrip()
    if compact.startswith("WITH"):
        if "SELECT" not in compact:
            return False, "WITH queries must contain SELECT"
    elif compact.startswith("EXPLAIN"):
        if "SELECT" not in compact:
            return False, "EXPLAIN is only allowed for SELECT queries"
    elif not compact.startswith("SELECT"):
        return False, "Only SELECT-style queries are allowed"

    return True, "Read-only SELECT query approved"


def build_check_id(area: str, name: str) -> str:
    area_key = re.sub(r"\s+", "_", (area or "general").strip().lower())
    name_key = re.sub(r"\s+", "_", (name or "check").strip().lower())
    if area_key and name_key:
        return f"dbcheck:{area_key}:{name_key}"
    digest = hashlib.sha256(f"{area}:{name}".encode("utf-8")).hexdigest()[:12]
    return f"dbcheck:title:{digest}"


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
    return key or "general"


def _checkout_checks(area: str) -> List[_CheckDraft]:
    label = area.title() if area else "Checkout"
    return [
        _CheckDraft(
            check_id=build_check_id(area, "validate_order_record_exists"),
            name=f"Validate {label} order record exists",
            description=f"Confirm an order row exists after the {label.lower()} flow reports success.",
            query=(
                "SELECT order_id, status FROM orders "
                "WHERE order_id = :order_id AND created_at >= :flow_started_at"
            ),
            database_type="postgresql",
            expected_result_type="row_exists",
            expected_value=None,
            priority=1,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_payment_status"),
            name=f"Validate {label} payment status",
            description=f"Confirm payment status is captured for the {label.lower()} order.",
            query=(
                "SELECT payment_status FROM payments "
                "WHERE order_id = :order_id"
            ),
            database_type="postgresql",
            expected_result_type="status_equals",
            expected_value="captured",
            priority=2,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_inventory_reservation"),
            name=f"Validate {label} inventory reservation",
            description=f"Confirm inventory reservation exists for the {label.lower()} order.",
            query=(
                "SELECT reservation_id, sku, quantity FROM inventory_reservations "
                "WHERE order_id = :order_id"
            ),
            database_type="postgresql",
            expected_result_type="row_exists",
            expected_value=None,
            priority=3,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_erp_sync_status"),
            name=f"Validate {label} ERP sync status",
            description=f"Confirm fulfillment or ERP sync status is present after {label.lower()}.",
            query=(
                "SELECT erp_sync_status FROM order_fulfillment "
                "WHERE order_id = :order_id"
            ),
            database_type="postgresql",
            expected_result_type="status_equals",
            expected_value="synced",
            priority=4,
        ),
    ]


def _payments_checks(area: str) -> List[_CheckDraft]:
    label = area.title() if area else "Payments"
    return [
        _CheckDraft(
            check_id=build_check_id(area, "validate_payment_transaction_exists"),
            name=f"Validate {label} transaction exists",
            description=f"Confirm a payment transaction row exists for the {label.lower()} flow.",
            query=(
                "SELECT transaction_id, status FROM payment_transactions "
                "WHERE transaction_id = :transaction_id"
            ),
            database_type="postgresql",
            expected_result_type="row_exists",
            expected_value=None,
            priority=1,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_amount_consistency"),
            name=f"Validate {label} amount consistency",
            description=f"Compare charged amount against order total for {label.lower()}.",
            query=(
                "SELECT pt.amount, o.total_amount FROM payment_transactions pt "
                "JOIN orders o ON o.order_id = pt.order_id "
                "WHERE pt.transaction_id = :transaction_id"
            ),
            database_type="postgresql",
            expected_result_type="scalar",
            expected_value="amounts_match",
            priority=2,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_payment_status_flag"),
            name=f"Validate {label} payment status",
            description=f"Confirm payment status is in an expected terminal state.",
            query=(
                "SELECT status FROM payment_transactions "
                "WHERE transaction_id = :transaction_id"
            ),
            database_type="postgresql",
            expected_result_type="status_equals",
            expected_value="succeeded",
            priority=3,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_reconciliation_flag"),
            name=f"Validate {label} reconciliation flag",
            description=f"Confirm reconciliation flag is set for the {label.lower()} transaction.",
            query=(
                "SELECT reconciled FROM payment_transactions "
                "WHERE transaction_id = :transaction_id"
            ),
            database_type="postgresql",
            expected_result_type="status_equals",
            expected_value="true",
            priority=4,
        ),
    ]


def _authentication_checks(area: str) -> List[_CheckDraft]:
    label = area.title() if area else "Authentication"
    return [
        _CheckDraft(
            check_id=build_check_id(area, "validate_user_session_record"),
            name=f"Validate {label} user session record",
            description=f"Confirm an active session row exists after {label.lower()} succeeds.",
            query=(
                "SELECT session_id, user_id, expires_at FROM user_sessions "
                "WHERE user_id = :user_id AND revoked_at IS NULL"
            ),
            database_type="postgresql",
            expected_result_type="row_exists",
            expected_value=None,
            priority=1,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_login_audit_event"),
            name=f"Validate {label} login audit event",
            description=f"Confirm a login audit event was recorded for the user.",
            query=(
                "SELECT event_id, event_type, created_at FROM auth_audit_events "
                "WHERE user_id = :user_id AND event_type = 'login_success'"
            ),
            database_type="postgresql",
            expected_result_type="row_exists",
            expected_value=None,
            priority=2,
        ),
        _CheckDraft(
            check_id=build_check_id(area, "validate_auth_token_issuance"),
            name=f"Validate {label} auth token issuance",
            description=f"Confirm an auth token issuance event exists after login.",
            query=(
                "SELECT token_id, issued_at FROM auth_tokens "
                "WHERE user_id = :user_id AND issued_at >= :flow_started_at"
            ),
            database_type="postgresql",
            expected_result_type="row_exists",
            expected_value=None,
            priority=3,
        ),
    ]


def _checks_for_area(area: str, *, severity_rank: int) -> List[_CheckDraft]:
    category = _area_category(area)
    if category == "checkout":
        drafts = _checkout_checks(area)
    elif category == "payments":
        drafts = _payments_checks(area)
    elif category == "authentication":
        drafts = _authentication_checks(area)
    else:
        display = area.title() if area else "Impacted"
        drafts = [
            _CheckDraft(
                check_id=build_check_id(area, "validate_domain_record_exists"),
                name=f"Validate {display} domain record exists",
                description=f"Confirm a domain record exists for the impacted {display.lower()} area.",
                query=(
                    f"SELECT id, status FROM {re.sub(r'[^a-z0-9_]', '_', _normalize_area(area) or 'domain')}_records "
                    "WHERE correlation_id = :correlation_id"
                ),
                database_type="postgresql",
                expected_result_type="row_exists",
                expected_value=None,
                priority=5,
            ),
        ]
    for draft in drafts:
        draft.priority += severity_rank
    return drafts


def _collect_areas(
    *,
    impact_map: List[IncidentImpactNode],
    hypotheses: List[IncidentHypothesis],
    decision_center: Optional[DecisionCenterSummary],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
) -> List[Tuple[str, int]]:
    areas: Dict[str, int] = {}
    severity_rank = {"high": 0, "medium": 1, "low": 2}

    for node in impact_map:
        key = _normalize_area(node.title)
        if key:
            rank = severity_rank.get(node.severity, 2)
            areas[key] = min(areas.get(key, 9), rank)

    if decision_center and decision_center.top_impacted_area:
        key = _normalize_area(decision_center.top_impacted_area)
        if key:
            areas.setdefault(key, 1)

    for h in hypotheses[:3]:
        for word in ("checkout", "payments", "payment", "auth", "authentication", "login"):
            if word in (h.statement or "").lower():
                areas.setdefault(word, 2)

    if historical_learning and historical_learning.similar_incidents:
        for item in historical_learning.similar_incidents[:2]:
            token = (item.title or "").split(" ", 1)[0]
            key = _normalize_area(token)
            if key:
                areas.setdefault(key, 2)

    for rec in (test_recommendations.recommendations if test_recommendations else [])[:4]:
        key = _normalize_area(
            rec.test_name.replace(" Regression Suite", "").replace(" Smoke Suite", "")
        )
        if key:
            areas.setdefault(key, 2)

    for action in recommended_actions[:4]:
        for word in ("checkout", "payments", "payment", "auth", "login"):
            blob = f"{action.title} {action.description}".lower()
            if word in blob:
                areas.setdefault(word, 2)

    return sorted(areas.items(), key=lambda item: (item[1], item[0]))


def _draft_to_check(draft: _CheckDraft) -> Optional[DatabaseValidationCheck]:
    safe, _ = validate_query_safety(draft.query)
    if not safe:
        return None
    return DatabaseValidationCheck(
        check_id=draft.check_id,
        name=draft.name,
        description=draft.description,
        query=draft.query,
        database_type=draft.database_type,
        expected_result_type=draft.expected_result_type,
        expected_value=draft.expected_value,
        enabled=True,
        requires_user_approval=True,
    )


def _report_confidence(check_count: int, deployment_risk: Optional[DeploymentRiskAssessment]) -> float:
    score = 0.5 + min(0.25, check_count * 0.04)
    if deployment_risk:
        score = max(score, min(0.92, float(deployment_risk.confidence) * 0.85))
    return round(min(1.0, score), 2)


def build_database_validation(
    *,
    impact_map: List[IncidentImpactNode],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    decision_center: Optional[DecisionCenterSummary],
    historical_learning: Optional[HistoricalLearningReport],
    hypotheses: List[IncidentHypothesis],
) -> Optional[DatabaseValidationReport]:
    """Build read-only database validation definitions from existing intelligence."""
    area_items = _collect_areas(
        impact_map=impact_map,
        hypotheses=hypotheses,
        decision_center=decision_center,
        historical_learning=historical_learning,
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
    )
    if not area_items:
        return None

    pool: Dict[str, DatabaseValidationCheck] = {}
    for area, severity_rank in area_items[:4]:
        for draft in _checks_for_area(area, severity_rank=severity_rank):
            check = _draft_to_check(draft)
            if check:
                pool[check.check_id] = check

    if not pool:
        return None

    checks = sorted(pool.values(), key=lambda c: (c.name.lower(), c.check_id))
    planned_results = [
        DatabaseValidationResult(
            check_id=check.check_id,
            status="PLANNED",
            summary="Validation definition prepared. Database connector execution is not enabled in this sprint.",
            observed_value=None,
            confidence=0.0,
            executed_at=None,
            read_only=True,
        )
        for check in checks
    ]

    return DatabaseValidationReport(
        checks=checks,
        results=planned_results,
        summary=(
            f"{len(checks)} read-only database validation check(s) prepared. "
            "Database connector execution is not enabled — configure a future connector to run validations."
        ),
        confidence=_report_confidence(len(checks), deployment_risk_assessment),
    )
