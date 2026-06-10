# services/approval_workflow_service.py
"""
Incident Investigator II-06B — Approval Workflow Foundation (read-only).

Generates approval request candidates from existing investigator outputs.
No execution, persistence, workflows, or external calls.
"""
from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from typing import Dict, List, Optional

from models.incident_models import (
    ApprovalRequest,
    ApprovalWorkflowSummary,
    DeploymentRiskAssessment,
    IncidentImpactNode,
    RecommendedAction,
    RecommendedTest,
    TestRecommendationReport,
)

_STATUS_RANK = {"PENDING": 0, "APPROVED": 1, "REJECTED": 2}
_RISK_PRIORITY_BOOST = {"critical": 0, "high": 1, "medium": 2, "low": 3}


@dataclass
class _RequestDraft:
    approval_id: str
    approval_type: str
    title: str
    description: str
    priority: int
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


def build_approval_id(
    approval_type: str,
    title: str,
    related_entity_type: Optional[str] = None,
    related_entity_id: Optional[str] = None,
) -> str:
    """Deterministic approval id — no UUIDs."""
    at = (approval_type or "approval").strip()
    et = (related_entity_type or "").strip()
    eid = (related_entity_id or "").strip()
    if et and eid:
        return f"approval:{at}:{et}:{eid}"
    norm = re.sub(r"\s+", " ", (title or at).strip().lower())
    digest = hashlib.sha256(norm.encode("utf-8")).hexdigest()[:12]
    return f"approval:{at}:title:{digest}"


def _risk_priority_boost(deployment_risk: Optional[DeploymentRiskAssessment]) -> int:
    if not deployment_risk:
        return 2
    return _RISK_PRIORITY_BOOST.get(str(deployment_risk.risk_level).lower(), 2)


def _impact_priority_boost(impact_map: List[IncidentImpactNode]) -> int:
    if any(n.severity == "high" for n in impact_map):
        return 0
    if any(n.severity == "medium" for n in impact_map):
        return 1
    return 2


def _upsert(pool: Dict[str, _RequestDraft], draft: _RequestDraft) -> None:
    existing = pool.get(draft.approval_id)
    if existing is None or draft.priority < existing.priority:
        pool[draft.approval_id] = draft


def _from_recommended_action(action: RecommendedAction, *, priority_boost: int) -> _RequestDraft:
    entity_type = (action.related_entity_type or "").strip() or None
    entity_id = (action.related_entity_id or "").strip() or None
    approval_type = (action.action_type or "recommended_action").strip()
    title = (action.title or approval_type).strip()
    return _RequestDraft(
        approval_id=build_approval_id(approval_type, title, entity_type, entity_id),
        approval_type=approval_type,
        title=title,
        description=(action.description or action.reason or title).strip(),
        priority=int(action.priority) + priority_boost,
        related_entity_type=entity_type,
        related_entity_id=entity_id,
    )


def _from_recommended_test(test: RecommendedTest, *, priority_boost: int) -> _RequestDraft:
    entity_type = (test.related_entity_type or "").strip() or None
    entity_id = (test.related_entity_id or "").strip() or None
    approval_type = (test.test_type or "run_test_suite").strip()
    title = (test.test_name or approval_type).strip()
    return _RequestDraft(
        approval_id=build_approval_id(approval_type, title, entity_type, entity_id),
        approval_type=approval_type,
        title=title,
        description=(test.reason or f"Approve running '{title}' before release.").strip(),
        priority=int(test.priority) + priority_boost,
        related_entity_type=entity_type,
        related_entity_id=entity_id,
    )


def build_approval_workflow(
    *,
    recommended_actions: List[RecommendedAction],
    test_recommendations: Optional[TestRecommendationReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment] = None,
    impact_map: List[IncidentImpactNode],
    created_at: str = "",
) -> Optional[ApprovalWorkflowSummary]:
    """Build read-only approval workflow summary from existing investigator outputs."""
    priority_boost = min(_risk_priority_boost(deployment_risk_assessment), _impact_priority_boost(impact_map))
    pool: Dict[str, _RequestDraft] = {}

    for action in recommended_actions:
        if not action.requires_user_approval:
            continue
        _upsert(pool, _from_recommended_action(action, priority_boost=priority_boost))

    for test in (test_recommendations.recommendations if test_recommendations else []):
        if not test.requires_user_approval:
            continue
        _upsert(pool, _from_recommended_test(test, priority_boost=priority_boost))

    if not pool:
        return None

    ordered = sorted(
        pool.values(),
        key=lambda d: (_STATUS_RANK["PENDING"], d.priority, d.title.lower()),
    )

    requests = [
        ApprovalRequest(
            approval_id=d.approval_id,
            approval_type=d.approval_type,
            title=d.title,
            description=d.description,
            status="PENDING",
            created_at=created_at,
            related_entity_type=d.related_entity_type,
            related_entity_id=d.related_entity_id,
        )
        for d in ordered
    ]

    return ApprovalWorkflowSummary(
        pending_count=sum(1 for r in requests if r.status == "PENDING"),
        approved_count=sum(1 for r in requests if r.status == "APPROVED"),
        rejected_count=sum(1 for r in requests if r.status == "REJECTED"),
        requests=requests,
    )
