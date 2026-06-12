# services/platform_observability_service.py
"""
OBS-02A — Vanya self observability (read-only).

Derives platform health from existing audit events, integration status,
report delivery history, and incident investigation activity.
No external calls, alerting, or remediation.
"""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import List, Optional, Tuple

from models.platform_observability_models import (
    IntegrationHealthSummary,
    PlatformHealthArea,
    PlatformHealthMetric,
    PlatformHealthStatus,
    PlatformObservabilityReport,
)
from services.audit_event_service import build_audit_summary, list_events
from services.integration_service import integration_service

logger = logging.getLogger("vanya.platform_observability")

_AUDIT_SAMPLE_LIMIT = 500


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _audit_result_counts(event_type: str) -> Tuple[int, int]:
    events = list_events(event_type=event_type, limit=_AUDIT_SAMPLE_LIMIT).events
    success = sum(1 for event in events if event.result == "SUCCESS")
    failure = sum(1 for event in events if event.result == "FAILURE")
    return success, failure


def _worst_status(*statuses: PlatformHealthStatus) -> PlatformHealthStatus:
    order = {"UNHEALTHY": 3, "DEGRADED": 2, "HEALTHY": 1, "UNKNOWN": 0}
    ranked = [order.get(status, 0) for status in statuses]
    if not ranked or max(ranked) == 0:
        return "UNKNOWN"
    if max(ranked) >= order["UNHEALTHY"]:
        return "UNHEALTHY"
    if max(ranked) >= order["DEGRADED"]:
        return "DEGRADED"
    return "HEALTHY"


def _ratio_status(*, success: int, failure: int, failure_threshold: int = 3) -> PlatformHealthStatus:
    total = success + failure
    if total == 0:
        return "UNKNOWN"
    if failure >= failure_threshold:
        return "UNHEALTHY"
    if failure > 0:
        return "DEGRADED"
    return "HEALTHY"


def _connector_bucket(health: str) -> str:
    normalized = (health or "unknown").lower()
    if normalized == "ok":
        return "healthy"
    if normalized == "degraded":
        return "degraded"
    if normalized in {"unreachable", "unconfigured"}:
        return "disconnected"
    return "unknown"


def _build_api_health() -> PlatformHealthArea:
    summary = build_audit_summary()
    total = summary.total_events
    if total == 0:
        return PlatformHealthArea(
            status="UNKNOWN",
            summary="No platform audit activity recorded yet.",
            metrics=[
                PlatformHealthMetric(
                    metric_name="audit_events",
                    status="UNKNOWN",
                    value="0",
                    summary="Audit trail has no recorded events.",
                )
            ],
        )

    return PlatformHealthArea(
        status="HEALTHY",
        summary=f"Platform audit trail active with {total} recorded events.",
        metrics=[
            PlatformHealthMetric(
                metric_name="audit_events",
                status="HEALTHY",
                value=str(total),
                summary="Centralized audit events are being recorded.",
            )
        ],
    )


def _build_authentication_health() -> PlatformHealthArea:
    from services.auth_identity_service import build_security_readiness_report

    readiness = build_security_readiness_report()
    sso_success, sso_failure = _audit_result_counts("SSO_PROVIDER_VALIDATED")
    login_success, login_failure = _audit_result_counts("SSO_LOGIN_URL_GENERATED")
    total_success = sso_success + login_success
    total_failure = sso_failure + login_failure

    status = _ratio_status(success=total_success, failure=total_failure, failure_threshold=2)
    if status == "UNKNOWN" and readiness.sso_ready:
        status = "HEALTHY"
    elif status == "UNKNOWN" and not readiness.sso_ready:
        status = "HEALTHY" if readiness.audit_ready else "UNKNOWN"

    summary = (
        f"Authentication foundation: {readiness.authentication_method}. "
        f"SSO validations succeeded {sso_success} and failed {sso_failure}. "
        f"Login URL generations: {login_success}."
    )

    return PlatformHealthArea(
        status=status,
        summary=summary,
        metrics=[
            PlatformHealthMetric(
                metric_name="sso_validations",
                status=_ratio_status(success=sso_success, failure=sso_failure, failure_threshold=2),
                value=f"{sso_success}/{sso_success + sso_failure}",
                summary=f"Successful SSO provider validations: {sso_success}.",
            ),
            PlatformHealthMetric(
                metric_name="login_activity",
                status="HEALTHY" if login_success > 0 else "UNKNOWN",
                value=str(login_success),
                summary=f"SSO login URL generations recorded: {login_success}.",
            ),
            PlatformHealthMetric(
                metric_name="security_readiness",
                status="HEALTHY" if readiness.security_score >= 50 else "DEGRADED",
                value=f"{readiness.security_score}/100",
                summary=readiness.summary,
            ),
        ],
    )


def _build_integration_health() -> Tuple[PlatformHealthArea, List[str]]:
    connectors = integration_service.list_connectors()
    enabled = [connector for connector in connectors if connector.enabled]
    risks: List[str] = []

    summary_counts = IntegrationHealthSummary()
    for connector in enabled:
        bucket = _connector_bucket(connector.health)
        if bucket == "healthy":
            summary_counts.healthy += 1
        elif bucket == "degraded":
            summary_counts.degraded += 1
        elif bucket == "disconnected":
            summary_counts.disconnected += 1
            label = connector.connector_name or connector.connector_id
            risks.append(f"{label} integration disconnected")

    validation_success, validation_failure = _audit_result_counts("INTEGRATION_VALIDATED")
    for event in list_events(event_type="INTEGRATION_VALIDATED", limit=_AUDIT_SAMPLE_LIMIT).events:
        if event.result != "FAILURE":
            continue
        connector_id = (event.resource_id or "").strip()
        if connector_id == "qmetry":
            risks.append("QMetry validation failures detected")
        elif connector_id == "github" and "GitHub integration disconnected" not in risks:
            risks.append("GitHub integration disconnected")

    if not enabled:
        status: PlatformHealthStatus = "UNKNOWN"
        area_summary = "No integrations are enabled."
    elif summary_counts.disconnected > 0:
        status = "UNHEALTHY" if summary_counts.disconnected >= len(enabled) else "DEGRADED"
        area_summary = (
            f"{summary_counts.healthy} healthy, {summary_counts.degraded} degraded, "
            f"{summary_counts.disconnected} disconnected of {len(enabled)} enabled integrations."
        )
    elif summary_counts.degraded > 0 or validation_failure > 0:
        status = "DEGRADED"
        area_summary = (
            f"{summary_counts.healthy} healthy and {summary_counts.degraded} degraded integrations. "
            f"Validation failures: {validation_failure}."
        )
    elif summary_counts.healthy == len(enabled):
        status = "HEALTHY"
        area_summary = f"All {len(enabled)} enabled integrations are healthy."
    else:
        status = "UNKNOWN"
        area_summary = "Integration health status is not fully known."

    metrics = [
        PlatformHealthMetric(
            metric_name="enabled_integrations",
            status=status,
            value=str(len(enabled)),
            summary=f"{len(enabled)} integrations enabled.",
        ),
        PlatformHealthMetric(
            metric_name="integration_validations",
            status=_ratio_status(
                success=validation_success,
                failure=validation_failure,
                failure_threshold=2,
            ),
            value=f"{validation_success}/{validation_success + validation_failure}",
            summary=f"Integration validation successes: {validation_success}, failures: {validation_failure}.",
        ),
    ]

    return (
        PlatformHealthArea(
            status=status,
            summary=area_summary,
            metrics=metrics,
            integration_summary=summary_counts,
        ),
        risks,
    )


def _build_report_delivery_health() -> Tuple[PlatformHealthArea, List[str]]:
    success, failure = _audit_result_counts("REPORT_SENT")
    status = _ratio_status(success=success, failure=failure, failure_threshold=2)
    risks: List[str] = []
    if failure > 0:
        risks.append("Report delivery failures observed")

    if success + failure == 0:
        summary = "No report delivery activity recorded yet."
    elif failure > 0:
        summary = f"Report delivery recorded {success} successes and {failure} failures."
    else:
        summary = f"Report delivery healthy with {success} successful sends."

    return (
        PlatformHealthArea(
            status=status,
            summary=summary,
            metrics=[
                PlatformHealthMetric(
                    metric_name="report_sends",
                    status=status,
                    value=f"{success}/{success + failure}",
                    summary=summary,
                )
            ],
        ),
        risks,
    )


def _build_incident_investigation_health() -> PlatformHealthArea:
    audit_summary = build_audit_summary()
    started = audit_summary.event_types.get("INCIDENT_INVESTIGATION_STARTED", 0)
    completed = audit_summary.event_types.get("INCIDENT_INVESTIGATION_COMPLETED", 0)
    pending = max(0, started - completed)

    if started == 0 and completed == 0:
        status: PlatformHealthStatus = "UNKNOWN"
        summary = "No incident investigation activity recorded yet."
    elif pending > 0:
        status = "DEGRADED"
        summary = (
            f"{completed} investigations completed with {pending} still in progress "
            f"of {started} started."
        )
    else:
        status = "HEALTHY"
        summary = f"{completed} incident investigations completed successfully."

    return PlatformHealthArea(
        status=status,
        summary=summary,
        metrics=[
            PlatformHealthMetric(
                metric_name="investigations_started",
                status="HEALTHY" if started > 0 else "UNKNOWN",
                value=str(started),
                summary=f"Investigations started: {started}.",
            ),
            PlatformHealthMetric(
                metric_name="investigations_completed",
                status=status,
                value=str(completed),
                summary=f"Investigations completed: {completed}.",
            ),
        ],
    )


def _build_top_platform_risks(
    *,
    integration_risks: List[str],
    report_risks: List[str],
    auth_failure_count: int,
) -> List[str]:
    risks: List[str] = []
    seen = set()

    def _add(risk: str) -> None:
        key = risk.strip().lower()
        if not key or key in seen:
            return
        seen.add(key)
        risks.append(risk)

    for risk in integration_risks + report_risks:
        _add(risk)

    if auth_failure_count >= 2:
        _add("Repeated authentication failures")

    return risks[:8]


def _build_executive_summary(
    *,
    api_health: PlatformHealthArea,
    authentication_health: PlatformHealthArea,
    integration_health: PlatformHealthArea,
    report_delivery_health: PlatformHealthArea,
    incident_investigation_health: PlatformHealthArea,
    top_platform_risks: List[str],
) -> str:
    overall = _worst_status(
        api_health.status,
        authentication_health.status,
        integration_health.status,
        report_delivery_health.status,
        incident_investigation_health.status,
    )

    if overall == "UNKNOWN":
        base = "Platform observability has limited historical data."
    elif overall == "HEALTHY":
        base = "Vanya platform health is stable across monitored surfaces."
    elif overall == "DEGRADED":
        base = "Vanya platform health shows degraded signals that need attention."
    else:
        base = "Vanya platform health has critical issues across one or more surfaces."

    if top_platform_risks:
        base += f" Top risks: {'; '.join(top_platform_risks[:3])}."
    return base


def build_platform_observability_report() -> PlatformObservabilityReport:
    """Build a read-only platform observability snapshot from existing platform data."""
    api_health = _build_api_health()
    authentication_health = _build_authentication_health()
    integration_health, integration_risks = _build_integration_health()
    report_delivery_health, report_risks = _build_report_delivery_health()
    incident_investigation_health = _build_incident_investigation_health()

    _, auth_failures = _audit_result_counts("SSO_PROVIDER_VALIDATED")
    top_platform_risks = _build_top_platform_risks(
        integration_risks=integration_risks,
        report_risks=report_risks,
        auth_failure_count=auth_failures,
    )

    executive_summary = _build_executive_summary(
        api_health=api_health,
        authentication_health=authentication_health,
        integration_health=integration_health,
        report_delivery_health=report_delivery_health,
        incident_investigation_health=incident_investigation_health,
        top_platform_risks=top_platform_risks,
    )

    return PlatformObservabilityReport(
        generated_at=_utc_now_iso(),
        api_health=api_health,
        authentication_health=authentication_health,
        integration_health=integration_health,
        report_delivery_health=report_delivery_health,
        incident_investigation_health=incident_investigation_health,
        top_platform_risks=top_platform_risks,
        executive_summary=executive_summary,
    )
