# tests/test_value_dashboard_service.py
"""ROI-01A — Value Dashboard aggregation."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.incident_models import (
    ApprovalRequest,
    ApprovalWorkflowSummary,
    ContractRiskAssessment,
    ContractRiskReport,
    DataJourney,
    DataJourneyReport,
    DataJourneyResult,
    DatabaseValidationCheck,
    DatabaseValidationReport,
    DegradationAssessment,
    DegradationSignal,
    DeploymentRiskAssessment,
    EarlyDegradationReport,
    EnvironmentProfile,
    ExecutiveQualityReport,
    MultiEnvironmentReport,
    ProjectIncidentInvestigationReport,
    RecommendedAction,
)
from models.jira_issue_intelligence_models import JiraIssueCorrelation, JiraIssueIntelligenceReport
from models.release_readiness_models import ReleaseReadinessView
from models.scheduled_report_models import ExecutiveReportCenter, ExecutiveReportSchedule
from services.value_dashboard_service import build_value_dashboard


def _report(**overrides) -> ProjectIncidentInvestigationReport:
    base = ProjectIncidentInvestigationReport(
        id="rep-vd-1",
        created_at="2026-06-10T08:00:00+00:00",
        project_id="demo",
        description="Value dashboard fixture",
        recommended_actions=[
            RecommendedAction(
                action_id="a1",
                title="Run smoke suite",
                description="",
                reason="",
                priority=1,
                action_type="test",
            ),
            RecommendedAction(
                action_id="a2",
                title="Validate payments",
                description="",
                reason="",
                priority=2,
                action_type="validation",
            ),
        ],
        approval_workflow=ApprovalWorkflowSummary(
            pending_count=1,
            requests=[
                ApprovalRequest(
                    approval_id="req-1",
                    approval_type="deployment_hold",
                    title="Approve deployment hold",
                    description="",
                    status="PENDING",
                ),
            ],
        ),
        database_validation=DatabaseValidationReport(
            checks=[
                DatabaseValidationCheck(
                    check_id="c1",
                    name="Orders row exists",
                    query="SELECT 1",
                ),
                DatabaseValidationCheck(
                    check_id="c2",
                    name="Inventory row exists",
                    query="SELECT 1",
                ),
            ],
        ),
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=92,
            risk_level="critical",
            confidence=0.9,
            summary="Critical deployment risk.",
        ),
        contract_risk_assessment=ContractRiskReport(
            assessments=[
                ContractRiskAssessment(
                    assessment_id="cr-1",
                    contract_id="payments",
                    overall_risk_level="CRITICAL",
                    risk_score=95,
                    confidence=0.9,
                ),
            ],
            confidence=0.9,
        ),
        executive_quality_report=ExecutiveQualityReport(
            report_id="eqr-1",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_quality_score=68,
            overall_risk_level="CRITICAL",
            quality_trend="DEGRADING",
        ),
        data_journey_validation=DataJourneyReport(
            journeys=[DataJourney(journey_id="checkout", name="Checkout", stages=[])],
            results=[DataJourneyResult(journey_id="checkout", status="BROKEN")],
            confidence=0.8,
        ),
        multi_environment=MultiEnvironmentReport(
            environments=[
                EnvironmentProfile(
                    environment_id="staging",
                    name="Staging",
                    type="staging",
                    status="DEGRADED",
                ),
            ],
        ),
        early_degradation=EarlyDegradationReport(
            overall_status="DEGRADING",
            assessments=[
                DegradationAssessment(
                    assessment_id="deg-1",
                    scope_type="module",
                    scope_name="Payments",
                    status="DEGRADING",
                    signals=[
                        DegradationSignal(
                            signal_id="sig-1",
                            scope_type="module",
                            scope_name="Payments",
                            severity="HIGH",
                            summary="Score dropped",
                        ),
                    ],
                ),
            ],
        ),
        jira_issue_intelligence=JiraIssueIntelligenceReport(
            connected=True,
            total_issues=4,
            correlated_issues=3,
            blocker_count=2,
            top_blockers=[
                JiraIssueCorrelation(
                    issue_key="PAY-451",
                    issue_type="Bug",
                    status="Open",
                    priority="Blocker",
                    summary="timeout in staging",
                    related_module="Payments",
                    is_blocker=True,
                ),
            ],
            issue_correlations=[],
            summary="2 blockers detected.",
        ),
        release_readiness=ReleaseReadinessView(
            project_id="demo",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_status="BLOCKED",
            summary="Release blocked.",
        ),
    )
    for key, value in overrides.items():
        setattr(base, key, value)
    return base


@patch("services.value_dashboard_service._scheduled_reports_count", return_value=4)
@patch("services.value_dashboard_service._load_incident_reports")
def test_activity_and_operational_metrics(mock_load, _mock_scheduled):
    mock_load.return_value = [_report(), _report(id="rep-vd-2")]

    dashboard = build_value_dashboard("demo")

    assert dashboard.incidents_investigated == 2
    assert dashboard.executive_reports_generated == 2
    assert dashboard.release_readiness_reports == 2
    assert dashboard.scheduled_reports_generated == 4
    assert dashboard.recommendations_generated == 4
    assert dashboard.approvals_requested == 2
    assert dashboard.validations_planned == 4
    assert dashboard.correlated_jira_issues == 6


@patch("services.value_dashboard_service._scheduled_reports_count", return_value=0)
@patch("services.value_dashboard_service._load_incident_reports")
def test_risk_metrics(mock_load, _mock_scheduled):
    mock_load.return_value = [_report()]

    dashboard = build_value_dashboard("demo")

    assert dashboard.blocked_releases == 1
    assert dashboard.critical_risks_identified >= 2
    assert dashboard.jira_blockers_detected == 2
    assert dashboard.degradation_events_detected == 1


@patch("services.value_dashboard_service._scheduled_reports_count", return_value=0)
@patch("services.value_dashboard_service._load_incident_reports")
def test_quality_metrics(mock_load, _mock_scheduled):
    mock_load.return_value = [_report()]

    dashboard = build_value_dashboard("demo")

    assert dashboard.quality_health_score == 68
    assert dashboard.quality_trend == "DEGRADING"
    assert dashboard.degraded_environments == 1
    assert dashboard.impacted_journeys == 1


@patch("services.value_dashboard_service._scheduled_reports_count", return_value=0)
@patch("services.value_dashboard_service._load_incident_reports", return_value=[])
def test_empty_project(_mock_load, _mock_scheduled):
    dashboard = build_value_dashboard("demo")

    assert dashboard.incidents_investigated == 0
    assert dashboard.executive_reports_generated == 0
    assert dashboard.blocked_releases == 0
    assert dashboard.top_value_metrics == []
    assert dashboard.quality_trend == "UNKNOWN"


@patch("services.value_dashboard_service._scheduled_reports_count", return_value=0)
@patch("services.value_dashboard_service._load_incident_reports")
def test_top_value_metrics(mock_load, _mock_scheduled):
    mock_load.return_value = [_report()]

    dashboard = build_value_dashboard("demo")

    assert dashboard.top_value_metrics
    assert dashboard.top_value_metrics[0].value > 0
    assert dashboard.top_value_metrics[0].metric_id


@patch("services.scheduled_report_service.build_executive_report_center")
@patch("services.value_dashboard_service._load_incident_reports", return_value=[])
def test_scheduled_reports_from_report_center(mock_load, mock_center):
    mock_center.return_value = ExecutiveReportCenter(
        schedules=[
            ExecutiveReportSchedule(
                schedule_id="s1",
                name="Weekly",
                frequency="WEEKLY",
                enabled=True,
                report_type="QUALITY_BRIEF",
                recipients_count=2,
            ),
            ExecutiveReportSchedule(
                schedule_id="s2",
                name="Monthly",
                frequency="MONTHLY",
                enabled=False,
                report_type="RELEASE_READINESS",
                recipients_count=1,
            ),
        ],
        total_schedules=2,
    )

    dashboard = build_value_dashboard("demo")
    assert dashboard.scheduled_reports_generated == 1
