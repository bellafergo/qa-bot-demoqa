# tests/test_executive_impact_service.py
"""ROI-01B — Executive Impact Metrics."""
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
    DeploymentRiskAssessment,
    EnvironmentProfile,
    ExecutiveQualityReport,
    MultiEnvironmentReport,
    ProjectIncidentInvestigationReport,
    RecommendedAction,
)
from models.jira_issue_intelligence_models import JiraIssueCorrelation, JiraIssueIntelligenceReport
from models.release_readiness_models import ReleaseReadinessView
from services.executive_impact_service import build_executive_impact_report


def _report(
    *,
    report_id: str,
    quality_score: int = 70,
    blocked: bool = False,
    critical_risks: bool = False,
    jira_blockers: int = 0,
    degraded_envs: int = 0,
    broken_journeys: int = 0,
    recommendations: int = 1,
    approvals: int = 0,
    validations: int = 0,
) -> ProjectIncidentInvestigationReport:
    journeys = []
    results = []
    for i in range(broken_journeys):
        jid = f"journey-{i}"
        journeys.append(DataJourney(journey_id=jid, name=f"Journey {i}", stages=[]))
        results.append(DataJourneyResult(journey_id=jid, status="BROKEN"))

    environments = [
        EnvironmentProfile(
            environment_id=f"env-{i}",
            name=f"Env {i}",
            type="staging",
            status="DEGRADED",
        )
        for i in range(degraded_envs)
    ]

    contract = None
    deployment = None
    eqr_risk = "HIGH"
    if critical_risks:
        contract = ContractRiskReport(
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
        )
        deployment = DeploymentRiskAssessment(
            risk_score=90,
            risk_level="critical",
            confidence=0.9,
            summary="Critical risk.",
        )
        eqr_risk = "CRITICAL"

    return ProjectIncidentInvestigationReport(
        id=report_id,
        created_at="2026-06-10T08:00:00+00:00",
        project_id="demo",
        description=f"Impact fixture {report_id}",
        recommended_actions=[
            RecommendedAction(
                action_id=f"rec-{i}",
                title=f"Action {i}",
                description="",
                reason="",
                priority=i + 1,
                action_type="test",
            )
            for i in range(recommendations)
        ],
        approval_workflow=ApprovalWorkflowSummary(
            requests=[
                ApprovalRequest(
                    approval_id=f"ap-{i}",
                    approval_type="deployment",
                    title=f"Approval {i}",
                    status="PENDING",
                )
                for i in range(approvals)
            ],
        ) if approvals else None,
        database_validation=DatabaseValidationReport(
            checks=[
                DatabaseValidationCheck(
                    check_id=f"chk-{i}",
                    name=f"Check {i}",
                    query="SELECT 1",
                )
                for i in range(validations)
            ],
        ) if validations else None,
        deployment_risk_assessment=deployment,
        contract_risk_assessment=contract,
        executive_quality_report=ExecutiveQualityReport(
            report_id=f"eqr-{report_id}",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_quality_score=quality_score,
            overall_risk_level=eqr_risk,
            quality_trend="STABLE",
        ),
        data_journey_validation=DataJourneyReport(
            journeys=journeys,
            results=results,
            confidence=0.8,
        ) if journeys else None,
        multi_environment=MultiEnvironmentReport(environments=environments) if environments else None,
        jira_issue_intelligence=JiraIssueIntelligenceReport(
            connected=True,
            blocker_count=jira_blockers,
            top_blockers=[
                JiraIssueCorrelation(
                    issue_key=f"PAY-{i}",
                    issue_type="Bug",
                    status="Open",
                    priority="Blocker",
                    summary="blocker",
                    is_blocker=True,
                )
                for i in range(jira_blockers)
            ],
            correlated_issues=jira_blockers,
            summary="blockers",
        ) if jira_blockers else None,
        release_readiness=ReleaseReadinessView(
            project_id="demo",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_status="BLOCKED" if blocked else "GO",
            summary="status",
        ),
    )


@patch("services.executive_impact_service._load_incident_reports", return_value=[])
def test_insufficient_history_empty_project(_mock_load):
    report = build_executive_impact_report("demo")
    assert report.has_sufficient_history is False
    assert report.quality_health_trend.direction == "UNKNOWN"
    assert report.top_improvements == []
    assert report.top_concerns == []


@patch("services.executive_impact_service._load_incident_reports")
def test_insufficient_history_single_report(mock_load):
    mock_load.return_value = [_report(report_id="r1", quality_score=80)]
    report = build_executive_impact_report("demo")
    assert report.has_sufficient_history is False
    assert report.quality_health_trend.current_value == 80
    assert report.quality_health_trend.direction == "UNKNOWN"


@patch("services.executive_impact_service._load_incident_reports")
def test_improving_jira_blockers(mock_load):
    mock_load.return_value = [
        _report(report_id="current", jira_blockers=2),
        _report(report_id="previous", jira_blockers=5),
    ]
    report = build_executive_impact_report("demo")
    assert report.has_sufficient_history is True
    assert report.jira_blocker_trend.direction == "IMPROVING"
    assert report.jira_blocker_trend.delta == -3
    assert report.jira_blocker_trend.current_value == 2
    assert report.jira_blocker_trend.previous_value == 5
    assert any(m.metric_id == "jira_blockers" for m in report.top_improvements)


@patch("services.executive_impact_service._load_incident_reports")
def test_degrading_quality_health(mock_load):
    mock_load.return_value = [
        _report(report_id="current", quality_score=62),
        _report(report_id="previous", quality_score=78),
    ]
    report = build_executive_impact_report("demo")
    assert report.quality_health_trend.direction == "DEGRADING"
    assert report.quality_health_trend.delta == -16
    assert any(m.metric_id == "quality_health" for m in report.top_concerns)


@patch("services.executive_impact_service._load_incident_reports")
def test_stable_blocked_releases(mock_load):
    mock_load.return_value = [
        _report(report_id="current", blocked=True),
        _report(report_id="previous", blocked=True),
    ]
    report = build_executive_impact_report("demo")
    assert report.blocked_release_trend.direction == "STABLE"
    assert report.blocked_release_trend.delta == 0


@patch("services.executive_impact_service._load_incident_reports")
def test_improving_quality_and_operations(mock_load):
    mock_load.return_value = [
        _report(report_id="current", quality_score=88, recommendations=4, validations=3),
        _report(report_id="previous", quality_score=72, recommendations=2, validations=1),
    ]
    report = build_executive_impact_report("demo")
    assert report.quality_health_trend.direction == "IMPROVING"
    assert report.recommendation_trend.direction == "IMPROVING"
    assert report.validation_trend.direction == "IMPROVING"
