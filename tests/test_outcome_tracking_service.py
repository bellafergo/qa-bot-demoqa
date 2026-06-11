# tests/test_outcome_tracking_service.py
"""ROI-02A — Outcome Tracking aggregation."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.incident_models import (
    ProjectIncidentInvestigationReport,
    RecommendedAction,
)
from models.jira_issue_intelligence_models import JiraIssueIntelligenceReport
from models.qmetry_recommendation_models import QMetryRecommendationReport
from models.release_readiness_models import ReleaseReadinessView
from services.outcome_tracking_service import build_outcome_tracking_report


def _report(**overrides) -> ProjectIncidentInvestigationReport:
    base = ProjectIncidentInvestigationReport(
        id="rep-ot-1",
        created_at="2026-06-10T08:00:00+00:00",
        project_id="demo",
        description="Outcome tracking fixture",
        recommended_actions=[
            RecommendedAction(
                action_id="a1",
                title="Run smoke suite",
                description="",
                reason="",
                priority=1,
                action_type="test",
            ),
        ],
        jira_issue_intelligence=JiraIssueIntelligenceReport(
            connected=True,
            blocker_count=3,
            correlated_issues=2,
            executive_summary="Three blockers found.",
        ),
        release_readiness=ReleaseReadinessView(
            project_id="demo",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_status="BLOCKED",
            summary="Release blocked.",
        ),
        qmetry_recommendation_report=QMetryRecommendationReport(
            generated_at="2026-06-10T08:00:00+00:00",
            connected=True,
            total_recommendations=5,
            executive_summary="Five recommendations.",
            recommendation_groups=[],
        ),
    )
    return base.model_copy(update=overrides)


@patch("services.outcome_tracking_service._count_executive_reports_sent")
@patch("services.outcome_tracking_service._load_incident_reports")
def test_blockers_counting(mock_load, mock_reports_sent):
    mock_load.return_value = [_report()]
    mock_reports_sent.return_value = 0

    report = build_outcome_tracking_report("demo")

    assert report.blockers_identified == 3


@patch("services.outcome_tracking_service._count_executive_reports_sent")
@patch("services.outcome_tracking_service._load_incident_reports")
def test_releases_counting(mock_load, mock_reports_sent):
    mock_load.return_value = [_report()]
    mock_reports_sent.return_value = 0

    report = build_outcome_tracking_report("demo")

    assert report.releases_blocked == 1


@patch("services.outcome_tracking_service._count_executive_reports_sent")
@patch("services.outcome_tracking_service._load_incident_reports")
def test_recommendations_counting(mock_load, mock_reports_sent):
    mock_load.return_value = [_report()]
    mock_reports_sent.return_value = 0

    report = build_outcome_tracking_report("demo")

    assert report.recommendations_generated == 6


@patch("services.outcome_tracking_service._count_executive_reports_sent")
@patch("services.outcome_tracking_service._load_incident_reports")
def test_reports_counting(mock_load, mock_reports_sent):
    mock_load.return_value = [_report(), _report(id="rep-ot-2")]
    mock_reports_sent.return_value = 8

    report = build_outcome_tracking_report("demo")

    assert report.incidents_investigated == 2
    assert report.executive_reports_sent == 8
    assert "delivered 8 executive reports" in report.executive_summary


@patch("services.outcome_tracking_service._count_executive_reports_sent")
@patch("services.outcome_tracking_service._load_incident_reports")
def test_empty_state(mock_load, mock_reports_sent):
    mock_load.return_value = []
    mock_reports_sent.return_value = 0

    report = build_outcome_tracking_report("demo")

    assert report.blockers_identified == 0
    assert report.releases_blocked == 0
    assert report.recommendations_generated == 0
    assert report.incidents_investigated == 0
    assert report.executive_reports_sent == 0
    assert report.executive_summary == "No measurable outcomes available yet."
    assert len(report.outcome_metrics) == 5


@patch("services.outcome_tracking_service._count_executive_reports_sent")
@patch("services.outcome_tracking_service._load_incident_reports")
def test_executive_summary_template(mock_load, mock_reports_sent):
    mock_load.return_value = [
        _report(
            jira_issue_intelligence=JiraIssueIntelligenceReport(
                connected=True,
                blocker_count=14,
                correlated_issues=4,
                executive_summary="Blockers found.",
            ),
            qmetry_recommendation_report=QMetryRecommendationReport(
                generated_at="2026-06-10T08:00:00+00:00",
                connected=True,
                total_recommendations=28,
                executive_summary="Recommendations ready.",
                recommendation_groups=[],
            ),
            recommended_actions=[],
        ),
    ]
    mock_reports_sent.return_value = 8

    report = build_outcome_tracking_report("demo")

    assert "identified 14 blockers" in report.executive_summary
    assert "generated 28 test recommendations" in report.executive_summary
    assert "delivered 8 executive reports" in report.executive_summary
