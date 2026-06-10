# tests/test_jira_issue_intelligence_service.py
"""JIRA-01B — Jira issue intelligence service tests."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.incident_models import (
    BlastRadiusModule,
    DeploymentRiskAssessment,
    EnvironmentProfile,
    MultiEnvironmentReport,
    ProjectIncidentInvestigationReport,
)
from models.jira_models import JiraConnectionStatus
from models.release_readiness_models import ReleaseReadinessView
from services.jira_issue_intelligence_service import build_jira_issue_intelligence_report


def _issue_raw(
    *,
    key: str = "QA-1",
    summary: str = "Payments checkout failure",
    priority: str = "High",
    status: str = "Open",
    description=None,
):
    return {
        "id": "1",
        "key": key,
        "fields": {
            "summary": summary,
            "issuetype": {"name": "Bug"},
            "status": {"name": status},
            "priority": {"name": priority},
            "description": description,
        },
    }


def _connected_status():
    return JiraConnectionStatus(connected=True, server_url="https://acme.atlassian.net")


def _incident_with_modules():
    return ProjectIncidentInvestigationReport(
        project_id="demo",
        description="test",
        impacted_modules=["Payments"],
        impacted_modules_ranked=[
            BlastRadiusModule(module="Payments", score=88.0, reason="checkout failures"),
        ],
        multi_environment=MultiEnvironmentReport(
            environments=[
                EnvironmentProfile(
                    environment_id="env:staging",
                    name="Staging",
                    type="staging",
                    status="DEGRADED",
                ),
            ],
        ),
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=82,
            risk_level="high",
            summary="Elevated deployment risk",
        ),
    )


class TestEmptyJira:
    def test_empty_when_not_connected(self):
        with patch(
            "services.jira_issue_intelligence_service.validate_jira_connection",
            return_value=JiraConnectionStatus(connected=False),
        ):
            report = build_jira_issue_intelligence_report()
        assert report.connected is False
        assert report.total_issues == 0
        assert "No Jira connection" in report.data_gaps[0]


class TestModuleCorrelation:
    def test_module_correlation(self):
        with patch(
            "services.jira_issue_intelligence_service.validate_jira_connection",
            return_value=_connected_status(),
        ), patch(
            "services.jira_issue_intelligence_service._fetch_issues",
            return_value=([_issue_raw(summary="Payments API timeout in checkout")], 1),
        ):
            report = build_jira_issue_intelligence_report(
                incident_report=_incident_with_modules(),
            )
        assert report.correlated_issues == 1
        corr = report.issue_correlations[0]
        assert corr.related_module == "Payments"
        assert corr.correlation_score >= 35
        assert "Module" in corr.correlation_reason or "Impacted module" in corr.correlation_reason


class TestEnvironmentCorrelation:
    def test_environment_correlation(self):
        with patch(
            "services.jira_issue_intelligence_service.validate_jira_connection",
            return_value=_connected_status(),
        ), patch(
            "services.jira_issue_intelligence_service._fetch_issues",
            return_value=([_issue_raw(summary="Staging login regression")], 1),
        ):
            report = build_jira_issue_intelligence_report(
                incident_report=_incident_with_modules(),
            )
        corr = report.issue_correlations[0]
        assert corr.related_environment == "Staging"
        assert "environment" in corr.correlation_reason.lower()


class TestBlockerDetection:
    def test_blocker_priority(self):
        with patch(
            "services.jira_issue_intelligence_service.validate_jira_connection",
            return_value=_connected_status(),
        ), patch(
            "services.jira_issue_intelligence_service._fetch_issues",
            return_value=([_issue_raw(priority="Blocker", summary="Payments outage")], 1),
        ):
            report = build_jira_issue_intelligence_report(
                incident_report=_incident_with_modules(),
            )
        assert report.blocker_count == 1
        assert report.top_blockers[0].is_blocker is True

    def test_highest_unresolved_is_blocker(self):
        with patch(
            "services.jira_issue_intelligence_service.validate_jira_connection",
            return_value=_connected_status(),
        ), patch(
            "services.jira_issue_intelligence_service._fetch_issues",
            return_value=([_issue_raw(priority="Highest", status="In Progress", summary="Auth bug")], 1),
        ):
            report = build_jira_issue_intelligence_report(
                incident_report=ProjectIncidentInvestigationReport(
                    project_id="demo",
                    description="test",
                    impacted_modules=["Authentication"],
                ),
            )
        assert report.blocker_count == 1


class TestEmptyIntelligence:
    def test_no_correlations_without_intelligence_context(self):
        with patch(
            "services.jira_issue_intelligence_service.validate_jira_connection",
            return_value=_connected_status(),
        ), patch(
            "services.jira_issue_intelligence_service._fetch_issues",
            return_value=([_issue_raw(priority="Low", summary="Unrelated docs task")], 1),
        ):
            report = build_jira_issue_intelligence_report()
        assert report.total_issues == 1
        assert report.correlated_issues == 0
        assert "No correlated issues" in report.data_gaps[0]

    def test_release_risk_boost_with_readiness(self):
        with patch(
            "services.jira_issue_intelligence_service.validate_jira_connection",
            return_value=_connected_status(),
        ), patch(
            "services.jira_issue_intelligence_service._fetch_issues",
            return_value=([_issue_raw(summary="Payments release blocker")], 1),
        ):
            report = build_jira_issue_intelligence_report(
                incident_report=_incident_with_modules(),
                release_readiness=ReleaseReadinessView(
                    project_id="demo",
                    generated_at="2026-01-01T00:00:00Z",
                    overall_status="BLOCKED",
                ),
            )
        assert report.correlated_issues == 1
        assert "Release risk context" in report.issue_correlations[0].correlation_reason


class TestIntelligenceRoute:
    def _client(self):
        from fastapi.testclient import TestClient
        from app import app

        return TestClient(app)

    def test_intelligence_route(self):
        from models.jira_issue_intelligence_models import JiraIssueIntelligenceReport

        fake = JiraIssueIntelligenceReport(
            connected=True,
            total_issues=3,
            correlated_issues=2,
            blocker_count=1,
            high_priority_count=2,
            summary="2 of 3 issues correlate",
        )
        with patch(
            "api.routes.jira_integration_routes.build_jira_issue_intelligence_report",
            return_value=fake,
        ):
            res = self._client().get("/integrations/jira/intelligence")
        assert res.status_code == 200
        assert res.json()["correlated_issues"] == 2
