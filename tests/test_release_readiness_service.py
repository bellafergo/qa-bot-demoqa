# tests/test_release_readiness_service.py
"""REL-01A — Release Readiness intelligence compositor."""
from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import patch

import pytest

from models.incident_models import (
    ContractRiskAssessment,
    ContractRiskReport,
    DataJourney,
    DataJourneyReport,
    DataJourneyResult,
    DecisionCenterInsight,
    DecisionCenterSummary,
    DeploymentRiskAssessment,
    ExecutiveQualityReport,
    MultiEnvironmentReport,
    PromotionReadiness,
    ProjectIncidentInvestigationReport,
    QualityHealthReport,
    QualityTrendReport,
    RiskFactor,
)
from models.azure_devops_integration_models import AzureDevOpsConnectionStatus
from models.github_integration_models import GitHubConnectionStatus
from models.scheduled_report_models import ExecutiveReportPreview
from services.release_readiness_service import (
    build_release_readiness_executive_preview,
    build_release_readiness_view,
    top_recommendations_from_view,
    top_risks_from_view,
)


def _incident(**overrides) -> ProjectIncidentInvestigationReport:
    base = ProjectIncidentInvestigationReport(
        id="rep-rel-1",
        created_at="2026-06-10T08:00:00+00:00",
        project_id="demo",
        description="Release validation",
        deployment_risk_assessment=DeploymentRiskAssessment(
            risk_score=78,
            risk_level="high",
            confidence=0.75,
            summary="Deployment risk elevated.",
            contributing_factors=[
                RiskFactor(
                    title="High-Risk PR Analysis",
                    description="PR overlap",
                    weight=0.2,
                ),
            ],
        ),
        contract_risk_assessment=ContractRiskReport(
            assessments=[
                ContractRiskAssessment(
                    assessment_id="a1",
                    contract_id="payments",
                    overall_risk_level="CRITICAL",
                    risk_score=92,
                    confidence=0.9,
                ),
            ],
            confidence=0.9,
        ),
        data_journey_validation=DataJourneyReport(
            journeys=[DataJourney(journey_id="checkout", name="Checkout", stages=[])],
            results=[DataJourneyResult(journey_id="checkout", status="BROKEN")],
            confidence=0.8,
        ),
        multi_environment=MultiEnvironmentReport(
            promotion_readiness=[
                PromotionReadiness(
                    source_environment_id="staging",
                    target_environment_id="production",
                    readiness_status="BLOCKED",
                    readiness_score=40,
                    blockers=["Staging validation incomplete"],
                    recommended_validations=["Run smoke suite on staging"],
                ),
            ],
        ),
        quality_health=QualityHealthReport(
            overall_score=82,
            overall_status="WARNING",
            trend="DEGRADING",
            summary="Quality health is degrading.",
        ),
        quality_trends=QualityTrendReport(
            overall_trend="DEGRADING",
            confidence=0.82,
            summary="Trend degrading.",
            trends=[],
        ),
        executive_quality_report=ExecutiveQualityReport(
            report_id="eqr-1",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_quality_score=82,
            overall_risk_level="HIGH",
            executive_summary="Executive attention required before release.",
            top_risks=["Checkout latency spike"],
            top_recommendations=["Run Payments Regression Suite"],
            open_incident_count=2,
            critical_contract_count=1,
            broken_journey_count=1,
        ),
        decision_center=DecisionCenterSummary(
            overall_status="ORANGE",
            executive_summary="Decision center recommends validation before release.",
            confidence=0.8,
            top_risk_level="HIGH",
            top_risk_score=78,
            key_takeaways=[
                DecisionCenterInsight(title="Validate Orders Database", description="", priority=1),
            ],
        ),
    )
    for key, value in overrides.items():
        setattr(base, key, value)
    return base


@patch("services.release_readiness_service.integration_dispatcher")
@patch("services.project_azure_devops_settings_service.get_project_azure_devops_status")
@patch("services.project_github_settings_service.get_project_github_status")
def test_build_release_readiness_view_composes_slices(mock_github, mock_azure, mock_dispatcher):
    mock_github.return_value = GitHubConnectionStatus(
        project_id="demo",
        connected=True,
        enabled=True,
        full_name="org/repo",
        provider="github_app",
    )
    mock_azure.return_value = AzureDevOpsConnectionStatus(
        project_id="demo",
        connected=False,
        enabled=False,
    )
    mock_dispatcher.readiness.return_value = {
        "slack": {"ready": True, "enabled": True, "health": "ok"},
    }

    incident = _incident()
    view = build_release_readiness_view(project_id="demo", incident_report=incident)

    assert view.project_id == "demo"
    assert view.source_incident_report_id == "rep-rel-1"
    assert view.deployment_risk_assessment is not None
    assert view.deployment_risk_assessment.risk_level == "high"
    assert view.decision_center is not None
    assert view.github is not None
    assert view.github.full_name == "org/repo"
    assert view.integration_readiness["slack"]["ready"] is True
    assert view.overall_status == "CAUTION"
    assert "Decision center recommends validation before release." in view.summary
    assert not any("Repository not connected" in gap for gap in view.data_gaps)

    risks = top_risks_from_view(view)
    assert any("Payments contract risk is CRITICAL" in r for r in risks)
    assert any("Checkout journey is BROKEN" in r for r in risks)
    assert any("Staging validation incomplete" in r for r in risks)

    recs = top_recommendations_from_view(view)
    assert "Run Payments Regression Suite" in recs
    assert "Validate Orders Database" in recs
    assert "Run smoke suite on staging" in recs


@patch("services.release_readiness_service._load_latest_incident_report", return_value=None)
@patch("services.release_readiness_service.integration_dispatcher")
@patch("services.project_azure_devops_settings_service.get_project_azure_devops_status")
@patch("services.project_github_settings_service.get_project_github_status")
def test_data_gaps_without_incident(mock_github, mock_azure, mock_dispatcher, _mock_load):
    mock_github.return_value = GitHubConnectionStatus(project_id="demo", connected=False, enabled=False)
    mock_azure.return_value = AzureDevOpsConnectionStatus(
        project_id="demo",
        connected=False,
        enabled=False,
    )
    mock_dispatcher.readiness.return_value = {}

    view = build_release_readiness_view(project_id="demo", incident_report=None)

    assert view.overall_status == "UNKNOWN"
    assert any("No incident investigation report" in gap for gap in view.data_gaps)
    assert any("Repository not connected" in gap for gap in view.data_gaps)


@patch("services.release_readiness_service.integration_dispatcher")
@patch("services.project_azure_devops_settings_service.get_project_azure_devops_status")
@patch("services.project_github_settings_service.get_project_github_status")
def test_executive_preview_mapping(mock_github, mock_azure, mock_dispatcher):
    mock_github.return_value = GitHubConnectionStatus(
        project_id="demo",
        connected=True,
        enabled=True,
        full_name="org/repo",
    )
    mock_azure.return_value = AzureDevOpsConnectionStatus(
        project_id="demo",
        connected=False,
        enabled=False,
    )
    mock_dispatcher.readiness.return_value = {}

    incident = _incident()
    view = build_release_readiness_view(project_id="demo", incident_report=incident)
    preview = build_release_readiness_executive_preview("demo", view)

    assert preview.preview_id == "exec_preview:demo:release_readiness"
    assert preview.title == "Release Readiness"
    assert preview.quality_score == 82
    assert preview.quality_trend == "DEGRADING"
    assert preview.risk_level == "HIGH"
    assert preview.incident_count == 2
    assert preview.critical_contract_count == 1
    assert preview.broken_journey_count == 1


@patch("services.release_readiness_service.integration_dispatcher")
@patch("services.project_azure_devops_settings_service.get_project_azure_devops_status")
@patch("services.project_github_settings_service.get_project_github_status")
def test_no_external_http(mock_github, mock_azure, mock_dispatcher):
    mock_github.return_value = GitHubConnectionStatus(project_id="demo", connected=False, enabled=False)
    mock_azure.return_value = AzureDevOpsConnectionStatus(
        project_id="demo",
        connected=False,
        enabled=False,
    )
    mock_dispatcher.readiness.return_value = {}

    with patch("urllib.request.urlopen", side_effect=AssertionError("external HTTP")):
        with patch("requests.request", side_effect=AssertionError("external HTTP")):
            view = build_release_readiness_view(project_id="demo", incident_report=_incident())

    assert view.project_id == "demo"


@patch("services.release_readiness_service.build_release_readiness_executive_preview")
@patch("services.release_readiness_service.build_release_readiness_view")
def test_scheduled_report_release_readiness_uses_compositor(mock_view, mock_preview):
    from services.scheduled_report_service import build_executive_report_preview

    incident = _incident()
    fake_view = SimpleNamespace(
        generated_at="2026-06-10T08:00:00+00:00",
        summary="composed",
    )
    mock_view.return_value = fake_view
    mock_preview.return_value = ExecutiveReportPreview(
        preview_id="exec_preview:demo:release_readiness",
        title="Release Readiness",
        generated_at="2026-06-10T08:00:00+00:00",
        quality_score=82,
        quality_trend="DEGRADING",
        risk_level="HIGH",
        executive_summary="composed",
    )

    preview = build_executive_report_preview(
        "demo",
        incident.model_dump(),
        report_type="RELEASE_READINESS",
    )

    mock_view.assert_called_once()
    mock_preview.assert_called_once()
    assert preview.title == "Release Readiness"
