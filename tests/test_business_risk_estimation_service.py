# tests/test_business_risk_estimation_service.py
"""ROI-01C — Executive Business Risk Estimation."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.incident_models import (
    ContractRiskAssessment,
    ContractRiskReport,
    DataJourney,
    DataJourneyReport,
    DataJourneyResult,
    DependencyNode,
    EnterpriseDependencyMap,
    EnvironmentProfile,
    MultiEnvironmentReport,
    ProjectIncidentInvestigationReport,
)
from models.jira_issue_intelligence_models import JiraIssueCorrelation, JiraIssueIntelligenceReport
from models.release_readiness_models import ReleaseReadinessView
from services.business_risk_estimation_service import (
    _confidence_from_evidence,
    _map_capability,
    build_business_risk_report,
)


def _report(**overrides) -> ProjectIncidentInvestigationReport:
    base = ProjectIncidentInvestigationReport(
        id="rep-br-1",
        created_at="2026-06-10T08:00:00+00:00",
        project_id="demo",
        description="Business risk fixture",
        contract_risk_assessment=ContractRiskReport(
            assessments=[
                ContractRiskAssessment(
                    assessment_id="cr-1",
                    contract_id="payments_api",
                    overall_risk_level="CRITICAL",
                    risk_score=95,
                    confidence=0.9,
                    affected_modules=["Payments"],
                ),
            ],
            confidence=0.9,
        ),
        data_journey_validation=DataJourneyReport(
            journeys=[
                DataJourney(
                    journey_id="checkout",
                    name="Checkout",
                    business_area="Checkout",
                    stages=[],
                ),
            ],
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
        jira_issue_intelligence=JiraIssueIntelligenceReport(
            connected=True,
            blocker_count=2,
            top_blockers=[
                JiraIssueCorrelation(
                    issue_key="PAY-451",
                    issue_type="Bug",
                    status="Open",
                    priority="Blocker",
                    summary="Payments timeout",
                    related_module="Payments",
                    is_blocker=True,
                ),
            ],
            correlated_issues=2,
            summary="blockers",
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


class TestCapabilityMapping:
    def test_maps_payments(self):
        assert _map_capability("payments_api") == "Revenue Collection"

    def test_maps_checkout(self):
        assert _map_capability("Checkout journey") == "Customer Purchase Flow"

    def test_maps_authentication(self):
        assert _map_capability("Authentication service") == "Customer Access"


class TestConfidenceRules:
    def test_low_confidence(self):
        assert _confidence_from_evidence(1) == "LOW"

    def test_medium_confidence(self):
        assert _confidence_from_evidence(2) == "MEDIUM"
        assert _confidence_from_evidence(3) == "MEDIUM"

    def test_high_confidence(self):
        assert _confidence_from_evidence(4) == "HIGH"


@patch("services.business_risk_estimation_service._load_incident_reports", return_value=[])
def test_empty_intelligence(_mock_load):
    report = build_business_risk_report("demo")
    assert report.has_intelligence is False
    assert "Insufficient intelligence" in report.executive_summary
    assert report.business_risks == []


@patch("services.business_risk_estimation_service._load_incident_reports")
def test_critical_broken_journey_and_contract(mock_load):
    mock_load.return_value = [_report()]
    report = build_business_risk_report("demo")
    assert report.has_intelligence is True
    checkout = next((r for r in report.business_risks if r.capability == "Customer Purchase Flow"), None)
    assert checkout is not None
    assert checkout.severity == "CRITICAL"
    assert checkout.confidence in ("MEDIUM", "HIGH")
    assert any("broken data journeys" in e for e in checkout.evidence)
    assert any("critical contract" in e for e in checkout.evidence)


@patch("services.business_risk_estimation_service._load_incident_reports")
def test_high_blocked_release_with_jira(mock_load):
    mock_load.return_value = [_report()]
    report = build_business_risk_report("demo")
    revenue = next((r for r in report.business_risks if r.capability == "Revenue Collection"), None)
    assert revenue is not None
    assert revenue.severity in ("HIGH", "CRITICAL", "MEDIUM")
    assert "Revenue Collection" in report.top_capabilities_at_risk
    assert report.overall_business_risk in ("HIGH", "CRITICAL")
    assert "elevated risk" in report.executive_summary.lower()


@patch("services.business_risk_estimation_service._load_incident_reports")
def test_medium_degraded_environment_without_jira(mock_load):
    mock_load.return_value = [
        _report(
            jira_issue_intelligence=None,
            release_readiness=ReleaseReadinessView(
                project_id="demo",
                generated_at="2026-06-10T08:00:00+00:00",
                overall_status="CAUTION",
                summary="Caution.",
            ),
            contract_risk_assessment=None,
            data_journey_validation=None,
        ),
    ]
    report = build_business_risk_report("demo")
    platform = next((r for r in report.business_risks if r.capability == "Platform Operations"), None)
    assert platform is not None
    assert platform.severity == "MEDIUM"
    assert any("degraded environments" in e for e in platform.evidence)


@patch("services.business_risk_estimation_service._load_incident_reports")
def test_executive_summary_and_signals(mock_load):
    mock_load.return_value = [_report()]
    report = build_business_risk_report("demo")
    assert report.executive_summary
    assert report.signals
    assert all(s.impacted_capability for s in report.signals)
