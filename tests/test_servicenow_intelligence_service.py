# tests/test_servicenow_intelligence_service.py
"""SNOW-01B — ServiceNow intelligence service tests."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.business_risk_models import BusinessRiskAssessment, BusinessRiskReport
from models.incident_models import (
    BlastRadiusModule,
    DependencyNode,
    EnterpriseDependencyMap,
    ProjectIncidentInvestigationReport,
)
from models.jira_issue_intelligence_models import JiraIssueCorrelation, JiraIssueIntelligenceReport
from models.release_readiness_models import ReleaseReadinessView
from models.servicenow_models import (
    ServiceNowChange,
    ServiceNowChangesResponse,
    ServiceNowCI,
    ServiceNowCMDBResponse,
    ServiceNowConnectionStatus,
    ServiceNowIncident,
    ServiceNowIncidentsResponse,
    ServiceNowService,
    ServiceNowServicesResponse,
)
from services.servicenow_intelligence_service import (
    build_servicenow_intelligence_report,
    enrich_executive_quality_top_risks_with_servicenow,
    servicenow_executive_risk_lines,
)


def _connected():
    return ServiceNowConnectionStatus(connected=True, instance_url="https://acme.service-now.com")


def _business_risk():
    return BusinessRiskReport(
        generated_at="2026-06-10T12:00:00Z",
        business_risks=[
            BusinessRiskAssessment(
                risk_id="br-1",
                capability="Revenue Collection",
                severity="HIGH",
                summary="Payments risk",
            ),
        ],
        top_capabilities_at_risk=["Revenue Collection"],
        has_intelligence=True,
    )


def _jira_intel():
    return JiraIssueIntelligenceReport(
        connected=True,
        blocker_count=1,
        top_blockers=[
            JiraIssueCorrelation(
                issue_key="QA-1",
                issue_type="Bug",
                status="Open",
                summary="Payments checkout blocked",
                correlation_score=55,
                correlation_reason="module",
                related_module="Payments",
                is_blocker=True,
            ),
        ],
    )


class TestEmptyServiceNow:
    def test_empty_when_not_connected(self):
        with patch(
            "services.servicenow_intelligence_service.validate_servicenow_connection",
            return_value=ServiceNowConnectionStatus(connected=False),
        ):
            report = build_servicenow_intelligence_report()
        assert report.connected is False
        assert "No ServiceNow connection" in report.data_gaps[0]


class TestIncidentCorrelation:
    def test_incident_correlates_with_business_risk(self):
        with patch(
            "services.servicenow_intelligence_service.validate_servicenow_connection",
            return_value=_connected(),
        ), patch(
            "services.servicenow_intelligence_service.list_incidents",
            return_value=ServiceNowIncidentsResponse(
                incidents=[
                    ServiceNowIncident(
                        number="INC0001",
                        short_description="Payments outage in checkout",
                        state="In Progress",
                        priority="1",
                    ),
                ],
                total=1,
            ),
        ), patch(
            "services.servicenow_intelligence_service.list_changes",
            return_value=ServiceNowChangesResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_services",
            return_value=ServiceNowServicesResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_cmdb_items",
            return_value=ServiceNowCMDBResponse(),
        ):
            report = build_servicenow_intelligence_report(business_risk_report=_business_risk())

        assert len(report.incident_correlations) == 1
        assert report.incident_correlations[0].entity_id == "INC0001"
        assert report.incident_correlations[0].capability == "Revenue Collection"
        assert report.incident_correlations[0].confidence == "LOW"


class TestChangeCorrelation:
    def test_change_correlates_with_multiple_signals(self):
        jira = JiraIssueIntelligenceReport(
            connected=True,
            blocker_count=1,
            top_blockers=[
                JiraIssueCorrelation(
                    issue_key="QA-2",
                    issue_type="Bug",
                    status="Open",
                    summary="Authentication login blocked",
                    correlation_score=40,
                    correlation_reason="module",
                    related_module="Authentication",
                    is_blocker=True,
                ),
            ],
        )
        with patch(
            "services.servicenow_intelligence_service.validate_servicenow_connection",
            return_value=_connected(),
        ), patch(
            "services.servicenow_intelligence_service.list_incidents",
            return_value=ServiceNowIncidentsResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_changes",
            return_value=ServiceNowChangesResponse(
                changes=[
                    ServiceNowChange(
                        number="CHG0001",
                        short_description="Authentication service patch",
                        state="Scheduled",
                        risk="Moderate",
                    ),
                ],
                total=1,
            ),
        ), patch(
            "services.servicenow_intelligence_service.list_services",
            return_value=ServiceNowServicesResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_cmdb_items",
            return_value=ServiceNowCMDBResponse(),
        ):
            report = build_servicenow_intelligence_report(
                business_risk_report=BusinessRiskReport(
                    generated_at="2026-06-10T12:00:00Z",
                    business_risks=[
                        BusinessRiskAssessment(
                            risk_id="br-2",
                            capability="Customer Access",
                            severity="HIGH",
                            summary="Auth risk",
                        ),
                    ],
                    top_capabilities_at_risk=["Customer Access"],
                    has_intelligence=True,
                ),
                jira_intel=jira,
            )

        assert len(report.change_correlations) == 1
        assert report.change_correlations[0].capability == "Customer Access"
        assert report.change_correlations[0].confidence == "MEDIUM"


class TestServiceAndCmdbCorrelation:
    def test_service_correlation(self):
        with patch(
            "services.servicenow_intelligence_service.validate_servicenow_connection",
            return_value=_connected(),
        ), patch(
            "services.servicenow_intelligence_service.list_incidents",
            return_value=ServiceNowIncidentsResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_changes",
            return_value=ServiceNowChangesResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_services",
            return_value=ServiceNowServicesResponse(
                services=[ServiceNowService(name="Payments Gateway", business_criticality="1")],
                total=1,
            ),
        ), patch(
            "services.servicenow_intelligence_service.list_cmdb_items",
            return_value=ServiceNowCMDBResponse(),
        ):
            report = build_servicenow_intelligence_report(business_risk_report=_business_risk())

        assert len(report.service_correlations) == 1
        assert report.service_correlations[0].entity_id == "Payments Gateway"

    def test_cmdb_correlation_with_dependency_map(self):
        incident = ProjectIncidentInvestigationReport(
            project_id="demo",
            description="test",
            enterprise_dependency_map=EnterpriseDependencyMap(
                nodes=[
                    DependencyNode(
                        node_id="n1",
                        node_type="service",
                        name="Payments Gateway",
                        risk_level="CRITICAL",
                    ),
                ],
            ),
        )
        with patch(
            "services.servicenow_intelligence_service.validate_servicenow_connection",
            return_value=_connected(),
        ), patch(
            "services.servicenow_intelligence_service.list_incidents",
            return_value=ServiceNowIncidentsResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_changes",
            return_value=ServiceNowChangesResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_services",
            return_value=ServiceNowServicesResponse(),
        ), patch(
            "services.servicenow_intelligence_service.list_cmdb_items",
            return_value=ServiceNowCMDBResponse(
                items=[ServiceNowCI(name="payments-gateway-01", class_name="cmdb_ci_server")],
                total=1,
            ),
        ):
            report = build_servicenow_intelligence_report(
                incident_report=incident,
                business_risk_report=_business_risk(),
            )

        assert len(report.cmdb_correlations) == 1
        assert report.cmdb_correlations[0].entity_id == "payments-gateway-01"


class TestExecutiveSummary:
    def test_executive_summary_and_risk_lines(self):
        from models.servicenow_intelligence_models import ServiceNowCorrelation, ServiceNowIntelligenceReport

        report = ServiceNowIntelligenceReport(
            connected=True,
            incident_correlations=[
                ServiceNowCorrelation(
                    entity_type="incident",
                    entity_id="INC0001",
                    capability="Revenue Collection",
                    correlation_reason="test",
                    confidence="HIGH",
                ),
            ],
            top_operational_risks=[
                "ServiceNow Incident INC0001: Revenue Collection (HIGH confidence)",
            ],
            executive_summary="1 ServiceNow operational correlation(s) detected; 1 incident(s).",
        )
        lines = servicenow_executive_risk_lines(report)
        assert len(lines) == 1
        assert "INC0001" in lines[0]

    def test_enrich_executive_quality(self):
        from models.incident_models import ExecutiveQualityReport
        from models.servicenow_intelligence_models import ServiceNowIntelligenceReport

        eqr = ExecutiveQualityReport(
            report_id="eqr-1",
            generated_at="2026-06-10T08:00:00+00:00",
            executive_summary="Base",
            top_risks=["Existing risk"],
        )

        intel = ServiceNowIntelligenceReport(
            connected=True,
            top_operational_risks=["ServiceNow Incident INC0001: Revenue Collection (HIGH confidence)"],
        )
        enrich_executive_quality_top_risks_with_servicenow(eqr, intel)
        assert any("ServiceNow" in line for line in eqr.top_risks)
