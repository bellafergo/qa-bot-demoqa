# tests/test_qmetry_coverage_intelligence_service.py
"""QMETRY-01B — deterministic coverage intelligence tests."""
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import patch

import pytest

from models.business_risk_models import BusinessRiskAssessment, BusinessRiskReport
from models.jira_issue_intelligence_models import JiraIssueCorrelation, JiraIssueIntelligenceReport
from models.qmetry_coverage_models import CoverageIntelligenceReport
from models.release_readiness_models import ReleaseReadinessView
from models.qmetry_models import QMetryTestCase, QMetryTestCasesResponse
from services import qmetry_coverage_intelligence_service as svc
from services.qmetry_integration_service import QMetryConnectionStatus


def _connected_status() -> QMetryConnectionStatus:
    return QMetryConnectionStatus(
        connected=True,
        base_url="https://jira.example.com",
        project_count=1,
        test_case_count=5,
        run_count=2,
        last_sync=datetime.now(timezone.utc),
    )


def _sample_test_cases() -> QMetryTestCasesResponse:
    return QMetryTestCasesResponse(
        test_cases=[
            QMetryTestCase(test_case_id="1", name="Checkout happy path", status="Approved"),
            QMetryTestCase(test_case_id="2", name="Payment gateway timeout", status="Approved"),
            QMetryTestCase(test_case_id="3", name="Login with MFA", status="Approved"),
            QMetryTestCase(test_case_id="4", name="Order cancellation flow", status="Approved"),
            QMetryTestCase(test_case_id="5", name="Inventory stock adjustment", status="Approved"),
            QMetryTestCase(test_case_id="6", name="Candidate profile update", status="Approved"),
            QMetryTestCase(test_case_id="7", name="Generic smoke test", status="Approved"),
        ],
        total=7,
    )


class TestModuleMatching:
    def test_match_checkout_module(self):
        hit = svc._match_test_case(QMetryTestCase(test_case_id="1", name="Checkout happy path"))
        assert hit is not None
        assert hit[0] == "Checkout"
        assert hit[1] == "Customer Purchase Flow"

    def test_match_payment_module(self):
        hit = svc._match_test_case(QMetryTestCase(test_case_id="2", name="Payment gateway timeout"))
        assert hit is not None
        assert hit[1] == "Revenue Collection"

    def test_match_authentication_module(self):
        hit = svc._match_test_case(QMetryTestCase(test_case_id="3", name="Login with MFA"))
        assert hit is not None
        assert hit[1] == "Customer Access"

    def test_no_match_returns_none(self):
        assert svc._match_test_case(QMetryTestCase(test_case_id="9", name="Generic smoke test")) is None


class TestCoverageScoring:
    def test_coverage_status_thresholds(self):
        assert svc._coverage_status(0) == "NONE"
        assert svc._coverage_status(1) == "WEAK"
        assert svc._coverage_status(3) == "MODERATE"
        assert svc._coverage_status(5) == "STRONG"


class TestCoverageReport:
    def test_empty_qmetry_connection(self):
        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=QMetryConnectionStatus(connected=False),
        ):
            report = svc.build_coverage_intelligence_report()

        assert report.connected is False
        assert "No QMetry connection configured" in report.data_gaps[0]

    def test_no_test_cases(self):
        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.qmetry_coverage_intelligence_service.list_test_cases",
            return_value=QMetryTestCasesResponse(),
        ):
            report = svc.build_coverage_intelligence_report()

        assert report.connected is True
        assert report.total_test_cases == 0
        assert "No QMetry test cases discovered" in report.data_gaps[0]

    def test_module_matching_and_capability_mapping(self):
        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.qmetry_coverage_intelligence_service.list_test_cases",
            return_value=_sample_test_cases(),
        ), patch(
            "services.qmetry_coverage_intelligence_service._business_risk_capabilities",
            return_value=set(),
        ):
            report = svc.build_coverage_intelligence_report()

        assert report.total_matches == 6
        checkout = next(a for a in report.coverage_assessments if a.capability == "Customer Purchase Flow")
        assert checkout.matched_tests == 1
        assert checkout.coverage_status == "WEAK"
        revenue = next(a for a in report.coverage_assessments if a.capability == "Revenue Collection")
        assert revenue.matched_tests == 1

    def test_gap_detection_medium_by_default(self):
        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.qmetry_coverage_intelligence_service.list_test_cases",
            return_value=QMetryTestCasesResponse(
                test_cases=[QMetryTestCase(test_case_id="1", name="Checkout flow")],
                total=1,
            ),
        ), patch(
            "services.qmetry_coverage_intelligence_service._business_risk_capabilities",
            return_value=set(),
        ):
            report = svc.build_coverage_intelligence_report()

        recruiting_gap = next(g for g in report.coverage_gaps if g.capability == "Recruiting Operations")
        assert recruiting_gap.severity == "MEDIUM"

    def test_gap_detection_critical_with_jira_blocker(self):
        incident = type("Report", (), {})()
        incident.jira_issue_intelligence = JiraIssueIntelligenceReport(
            connected=True,
            blocker_count=1,
            top_blockers=[
                JiraIssueCorrelation(
                    issue_key="QA-1",
                    issue_type="Bug",
                    status="Open",
                    summary="Payments API outage",
                    correlation_score=80,
                    correlation_reason="module match",
                    related_module="payments",
                    is_blocker=True,
                )
            ],
        )
        incident.release_readiness = None

        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.qmetry_coverage_intelligence_service.list_test_cases",
            return_value=QMetryTestCasesResponse(
                test_cases=[QMetryTestCase(test_case_id="1", name="Checkout flow")],
                total=1,
            ),
        ), patch(
            "services.qmetry_coverage_intelligence_service._business_risk_capabilities",
            return_value=set(),
        ):
            report = svc.build_coverage_intelligence_report(incident_report=incident)

        revenue_gap = next(g for g in report.coverage_gaps if g.capability == "Revenue Collection")
        assert revenue_gap.severity == "CRITICAL"

    def test_blocked_release_does_not_pressure_unrelated_capabilities(self):
        incident = type("Report", (), {})()
        incident.jira_issue_intelligence = None
        incident.release_readiness = ReleaseReadinessView(
            project_id="demo",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_status="BLOCKED",
            summary="Release blocked with no mapped top risks.",
        )

        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.qmetry_coverage_intelligence_service.list_test_cases",
            return_value=QMetryTestCasesResponse(
                test_cases=[QMetryTestCase(test_case_id="1", name="Checkout flow")],
                total=1,
            ),
        ), patch(
            "services.qmetry_coverage_intelligence_service._business_risk_capabilities",
            return_value=set(),
        ), patch(
            "services.release_readiness_service.top_risks_from_view",
            return_value=[],
        ):
            report = svc.build_coverage_intelligence_report(incident_report=incident)

        recruiting_gap = next(g for g in report.coverage_gaps if g.capability == "Recruiting Operations")
        assert recruiting_gap.severity == "MEDIUM"

    def test_prefers_attached_business_risk_report(self):
        attached = BusinessRiskReport(
            generated_at="2026-06-10T08:00:00+00:00",
            has_intelligence=True,
            business_risks=[
                BusinessRiskAssessment(
                    risk_id="risk:revenue",
                    capability="Revenue Collection",
                    severity="HIGH",
                    confidence="MEDIUM",
                    summary="Revenue at risk",
                ),
            ],
            top_capabilities_at_risk=["Revenue Collection"],
        )

        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.qmetry_coverage_intelligence_service.list_test_cases",
            return_value=QMetryTestCasesResponse(
                test_cases=[QMetryTestCase(test_case_id="1", name="Checkout flow")],
                total=1,
            ),
        ), patch(
            "services.business_risk_estimation_service.build_business_risk_report",
        ) as mock_build:
            report = svc.build_coverage_intelligence_report(
                project_id="demo",
                business_risk_report=attached,
            )

        mock_build.assert_not_called()
        revenue_gap = next(g for g in report.coverage_gaps if g.capability == "Revenue Collection")
        assert revenue_gap.severity == "CRITICAL"

    def test_executive_summary_template(self):
        with patch(
            "services.qmetry_coverage_intelligence_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.qmetry_coverage_intelligence_service.list_test_cases",
            return_value=QMetryTestCasesResponse(
                test_cases=[QMetryTestCase(test_case_id="1", name="Payment capture")],
                total=1,
            ),
        ), patch(
            "services.qmetry_coverage_intelligence_service._business_risk_capabilities",
            return_value=set(),
        ):
            report = svc.build_coverage_intelligence_report()

        assert "Revenue Collection" in report.executive_summary or "weak" in report.executive_summary.lower()
        assert report.executive_summary.endswith(".")


class TestCoverageApiRoute:
    def _client(self):
        from fastapi.testclient import TestClient
        from app import app

        return TestClient(app)

    def test_coverage_route(self):
        fake = CoverageIntelligenceReport(
            generated_at=datetime.now(timezone.utc),
            connected=True,
            total_test_cases=3,
            total_matches=2,
            executive_summary="Customer Purchase Flow has weak test coverage (1 matching test case).",
        )
        with patch(
            "api.routes.qmetry_integration_routes.build_coverage_intelligence_report",
            return_value=fake,
        ):
            res = self._client().get("/integrations/qmetry/coverage")
        assert res.status_code == 200
        data = res.json()
        assert data["connected"] is True
        assert data["total_matches"] == 2
