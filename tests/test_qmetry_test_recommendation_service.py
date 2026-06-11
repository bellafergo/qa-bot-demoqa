# tests/test_qmetry_test_recommendation_service.py
"""QMETRY-01C — deterministic test recommendation correlation tests."""
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import patch

import pytest

from models.business_risk_models import BusinessRiskAssessment, BusinessRiskReport
from models.jira_issue_intelligence_models import JiraIssueCorrelation, JiraIssueIntelligenceReport
from models.qmetry_coverage_models import (
    CoverageAssessment,
    CoverageGap,
    CoverageIntelligenceReport,
    CoverageMatch,
)
from models.release_readiness_models import ReleaseReadinessView
from services import qmetry_test_recommendation_service as svc
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


def _sample_coverage() -> CoverageIntelligenceReport:
    return CoverageIntelligenceReport(
        generated_at=datetime.now(timezone.utc),
        connected=True,
        total_test_cases=5,
        total_matches=4,
        coverage_matches=[
            CoverageMatch(
                test_case_id="1",
                test_case_name="Payment gateway timeout",
                matched_module="Payment",
                matched_capability="Revenue Collection",
                match_reason='Test case name contains "Payment"',
            ),
            CoverageMatch(
                test_case_id="2",
                test_case_name="Checkout happy path",
                matched_module="Checkout",
                matched_capability="Customer Purchase Flow",
                match_reason='Test case name contains "Checkout"',
            ),
            CoverageMatch(
                test_case_id="3",
                test_case_name="Login with MFA",
                matched_module="Login",
                matched_capability="Customer Access",
                match_reason='Test case name contains "Login"',
            ),
            CoverageMatch(
                test_case_id="4",
                test_case_name="Order cancellation flow",
                matched_module="Order",
                matched_capability="Order Processing",
                match_reason='Test case name contains "Order"',
            ),
        ],
        coverage_assessments=[
            CoverageAssessment(capability="Revenue Collection", matched_tests=1, total_tests=5, coverage_status="WEAK"),
            CoverageAssessment(capability="Customer Purchase Flow", matched_tests=1, total_tests=5, coverage_status="WEAK"),
            CoverageAssessment(capability="Customer Access", matched_tests=1, total_tests=5, coverage_status="WEAK"),
            CoverageAssessment(capability="Order Processing", matched_tests=1, total_tests=5, coverage_status="WEAK"),
            CoverageAssessment(capability="Recruiting Operations", matched_tests=0, total_tests=5, coverage_status="NONE"),
        ],
        coverage_gaps=[
            CoverageGap(
                capability="Recruiting Operations",
                module="Candidate",
                severity="MEDIUM",
                reason="No tests",
            ),
        ],
        executive_summary="Sample coverage",
    )


class TestRecommendationPriority:
    def test_critical_from_jira_blocker(self):
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

        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ):
            report = svc.build_qmetry_recommendation_report(
                coverage_intelligence=_sample_coverage(),
                incident_report=incident,
            )

        revenue = next(
            t
            for g in report.recommendation_groups
            for t in g.recommended_tests
            if t.test_case_id == "1"
        )
        assert revenue.priority == "CRITICAL"
        assert "Jira blocker" in revenue.recommendation_reason

    def test_high_from_business_risk(self):
        business_risk = BusinessRiskReport(
            generated_at="2026-06-10T08:00:00+00:00",
            has_intelligence=True,
            business_risks=[
                BusinessRiskAssessment(
                    risk_id="risk:checkout",
                    capability="Customer Purchase Flow",
                    severity="HIGH",
                    confidence="MEDIUM",
                    summary="Checkout instability",
                ),
            ],
            top_capabilities_at_risk=["Customer Purchase Flow"],
        )

        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ):
            report = svc.build_qmetry_recommendation_report(
                coverage_intelligence=_sample_coverage(),
                business_risk_report=business_risk,
            )

        checkout = next(
            t
            for g in report.recommendation_groups
            for t in g.recommended_tests
            if t.test_case_id == "2"
        )
        assert checkout.priority == "HIGH"

    def test_high_from_coverage_gap(self):
        coverage = _sample_coverage()
        coverage.coverage_gaps.append(
            CoverageGap(
                capability="Order Processing",
                module="Order",
                severity="MEDIUM",
                reason="Weak coverage under release pressure",
            )
        )

        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ):
            report = svc.build_qmetry_recommendation_report(coverage_intelligence=coverage)

        order = next(
            t
            for g in report.recommendation_groups
            for t in g.recommended_tests
            if t.test_case_id == "4"
        )
        assert order.priority == "HIGH"

    def test_medium_from_coverage_assessment_only(self):
        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ):
            report = svc.build_qmetry_recommendation_report(coverage_intelligence=_sample_coverage())

        access = next(
            t
            for g in report.recommendation_groups
            for t in g.recommended_tests
            if t.test_case_id == "3"
        )
        assert access.priority == "MEDIUM"


class TestRecommendationGrouping:
    def test_groups_by_capability(self):
        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ):
            report = svc.build_qmetry_recommendation_report(coverage_intelligence=_sample_coverage())

        capabilities = [g.capability for g in report.recommendation_groups]
        assert "Revenue Collection" in capabilities
        assert "Customer Access" in capabilities
        assert all(len(g.recommended_tests) >= 1 for g in report.recommendation_groups)

    def test_executive_summary_mentions_count(self):
        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ):
            report = svc.build_qmetry_recommendation_report(coverage_intelligence=_sample_coverage())

        assert report.total_recommendations == 4
        assert "recommended for review" in report.executive_summary.lower()


class TestEmptyStates:
    def test_empty_qmetry_connection(self):
        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=QMetryConnectionStatus(connected=False),
        ):
            report = svc.build_qmetry_recommendation_report()

        assert report.connected is False
        assert report.total_recommendations == 0
        assert "not connected" in report.executive_summary.lower()

    def test_no_coverage_matches(self):
        coverage = CoverageIntelligenceReport(
            generated_at=datetime.now(timezone.utc),
            connected=True,
            total_test_cases=3,
            total_matches=0,
            coverage_matches=[],
        )
        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ):
            report = svc.build_qmetry_recommendation_report(coverage_intelligence=coverage)

        assert report.total_recommendations == 0
        assert any("coverage intelligence" in gap.lower() for gap in report.data_gaps)

    def test_blocked_release_critical_capability(self):
        incident = type("Report", (), {})()
        incident.jira_issue_intelligence = None
        incident.release_readiness = ReleaseReadinessView(
            project_id="demo",
            generated_at="2026-06-10T08:00:00+00:00",
            overall_status="BLOCKED",
            summary="Blocked release",
        )

        with patch(
            "services.qmetry_test_recommendation_service.validate_qmetry_connection",
            return_value=_connected_status(),
        ), patch(
            "services.release_readiness_service.top_risks_from_view",
            return_value=["Payment processing regression"],
        ):
            report = svc.build_qmetry_recommendation_report(
                coverage_intelligence=_sample_coverage(),
                incident_report=incident,
                release_readiness=incident.release_readiness,
            )

        revenue = next(
            t
            for g in report.recommendation_groups
            for t in g.recommended_tests
            if t.test_case_id == "1"
        )
        assert revenue.priority == "CRITICAL"
