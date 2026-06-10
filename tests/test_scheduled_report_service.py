# tests/test_scheduled_report_service.py
"""ENT-02B — Scheduled Executive Reports (read-only foundation)."""
from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import MagicMock, patch

import pytest

from services.scheduled_report_service import (
    build_executive_report_center,
    build_executive_report_preview,
)


def _incident_report(**overrides):
    base = {
        "id": "rep-1",
        "project_id": "demo",
        "description": "Scheduled report preview fixture",
        "created_at": "2026-06-04T08:00:00+00:00",
        "quality_health": {
            "overall_score": 82,
            "overall_status": "GOOD",
            "confidence": 0.8,
            "trend": "DEGRADING",
            "summary": "Quality health is degrading across payments.",
            "scores": [],
        },
        "quality_trends": {
            "overall_trend": "DEGRADING",
            "confidence": 0.82,
            "summary": "Overall quality trend is DEGRADING.",
            "trends": [],
        },
        "executive_quality_report": {
            "report_id": "eqr-1",
            "generated_at": "2026-06-04T08:00:00+00:00",
            "overall_quality_score": 82,
            "overall_risk_level": "HIGH",
            "confidence": 0.8,
            "executive_summary": "Payments and checkout require executive attention.",
            "top_risks": ["Checkout latency spike"],
            "top_recommendations": ["Run Payments Regression Suite", "Validate Orders Database"],
            "open_incident_count": 2,
            "critical_contract_count": 1,
            "broken_journey_count": 1,
            "quality_trend": "degrading",
        },
        "contract_risk_assessment": {
            "assessments": [
                {
                    "assessment_id": "a1",
                    "contract_id": "payments",
                    "overall_risk_level": "CRITICAL",
                    "risk_score": 92,
                    "confidence": 0.9,
                    "summary": "",
                    "factors": [],
                }
            ],
            "summary": "",
            "confidence": 0.9,
        },
        "data_journey_validation": {
            "journeys": [{"journey_id": "checkout", "name": "Checkout", "stages": []}],
            "results": [{"journey_id": "checkout", "status": "BROKEN", "summary": ""}],
            "summary": "",
            "confidence": 0.8,
        },
        "deployment_risk_assessment": {
            "risk_score": 78,
            "risk_level": "high",
            "confidence": 0.75,
            "summary": "Deployment risk elevated.",
            "contributing_factors": [],
        },
        "decision_center": {
            "overall_status": "ORANGE",
            "executive_summary": "Decision center recommends validation before release.",
            "confidence": 0.8,
            "top_risk_level": "HIGH",
            "top_risk_score": 78,
            "key_takeaways": [
                {"title": "Validate Orders Database", "description": "", "priority": 1},
            ],
        },
        "historical_learning": {
            "similar_incidents": [{"incident_id": "inc-1", "title": "Payments", "similarity_score": 0.8, "summary": ""}],
            "pattern_summary": "",
            "confidence": 0.7,
        },
    }
    base.update(overrides)
    return base


def test_empty_center_for_blank_project_id():
    assert build_executive_report_center("") is None
    assert build_executive_report_center("   ") is None


@patch("services.db.project_repository.project_repo")
def test_empty_center_for_missing_project(mock_project_repo):
    mock_project_repo.get_project.return_value = None
    center = build_executive_report_center("demo")
    assert center is not None
    assert center.schedules == []
    assert center.latest_preview is None
    assert center.total_schedules == 0


@patch("services.db.incident_report_repository.incident_report_repo")
@patch("services.db.project_repository.project_repo")
def test_default_schedules(mock_project_repo, mock_report_repo):
    mock_project_repo.get_project.return_value = SimpleNamespace(id="demo")
    mock_report_repo.list_reports.return_value = []
    mock_report_repo.get.return_value = None

    center = build_executive_report_center("demo")
    assert center is not None
    assert center.total_schedules == 4
    assert len(center.schedules) == 4
    assert center.latest_preview is None

    types = {s.report_type for s in center.schedules}
    assert types == {"QUALITY_BRIEF", "EXECUTIVE_SUMMARY", "RELEASE_READINESS", "INCIDENT_REVIEW"}

    quality_brief = next(s for s in center.schedules if s.report_type == "QUALITY_BRIEF")
    assert quality_brief.frequency == "WEEKLY"
    assert quality_brief.enabled is True
    assert quality_brief.recipients_count == 3
    assert quality_brief.next_run_preview == "Monday 8:00 AM"


@patch("services.db.incident_report_repository.incident_report_repo")
def test_preview_generation(mock_report_repo):
    mock_report_repo.list_reports.return_value = [{"id": "rep-1"}]
    mock_report_repo.get.return_value = _incident_report()

    preview = build_executive_report_preview("demo", _incident_report())
    assert preview.preview_id == "exec_preview:demo:quality_brief"
    assert preview.title == "Weekly Quality Brief"
    assert preview.quality_score == 82
    assert preview.quality_trend == "DEGRADING"
    assert preview.risk_level == "HIGH"
    assert preview.incident_count == 1
    assert preview.critical_contract_count == 1
    assert preview.broken_journey_count == 1
    assert any("Payments contract risk is CRITICAL" in r for r in preview.top_risks)
    assert any("Checkout journey is BROKEN" in r for r in preview.top_risks)
    assert "Run Payments Regression Suite" in preview.top_recommendations


@patch("services.db.incident_report_repository.incident_report_repo")
def test_deterministic_output(mock_report_repo):
    report = _incident_report()
    first = build_executive_report_preview("demo", report)
    second = build_executive_report_preview("demo", report)
    assert first.model_dump() == second.model_dump()


@patch("services.db.incident_report_repository.incident_report_repo")
def test_quality_score_integration(mock_report_repo):
    report = _incident_report(
        quality_health={"overall_score": 91, "trend": "IMPROVING", "summary": "", "scores": []},
        executive_quality_report=None,
        quality_trends=None,
    )
    preview = build_executive_report_preview("demo", report)
    assert preview.quality_score == 91
    assert preview.quality_trend == "IMPROVING"


@patch("services.db.incident_report_repository.incident_report_repo")
def test_trend_integration(mock_report_repo):
    report = _incident_report(
        quality_health={"overall_score": 80, "trend": "STABLE", "summary": "", "scores": []},
        quality_trends={"overall_trend": "DEGRADING", "confidence": 0.7, "trends": []},
    )
    preview = build_executive_report_preview("demo", report)
    assert preview.quality_trend == "DEGRADING"


@patch("services.release_readiness_service.build_release_readiness_executive_preview")
@patch("services.release_readiness_service.build_release_readiness_view")
def test_release_readiness_preview_uses_compositor(mock_view, mock_preview):
    from models.scheduled_report_models import ExecutiveReportPreview

    incident = _incident_report()
    mock_view.return_value = SimpleNamespace(
        generated_at="2026-06-04T08:00:00+00:00",
        summary="composed",
    )
    mock_preview.return_value = ExecutiveReportPreview(
        preview_id="exec_preview:demo:release_readiness",
        title="Release Readiness",
        generated_at="2026-06-04T08:00:00+00:00",
        quality_score=82,
        quality_trend="DEGRADING",
        risk_level="HIGH",
        executive_summary="composed",
    )

    preview = build_executive_report_preview("demo", incident, report_type="RELEASE_READINESS")
    mock_view.assert_called_once()
    mock_preview.assert_called_once()
    assert preview.title == "Release Readiness"
    assert preview.preview_id == "exec_preview:demo:release_readiness"


@patch("services.db.incident_report_repository.incident_report_repo")
@patch("services.db.project_repository.project_repo")
def test_no_external_calls(mock_project_repo, mock_report_repo):
    mock_project_repo.get_project.return_value = SimpleNamespace(id="demo")
    mock_report_repo.list_reports.return_value = [{"id": "rep-1"}]
    mock_report_repo.get.return_value = _incident_report()

    with patch("urllib.request.urlopen", side_effect=AssertionError("external HTTP")):
        with patch("requests.request", side_effect=AssertionError("external HTTP")):
            center = build_executive_report_center("demo")

    assert center is not None
    assert center.latest_preview is not None
    assert center.latest_preview.quality_score == 82
