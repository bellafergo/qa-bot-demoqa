# tests/test_quality_trend_service.py
"""OBS-01B — Quality Trends (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    ExecutiveQualityReport,
    ProjectIncidentInvestigationReport,
    QualityHealthReport,
    QualityHealthScore,
)
from services.quality_trend_service import build_quality_trend_report, build_trend_id


def _report(created_at: str, overall: int, scopes=None):
    scores = scopes or []
    return {
        "id": f"rep-{created_at}",
        "created_at": created_at,
        "quality_health": {
            "overall_score": overall,
            "overall_status": "GOOD",
            "confidence": 0.8,
            "trend": "STABLE",
            "summary": "",
            "scores": scores,
        },
    }


@patch("services.db.incident_report_repository.incident_report_repo")
def test_empty_history_returns_none(mock_repo):
    mock_repo.list_reports.return_value = []
    mock_repo.get.return_value = None
    assert build_quality_trend_report("demo") is None


@patch("services.db.incident_report_repository.incident_report_repo")
def test_improving_trend(mock_repo):
    mock_repo.list_reports.return_value = [
        {"id": "r1", "created_at": "2026-06-01T10:00:00+00:00"},
        {"id": "r2", "created_at": "2026-06-02T10:00:00+00:00"},
        {"id": "r3", "created_at": "2026-06-03T10:00:00+00:00"},
        {"id": "r4", "created_at": "2026-06-04T10:00:00+00:00"},
    ]

    def _get(rid):
        mapping = {
            "r1": _report("2026-06-01T10:00:00+00:00", 70),
            "r2": _report("2026-06-02T10:00:00+00:00", 75),
            "r3": _report("2026-06-03T10:00:00+00:00", 82),
            "r4": _report("2026-06-04T10:00:00+00:00", 88),
        }
        return mapping.get(rid)

    mock_repo.get.side_effect = _get
    report = build_quality_trend_report("demo")
    assert report is not None
    project = next(t for t in report.trends if t.scope_type == "project")
    assert project.trend_direction == "IMPROVING"
    assert project.score_change == 18


@patch("services.db.incident_report_repository.incident_report_repo")
def test_stable_trend(mock_repo):
    mock_repo.list_reports.return_value = [
        {"id": "r1", "created_at": "2026-06-01T10:00:00+00:00"},
        {"id": "r2", "created_at": "2026-06-02T10:00:00+00:00"},
        {"id": "r3", "created_at": "2026-06-03T10:00:00+00:00"},
        {"id": "r4", "created_at": "2026-06-04T10:00:00+00:00"},
    ]

    def _get(rid):
        mapping = {
            "r1": _report("2026-06-01T10:00:00+00:00", 85),
            "r2": _report("2026-06-02T10:00:00+00:00", 84),
            "r3": _report("2026-06-03T10:00:00+00:00", 86),
            "r4": _report("2026-06-04T10:00:00+00:00", 85),
        }
        return mapping.get(rid)

    mock_repo.get.side_effect = _get
    report = build_quality_trend_report("demo")
    project = next(t for t in report.trends if t.scope_type == "project")
    assert project.trend_direction == "STABLE"
    assert project.score_change == 0


@patch("services.db.incident_report_repository.incident_report_repo")
def test_degrading_trend(mock_repo):
    mock_repo.list_reports.return_value = [
        {"id": "r1", "created_at": "2026-06-01T10:00:00+00:00"},
        {"id": "r2", "created_at": "2026-06-02T10:00:00+00:00"},
        {"id": "r3", "created_at": "2026-06-03T10:00:00+00:00"},
        {"id": "r4", "created_at": "2026-06-04T10:00:00+00:00"},
    ]

    def _get(rid):
        mapping = {
            "r1": _report("2026-06-01T10:00:00+00:00", 91),
            "r2": _report("2026-06-02T10:00:00+00:00", 88),
            "r3": _report("2026-06-03T10:00:00+00:00", 84),
            "r4": _report("2026-06-04T10:00:00+00:00", 82),
        }
        return mapping.get(rid)

    mock_repo.get.side_effect = _get
    report = build_quality_trend_report("demo")
    assert report.overall_trend == "DEGRADING"
    project = next(t for t in report.trends if t.scope_type == "project")
    assert project.score_change == -9


@patch("services.db.incident_report_repository.incident_report_repo")
def test_confidence_calculation(mock_repo):
    mock_repo.list_reports.return_value = [
        {"id": "r1", "created_at": "2026-06-01T10:00:00+00:00"},
        {"id": "r2", "created_at": "2026-06-02T10:00:00+00:00"},
    ]
    mock_repo.get.side_effect = lambda rid: _report(
        "2026-06-01T10:00:00+00:00" if rid == "r1" else "2026-06-02T10:00:00+00:00",
        80 if rid == "r1" else 85,
    )
    report = build_quality_trend_report("demo")
    assert report.confidence > 0
    assert report.trends[0].confidence > 0


@patch("services.db.incident_report_repository.incident_report_repo")
def test_project_trend(mock_repo):
    mock_repo.list_reports.return_value = [{"id": "r1", "created_at": "2026-06-01T10:00:00+00:00"}, {"id": "r2", "created_at": "2026-06-02T10:00:00+00:00"}]
    mock_repo.get.side_effect = lambda rid: _report("2026-06-01T10:00:00+00:00" if rid == "r1" else "2026-06-02T10:00:00+00:00", 70 if rid == "r1" else 78)
    report = build_quality_trend_report("demo")
    assert any(t.scope_type == "project" for t in report.trends)


@patch("services.db.incident_report_repository.incident_report_repo")
def test_environment_trend(mock_repo):
    scopes = [
        {
            "score_id": "quality_health:environment:staging",
            "scope_type": "environment",
            "scope_name": "STAGING",
            "environment": "STAGING",
            "score": 60,
            "status": "ATTENTION",
            "confidence": 0.7,
            "trend": "DEGRADING",
            "contributing_factors": [],
        }
    ]
    mock_repo.list_reports.return_value = [{"id": "r1", "created_at": "2026-06-01T10:00:00+00:00"}, {"id": "r2", "created_at": "2026-06-02T10:00:00+00:00"}]

    def _get(rid):
        if rid == "r1":
            return _report("2026-06-01T10:00:00+00:00", 80, scopes)
        scopes2 = [{**scopes[0], "score": 52}]
        return _report("2026-06-02T10:00:00+00:00", 75, scopes2)

    mock_repo.get.side_effect = _get
    report = build_quality_trend_report("demo")
    env = next(t for t in report.trends if t.scope_type == "environment")
    assert env.scope_name == "STAGING"
    assert env.trend_direction == "DEGRADING"


@patch("services.db.incident_report_repository.incident_report_repo")
def test_deterministic_output(mock_repo):
    mock_repo.list_reports.return_value = [{"id": "r1", "created_at": "2026-06-01T10:00:00+00:00"}, {"id": "r2", "created_at": "2026-06-02T10:00:00+00:00"}]
    mock_repo.get.side_effect = lambda rid: _report("2026-06-01T10:00:00+00:00" if rid == "r1" else "2026-06-02T10:00:00+00:00", 70 if rid == "r1" else 75)
    first = build_quality_trend_report("demo")
    second = build_quality_trend_report("demo")
    assert first is not None and second is not None
    assert first.model_dump() == second.model_dump()


def test_no_external_calls():
    import services.quality_trend_service as mod

    source = open(mod.__file__, encoding="utf-8").read()
    assert "requests." not in source
    assert "httpx." not in source


def test_report_field_exists():
    report = ProjectIncidentInvestigationReport(project_id="demo", description="test")
    assert hasattr(report, "quality_trends")


def test_deterministic_ids():
    assert build_trend_id("project", "Project") == "quality_trend:project:project"


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


@patch("services.quality_trend_service.build_quality_trend_report")
def test_quality_trends_api(mock_build, client: TestClient):
    from models.incident_models import QualityTrend, QualityTrendPoint, QualityTrendReport

    mock_build.return_value = QualityTrendReport(
        overall_trend="STABLE",
        confidence=0.8,
        trends=[
            QualityTrend(
                trend_id="quality_trend:project:project",
                scope_type="project",
                scope_name="Project",
                trend_direction="STABLE",
                score_change=0,
                confidence=0.8,
                points=[
                    QualityTrendPoint(timestamp="2026-06-01T10:00:00+00:00", score=80, status="GOOD"),
                    QualityTrendPoint(timestamp="2026-06-02T10:00:00+00:00", score=82, status="GOOD"),
                ],
            )
        ],
        summary="Stable",
    )
    resp = client.get("/projects/demo/quality-trends")
    assert resp.status_code == 200
    assert resp.json()["overall_trend"] == "STABLE"
