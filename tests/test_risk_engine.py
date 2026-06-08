# tests/test_risk_engine.py
"""Risk Engine v1 — unit tests."""
from __future__ import annotations

from datetime import datetime, timedelta, timezone
from unittest.mock import MagicMock, patch

import pytest

from models.project_knowledge_models import (
    KnowledgeFailureEntry,
    KnowledgeIncidentEntry,
    KnowledgeModule,
    KnowledgeRelatedTest,
    ProjectKnowledge,
)
from services.project_risk_service import apply_risk_to_knowledge, assess_project_risk
from services.risk_engine_service import (
    compute_module_risk,
    compute_project_risk,
    is_recent,
    risk_level_from_score,
)


def _recent_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _old_iso() -> str:
    return (datetime.now(timezone.utc) - timedelta(days=30)).isoformat()


def test_risk_level_thresholds():
    assert risk_level_from_score(10) == "LOW"
    assert risk_level_from_score(30) == "MEDIUM"
    assert risk_level_from_score(60) == "HIGH"
    assert risk_level_from_score(80) == "CRITICAL"


def test_is_recent_within_window():
    assert is_recent(_recent_iso()) is True
    assert is_recent(_old_iso()) is False


def test_compute_module_risk_high_with_regressions_and_incidents():
    mod = compute_module_risk(
        "Candidates",
        regression_count=2,
        failure_count=3,
        flaky_count=1,
        incident_count=2,
        incident_severity_sum=8.0,
        pass_rate=33.0,
        test_count=2,
        recent_failure_count=2,
    )
    assert mod.module_risk_score >= 50
    assert mod.module_risk_level in ("HIGH", "CRITICAL")
    assert mod.regression_count == 2
    assert len(mod.factors) >= 3


def test_compute_project_risk_explanation_and_recommendations():
    assessment = compute_project_risk(
        "proj-1",
        pass_rate=67.0,
        run_fail_rate=0.33,
        regressions=[
            {"test_case_id": "TC-14", "module": "Candidates"},
            {"test_case_id": "TC-22", "module": "Vacancies"},
        ],
        flaky_tests=[
            {"test_case_id": "TC-31", "suspected_flaky": True},
            {"test_case_id": "TC-99", "suspected_flaky": False},
        ],
        failure_history=[
            {"test_case_id": "TC-14", "module": "Candidates", "count": 2, "last_failed_at": _recent_iso()},
        ],
        incidents=[
            {"id": "1", "severity": "high", "created_at": _recent_iso(), "description": "Candidates broken"},
            {"id": "2", "severity": "medium", "created_at": _recent_iso(), "description": "Vacancies slow"},
            {"id": "3", "severity": "low", "created_at": _old_iso(), "description": "old"},
            {"id": "4", "severity": "critical", "created_at": _recent_iso(), "description": "checkout"},
        ],
        module_stats={
            "Candidates": {
                "test_count": 5,
                "regression_count": 1,
                "failure_count": 2,
                "flaky_count": 1,
                "incident_count": 2,
                "incident_severity_sum": 10.0,
                "pass_rate": 60.0,
                "recent_failure_count": 1,
            },
            "Vacancies": {
                "test_count": 3,
                "regression_count": 1,
                "failure_count": 0,
                "flaky_count": 0,
                "incident_count": 1,
                "incident_severity_sum": 2.5,
                "pass_rate": 70.0,
            },
            "Companies": {"test_count": 10, "pass_rate": 95.0},
        },
        related_tests=[
            {"test_case_id": "TC-14", "name": "Login Candidates", "module": "Candidates", "priority": "high", "last_run_status": "fail"},
            {"test_case_id": "TC-22", "name": "Vacancy list", "module": "Vacancies", "priority": "medium", "last_run_status": "pass"},
            {"test_case_id": "TC-31", "name": "Candidate edit", "module": "Candidates", "priority": "critical", "last_run_status": "fail"},
        ],
    )

    assert assessment.risk_score >= 40
    assert assessment.risk_level in ("MEDIUM", "HIGH", "CRITICAL")
    assert assessment.regression_count == 2
    assert assessment.flaky_count == 1
    assert assessment.recent_incident_count >= 3
    assert any("67%" in line or "success" in line.lower() for line in assessment.explanation)
    assert assessment.module_risks[0].module == "Candidates"
    rec_ids = [r.test_case_id for r in assessment.recommended_tests]
    assert "TC-14" in rec_ids or "TC-31" in rec_ids


def test_apply_risk_to_knowledge_enriches_document():
    knowledge = ProjectKnowledge(
        project_id="test-proj",
        modules=[KnowledgeModule(name="Candidates", test_count=3)],
        failure_history=[
            KnowledgeFailureEntry(test_case_id="TC-1", module="Candidates", count=2, last_failed_at=_recent_iso()),
        ],
        incident_history=[
            KnowledgeIncidentEntry(id="inc-1", severity="high", description="Candidates issue", created_at=_recent_iso()),
        ],
        related_tests=[
            KnowledgeRelatedTest(test_case_id="TC-14", name="TC-14", module="Candidates", priority="high", last_run_status="fail"),
        ],
        metadata={"run_fail_rate": 0.4},
    )

    mock_assessment = MagicMock()
    mock_assessment.risk_score = 72.0
    mock_assessment.risk_level = "HIGH"
    mock_assessment.explanation = ["72/100 overall"]
    mock_assessment.module_risks = []
    mock_assessment.recommended_tests = []
    mock_assessment.engine_version = "v1"
    mock_assessment.factors = []
    mock_assessment.pass_rate = 60.0
    mock_assessment.regression_count = 1
    mock_assessment.flaky_count = 0
    mock_assessment.recent_incident_count = 1

    with patch("services.project_risk_service.assess_project_risk", return_value=mock_assessment):
        enriched = apply_risk_to_knowledge(knowledge)

    assert enriched.risk_score == 72.0
    assert enriched.risk_level == "HIGH"
    assert enriched.metadata.get("risk_engine_version") == "v1"


def test_assess_project_risk_integration(tmp_path, monkeypatch):
    monkeypatch.setenv("VANYA_DB_PATH", str(tmp_path / "risk.db"))
    from services.db.init_db import init_catalog_db

    init_catalog_db()

    with patch("services.failure_intelligence_service.failure_intelligence_service") as fi:
        fi.get_regressions.return_value = []
        fi.get_flaky_tests.return_value = []
        with patch("services.run_analytics_service.get_runs_dashboard", return_value={"summary": {"pass_rate": 90.0}}):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = [("TC-1", "Auth")]
                cat.list_test_cases.return_value = []
                with patch("services.run_history_service.run_history_service") as rh:
                    rh.list_runs.return_value = []
                    assessment = assess_project_risk("proj-x", knowledge=None)

    assert assessment.project_id == "proj-x"
    assert 0 <= assessment.risk_score <= 100
