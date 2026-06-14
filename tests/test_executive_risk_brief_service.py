# tests/test_executive_risk_brief_service.py
from __future__ import annotations

from unittest.mock import MagicMock, patch

import pytest

from models.failure_intelligence_models import BlastRadius, FailureCluster
from models.risk_engine_models import ModuleRisk, RecommendedTest, RiskAssessment
from services.executive_risk_brief_service import (
    build_executive_risk_brief,
    select_top_cluster,
)


def _cluster(
    *,
    cluster_id: str,
    module: str,
    total_failures: int,
    confidence: str,
    severity: str = "medium",
    category: str = "auth_issue",
    rep_tc: str = "TC-TOS-009",
) -> FailureCluster:
    return FailureCluster(
        cluster_id=cluster_id,
        root_cause_category=category,
        impacted_layer="ui",
        module=module,
        representative_test_case_id=rep_tc,
        run_ids=["r1"],
        total_failures=total_failures,
        common_signals=["auth_error"],
        probable_cause="Authentication check failed on login flow.",
        confidence=confidence,
        summary=f"{total_failures} auth failures in {module}",
        recommended_action="Run authentication smoke tests for the affected module.",
        blast_radius=BlastRadius(
            affected_modules=[module],
            affected_tests_count=total_failures,
            estimated_severity=severity,
            impact_scope="localized_regression",
        ),
    )


def test_select_top_cluster_prefers_occurrences_then_confidence_then_impact():
    low = _cluster(cluster_id="c1", module="AUTH", total_failures=1, confidence="high", severity="critical")
    winner = _cluster(cluster_id="c2", module="AUTH", total_failures=2, confidence="medium", severity="low")
    assert select_top_cluster([low, winner]).cluster_id == "c2"

    high_conf = _cluster(cluster_id="c3", module="DASH", total_failures=2, confidence="high", severity="low")
    med_conf = _cluster(cluster_id="c4", module="DASH", total_failures=2, confidence="medium", severity="critical")
    assert select_top_cluster([med_conf, high_conf]).cluster_id == "c3"


@patch("services.project_knowledge_service.get_project_knowledge")
@patch("services.project_risk_service.assess_project_risk")
@patch("services.failure_intelligence_service.failure_intelligence_service.get_clusters")
def test_build_executive_risk_brief_from_top_cluster(mock_get_clusters, mock_risk, mock_knowledge):
    mock_get_clusters.return_value = [
        _cluster(cluster_id="c-auth", module="AUTH", total_failures=2, confidence="high"),
    ]
    mock_risk.return_value = RiskAssessment(
        project_id="demo",
        risk_score=55.0,
        recommended_tests=[
            RecommendedTest(test_case_id="TC-TOS-009", module="AUTH", reason="regression"),
        ],
    )
    mock_knowledge.return_value = None

    brief = build_executive_risk_brief("demo")

    assert brief.has_risk is True
    assert brief.module == "AUTH"
    assert brief.confidence == "high"
    assert brief.representative_test_case_id == "TC-TOS-009"
    assert brief.cluster_id == "c-auth"
    assert any(e.kind == "occurrences" and e.count == 2 for e in brief.evidence)
    assert any(e.kind == "module" and e.module == "AUTH" for e in brief.evidence)
    assert "sign in" in brief.impact.lower() or "auth" in brief.impact.lower()
    assert "smoke" in brief.recommendation.lower()


@patch("services.project_knowledge_service.get_project_knowledge")
@patch("services.project_risk_service.assess_project_risk")
@patch("services.failure_intelligence_service.failure_intelligence_service.get_clusters")
def test_build_executive_risk_brief_fallback_module_risk(mock_get_clusters, mock_risk, mock_knowledge):
    mock_get_clusters.return_value = []
    mock_risk.return_value = RiskAssessment(
        project_id="demo",
        risk_score=72.0,
        module_risks=[
            ModuleRisk(module="AUTH", module_risk_score=72, module_risk_level="HIGH", regression_count=3),
        ],
    )
    mock_knowledge.return_value = None

    brief = build_executive_risk_brief("demo")
    assert brief.has_risk is True
    assert brief.module == "AUTH"
    assert brief.confidence in ("high", "medium")


def test_build_executive_risk_brief_requires_project_id():
    with pytest.raises(ValueError):
        build_executive_risk_brief("")
