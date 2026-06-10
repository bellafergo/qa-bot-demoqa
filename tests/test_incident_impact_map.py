# tests/test_incident_impact_map.py
"""II-03C — Impact Map (read-only)."""
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import (
    BlastRadiusModule,
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    ProjectIncidentInvestigationReport,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
)
from services.incident_impact_map_service import build_incident_impact_map


def _base(**overrides):
    base = {
        "hypotheses": [],
        "evidence_correlation": None,
        "related_runs": [],
        "related_pr_analysis": [],
        "browser_events": [],
        "clusters": [],
        "timeline": [],
        "storyline": [],
        "impacted_modules": [],
        "impacted_modules_ranked": [],
        "temporal_correlation": None,
        "meta": None,
    }
    base.update(overrides)
    return base


def test_empty_evidence_returns_empty_impact_map():
    impact_map = build_incident_impact_map(**_base())
    assert impact_map == []


def test_pr_related_evidence_creates_impacted_area():
    impact_map = build_incident_impact_map(
        **_base(
            related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="483",
                provider="github",
                pr_risk_score=80.0,
                risk_level="HIGH",
                impacted_modules=["checkout"],
                reason="same module affected: checkout",
            ),
        ],
        evidence_correlation=EvidenceCorrelationSummary(
            total_correlations=1,
            strongest_source="pr_analysis",
            evidence=[
                CorrelatedEvidence(
                    source="pr_analysis",
                    confidence=0.84,
                    title="PR Analysis",
                    detail="PR Analysis references impacted module checkout",
                    reason="Stored PR Analysis references checkout",
                    related_entity_type="pr_analysis",
                    related_entity_id="github:483",
                ),
            ],
        ),
        )
    )
    assert len(impact_map) >= 1
    checkout = next(n for n in impact_map if n.title.lower() == "checkout")
    assert checkout.related_entity_type == "pr_analysis"
    assert checkout.related_entity_count >= 2


def test_browser_watch_evidence_creates_impacted_area():
    impact_map = build_incident_impact_map(
        **_base(
            browser_events=[
            {
                "watch_id": "watch_123",
                "summary": "Checkout UI changed",
                "timestamp": "2026-06-09T09:00:00+00:00",
            },
        ],
        evidence_correlation=EvidenceCorrelationSummary(
            total_correlations=1,
            strongest_source="browser_watch",
            evidence=[
                CorrelatedEvidence(
                    source="browser_watch",
                    confidence=0.88,
                    title="Browser alert",
                    detail="Checkout UI changed",
                    reason="Browser Watch detected change",
                    related_entity_type="browser_watch",
                    related_entity_id="watch_123",
                ),
            ],
        ),
        )
    )
    checkout = next(n for n in impact_map if "checkout" in n.title.lower())
    assert checkout.related_entity_type == "browser_watch"
    assert checkout.related_entity_id == "watch_123"


def test_failed_runs_and_cluster_increase_severity():
    impact_map = build_incident_impact_map(
        **_base(
            related_runs=[
                RelatedRunSummary(
                    run_id="RUN-1",
                    test_id="CHK-001",
                    test_name="Checkout submit",
                    status="failed",
                    module="checkout",
                    error_summary="timeout",
                ),
                RelatedRunSummary(
                    run_id="RUN-2",
                    test_id="CHK-002",
                    test_name="Checkout cart",
                    status="failed",
                    module="checkout",
                    error_summary="assertion",
                ),
            ],
            clusters=[
                {
                    "cluster_id": "cluster_7",
                    "module": "checkout",
                    "total_failures": 12,
                    "root_cause_category": "timeout",
                },
            ],
            impacted_modules_ranked=[
                BlastRadiusModule(module="checkout", score=88.0, reason="multiple failures"),
            ],
        )
    )
    checkout = next(n for n in impact_map if n.title.lower() == "checkout")
    assert checkout.severity == "high"
    assert checkout.related_entity_count >= 3
    assert checkout.confidence >= 0.75


def test_duplicate_area_names_are_merged():
    impact_map = build_incident_impact_map(
        **_base(
            impacted_modules=["checkout"],
            impacted_modules_ranked=[
                BlastRadiusModule(module="Checkout Flow", score=70.0, reason="overlap"),
            ],
            browser_events=[
                {"watch_id": "w1", "summary": "checkout flow regression detected"},
            ],
        )
    )
    keys = {n.title.lower().replace(" flow", "") for n in impact_map}
    checkout_nodes = [n for n in impact_map if "checkout" in n.title.lower()]
    assert len(checkout_nodes) == 1
    assert checkout_nodes[0].related_entity_count >= 2


def test_sorting_by_severity_confidence_and_signal_count():
    impact_map = build_incident_impact_map(
        **_base(
            impacted_modules=["checkout", "search"],
            related_runs=[
                RelatedRunSummary(
                    run_id="RUN-1",
                    test_id="CHK-001",
                    module="checkout",
                    status="failed",
                    error_summary="fail",
                ),
                RelatedRunSummary(
                    run_id="RUN-2",
                    test_id="CHK-002",
                    module="checkout",
                    status="failed",
                    error_summary="fail",
                ),
            ],
            clusters=[
                {"cluster_id": "c1", "module": "checkout", "total_failures": 10},
            ],
            browser_events=[
                {"watch_id": "w1", "summary": "Product Search UI changed"},
            ],
        )
    )
    assert len(impact_map) >= 2
    severities = [n.severity for n in impact_map]
    assert severities.index("high") < severities.index("medium") or "medium" not in severities
    if len(impact_map) >= 2:
        high_nodes = [n for n in impact_map if n.severity == "high"]
        medium_nodes = [n for n in impact_map if n.severity == "medium"]
        if high_nodes and medium_nodes:
            assert high_nodes[0].confidence >= medium_nodes[0].confidence or (
                high_nodes[0].related_entity_count >= medium_nodes[0].related_entity_count
            )


def test_backward_compat_report_without_impact_map():
    old = {
        "project_id": "demo",
        "description": "Login broken",
        "hypotheses": [{"statement": "test", "confidence": 0.5}],
        "meta": {"engine_version": "incident-v1.4a", "analyze_only": True},
    }
    report = ProjectIncidentInvestigationReport.model_validate(old)
    assert report.impact_map == []


@pytest.fixture()
def client():
    from app import app

    return TestClient(app)


def _mock_project():
    class _P:
        id = "demo"
        name = "Demo"
    return _P()


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_investigate_includes_impact_map_analyze_only_unchanged(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="AUTH-014",
            status="failed",
            started_at="2026-06-09T10:00:00+00:00",
            module="auth",
            error_summary="fail",
        ),
    ]
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Login broken after deploy", "module": "auth"},
    )
    assert r.status_code == 200
    body = r.json()
    assert "impact_map" in body
    assert isinstance(body["impact_map"], list)
    assert len(body["impact_map"]) >= 1
    assert body["meta"]["analyze_only"] is True
    for action in body.get("actions_available") or []:
        assert action["requires_user_approval"] is True
