# tests/test_project_knowledge.py
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import MagicMock, patch

import pytest
from fastapi.testclient import TestClient

from models.project_knowledge_models import KnowledgeRoute, ProjectKnowledge, ProjectKnowledgeRefreshRequest
from models.test_run import TestRun as TestRunModel
from services.app_knowledge_graph import (
    compute_risk_score,
    explorer_page_to_entities,
    inspection_summaries_to_entities,
    merge_routes,
)
from services.db.project_knowledge_repository import project_knowledge_repo
from services.project_knowledge_service import (
    get_knowledge_context,
    ingest_explorer_result,
    refresh_project_knowledge,
)


@pytest.fixture()
def client():
    from app import app

    return TestClient(app)


def test_merge_routes_dedupes_by_url():
    a = KnowledgeRoute(url="https://demo.example.com/dashboard", title="Dash", source="explorer")
    b = KnowledgeRoute(url="https://demo.example.com/dashboard/", title="Dashboard", source="catalog")
    merged = merge_routes([a], [b])
    assert len(merged) == 1
    assert merged[0].title in ("Dash", "Dashboard")


def test_explorer_page_to_entities_extracts_forms_and_routes():
    page = {
        "url": "https://demo.example.com/login",
        "title": "Login",
        "forms": [{"name": "login", "fields": ["user", "pass"], "buttons": ["Submit"]}],
        "inputs": [{"name": "user", "selector": "#user"}],
        "buttons": [{"name": "Submit", "selector": "button"}],
        "links": [{"text": "API", "href": "https://demo.example.com/api/health"}],
    }
    routes, forms, _tables, apis, components = explorer_page_to_entities(page)
    assert any("login" in f.name for f in forms)
    assert len(components) >= 2
    assert any("/api/" in a.url for a in apis)


def test_compute_risk_score_increases_with_failures():
    low = compute_risk_score(failure_history=[], incident_history=[])
    from models.project_knowledge_models import KnowledgeFailureEntry, KnowledgeIncidentEntry

    high = compute_risk_score(
        failure_history=[KnowledgeFailureEntry(test_case_id="T1", count=3)],
        incident_history=[KnowledgeIncidentEntry(id="1", severity="critical", description="x")],
        run_fail_rate=0.5,
    )
    assert high > low


def test_ingest_explorer_result_persists_routes(tmp_path, monkeypatch):
    monkeypatch.setenv("VANYA_DB_PATH", str(tmp_path / "test.db"))
    from services.db.init_db import init_catalog_db

    init_catalog_db()

    exploration = {
        "start_url": "https://example.com",
        "visited_count": 1,
        "pages": [{
            "url": "https://example.com",
            "title": "Home",
            "forms": [],
            "inputs": [],
            "buttons": [],
            "links": [],
        }],
        "errors": [],
    }
    result = ingest_explorer_result("demo", exploration)
    assert result is not None
    assert len(result.routes) >= 1


def test_get_knowledge_context_hints_module_match():
    mem = ProjectKnowledge(
        project_id="demo",
        modules=[{"name": "dashboard", "test_count": 2, "source": "catalog", "last_seen_at": ""}],
        failure_history=[],
        incident_history=[],
    )
    with patch("services.project_knowledge_service.get_memory", return_value=mem):
        ctx = get_knowledge_context("demo", incident_description="El dashboard no carga")
    assert ctx is not None
    assert any("dashboard" in h.lower() for h in ctx.hints)


def test_refresh_project_knowledge_endpoint(client: TestClient):
    with patch("services.project_knowledge_service.refresh_project_knowledge") as mock_refresh:
        mock_refresh.return_value = ProjectKnowledge(project_id="demo", project_name="Demo")
        r = client.post("/projects/demo/knowledge/refresh")
    assert r.status_code == 200
    assert r.json()["project_id"] == "demo"


def test_get_knowledge_404_when_missing(client: TestClient):
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=None):
        r = client.get("/projects/missing/knowledge")
    assert r.status_code == 404


def test_inspection_summaries_to_entities_rebuilds_routes_and_workflows():
    chunk = inspection_summaries_to_entities(
        url="https://app.example.com/dash",
        title="Dashboard",
        executed_at="2026-01-01T00:00:00Z",
        browser_inspection_summary={
            "final_url": "https://app.example.com/dash",
            "title": "Dashboard",
            "counts": {"forms_count": 2},
        },
        app_map_summary={
            "page_type": ["dashboard"],
            "main_navigation_summary": [{"text": "API", "href": "https://app.example.com/api/health"}],
            "suggested_test_flows": ["Open dashboard and verify widgets"],
            "primary_actions_summary": [{"text": "Save", "kind": "button"}],
        },
    )
    assert len(chunk["routes"]) >= 2
    assert len(chunk["forms"]) == 1
    assert len(chunk["workflows"]) == 1
    assert len(chunk["components"]) == 1
    assert any("/api/" in a.url for a in chunk["apis"])


def _mock_refresh_deps():
    mock_tc = MagicMock()
    mock_tc.test_case_id = "TC-001"
    mock_tc.module = "dashboard"
    mock_tc.name = "Load dashboard"
    mock_tc.base_url = "https://demoqa.com/dashboard"
    mock_tc.test_type = "ui"
    mock_tc.priority = "P1"

    mock_reg = MagicMock()
    mock_reg.test_case_id = "TC-001"
    mock_reg.module = "dashboard"
    mock_reg.summary = "Dashboard fails"
    mock_reg.repeated_failures = 3

    mock_run = MagicMock(status="fail")

    mock_incident = {
        "id": "inc-1",
        "incident_description": "Dashboard loading",
        "severity": "high",
        "suspected_area": "frontend",
        "target_url": "https://demoqa.com",
        "created_at": "2026-01-01T00:00:00Z",
    }

    inspection_run = TestRunModel(
        run_id="insp-1",
        test_case_id="_browser_inspection",
        test_name="[Browser inspection] Dashboard",
        executed_at=datetime(2026, 1, 1, tzinfo=timezone.utc),
        status="pass",
        meta={
            "source": "browser_inspection",
            "project_id": "demo",
            "browser_inspection_summary": {
                "url": "https://demoqa.com/dashboard",
                "final_url": "https://demoqa.com/dashboard",
                "title": "Dashboard",
                "counts": {"forms_count": 1},
            },
            "app_map_summary": {
                "page_type": ["dashboard"],
                "suggested_test_flows": ["Verify dashboard loads"],
                "main_navigation_summary": [],
                "primary_actions_summary": [],
            },
        },
    )

    return {
        "catalog": [mock_tc],
        "runs": [mock_run, mock_run, MagicMock(status="pass")],
        "regressions": [mock_reg],
        "incidents": [mock_incident],
        "inspections": [inspection_run],
    }


def test_refresh_replace_rebuilds_from_empty():
    pid = "demo-refresh-empty"
    project_knowledge_repo.delete(pid)

    mocks = _mock_refresh_deps()
    with patch("services.db.catalog_repository.catalog_repo.list_test_cases", return_value=mocks["catalog"]), \
         patch("services.run_history_service.run_history_service.list_runs", return_value=mocks["runs"]), \
         patch("services.failure_intelligence_service.failure_intelligence_service.get_regressions", return_value=mocks["regressions"]), \
         patch("services.db.incident_investigation_repository.incident_investigation_repo.list_runs", return_value=mocks["incidents"]), \
         patch("services.db.test_run_repository.test_run_repo.list_browser_inspection_runs", return_value=mocks["inspections"]), \
         patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):

        result = refresh_project_knowledge(pid, ProjectKnowledgeRefreshRequest(mode="replace"))

    assert len(result.modules) >= 1
    assert len(result.related_tests) >= 1
    assert len(result.failure_history) >= 1
    assert len(result.incident_history) >= 1
    assert len(result.routes) >= 1
    assert len(result.forms) >= 1
    assert len(result.workflows) >= 1
    assert result.metadata.get("refresh_mode") == "replace"
    assert result.metadata.get("explorer_reconstructible") is False


def test_refresh_replace_drops_stale_routes():
    pid = "demo-refresh-stale"
    stale = ProjectKnowledge(
        project_id=pid,
        routes=[KnowledgeRoute(url="https://stale.example.com/ghost", title="ghost", source="manual")],
    )
    project_knowledge_repo.upsert(stale)

    mocks = _mock_refresh_deps()
    with patch("services.db.catalog_repository.catalog_repo.list_test_cases", return_value=mocks["catalog"]), \
         patch("services.run_history_service.run_history_service.list_runs", return_value=[]), \
         patch("services.failure_intelligence_service.failure_intelligence_service.get_regressions", return_value=[]), \
         patch("services.db.incident_investigation_repository.incident_investigation_repo.list_runs", return_value=[]), \
         patch("services.db.test_run_repository.test_run_repo.list_browser_inspection_runs", return_value=mocks["inspections"]), \
         patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):

        result = refresh_project_knowledge(pid, ProjectKnowledgeRefreshRequest(mode="replace"))

    urls = [r.url for r in result.routes]
    assert not any("stale.example.com" in u for u in urls)
    assert any("demoqa.com" in u for u in urls)


def test_refresh_merge_preserves_stale_routes():
    pid = "demo-refresh-merge"
    stale = ProjectKnowledge(
        project_id=pid,
        routes=[KnowledgeRoute(url="https://stale.example.com/kept", title="kept", source="manual")],
    )
    project_knowledge_repo.upsert(stale)

    mocks = _mock_refresh_deps()
    with patch("services.db.catalog_repository.catalog_repo.list_test_cases", return_value=mocks["catalog"]), \
         patch("services.run_history_service.run_history_service.list_runs", return_value=[]), \
         patch("services.failure_intelligence_service.failure_intelligence_service.get_regressions", return_value=[]), \
         patch("services.db.incident_investigation_repository.incident_investigation_repo.list_runs", return_value=[]), \
         patch("services.db.test_run_repository.test_run_repo.list_browser_inspection_runs", return_value=[]), \
         patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):

        result = refresh_project_knowledge(pid, ProjectKnowledgeRefreshRequest(mode="merge"))

    urls = [r.url for r in result.routes]
    assert any("stale.example.com/kept" in u for u in urls)
    assert any("demoqa.com" in u for u in urls)
