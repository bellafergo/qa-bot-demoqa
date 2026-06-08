# tests/test_project_knowledge.py
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.project_knowledge_models import KnowledgeRoute, ProjectKnowledge
from services.app_knowledge_graph import (
    compute_risk_score,
    explorer_page_to_entities,
    merge_routes,
)
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
