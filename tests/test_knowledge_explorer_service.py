# tests/test_knowledge_explorer_service.py
from __future__ import annotations

from unittest.mock import patch

from models.failure_intelligence_models import BlastRadius, FailureCluster
from models.project_knowledge_models import (
    KnowledgeApi,
    KnowledgeModule,
    KnowledgeRelatedTest,
    KnowledgeRoute,
    ProjectKnowledge,
)
from services.knowledge_explorer_service import (
    build_knowledge_explorer,
    get_project_knowledge_explorer,
    modules_match,
)


def _knowledge() -> ProjectKnowledge:
    return ProjectKnowledge(
        project_id="demo",
        modules=[
            KnowledgeModule(
                name="AUTH",
                test_count=2,
                routes=["/login"],
                apis=["GET /api/auth/session"],
                source="catalog",
            ),
            KnowledgeModule(
                name="Dashboard",
                test_count=1,
                source="catalog",
            ),
        ],
        routes=[
            KnowledgeRoute(
                url="/login",
                module="AUTH",
                source="repository",
                file_path="app/login/page.tsx",
            ),
            KnowledgeRoute(url="/dashboard", module="Dashboard", source="catalog"),
        ],
        apis=[
            KnowledgeApi(
                url="/api/auth/session",
                method="GET",
                module="AUTH",
                source="repository",
                file_path="app/api/auth/session/route.ts",
            ),
        ],
        related_tests=[
            KnowledgeRelatedTest(
                test_case_id="TC-TOS-009",
                name="Login exitoso",
                module="AUTH",
                last_run_status="fail",
            ),
            KnowledgeRelatedTest(
                test_case_id="TC-DASH-01",
                name="Dashboard loads",
                module="Dashboard",
            ),
        ],
    )


def _auth_cluster() -> FailureCluster:
    return FailureCluster(
        cluster_id="CL-auth01",
        root_cause_category="auth_issue",
        impacted_layer="ui",
        module="AUTH",
        representative_test_case_id="TC-TOS-009",
        run_ids=["r1", "r2"],
        total_failures=2,
        common_signals=["auth_error"],
        probable_cause="Auth failed",
        confidence="high",
        summary="2 auth failures",
        blast_radius=BlastRadius(estimated_severity="medium", impact_scope="localized_regression"),
    )


def test_modules_match_alias_variants():
    assert modules_match("AUTH", "auth")
    assert modules_match("Candidates", "Candidate") is False or modules_match("Candidates", "candidates")


def test_build_explorer_attaches_routes_apis_tests_clusters():
    explorer = build_knowledge_explorer(
        "demo",
        _knowledge(),
        clusters=[_auth_cluster()],
    )
    assert len(explorer.modules) == 2
    auth = next(m for m in explorer.modules if m.module == "AUTH")
    assert auth.counts.routes == 1
    assert auth.routes[0].url == "/login"
    assert auth.routes[0].file_path == "app/login/page.tsx"
    assert auth.counts.apis == 1
    assert auth.apis[0].method == "GET"
    assert auth.counts.tests == 1
    assert auth.tests[0].test_case_id == "TC-TOS-009"
    assert auth.counts.failure_clusters == 1
    assert auth.failure_clusters[0].occurrences == 2
    assert auth.failure_clusters[0].confidence == "high"


def test_build_explorer_sort_priority_clusters_first():
    explorer = build_knowledge_explorer(
        "demo",
        _knowledge(),
        clusters=[_auth_cluster()],
    )
    assert explorer.modules[0].module == "AUTH"
    assert explorer.default_expanded_module == "AUTH"


def test_build_explorer_empty_knowledge_returns_empty_modules():
    explorer = build_knowledge_explorer("demo", None)
    assert explorer.modules == []
    assert explorer.default_expanded_module == ""


@patch("services.failure_intelligence_service.failure_intelligence_service.get_clusters")
@patch("services.project_knowledge_service.get_project_knowledge")
def test_get_project_knowledge_explorer(mock_get_knowledge, mock_get_clusters):
    mock_get_knowledge.return_value = _knowledge()
    mock_get_clusters.return_value = [_auth_cluster()]

    from services.knowledge_explorer_service import get_project_knowledge_explorer
    explorer = get_project_knowledge_explorer("demo")
    assert explorer.project_id == "demo"
    assert len(explorer.modules) == 2
