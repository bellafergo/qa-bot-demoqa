# tests/test_business_workflow_service.py
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
from services.business_workflow_service import (
    build_business_workflows,
    compute_confidence,
    get_project_knowledge_workflows,
)


def _auth_knowledge() -> ProjectKnowledge:
    return ProjectKnowledge(
        project_id="demo",
        modules=[KnowledgeModule(name="AUTH", test_count=1, source="catalog")],
        routes=[KnowledgeRoute(url="/login", module="AUTH", source="repository")],
        apis=[
            KnowledgeApi(
                url="/api/auth/session",
                method="GET",
                module="AUTH",
                source="repository",
            ),
        ],
        related_tests=[
            KnowledgeRelatedTest(
                test_case_id="TC-TOS-009",
                name="Login exitoso",
                module="AUTH",
            ),
        ],
    )


def _candidate_knowledge() -> ProjectKnowledge:
    return ProjectKnowledge(
        project_id="demo",
        modules=[KnowledgeModule(name="Candidates", test_count=2, source="catalog")],
        routes=[
            KnowledgeRoute(url="/candidates", module="Candidates", source="repository"),
            KnowledgeRoute(url="/candidates/new", module="Candidates", source="repository"),
        ],
        apis=[
            KnowledgeApi(
                url="/api/candidates",
                method="GET",
                module="Candidates",
                source="repository",
            ),
        ],
        related_tests=[
            KnowledgeRelatedTest(
                test_case_id="TC-CAND-01",
                name="List candidates",
                module="Candidates",
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
        run_ids=["r1"],
        total_failures=2,
        common_signals=["auth_error"],
        probable_cause="Auth failed",
        confidence="high",
        summary="2 auth failures",
        blast_radius=BlastRadius(estimated_severity="medium", impact_scope="localized_regression"),
    )


def _orphan_cluster() -> FailureCluster:
    return FailureCluster(
        cluster_id="CL-only",
        root_cause_category="auth_issue",
        impacted_layer="ui",
        module="AUTH",
        representative_test_case_id="TC-TOS-009",
        run_ids=["r1"],
        total_failures=1,
        common_signals=[],
        probable_cause="Auth failed",
        confidence="low",
        summary="orphan",
        blast_radius=BlastRadius(estimated_severity="low", impact_scope="localized_regression"),
    )


def test_auth_workflow_from_route_api_test():
    result = build_business_workflows("demo", _auth_knowledge(), clusters=[_auth_cluster()])
    auth = next(w for w in result.workflows if w.type == "authentication")
    assert auth.name == "Authentication Flow"
    assert auth.confidence == "high"
    assert "/login" in auth.routes
    assert "GET /api/auth/session" in auth.apis
    assert "TC-TOS-009" in auth.tests
    assert "CL-auth01" in auth.failure_clusters
    assert auth.coverage.routes == 1
    assert auth.coverage.apis == 1
    assert auth.coverage.tests == 1
    assert auth.coverage.clusters == 1


def test_candidate_lifecycle_from_routes_apis_tests():
    result = build_business_workflows("demo", _candidate_knowledge())
    cand = next(w for w in result.workflows if w.type == "candidate_lifecycle")
    assert cand.name == "Candidate Lifecycle"
    assert len(cand.routes) == 2
    assert "GET /api/candidates" in cand.apis
    assert "TC-CAND-01" in cand.tests


def test_no_workflow_if_only_one_evidence():
    knowledge = ProjectKnowledge(
        project_id="demo",
        routes=[KnowledgeRoute(url="/login", module="AUTH", source="repository")],
    )
    result = build_business_workflows("demo", knowledge)
    assert not any(w.type == "authentication" for w in result.workflows)


def test_clusters_attach_but_do_not_create_workflow_alone():
    knowledge = ProjectKnowledge(project_id="demo")
    result = build_business_workflows("demo", knowledge, clusters=[_orphan_cluster()])
    assert result.workflows == []


def test_confidence_high_medium_low():
    assert compute_confidence(
        routes={"/login"},
        apis={"GET /api/auth/session"},
        tests={"TC-1"},
        clusters=set(),
        structural_count=3,
    ) == "high"
    assert compute_confidence(
        routes={"/login"},
        apis=set(),
        tests={"TC-1"},
        clusters=set(),
        structural_count=2,
    ) == "low"
    assert compute_confidence(
        routes={"/login"},
        apis={"GET /api/x"},
        tests=set(),
        clusters=set(),
        structural_count=2,
    ) == "low"
    assert compute_confidence(
        routes={"/login"},
        apis={"GET /api/x"},
        tests=set(),
        clusters={"CL-1"},
        structural_count=2,
    ) == "medium"


def test_sort_order_clusters_tests_confidence_name():
    knowledge = ProjectKnowledge(
        project_id="demo",
        routes=[
            KnowledgeRoute(url="/login", module="AUTH", source="repository"),
            KnowledgeRoute(url="/candidates", module="Candidates", source="repository"),
            KnowledgeRoute(url="/vacancies", module="Vacancies", source="repository"),
        ],
        apis=[
            KnowledgeApi(url="/api/auth/session", method="GET", module="AUTH", source="repository"),
            KnowledgeApi(url="/api/candidates", method="GET", module="Candidates", source="repository"),
            KnowledgeApi(url="/api/vacancies", method="GET", module="Vacancies", source="repository"),
        ],
        related_tests=[
            KnowledgeRelatedTest(test_case_id="TC-A", name="Auth", module="AUTH"),
            KnowledgeRelatedTest(test_case_id="TC-C1", name="Cand", module="Candidates"),
            KnowledgeRelatedTest(test_case_id="TC-C2", name="Cand2", module="Candidates"),
            KnowledgeRelatedTest(test_case_id="TC-V", name="Vac", module="Vacancies"),
        ],
    )
    clusters = [
        FailureCluster(
            cluster_id="CL-cand",
            root_cause_category="candidate",
            impacted_layer="ui",
            module="Candidates",
            representative_test_case_id="TC-C1",
            run_ids=["r1"],
            total_failures=1,
            common_signals=[],
            probable_cause="x",
            confidence="medium",
            summary="cand",
            blast_radius=BlastRadius(estimated_severity="low", impact_scope="localized_regression"),
        ),
        _auth_cluster(),
    ]
    result = build_business_workflows("demo", knowledge, clusters=clusters)
    names = [w.name for w in result.workflows]
    assert names.index("Candidate Lifecycle") < names.index("Authentication Flow")


@patch("services.project_knowledge_service.get_project_knowledge")
@patch("services.failure_intelligence_service.failure_intelligence_service.get_clusters")
def test_get_project_knowledge_workflows_endpoint_service(mock_clusters, mock_knowledge):
    mock_knowledge.return_value = _auth_knowledge()
    mock_clusters.return_value = [_auth_cluster()]
    result = get_project_knowledge_workflows("demo")
    assert result.project_id == "demo"
    assert len(result.workflows) >= 1
