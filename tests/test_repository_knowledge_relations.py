# tests/test_repository_knowledge_relations.py
from __future__ import annotations

from models.project_knowledge_models import KnowledgeApi, KnowledgeModule, KnowledgeRelatedTest, KnowledgeRoute
from services.repository_knowledge_relations import (
    consolidate_modules_by_alias,
    infer_module_for_prisma_model,
    infer_module_for_url,
    link_entities_to_modules,
)


def test_infer_module_for_url_candidates():
    mod = infer_module_for_url("/candidates/[id]", ["Candidates", "Auth"])
    assert mod == "Candidates"


def test_infer_module_for_prisma_model_alias():
    mod = infer_module_for_prisma_model("Candidate", ["Candidates", "Auth"])
    assert mod == "Candidates"


def test_infer_module_low_confidence_returns_empty():
    mod = infer_module_for_url("/settings/profile", ["Candidates"])
    assert mod == ""


def test_consolidate_modules_by_alias_merges_catalog_and_prisma():
    catalog = KnowledgeModule(name="Candidates", test_count=3, source="catalog")
    prisma = KnowledgeModule(
        name="Candidate",
        source="repository",
        entity_type="prisma_model",
        fields=["id", "email"],
        relations=["Vacancy"],
    )
    merged = consolidate_modules_by_alias([catalog, prisma])
    assert len(merged) == 1
    assert merged[0].test_count == 3
    assert "email" in merged[0].fields
    assert "Vacancy" in merged[0].relations


def test_link_entities_enriches_module_graph():
    modules = [KnowledgeModule(name="Candidates", test_count=2, source="catalog")]
    routes = [KnowledgeRoute(url="/candidates", source="repository")]
    apis = [KnowledgeApi(url="/api/candidates", method="GET", source="repository")]
    tests = [KnowledgeRelatedTest(test_case_id="T1", module="Candidates")]

    enriched_mods, linked_routes, linked_apis = link_entities_to_modules(
        modules, routes, apis, related_tests=tests,
    )
    assert linked_routes[0].module == "Candidates"
    assert linked_apis[0].module == "Candidates"
    assert "/candidates" in enriched_mods[0].routes
    assert any("GET" in a for a in enriched_mods[0].apis)
    assert enriched_mods[0].test_count >= 2
