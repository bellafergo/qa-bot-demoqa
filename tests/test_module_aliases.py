# tests/test_module_aliases.py
"""Bilingual module alias matching for PR Analysis change impact."""
from __future__ import annotations

import pytest

from models.project_knowledge_models import KnowledgeModule, KnowledgeRelatedTest, ProjectKnowledge
from services.change_impact_service import _score_token_module, map_changed_files, map_file_to_modules
from services.module_aliases import alias_equivalent


@pytest.mark.parametrize(
    "token,module",
    [
        ("candidates", "Candidatos"),
        ("vacancies", "Vacantes"),
        ("proposals", "Propuestas"),
        ("auth", "Autenticacion"),
        ("login", "auth"),
        ("signin", "auth"),
        ("basket", "cart"),
        ("inventory", "product-listing"),
        ("menu", "navigation"),
        ("order", "orders"),
        ("dashboard", "Tablero"),
        ("companies", "Empresas"),
        ("contacts", "Contactos"),
        ("opportunities", "Oportunidades"),
    ],
)
def test_bilingual_alias_pairs_score_above_threshold(token, module):
    assert alias_equivalent(token, module)
    assert _score_token_module(token, module) >= 0.65


def test_english_module_still_exact_match():
    assert _score_token_module("candidates", "Candidates") == 1.0


def test_unrelated_token_module_still_zero():
    assert _score_token_module("candidates", "Vacantes") == 0.0
    assert not alias_equivalent("candidates", "Vacantes")


def test_lib_candidates_queries_maps_to_candidatos_module():
    knowledge = ProjectKnowledge(
        project_id="zuperio-talent-os",
        modules=[KnowledgeModule(name="Candidatos", test_count=3)],
        related_tests=[
            KnowledgeRelatedTest(test_case_id="TC-14", name="Ver candidatos", module="Candidatos"),
        ],
    )
    matches = map_file_to_modules(
        "lib/candidates/queries.ts",
        known_modules=["Candidatos"],
        knowledge=knowledge,
    )
    assert matches
    assert matches[0][0] == "Candidatos"
    assert matches[0][1] >= 0.65
    assert "candidates" in matches[0][2]


def test_map_changed_files_spanish_catalog_modules():
    knowledge = ProjectKnowledge(
        project_id="proj-es",
        modules=[
            KnowledgeModule(name="Candidatos", test_count=2),
            KnowledgeModule(name="Vacantes", test_count=1),
        ],
    )
    mappings = map_changed_files(
        ["lib/candidates/queries.ts", "src/pages/vacancies/list.tsx"],
        knowledge=knowledge,
        catalog_modules=["Candidatos", "Vacantes"],
    )
    assert mappings["lib/candidates/queries.ts"][0][0] == "Candidatos"
    assert mappings["src/pages/vacancies/list.tsx"][0][0] == "Vacantes"
