# tests/test_change_impact_debug_logging.py
"""PR Analysis module mapping — diagnostic debug logs."""
from __future__ import annotations

import logging

from models.project_knowledge_models import KnowledgeModule, KnowledgeRelatedTest, ProjectKnowledge
from services.change_impact_service import map_changed_files, resolve_impacted_modules

logger_name = "vanya.change_impact"


def test_debug_logs_candidatos_mapping(caplog):
    caplog.set_level(logging.DEBUG, logger=logger_name)
    knowledge = ProjectKnowledge(
        project_id="zuperio-talent-os",
        modules=[KnowledgeModule(name="Candidatos", test_count=3)],
        related_tests=[
            KnowledgeRelatedTest(test_case_id="TC-14", name="Ver candidatos", module="Candidatos"),
        ],
    )
    file_map = map_changed_files(
        ["lib/candidates/queries.ts"],
        knowledge=knowledge,
        catalog_modules=["Candidatos"],
    )
    impacted = resolve_impacted_modules(file_map)

    text = caplog.text
    assert "PR_ANALYSIS_DEBUG" in text
    assert "file=lib/candidates/queries.ts" in text
    assert "tokens=['lib', 'candidates', 'queries']" in text
    assert "module='Candidatos'" in text
    assert "score=0.8" in text
    assert "alias_match=true" in text
    assert "memory_modules=['Candidatos']" in text
    assert "catalog_modules=['Candidatos']" in text
    assert "impacted_modules=[('Candidatos', ['lib/candidates/queries.ts'], 0.8)]" in text
    assert impacted[0][0] == "Candidatos"


def test_debug_logs_unmapped_module(caplog):
    caplog.set_level(logging.DEBUG, logger=logger_name)
    knowledge = ProjectKnowledge(
        project_id="proj-es",
        modules=[KnowledgeModule(name="Vacantes", test_count=1)],
    )
    file_map = map_changed_files(
        ["lib/candidates/queries.ts"],
        knowledge=knowledge,
        catalog_modules=["Vacantes"],
    )
    resolve_impacted_modules(file_map)

    text = caplog.text
    assert "module='Vacantes'" in text
    assert "score=0.0" in text
    assert "alias_match=false" in text
    assert "file_mappings=[]" in text
