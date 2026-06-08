# tests/test_pr_analysis_v1.py
"""PR Analysis v1 — project-scoped change impact tests."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.pr_analysis_models import ProjectPRAnalysisRequest
from models.project_knowledge_models import (
    KnowledgeModule,
    KnowledgeRelatedTest,
    ProjectKnowledge,
)
from models.risk_engine_models import ModuleRisk, RecommendedTest
from services.change_impact_service import map_changed_files, map_file_to_modules, resolve_impacted_modules
from services.pr_analysis_service import pr_analysis_service


def _knowledge() -> ProjectKnowledge:
    return ProjectKnowledge(
        project_id="proj-1",
        project_name="Zuperio Talent OS",
        modules=[
            KnowledgeModule(name="Candidates", test_count=5, routes=["/candidates"]),
            KnowledgeModule(name="Vacancies", test_count=3, routes=["/vacancies"]),
        ],
        related_tests=[
            KnowledgeRelatedTest(test_case_id="TC-14", name="Candidate login", module="Candidates", last_run_status="fail"),
            KnowledgeRelatedTest(test_case_id="TC-22", name="Vacancy list", module="Vacancies", last_run_status="pass"),
        ],
        risk_score=82.0,
        risk_level="HIGH",
        risk_explanation=["4 recent incident(s)", "67% historical success rate"],
        module_risks=[
            ModuleRisk(module="Candidates", module_risk_score=78.0, module_risk_level="HIGH", regression_count=2),
            ModuleRisk(module="Vacancies", module_risk_score=45.0, module_risk_level="MEDIUM"),
        ],
        recommended_tests=[
            RecommendedTest(test_case_id="TC-14", name="Candidate login", module="Candidates", reason="recurrent regression"),
            RecommendedTest(test_case_id="TC-31", name="Candidate edit", module="Candidates", reason="suspected flaky"),
        ],
    )


def test_map_candidate_file_to_candidates_module():
    knowledge = _knowledge()
    matches = map_file_to_modules(
        "src/components/CandidateForm.tsx",
        known_modules=["Candidates", "Vacancies", "Companies"],
        knowledge=knowledge,
    )
    assert matches
    assert matches[0][0] == "Candidates"
    assert matches[0][1] >= 0.65


def test_map_changed_files_and_resolve_impacted():
    knowledge = _knowledge()
    mappings = map_changed_files(
        [
            "src/components/CandidateForm.tsx",
            "src/services/vacancy_service.py",
        ],
        knowledge=knowledge,
        catalog_modules=["Candidates", "Vacancies"],
    )
    impacted = resolve_impacted_modules(mappings)
    modules = [m[0] for m in impacted]
    assert "Candidates" in modules
    assert "Vacancies" in modules


def test_analyze_for_project_uses_risk_engine_without_recalc():
    knowledge = _knowledge()
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Zuperio Talent OS"):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = [
                    ("TC-14", "Candidates"),
                    ("TC-22", "Vacancies"),
                ]
                report = pr_analysis_service.analyze_for_project(
                    "proj-1",
                    ProjectPRAnalysisRequest(
                        changed_files=[
                            "src/components/CandidateForm.tsx",
                            "src/pages/VacanciesPage.tsx",
                        ],
                    ),
                )

    assert report.risk_score == 82.0
    assert report.risk_level == "HIGH"
    assert report.changed_files_count == 2
    assert any(m.module == "Candidates" for m in report.impacted_modules)
    rec_ids = [t.test_case_id for t in report.recommended_tests]
    assert "TC-14" in rec_ids
    assert any("HIGH" in r or "Candidates" in r for r in report.reasoning)


def test_analyze_for_project_requires_files():
    with pytest.raises(ValueError, match="changed_files"):
        pr_analysis_service.analyze_for_project("proj-1", ProjectPRAnalysisRequest(changed_files=[]))
