# tests/test_pr_analysis_memory_fallback.py
"""PR Analysis — automatic System Memory refresh fallback."""
from __future__ import annotations

from unittest.mock import patch

from models.pr_analysis_models import ProjectPRAnalysisRequest
from models.project_knowledge_models import (
    KnowledgeModule,
    KnowledgeRelatedTest,
    ProjectKnowledge,
)
from models.risk_engine_models import ModuleRisk
from services.pr_analysis_service import load_project_knowledge_for_analysis, pr_analysis_service


def _candidatos_knowledge() -> ProjectKnowledge:
    return ProjectKnowledge(
        project_id="zuperio-talent-os",
        project_name="Zuperio Talent OS",
        modules=[KnowledgeModule(name="Candidatos", test_count=3)],
        related_tests=[
            KnowledgeRelatedTest(test_case_id="TC-14", name="Ver candidatos", module="Candidatos"),
        ],
        risk_score=18.0,
        risk_level="LOW",
        module_risks=[
            ModuleRisk(module="Candidatos", module_risk_score=22.0, module_risk_level="LOW"),
        ],
    )


def test_analyze_with_existing_memory_does_not_refresh():
    knowledge = _candidatos_knowledge()
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service.refresh_project_knowledge") as refresh:
            with patch("services.project_knowledge_service._resolve_project_name", return_value="Zuperio Talent OS"):
                with patch("services.db.catalog_repository.catalog_repo") as cat:
                    cat.all_modules_for_project.return_value = [("TC-14", "Candidatos")]
                    report = pr_analysis_service.analyze_for_project(
                        "zuperio-talent-os",
                        ProjectPRAnalysisRequest(changed_files=["lib/candidates/queries.ts"]),
                    )

    refresh.assert_not_called()
    assert any(m.module == "Candidatos" for m in report.impacted_modules)


def test_analyze_without_memory_refreshes_and_maps_candidatos():
    knowledge = _candidatos_knowledge()
    with patch(
        "services.project_knowledge_service.get_project_knowledge",
        side_effect=[None, knowledge],
    ):
        with patch("services.project_knowledge_service.refresh_project_knowledge", return_value=knowledge) as refresh:
            with patch("services.project_knowledge_service._resolve_project_name", return_value="Zuperio Talent OS"):
                with patch("services.db.catalog_repository.catalog_repo") as cat:
                    cat.all_modules_for_project.return_value = [("TC-14", "Candidatos")]
                    report = pr_analysis_service.analyze_for_project(
                        "zuperio-talent-os",
                        ProjectPRAnalysisRequest(changed_files=["lib/candidates/queries.ts"]),
                    )

    refresh.assert_called_once()
    assert any(m.module == "Candidatos" for m in report.impacted_modules)
    assert report.file_mappings[0].module == "Candidatos"


def test_load_project_knowledge_triggers_refresh_when_missing():
    knowledge = _candidatos_knowledge()
    with patch(
        "services.project_knowledge_service.get_project_knowledge",
        side_effect=[None, knowledge],
    ):
        with patch("services.project_knowledge_service.refresh_project_knowledge") as refresh:
            result, attempted = load_project_knowledge_for_analysis("zuperio-talent-os")

    refresh.assert_called_once()
    assert attempted is True
    assert result is knowledge


def test_load_project_knowledge_skips_refresh_when_present():
    knowledge = _candidatos_knowledge()
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service.refresh_project_knowledge") as refresh:
            result, attempted = load_project_knowledge_for_analysis("zuperio-talent-os")

    refresh.assert_not_called()
    assert attempted is False
    assert result is knowledge


def test_analyze_refresh_failure_returns_clear_message_without_500():
    with patch(
        "services.project_knowledge_service.get_project_knowledge",
        return_value=None,
    ):
        with patch(
            "services.project_knowledge_service.refresh_project_knowledge",
            side_effect=RuntimeError("catalog unavailable"),
        ):
            with patch("services.project_knowledge_service._resolve_project_name", return_value="Zuperio Talent OS"):
                report = pr_analysis_service.analyze_for_project(
                    "zuperio-talent-os",
                    ProjectPRAnalysisRequest(changed_files=["lib/candidates/queries.ts"]),
                )

    assert "No System Memory found" in report.reasoning[0]
    assert any("Automatic System Memory refresh" in r for r in report.reasoning)
    assert report.impacted_modules == []


def test_load_project_knowledge_refresh_exception_returns_none():
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=None):
        with patch(
            "services.project_knowledge_service.refresh_project_knowledge",
            side_effect=RuntimeError("catalog unavailable"),
        ):
            result, attempted = load_project_knowledge_for_analysis("zuperio-talent-os")

    assert result is None
    assert attempted is True
