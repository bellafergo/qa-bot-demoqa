# tests/test_pr_analysis_risk_split.py
"""PR Analysis Phase 0 — project vs PR risk field separation."""
from __future__ import annotations

from unittest.mock import patch

from models.pr_analysis_models import ProjectPRAnalysisRequest
from models.project_knowledge_models import KnowledgeModule, ProjectKnowledge
from services.change_classification_service import classify_changed_files
from services.pr_analysis_service import pr_analysis_service


def test_report_exposes_project_and_pr_risk_fields():
    knowledge = ProjectKnowledge(
        project_id="proj-1",
        project_name="Demo",
        modules=[KnowledgeModule(name="Auth", test_count=1)],
        risk_score=18.0,
        risk_level="LOW",
    )
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = [("TC-1", "Auth")]
                report = pr_analysis_service.analyze_for_project(
                    "proj-1",
                    ProjectPRAnalysisRequest(changed_files=["src/auth/login.ts"]),
                )

    assert report.project_risk_score == 18.0
    assert report.project_risk_level == "LOW"
    assert report.pr_risk_score != report.project_risk_score or report.pr_risk_level != report.project_risk_level
    assert report.risk_score == report.pr_risk_score
    assert report.risk_level == report.pr_risk_level
    assert report.engine_version == "pr-v1.2"


def test_analyze_includes_file_classifications_from_patches():
    knowledge = ProjectKnowledge(
        project_id="proj-1",
        modules=[KnowledgeModule(name="Candidatos", test_count=1)],
        risk_score=10.0,
        risk_level="LOW",
    )
    diff_patch = """@@ -1,3 +1,4 @@
 export function q() {
+  // solo comentario
   return 1;
 }
"""
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = [("TC-1", "Candidatos")]
                report = pr_analysis_service.analyze_for_project(
                    "proj-1",
                    ProjectPRAnalysisRequest(
                        changed_files=["lib/candidates/queries.ts"],
                        file_patches={"lib/candidates/queries.ts": diff_patch},
                    ),
                )

    assert len(report.file_classifications) == 1
    assert report.file_classifications[0].primary_class == "comments"
    assert any("comment-only" in r.lower() for r in report.reasoning)


def test_github_analyze_passes_patches_to_pr_analysis():
    from models.github_integration_models import GitHubPRFileEntry, GitHubPRFilesResponse
    from services.github_integration_service import analyze_pull_request

    knowledge = ProjectKnowledge(
        project_id="demo",
        modules=[KnowledgeModule(name="Candidatos", test_count=1)],
        risk_score=18.0,
        risk_level="LOW",
    )
    diff_patch = "+  // comentario\n"

    with patch("services.github_integration_service._require_client") as req:
        req.return_value = ({}, object())
        with patch("services.github_integration_service.get_pull_request_files") as gpf:
            gpf.return_value = GitHubPRFilesResponse(
                number=7,
                title="Comment",
                branch="feat/x",
                changed_files=["lib/candidates/queries.ts"],
                files=[
                    GitHubPRFileEntry(
                        filename="lib/candidates/queries.ts",
                        status="modified",
                        additions=1,
                        deletions=0,
                        patch=diff_patch,
                    ),
                ],
            )
            with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
                with patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):
                    with patch("services.db.catalog_repository.catalog_repo") as cat:
                        cat.all_modules_for_project.return_value = [("TC-1", "Candidatos")]
                        response = analyze_pull_request("demo", 7)

    assert response.analysis.file_classifications
    assert response.analysis.file_classifications[0].primary_class == "comments"
    assert response.pull_request.files[0].patch == diff_patch
