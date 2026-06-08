# tests/test_pr_analysis_project_debug.py
"""PR Analysis project_id lookup — diagnostic debug logs."""
from __future__ import annotations

import logging
from unittest.mock import patch

from models.pr_analysis_models import ProjectPRAnalysisRequest
from models.project_knowledge_models import KnowledgeModule, ProjectKnowledge
from services.github_integration_service import analyze_pull_request
from services.pr_analysis_service import pr_analysis_service
from services.project_knowledge_service import get_project_knowledge
from services.project_memory_service import get_memory

logger_name = "vanya.pr_analysis.project_debug"


def test_get_memory_debug_logs_found(caplog):
    caplog.set_level(logging.DEBUG, logger=logger_name)
    knowledge = ProjectKnowledge(
        project_id="zuperio-talent-os",
        project_name="Zuperio Talent OS",
        modules=[KnowledgeModule(name="Candidatos", test_count=1)],
    )
    with patch("services.project_memory_service.project_knowledge_repo.get", return_value=knowledge):
        with patch(
            "services.pr_analysis_project_debug._available_project_ids",
            return_value=["zuperio-talent-os", "demo"],
        ):
            result = get_memory("zuperio-talent-os")

    assert result is knowledge
    text = caplog.text
    assert "PR_ANALYSIS_PROJECT_DEBUG" in text
    assert "project_id='zuperio-talent-os'" in text
    assert "normalized_project_id='zuperio-talent-os'" in text
    assert "memory_found=true" in text
    assert "available_project_ids=['zuperio-talent-os', 'demo']" in text


def test_get_project_knowledge_debug_logs_missing(caplog):
    caplog.set_level(logging.DEBUG, logger=logger_name)
    with patch("services.project_knowledge_service.get_memory", return_value=None):
        with patch(
            "services.pr_analysis_project_debug._available_project_ids",
            return_value=["other-project"],
        ):
            result = get_project_knowledge(" Zuperio-Talent-OS ")

    assert result is None
    text = caplog.text
    assert "project_id=' Zuperio-Talent-OS '" in text
    assert "normalized_project_id='Zuperio-Talent-OS'" in text
    assert "memory_found=false" in text


def test_analyze_for_project_debug_logs_no_memory(caplog):
    caplog.set_level(logging.DEBUG, logger=logger_name)
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=None):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Zuperio Talent OS"):
            with patch(
                "services.pr_analysis_project_debug._available_project_ids",
                return_value=["zuperio-talent-os"],
            ):
                report = pr_analysis_service.analyze_for_project(
                    "zuperio-talent-os",
                    ProjectPRAnalysisRequest(changed_files=["lib/candidates/queries.ts"]),
                )

    assert "No System Memory found" in report.reasoning[0]
    text = caplog.text
    assert "project_id='zuperio-talent-os'" in text
    assert "memory_found=false" in text


def test_analyze_pull_request_debug_logs_project_id(caplog):
    caplog.set_level(logging.DEBUG, logger=logger_name)
    pr_files = type("PR", (), {
        "changed_files": ["lib/candidates/queries.ts"],
        "title": "test",
        "branch": "main",
    })()
    with patch("services.github_integration_service.get_pull_request_files", return_value=pr_files):
        with patch("services.github_integration_service.pr_analysis_service") as svc:
            with patch("services.github_integration_service.GitHubPRAnalyzeResponse") as resp_cls:
                resp_cls.return_value = object()
                svc.analyze_for_project.return_value = object()
                with patch(
                    "services.pr_analysis_project_debug._available_project_ids",
                    return_value=["zuperio-talent-os"],
                ):
                    analyze_pull_request("zuperio-talent-os", 4)

    assert "project_id='zuperio-talent-os'" in caplog.text
    assert "available_project_ids=['zuperio-talent-os']" in caplog.text
