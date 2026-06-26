# tests/test_project_knowledge_refresh_utils.py
from __future__ import annotations

from unittest.mock import MagicMock, patch

from models.project_knowledge_models import ProjectKnowledgeRefreshRequest
from services.project_knowledge_refresh_utils import (
    project_has_github_repository,
    resolve_refresh_request,
)


def test_resolve_refresh_auto_enables_repository_when_github_connected():
    req = ProjectKnowledgeRefreshRequest(mode="replace", include_repository=False)
    with patch(
        "services.project_knowledge_refresh_utils.project_has_github_repository",
        return_value=True,
    ):
        resolved = resolve_refresh_request("demo-enterprise-qa", req)
    assert resolved.include_repository is True


def test_resolve_refresh_respects_explicit_false():
    req = ProjectKnowledgeRefreshRequest(mode="replace", include_repository=False)
    with patch(
        "services.project_knowledge_refresh_utils.project_has_github_repository",
        return_value=True,
    ):
        resolved = resolve_refresh_request(
            "demo-enterprise-qa",
            req,
            include_repository_explicit=False,
        )
    assert resolved.include_repository is False


def test_project_has_github_repository_requires_installation_and_repo():
    project = MagicMock()
    project.settings = {
        "github": {
            "installation_id": "123",
            "owner": "acme",
            "repo": "shop",
        },
    }
    with patch("services.db.project_repository.project_repo.get_project", return_value=project):
        assert project_has_github_repository("acme-shop") is True

    project.settings = {"github": {"installation_id": "123"}}
    with patch("services.db.project_repository.project_repo.get_project", return_value=project):
        assert project_has_github_repository("acme-shop") is False
