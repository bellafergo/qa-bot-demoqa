# tests/test_repository_knowledge_service.py
from __future__ import annotations

from unittest.mock import MagicMock, patch

from services.repository_knowledge_service import index_repository_knowledge


MOCK_TREE = [
    "app/dashboard/page.tsx",
    "app/api/candidates/route.ts",
    "api/routes/candidates.py",
    "prisma/schema.prisma",
    "db/migrations/001.sql",
]

MOCK_CONTENT = {
    "app/api/candidates/route.ts": "export async function GET() {}\nexport async function POST() {}",
    "api/routes/candidates.py": '@router.get("/")\nasync def list_candidates():\n    pass',
    "prisma/schema.prisma": """
model Candidate {
  id String @id
  email String
}
""",
    "db/migrations/001.sql": "CREATE TABLE candidates (id TEXT);",
}


def test_index_repository_skipped_without_github():
    result = index_repository_knowledge("missing-project")
    assert result.skipped is True
    assert result.metadata.get("repository_indexed") is False


@patch("services.project_github_settings_service._client_for_project")
def test_index_repository_indexes_routes_apis_models(mock_client_fn):
    mock_client = MagicMock()
    mock_client.get_branch_head_sha.return_value = "abc123"
    mock_client.list_tree_recursive.return_value = (MOCK_TREE, False)

    def fake_get_file(path, ref):
        return MOCK_CONTENT.get(path, "")

    mock_client.get_file_text.side_effect = fake_get_file
    mock_client_fn.return_value = ({"default_branch": "main"}, mock_client)

    result = index_repository_knowledge(
        "demo-project",
        known_module_names=["Candidates"],
    )
    assert result.skipped is False
    assert result.metadata["repository_indexed"] is True
    assert result.metadata["repository_files_scanned"] == len(MOCK_TREE)
    assert result.metadata["repository_routes_detected"] >= 1
    assert result.metadata["repository_apis_detected"] >= 2
    assert result.metadata["repository_models_detected"] >= 1
    assert "candidates" in (result.metadata.get("repository_sql_tables") or [])

    route_urls = {r.url for r in result.routes}
    assert "/dashboard" in route_urls

    api_methods = {(a.method, a.url) for a in result.apis}
    assert ("GET", "/api/candidates") in api_methods or any("candidates" in u for _, u in api_methods)

    model_names = {m.name for m in result.modules}
    assert "Candidate" in model_names or "Candidates" in model_names


@patch("services.project_github_settings_service._client_for_project")
def test_index_repository_graceful_github_error(mock_client_fn):
    from services.github_repository_service import GitHubAPIError

    mock_client = MagicMock()
    mock_client.get_branch_head_sha.side_effect = GitHubAPIError("rate limit", code="rate_limit")
    mock_client_fn.return_value = ({"default_branch": "main"}, mock_client)

    result = index_repository_knowledge("demo-project")
    assert result.skipped is True
    assert result.warnings
