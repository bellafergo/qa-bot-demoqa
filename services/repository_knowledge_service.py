# services/repository_knowledge_service.py
"""
Repository knowledge indexer — GitHub tree scan + extractors (Phase B).

Opt-in via ``include_repository`` on knowledge refresh.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Set

from models.project_knowledge_models import (
    KnowledgeApi,
    KnowledgeModule,
    KnowledgeRoute,
    _utc_now_iso,
)
from services.github_repository_service import GitHubAPIError
from services.repository_knowledge_extractors import (
    classify_repository_paths,
    extract_api_endpoints,
    extract_sql_table_names,
    extract_ui_routes_from_paths,
    parse_prisma_schema,
)
from services.repository_knowledge_models import RepositoryIndexResult
from services.repository_knowledge_relations import (
    consolidate_modules_by_alias,
    infer_module_for_prisma_model,
    link_entities_to_modules,
)

logger = logging.getLogger("vanya.repository_knowledge")

_MAX_CONTENT_FILES = 80
_MAX_CONTENT_BYTES = 500_000
_TREE_MAX_PATHS = 15000


def _empty_result(*, skipped: bool = True, warnings: Optional[List[str]] = None) -> RepositoryIndexResult:
    return RepositoryIndexResult(
        skipped=skipped,
        warnings=warnings or [],
        metadata={
            "repository_indexed": False,
            "repository_files_scanned": 0,
            "repository_routes_detected": 0,
            "repository_apis_detected": 0,
            "repository_models_detected": 0,
            "repository_workflows_detected": 0,
            "repository_warnings": warnings or [],
        },
    )


def _select_content_paths(buckets: Dict[str, List[str]]) -> List[str]:
    seen: Set[str] = set()
    ordered: List[str] = []
    for path in buckets.get("content_fetch") or []:
        if path not in seen:
            seen.add(path)
            ordered.append(path)
    return ordered[:_MAX_CONTENT_FILES]


def index_repository_knowledge(
    project_id: str,
    *,
    known_module_names: Optional[List[str]] = None,
    related_tests: Optional[List[Any]] = None,
) -> RepositoryIndexResult:
    """
    Index connected GitHub repository into knowledge entities.

    Returns empty/skipped result when GitHub is not configured.
    """
    pid = (project_id or "").strip()
    warnings: List[str] = []
    now = _utc_now_iso()

    try:
        from services.project_github_settings_service import _client_for_project

        gh_settings, client = _client_for_project(pid)
    except (LookupError, ValueError) as e:
        logger.info("repository index skipped project_id=%s reason=%s", pid, e)
        return _empty_result(skipped=True, warnings=[str(e)])
    except Exception:
        logger.exception("repository index client init failed project_id=%s", pid)
        return _empty_result(skipped=True, warnings=["github_client_init_failed"])

    branch = str(gh_settings.get("default_branch") or "main").strip() or "main"
    try:
        head_sha = client.get_branch_head_sha(branch)
        paths, truncated = client.list_tree_recursive(head_sha, max_paths=_TREE_MAX_PATHS)
    except GitHubAPIError as e:
        logger.warning("repository index github error project_id=%s: %s", pid, e)
        return _empty_result(skipped=True, warnings=[str(e)])

    if truncated:
        warnings.append("tree_truncated")

    buckets = classify_repository_paths(paths)
    ui_paths = (
        buckets["ui_app_router"]
        + buckets["ui_pages_router"]
        + buckets["ui_vite"]
    )
    api_paths = (
        buckets["api_next_app"]
        + buckets["api_next_pages"]
        + buckets["api_python"]
        + buckets["api_node"]
    )

    parsed_ui = extract_ui_routes_from_paths(ui_paths)
    routes: List[KnowledgeRoute] = [
        KnowledgeRoute(
            url=r.url,
            title=r.title,
            source="repository",
            last_seen_at=now,
            file_path=r.file_path,
        )
        for r in parsed_ui
    ]

    apis: List[KnowledgeApi] = []
    modules: List[KnowledgeModule] = []
    sql_tables: List[str] = []
    content_paths = _select_content_paths(buckets)
    content_fetched = 0
    content_bytes = 0
    budget_exceeded = False

    file_contents: Dict[str, str] = {}
    for path in content_paths:
        if content_fetched >= _MAX_CONTENT_FILES or content_bytes >= _MAX_CONTENT_BYTES:
            budget_exceeded = True
            break
        try:
            text = client.get_file_text(path, ref=head_sha)
        except GitHubAPIError as e:
            warnings.append(f"content_fetch_failed:{path}")
            logger.debug("content fetch failed path=%s err=%s", path, e)
            continue
        if not text:
            continue
        size = len(text.encode("utf-8", errors="replace"))
        if content_bytes + size > _MAX_CONTENT_BYTES:
            budget_exceeded = True
            break
        file_contents[path] = text
        content_fetched += 1
        content_bytes += size

    if budget_exceeded:
        warnings.append("content_budget_exceeded")

    known = list(known_module_names or [])

    for path in api_paths:
        endpoints = extract_api_endpoints(path, content=file_contents.get(path))
        for ep in endpoints:
            apis.append(KnowledgeApi(
                url=ep.url,
                method=ep.method,
                source="repository",
                last_seen_at=now,
                file_path=ep.file_path,
            ))

    prisma_paths = buckets.get("prisma_schema") or []
    if not prisma_paths:
        warnings.append("prisma_schema_not_found")

    for path in prisma_paths:
        schema_text = file_contents.get(path)
        if not schema_text:
            try:
                schema_text = client.get_file_text(path, ref=head_sha)
            except GitHubAPIError:
                warnings.append(f"prisma_fetch_failed:{path}")
                continue
        for model in parse_prisma_schema(schema_text, file_path=path):
            linked_mod = infer_module_for_prisma_model(model.name, known)
            modules.append(KnowledgeModule(
                name=model.name,
                source="repository",
                entity_type="prisma_model",
                fields=model.fields,
                relations=model.relations,
                file_path=path,
                last_seen_at=now,
            ))
            if linked_mod:
                known.append(linked_mod)

    for path in buckets.get("sql_files") or []:
        text = file_contents.get(path)
        if not text:
            continue
        for table in extract_sql_table_names(text):
            if table not in sql_tables:
                sql_tables.append(table)

    modules = consolidate_modules_by_alias(modules)
    modules, routes, apis = link_entities_to_modules(
        modules,
        routes,
        apis,
        related_tests=related_tests or [],
    )

    metadata: Dict[str, Any] = {
        "repository_indexed": True,
        "repository_files_scanned": len(paths),
        "repository_routes_detected": len(routes),
        "repository_apis_detected": len(apis),
        "repository_models_detected": sum(
            1 for m in modules if (m.entity_type or "") == "prisma_model"
        ),
        "repository_workflows_detected": 0,
        "repository_warnings": warnings,
        "repository_branch": branch,
        "repository_indexed_at": now,
        "repository_content_files_fetched": content_fetched,
        "repository_sql_tables": sql_tables[:50],
    }

    return RepositoryIndexResult(
        routes=routes,
        apis=apis,
        modules=modules,
        metadata=metadata,
        skipped=False,
        warnings=warnings,
    )
