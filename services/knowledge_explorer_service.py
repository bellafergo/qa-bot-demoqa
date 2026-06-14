# services/knowledge_explorer_service.py
"""
Memory Explorer — navigable project knowledge graph (read-only composition).

Builds module-centric views from existing ProjectKnowledge + Failure Intelligence.
No new persistence or scoring engines.
"""
from __future__ import annotations

from typing import List, Optional, Set, Tuple

from models.failure_intelligence_models import FailureCluster
from models.project_knowledge_models import (
    ExplorerApiItem,
    ExplorerFailureClusterItem,
    ExplorerModuleCounts,
    ExplorerModuleNode,
    ExplorerRouteItem,
    ExplorerTestItem,
    KnowledgeApi,
    KnowledgeModule,
    KnowledgeRelatedTest,
    KnowledgeRoute,
    ProjectKnowledge,
    ProjectKnowledgeExplorer,
    _utc_now_iso,
)
from services.module_aliases import alias_equivalent
from services.module_canonical import canonical_module_key


def _singular(token: str) -> str:
    t = (token or "").lower()
    if len(t) > 4 and t.endswith("ies"):
        return t[:-3] + "y"
    if len(t) > 3 and t.endswith("s"):
        return t[:-1]
    return t


def modules_match(a: str, b: str) -> bool:
    if not a or not b:
        return False
    if canonical_module_key(a) == canonical_module_key(b):
        return True
    al, bl = a.lower(), b.lower()
    return alias_equivalent(al, bl) or alias_equivalent(_singular(al), _singular(bl))


def _normalize_url(url: str) -> str:
    return (url or "").strip().rstrip("/") or "/"


def _parse_api_ref(ref: str) -> Tuple[str, str]:
    """Parse 'GET /api/foo' or '/api/foo' into (method, url)."""
    raw = (ref or "").strip()
    if not raw:
        return "GET", ""
    parts = raw.split(None, 1)
    if len(parts) == 2 and parts[0].upper() in {"GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS", "UNKNOWN"}:
        return parts[0].upper(), parts[1]
    return "GET", raw


def _module_summary(
    module_name: str,
    *,
    route_count: int,
    api_count: int,
    test_count: int,
    cluster_count: int,
    entity_type: str = "",
) -> str:
    focus = module_name or "module"
    if entity_type == "prisma_model":
        base = f"Domain entity '{focus}' with linked routes, APIs, tests and failure signals."
    else:
        base = f"{focus}-related routes, APIs, tests and failures."
    parts = []
    if route_count:
        parts.append(f"{route_count} route(s)")
    if api_count:
        parts.append(f"{api_count} API(s)")
    if test_count:
        parts.append(f"{test_count} test(s)")
    if cluster_count:
        parts.append(f"{cluster_count} failure cluster(s)")
    if parts:
        return f"{base} Linked: {', '.join(parts)}."
    return f"No linked routes, APIs, tests or failure clusters detected for {focus}."


def _sort_modules(nodes: List[ExplorerModuleNode]) -> List[ExplorerModuleNode]:
    return sorted(
        nodes,
        key=lambda n: (
            -(n.counts.failure_clusters or 0),
            -(n.counts.tests or 0),
            -((n.counts.routes or 0) + (n.counts.apis or 0)),
            (n.module or "").lower(),
        ),
    )


def _routes_for_module(
    module_name: str,
    routes: List[KnowledgeRoute],
    module_route_urls: Set[str],
) -> List[ExplorerRouteItem]:
    seen: Set[str] = set()
    out: List[ExplorerRouteItem] = []
    for route in routes:
        url = _normalize_url(route.url)
        if not url:
            continue
        matched = (
            modules_match(route.module, module_name)
            or url in module_route_urls
            or any(_normalize_url(u) == url for u in module_route_urls)
        )
        if not matched or url in seen:
            continue
        seen.add(url)
        out.append(ExplorerRouteItem(
            url=url,
            file_path=route.file_path or "",
            source=route.source or "unknown",
            title=route.title or "",
        ))
    return out


def _apis_for_module(
    module_name: str,
    apis: List[KnowledgeApi],
    module_api_refs: Set[str],
) -> List[ExplorerApiItem]:
    seen: Set[str] = set()
    out: List[ExplorerApiItem] = []
    for api in apis:
        method = (api.method or "GET").upper()
        url = _normalize_url(api.url)
        key = f"{method} {url}"
        matched = modules_match(api.module, module_name)
        if not matched:
            for ref in module_api_refs:
                ref_method, ref_url = _parse_api_ref(ref)
                if ref_url and _normalize_url(ref_url) == url and (not ref_method or ref_method == method):
                    matched = True
                    break
        if not matched or key in seen:
            continue
        seen.add(key)
        out.append(ExplorerApiItem(
            method=method,
            url=url,
            file_path=api.file_path or "",
            source=api.source or "unknown",
        ))

    # Module may only have string refs without full KnowledgeApi rows
    for ref in module_api_refs:
        method, url = _parse_api_ref(ref)
        url = _normalize_url(url)
        if not url:
            continue
        key = f"{method} {url}"
        if key in seen:
            continue
        seen.add(key)
        out.append(ExplorerApiItem(method=method, url=url, source="knowledge"))
    return out


def _tests_for_module(
    module_name: str,
    related_tests: List[KnowledgeRelatedTest],
) -> List[ExplorerTestItem]:
    seen: Set[str] = set()
    out: List[ExplorerTestItem] = []
    for tc in related_tests:
        if not modules_match(tc.module, module_name):
            continue
        tid = (tc.test_case_id or "").strip()
        if not tid or tid in seen:
            continue
        seen.add(tid)
        out.append(ExplorerTestItem(
            test_case_id=tid,
            name=tc.name or "",
            last_run_status=tc.last_run_status or "",
            priority=tc.priority or "",
        ))
    return out


def _clusters_for_module(
    module_name: str,
    clusters: List[FailureCluster],
) -> List[ExplorerFailureClusterItem]:
    out: List[ExplorerFailureClusterItem] = []
    seen: Set[str] = set()
    for cluster in clusters:
        if not modules_match(cluster.module, module_name):
            continue
        cid = cluster.cluster_id or ""
        if cid in seen:
            continue
        seen.add(cid)
        out.append(ExplorerFailureClusterItem(
            cluster_id=cid,
            category=(cluster.root_cause_category or "unknown").upper(),
            occurrences=int(cluster.total_failures or 0),
            confidence=(cluster.confidence or "low").lower(),
            representative_test_case_id=cluster.representative_test_case_id or "",
        ))
    return out


def build_knowledge_explorer(
    project_id: str,
    knowledge: Optional[ProjectKnowledge],
    *,
    clusters: Optional[List[FailureCluster]] = None,
) -> ProjectKnowledgeExplorer:
    pid = (project_id or "").strip()
    now = _utc_now_iso()
    if not knowledge or not pid:
        return ProjectKnowledgeExplorer(project_id=pid, generated_at=now, modules=[])

    cluster_list = list(clusters or [])
    nodes: List[ExplorerModuleNode] = []

    for mod in knowledge.modules or []:
        module_name = mod.name or ""
        if not module_name:
            continue

        route_urls = {_normalize_url(u) for u in (mod.routes or []) if u}
        api_refs = {str(a).strip() for a in (mod.apis or []) if a}

        routes = _routes_for_module(module_name, knowledge.routes or [], route_urls)
        apis = _apis_for_module(module_name, knowledge.apis or [], api_refs)
        tests = _tests_for_module(module_name, knowledge.related_tests or [])
        failure_clusters = _clusters_for_module(module_name, cluster_list)

        counts = ExplorerModuleCounts(
            routes=len(routes),
            apis=len(apis),
            tests=len(tests),
            failure_clusters=len(failure_clusters),
        )
        nodes.append(ExplorerModuleNode(
            module=module_name,
            summary=_module_summary(
                module_name,
                route_count=counts.routes,
                api_count=counts.apis,
                test_count=counts.tests,
                cluster_count=counts.failure_clusters,
                entity_type=mod.entity_type or "",
            ),
            routes=routes,
            apis=apis,
            tests=tests,
            failure_clusters=failure_clusters,
            counts=counts,
        ))

    sorted_nodes = _sort_modules(nodes)
    default_mod = sorted_nodes[0].module if sorted_nodes else ""

    return ProjectKnowledgeExplorer(
        project_id=pid,
        generated_at=now,
        modules=sorted_nodes,
        default_expanded_module=default_mod,
    )


def get_project_knowledge_explorer(project_id: str) -> ProjectKnowledgeExplorer:
    from services.project_knowledge_service import get_project_knowledge

    pid = (project_id or "").strip()
    if not pid:
        raise ValueError("project_id is required")

    knowledge = get_project_knowledge(pid)
    clusters: List[FailureCluster] = []
    try:
        from services.failure_intelligence_service import failure_intelligence_service
        clusters = failure_intelligence_service.get_clusters(project_id=pid, limit=200)
    except Exception:
        clusters = []

    return build_knowledge_explorer(pid, knowledge, clusters=clusters)
