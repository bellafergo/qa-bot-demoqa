# services/business_workflow_service.py
"""
Business Workflow Intelligence — deterministic functional process inference.

Composes workflows from existing ProjectKnowledge + Failure Intelligence.
No LLM, no new persistence, no scoring changes.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple

from models.failure_intelligence_models import FailureCluster
from models.project_knowledge_models import (
    BusinessWorkflowCoverage,
    BusinessWorkflowItem,
    KnowledgeApi,
    KnowledgeRelatedTest,
    KnowledgeRoute,
    ProjectKnowledge,
    ProjectKnowledgeWorkflows,
    _utc_now_iso,
)
from services.knowledge_explorer_service import modules_match


@dataclass(frozen=True)
class WorkflowPattern:
    workflow_type: str
    name: str
    tokens: Tuple[str, ...]


WORKFLOW_PATTERNS: Tuple[WorkflowPattern, ...] = (
    WorkflowPattern("authentication", "Authentication Flow", ("auth", "login", "session", "signin", "sign-in", "logout")),
    WorkflowPattern("candidate_lifecycle", "Candidate Lifecycle", ("candidate", "candidates", "candidato", "candidatos", "cv", "resume", "applicant")),
    WorkflowPattern("vacancy_recruiting", "Vacancy / Recruiting Flow", ("vacancy", "vacancies", "vacante", "vacantes", "job", "position")),
    WorkflowPattern("opportunity_sales", "Opportunity / Sales Flow", ("opportunity", "opportunities", "oportunidad", "lead", "prospect")),
    WorkflowPattern("company_contact", "Company / Contact Management", ("company", "companies", "empresa", "contact", "contacto")),
    WorkflowPattern("proposal_pdf", "Proposal / PDF Flow", ("proposal", "pdf", "document", "export")),
    WorkflowPattern("matching", "Matching Flow", ("matching", "match", "score", "recommendation")),
    WorkflowPattern("admin_settings", "Admin / Settings Flow", ("admin", "settings", "configuration", "users", "roles")),
)

_CONFIDENCE_RANK = {"high": 3, "medium": 2, "low": 1}

_SUMMARY_BY_TYPE: Dict[str, str] = {
    "authentication": "Login and session validation flow supported by {modules} routes, APIs, tests and recent failures.",
    "candidate_lifecycle": "Candidate management lifecycle supported by {modules} routes, APIs, tests and recent failures.",
    "vacancy_recruiting": "Vacancy and recruiting flow supported by {modules} routes, APIs, tests and recent failures.",
    "opportunity_sales": "Opportunity and sales flow supported by {modules} routes, APIs, tests and recent failures.",
    "company_contact": "Company and contact management flow supported by {modules} routes, APIs, tests and recent failures.",
    "proposal_pdf": "Proposal and document export flow supported by {modules} routes, APIs, tests and recent failures.",
    "matching": "Matching and recommendation flow supported by {modules} routes, APIs, tests and recent failures.",
    "admin_settings": "Administration and settings flow supported by {modules} routes, APIs, tests and recent failures.",
}


def _normalize_url(url: str) -> str:
    return (url or "").strip().rstrip("/") or "/"


def _api_key(api: KnowledgeApi) -> str:
    return f"{(api.method or 'GET').upper()} {_normalize_url(api.url)}"


def text_matches_tokens(text: str, tokens: Tuple[str, ...]) -> bool:
    lower = (text or "").lower().replace("_", "-")
    if not lower:
        return False
    return any(token in lower for token in tokens)


def compute_confidence(
    *,
    routes: Set[str],
    apis: Set[str],
    tests: Set[str],
    clusters: Set[str],
    structural_count: int,
) -> str:
    total = structural_count + len(clusters)
    has_route_api_test = bool(routes and apis and tests)
    if total >= 4 or has_route_api_test:
        return "high"
    if total >= 3:
        return "medium"
    return "low"


def _workflow_summary(pattern: WorkflowPattern, modules: List[str]) -> str:
    template = _SUMMARY_BY_TYPE.get(
        pattern.workflow_type,
        "{name} supported by {modules} routes, APIs, tests and recent failures.",
    )
    mod_label = ", ".join(modules[:3]) if modules else "related"
    return template.format(name=pattern.name, modules=mod_label)


def _cluster_relates_to_workflow(
    cluster: FailureCluster,
    *,
    modules: Set[str],
    routes: Set[str],
    apis: Set[str],
    tests: Set[str],
    tokens: Tuple[str, ...],
) -> bool:
    if cluster.module:
        for mod in modules:
            if modules_match(cluster.module, mod):
                return True
    blob = " ".join([
        cluster.module or "",
        cluster.root_cause_category or "",
        cluster.representative_test_case_id or "",
        cluster.cluster_id or "",
    ])
    if text_matches_tokens(blob, tokens):
        return True
    if cluster.representative_test_case_id and cluster.representative_test_case_id in tests:
        return True
    for api in apis:
        if cluster.module and text_matches_tokens(f"{cluster.module} {api}", tokens):
            return True
    for route in routes:
        if text_matches_tokens(route, tokens) and cluster.module:
            return True
    return False


def infer_workflow_for_pattern(
    pattern: WorkflowPattern,
    knowledge: ProjectKnowledge,
    clusters: List[FailureCluster],
) -> Optional[BusinessWorkflowItem]:
    matched_modules: Set[str] = set()
    related_modules: Set[str] = set()
    routes: Set[str] = set()
    apis: Set[str] = set()
    tests: Set[str] = set()

    for mod in knowledge.modules or []:
        if text_matches_tokens(mod.name or "", pattern.tokens):
            name = (mod.name or "").strip()
            if name:
                matched_modules.add(name)
                related_modules.add(name)

    for route in knowledge.routes or []:
        blob = f"{route.url} {route.title} {route.module} {route.file_path}"
        if text_matches_tokens(blob, pattern.tokens):
            url = _normalize_url(route.url)
            if url:
                routes.add(url)
                if route.module:
                    related_modules.add(route.module)

    for api in knowledge.apis or []:
        blob = f"{api.method} {api.url} {api.module} {api.file_path}"
        if text_matches_tokens(blob, pattern.tokens):
            key = _api_key(api)
            if api.url:
                apis.add(key)
                if api.module:
                    related_modules.add(api.module)

    for tc in knowledge.related_tests or []:
        blob = f"{tc.test_case_id} {tc.name} {tc.module}"
        if text_matches_tokens(blob, pattern.tokens):
            tid = (tc.test_case_id or "").strip()
            if tid:
                tests.add(tid)
                if tc.module:
                    related_modules.add(tc.module)

    structural_count = len(matched_modules) + len(routes) + len(apis) + len(tests)
    evidence_types = sum(
        1 for bucket in (matched_modules, routes, apis, tests) if bucket
    )
    if evidence_types < 2 or structural_count < 2:
        return None

    failure_clusters: Set[str] = set()
    for cluster in clusters or []:
        if _cluster_relates_to_workflow(
            cluster,
            modules=related_modules,
            routes=routes,
            apis=apis,
            tests=tests,
            tokens=pattern.tokens,
        ):
            cid = (cluster.cluster_id or "").strip()
            if cid:
                failure_clusters.add(cid)

    confidence = compute_confidence(
        routes=routes,
        apis=apis,
        tests=tests,
        clusters=failure_clusters,
        structural_count=structural_count,
    )

    sorted_modules = sorted(related_modules, key=str.lower)
    sorted_routes = sorted(routes, key=str.lower)
    sorted_apis = sorted(apis, key=str.lower)
    sorted_tests = sorted(tests, key=str.lower)
    sorted_clusters = sorted(failure_clusters, key=str.lower)

    return BusinessWorkflowItem(
        name=pattern.name,
        type=pattern.workflow_type,
        confidence=confidence,
        modules=sorted_modules,
        routes=sorted_routes,
        apis=sorted_apis,
        tests=sorted_tests,
        failure_clusters=sorted_clusters,
        coverage=BusinessWorkflowCoverage(
            routes=len(sorted_routes),
            apis=len(sorted_apis),
            tests=len(sorted_tests),
            clusters=len(sorted_clusters),
        ),
        summary=_workflow_summary(pattern, sorted_modules),
    )


def sort_workflows(workflows: List[BusinessWorkflowItem]) -> List[BusinessWorkflowItem]:
    return sorted(
        workflows,
        key=lambda w: (
            -(w.coverage.clusters or 0),
            -(w.coverage.tests or 0),
            -_CONFIDENCE_RANK.get((w.confidence or "low").lower(), 1),
            (w.name or "").lower(),
        ),
    )


def build_business_workflows(
    project_id: str,
    knowledge: Optional[ProjectKnowledge],
    *,
    clusters: Optional[List[FailureCluster]] = None,
) -> ProjectKnowledgeWorkflows:
    pid = (project_id or "").strip()
    now = _utc_now_iso()
    if not knowledge or not pid:
        return ProjectKnowledgeWorkflows(project_id=pid, generated_at=now, workflows=[])

    cluster_list = list(clusters or [])
    workflows: List[BusinessWorkflowItem] = []
    for pattern in WORKFLOW_PATTERNS:
        item = infer_workflow_for_pattern(pattern, knowledge, cluster_list)
        if item:
            workflows.append(item)

    return ProjectKnowledgeWorkflows(
        project_id=pid,
        generated_at=now,
        workflows=sort_workflows(workflows),
    )


def get_project_knowledge_workflows(project_id: str) -> ProjectKnowledgeWorkflows:
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

    return build_business_workflows(pid, knowledge, clusters=clusters)
