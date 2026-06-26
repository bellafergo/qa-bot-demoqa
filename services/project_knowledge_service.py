# services/project_knowledge_service.py
"""
System Memory / App Knowledge Graph orchestration (Phase 1).

Aggregates existing Vanya data per project without duplicating raw stores.
Write hooks: Explorer, App Map, Runs. Read hooks: Incident Investigator, UI refresh.
"""
from __future__ import annotations

import logging
from collections import defaultdict
from typing import Any, Dict, List, Optional

from models.project_knowledge_models import (
    KnowledgeFailureEntry,
    KnowledgeIncidentEntry,
    KnowledgeModule,
    KnowledgeRelatedTest,
    KnowledgeRoute,
    ProjectKnowledge,
    ProjectKnowledgeContext,
    ProjectKnowledgeRefreshRequest,
    _utc_now_iso,
)
from services.app_knowledge_graph import (
    app_map_to_entities,
    assemble_fresh_knowledge,
    compute_risk_score,
    explorer_page_to_entities,
    inspection_summaries_to_entities,
)
from services.repository_knowledge_relations import link_entities_to_modules
from services.project_memory_service import get_memory, get_or_create, merge_memory_patch, save_memory
from services.pr_analysis_project_debug import log_project_id_lookup

logger = logging.getLogger("vanya.project_knowledge")


def _resolve_project_name(project_id: str) -> str:
    try:
        from services.db.project_repository import project_repo

        proj = project_repo.get_project(project_id)
        if proj is not None:
            return str(getattr(proj, "name", None) or (proj.get("name") if isinstance(proj, dict) else "") or project_id)
    except Exception as e:
        logger.debug("project name lookup failed: %s", e)
    return project_id


def ingest_explorer_result(
    project_id: str,
    exploration: Dict[str, Any],
    *,
    source: str = "explorer",
) -> Optional[ProjectKnowledge]:
    pid = (project_id or "").strip()
    if not pid or not isinstance(exploration, dict):
        return None
    try:
        routes: List[KnowledgeRoute] = []
        forms = []
        tables = []
        apis = []
        components: List[Dict[str, Any]] = []

        for page in exploration.get("pages") or []:
            if not isinstance(page, dict):
                continue
            r, f, t, a, c = explorer_page_to_entities(page, source=source)
            routes.extend(r)
            forms.extend(f)
            tables.extend(t)
            apis.extend(a)
            components.extend(c)

        start = (exploration.get("start_url") or "").strip()
        if start:
            routes.append(KnowledgeRoute(url=start, title="start", source=source, last_seen_at=_utc_now_iso()))

        return merge_memory_patch(
            pid,
            {
                "routes": [x.model_dump() for x in routes],
                "forms": [x.model_dump() for x in forms],
                "tables": [x.model_dump() for x in tables],
                "apis": [x.model_dump() for x in apis],
                "components": components,
                "metadata": {
                    "last_explorer_at": _utc_now_iso(),
                    "last_explorer_pages": int(exploration.get("visited_count") or len(exploration.get("pages") or [])),
                },
            },
            project_name=_resolve_project_name(pid),
        )
    except Exception:
        logger.exception("ingest_explorer_result failed project_id=%s", pid)
        return None


def ingest_app_map(project_id: str, app_map: Dict[str, Any], *, source: str = "app_map") -> Optional[ProjectKnowledge]:
    pid = (project_id or "").strip()
    if not pid or not isinstance(app_map, dict):
        return None
    try:
        routes, forms, tables, workflows, nav_routes = app_map_to_entities(app_map, source=source)
        routes = routes + nav_routes
        return merge_memory_patch(
            pid,
            {
                "routes": [x.model_dump() for x in routes],
                "forms": [x.model_dump() for x in forms],
                "tables": [x.model_dump() for x in tables],
                "workflows": [x.model_dump() for x in workflows],
                "metadata": {
                    "last_app_map_at": _utc_now_iso(),
                    "last_app_map_url": app_map.get("final_url") or app_map.get("url"),
                },
            },
            project_name=_resolve_project_name(pid),
        )
    except Exception:
        logger.exception("ingest_app_map failed project_id=%s", pid)
        return None


def ingest_run_completed(project_id: str, *, test_case_id: str, status: str, module: str = "", test_name: str = "") -> None:
    """Lightweight hook after catalog/run persistence — updates test metrics only."""
    pid = (project_id or "").strip()
    if not pid or not test_case_id:
        return
    try:
        now = _utc_now_iso()
        related = [KnowledgeRelatedTest(
            test_case_id=test_case_id,
            name=test_name,
            module=module,
            last_run_status=status,
            last_run_at=now,
        ).model_dump()]
        meta_patch: Dict[str, Any] = {"last_run_ingest_at": now}
        if status in ("fail", "error"):
            meta_patch["recent_fail_count"] = int((get_memory(pid) or ProjectKnowledge(project_id=pid)).metadata.get("recent_fail_count") or 0) + 1
        merge_memory_patch(pid, {"related_tests": related, "metadata": meta_patch}, project_name=_resolve_project_name(pid))
    except Exception:
        logger.exception("ingest_run_completed failed project_id=%s test_case_id=%s", pid, test_case_id)


def ingest_incident_completed(project_id: str, incident: Dict[str, Any]) -> None:
    pid = (project_id or "").strip()
    if not pid or not isinstance(incident, dict):
        return
    try:
        entry = KnowledgeIncidentEntry(
            id=str(incident.get("id") or ""),
            description=str(incident.get("incident_description") or "")[:400],
            severity=str(incident.get("severity") or "info"),
            suspected_area=str(incident.get("suspected_area") or "unknown"),
            target_url=str(incident.get("target_url") or "")[:400],
            created_at=str(incident.get("created_at") or _utc_now_iso()),
        )
        existing = get_memory(pid) or get_or_create(pid)
        history = [e.model_dump() for e in existing.incident_history]
        history = [entry.model_dump()] + [h for h in history if h.get("id") != entry.id][:49]
        merge_memory_patch(pid, {"incident_history": history}, project_name=_resolve_project_name(pid))
    except Exception:
        logger.exception("ingest_incident_completed failed project_id=%s", pid)


def _collect_discovery_from_inspection_history(project_id: str) -> Dict[str, List[Any]]:
    """Rebuild routes/forms/workflows/apis/components from persisted browser inspection rows."""
    pid = (project_id or "").strip()
    out: Dict[str, List[Any]] = {
        "routes": [],
        "forms": [],
        "tables": [],
        "apis": [],
        "workflows": [],
        "components": [],
    }
    if not pid:
        return out
    try:
        from services.db.test_run_repository import test_run_repo

        rows = test_run_repo.list_browser_inspection_runs(project_id=pid, limit=100)
        for row in rows or []:
            meta = getattr(row, "meta", None) or {}
            if not isinstance(meta, dict):
                continue
            bis = meta.get("browser_inspection_summary")
            ams = meta.get("app_map_summary")
            executed_at = ""
            ex = getattr(row, "executed_at", None)
            if hasattr(ex, "isoformat"):
                executed_at = ex.isoformat()
            elif ex:
                executed_at = str(ex)
            src = str(meta.get("source") or "browser_inspection").strip() or "browser_inspection"
            chunk = inspection_summaries_to_entities(
                url="",
                title=getattr(row, "test_name", None) or "",
                executed_at=executed_at,
                browser_inspection_summary=bis if isinstance(bis, dict) else None,
                app_map_summary=ams if isinstance(ams, dict) else None,
                source=src,
            )
            for key in out:
                out[key].extend(chunk.get(key) or [])
    except Exception:
        logger.exception("refresh: browser inspection history failed project_id=%s", pid)
    return out


def refresh_project_knowledge(
    project_id: str,
    req: Optional[ProjectKnowledgeRefreshRequest] = None,
    *,
    include_repository_explicit: Optional[bool] = None,
) -> ProjectKnowledge:
    """Rebuild derived slices from existing stores (no duplicate raw data)."""
    from services.project_knowledge_refresh_utils import resolve_refresh_request

    pid = (project_id or "").strip()
    if not pid:
        raise ValueError("project_id is required")
    opts = resolve_refresh_request(pid, req, include_repository_explicit=include_repository_explicit)
    mode = (opts.mode or "replace").strip().lower()
    if mode not in ("replace", "merge"):
        raise ValueError("mode must be 'replace' or 'merge'")
    name = _resolve_project_name(pid)
    existing = get_memory(pid)

    modules: List[KnowledgeModule] = []
    related_tests: List[KnowledgeRelatedTest] = []
    routes: List[KnowledgeRoute] = []
    forms: List[Any] = []
    tables: List[Any] = []
    apis: List[Any] = []
    workflows: List[Any] = []
    components: List[Dict[str, Any]] = []

    if opts.include_catalog:
        try:
            from services.db.catalog_repository import catalog_repo

            cases = catalog_repo.list_test_cases(project_id=pid, limit=500)
            mod_counts: Dict[str, int] = defaultdict(int)
            raw_modules: List[str] = []
            from services.module_canonical import (
                build_module_label_map,
                canonical_module_key,
                module_display_label,
            )

            tc_rows: List[tuple] = []
            for tc in cases or []:
                tid = str(getattr(tc, "test_case_id", None) or (tc.get("test_case_id") if isinstance(tc, dict) else "") or "")
                mod = str(getattr(tc, "module", None) or (tc.get("module") if isinstance(tc, dict) else "") or "general")
                raw_modules.append(mod)
                mod_key = canonical_module_key(mod)
                mod_counts[mod_key] += 1
                base = str(getattr(tc, "base_url", None) or (tc.get("base_url") if isinstance(tc, dict) else "") or "")
                tc_rows.append((tid, mod, mod_key, base, tc))
            label_map = build_module_label_map(raw_modules)
            for tid, mod, mod_key, base, tc in tc_rows:
                if base:
                    routes.append(KnowledgeRoute(
                        url=base,
                        title=module_display_label(mod_key, labels_by_key=label_map),
                        source="catalog",
                        last_seen_at=_utc_now_iso(),
                    ))
                related_tests.append(KnowledgeRelatedTest(
                    test_case_id=tid,
                    name=str(getattr(tc, "name", None) or (tc.get("name") if isinstance(tc, dict) else "") or ""),
                    module=mod,
                    test_type=str(getattr(tc, "test_type", None) or (tc.get("test_type") if isinstance(tc, dict) else "") or "ui"),
                    priority=str(getattr(tc, "priority", None) or (tc.get("priority") if isinstance(tc, dict) else "") or ""),
                ))
            for mod_key, count in mod_counts.items():
                modules.append(KnowledgeModule(
                    name=module_display_label(mod_key, labels_by_key=label_map),
                    test_count=count,
                    source="catalog",
                    last_seen_at=_utc_now_iso(),
                ))
        except Exception:
            logger.exception("refresh: catalog failed project_id=%s", pid)

    failure_history: List[KnowledgeFailureEntry] = []
    run_fail_rate = 0.0
    if opts.include_runs or opts.include_failures:
        try:
            from services.run_history_service import run_history_service

            runs = run_history_service.list_runs(limit=100, project_id=pid)
            statuses = [str(getattr(r, "status", None) or (r.get("status") if isinstance(r, dict) else "") or "") for r in runs or []]
            if statuses:
                fails = sum(1 for s in statuses if s in ("fail", "error"))
                run_fail_rate = fails / len(statuses)
        except Exception:
            logger.exception("refresh: runs metrics failed project_id=%s", pid)

    if opts.include_failures:
        try:
            from services.failure_intelligence_service import failure_intelligence_service

            regressions = failure_intelligence_service.get_regressions(project_id=pid)
            for reg in regressions or []:
                failure_history.append(KnowledgeFailureEntry(
                    test_case_id=str(getattr(reg, "test_case_id", None) or ""),
                    test_name=str(getattr(reg, "summary", None) or reg.test_case_id if hasattr(reg, "test_case_id") else ""),
                    module=str(getattr(reg, "module", None) or ""),
                    failure_type="regression",
                    count=int(getattr(reg, "repeated_failures", None) or 1),
                ))
        except Exception:
            logger.exception("refresh: failure intelligence failed project_id=%s", pid)

    incident_history: List[KnowledgeIncidentEntry] = []
    if opts.include_incidents:
        try:
            from services.db.incident_investigation_repository import incident_investigation_repo

            rows = incident_investigation_repo.list_runs(limit=30, project_id=pid)
            incident_history = []
            for row in rows or []:
                if not isinstance(row, dict):
                    continue
                incident_history.append(KnowledgeIncidentEntry(
                    id=str(row.get("id") or ""),
                    description=str(row.get("incident_description") or "")[:400],
                    severity=str(row.get("severity") or "info"),
                    suspected_area=str(row.get("suspected_area") or "unknown"),
                    target_url=str(row.get("target_url") or "")[:400],
                    created_at=str(row.get("created_at") or ""),
                ))
        except Exception:
            logger.exception("refresh: incidents failed project_id=%s", pid)

    try:
        from services.db.project_repository import project_repo

        proj = project_repo.get_project(pid)
        if proj is not None:
            bu = getattr(proj, "base_url", None) or (proj.get("base_url") if isinstance(proj, dict) else None)
            if bu:
                routes.append(KnowledgeRoute(url=str(bu), title="project_base", source="project", last_seen_at=_utc_now_iso()))
    except Exception:
        pass

    discovery_rows = 0
    if opts.include_discovery:
        disc = _collect_discovery_from_inspection_history(pid)
        routes.extend(disc["routes"])
        forms.extend(disc["forms"])
        tables.extend(disc["tables"])
        apis.extend(disc["apis"])
        workflows.extend(disc["workflows"])
        components.extend(disc["components"])
        discovery_rows = len(disc["routes"])

    reconstruction_sources: List[str] = []
    if opts.include_catalog:
        reconstruction_sources.append("catalog")
    if opts.include_discovery:
        reconstruction_sources.append("browser_inspection")
    if opts.include_runs:
        reconstruction_sources.append("runs")
    if opts.include_failures:
        reconstruction_sources.append("failure_intelligence")
    if opts.include_incidents:
        reconstruction_sources.append("incidents")

    repository_meta: Dict[str, Any] = {}
    if opts.include_repository:
        try:
            from services.repository_knowledge_service import index_repository_knowledge

            known_names = [m.name for m in modules if m.name]
            repo = index_repository_knowledge(
                pid,
                known_module_names=known_names,
                related_tests=related_tests,
            )
            routes.extend(repo.routes)
            apis.extend(repo.apis)
            modules.extend(repo.modules)
            repository_meta = dict(repo.metadata or {})
            if repo.warnings:
                repository_meta["repository_warnings"] = repo.warnings
            if repo.skipped and repo.warnings:
                from services.project_knowledge_refresh_utils import github_app_configuration_diagnostic

                repository_meta["repository_index_skipped"] = True
                repository_meta["repository_index_diagnostic"] = github_app_configuration_diagnostic()
            if not repo.skipped:
                reconstruction_sources.append("repository")
        except Exception:
            logger.exception("refresh: repository index failed project_id=%s", pid)
            repository_meta = {
                "repository_indexed": False,
                "repository_warnings": ["repository_index_failed"],
            }

    modules, routes, apis = link_entities_to_modules(
        modules,
        routes,
        apis,
        related_tests=related_tests,
    )

    refresh_meta: Dict[str, Any] = {
        "last_refresh_at": _utc_now_iso(),
        "run_fail_rate": round(run_fail_rate, 4),
        "refresh_mode": mode,
        "reconstruction_sources": reconstruction_sources,
        "discovery_inspection_rows": discovery_rows,
        "explorer_reconstructible": False,
        "reconstruction_notes": (
            "Explorer (/app-explorer) HTML inventories are not persisted in test_runs; "
            "only browser_inspection + app_map_summary rows are rebuilt on refresh. "
            "Full forms/tables field lists require live ingest hooks."
        ),
        **repository_meta,
    }

    if mode == "replace":
        updated = assemble_fresh_knowledge(
            project_id=pid,
            project_name=name,
            modules=modules,
            routes=routes,
            forms=forms,
            tables=tables,
            apis=apis,
            workflows=workflows,
            components=components,
            related_tests=related_tests,
            failure_history=failure_history,
            incident_history=incident_history,
            metadata=refresh_meta,
            run_fail_rate=run_fail_rate,
        )
        return save_memory(_enrich_with_risk(updated))

    # merge — additive refresh (legacy behaviour)
    knowledge = existing or get_or_create(pid, project_name=name)
    patch = {
        "project_name": name,
        "modules": [m.model_dump() for m in modules],
        "routes": [r.model_dump() for r in routes],
        "forms": [f.model_dump() if hasattr(f, "model_dump") else f for f in forms],
        "tables": [t.model_dump() if hasattr(t, "model_dump") else t for t in tables],
        "apis": [a.model_dump() if hasattr(a, "model_dump") else a for a in apis],
        "workflows": [w.model_dump() if hasattr(w, "model_dump") else w for w in workflows],
        "components": components,
        "related_tests": [t.model_dump() for t in related_tests],
        "failure_history": [f.model_dump() for f in failure_history],
        "incident_history": [i.model_dump() for i in incident_history],
        "metadata": {
            **(knowledge.metadata or {}),
            **refresh_meta,
        },
    }
    updated = merge_memory_patch(pid, patch, project_name=name)
    updated.risk_score = compute_risk_score(
        failure_history=updated.failure_history,
        incident_history=updated.incident_history,
        run_fail_rate=run_fail_rate,
    )
    return save_memory(_enrich_with_risk(updated))


def get_knowledge_context(
    project_id: Optional[str],
    *,
    target_url: Optional[str] = None,
    incident_description: str = "",
) -> Optional[ProjectKnowledgeContext]:
    pid = (project_id or "").strip()
    if not pid:
        return None
    mem = get_memory(pid)
    if not mem:
        mem = refresh_project_knowledge(pid, ProjectKnowledgeRefreshRequest(include_runs=False))
    else:
        mem = _enrich_with_risk(mem)

    hints: List[str] = []
    module_risk_hints: List[str] = []
    modules = [m.name for m in mem.modules if m.name][:15]
    known_routes = [r.url for r in mem.routes if r.url][:20]
    related = [t.test_case_id for t in mem.related_tests if t.test_case_id][:15]

    desc_lower = (incident_description or "").lower()
    url_lower = (target_url or "").lower()
    for mod in modules:
        if mod.lower() in desc_lower:
            hints.append(f"Incident mentions known module '{mod}' ({next((m.test_count for m in mem.modules if m.name == mod), 0)} tests).")
    for fail in mem.failure_history[:5]:
        if fail.module and fail.module.lower() in desc_lower:
            hints.append(f"Module '{fail.module}' has recent regression history ({fail.test_name or fail.test_case_id}).")

    for mr in (mem.module_risks or [])[:8]:
        if mr.module_risk_level in ("HIGH", "CRITICAL"):
            if mr.module.lower() in desc_lower or mr.module.lower() in url_lower:
                module_risk_hints.append(
                    f"Module '{mr.module}' has {mr.module_risk_level} risk "
                    f"({mr.module_risk_score:.0f}/100)."
                )
    if mem.risk_level in ("HIGH", "CRITICAL"):
        hints.append(
            f"Project risk level is {mem.risk_level} ({mem.risk_score:.0f}/100)."
        )
    if url_lower:
        for r in mem.routes:
            if r.url and r.url.lower() in url_lower or url_lower in r.url.lower():
                hints.append(f"Target URL matches known route '{r.title or r.url}' (source: {r.source}).")

    recent_failures = [
        f"{f.test_name or f.test_case_id} ({f.module})" for f in mem.failure_history[:5] if f.test_case_id or f.test_name
    ]
    recent_incidents = [
        f"{i.description[:80]}…" if len(i.description) > 80 else i.description
        for i in mem.incident_history[:5] if i.description
    ]

    all_hints = (module_risk_hints + hints)[:8]
    return ProjectKnowledgeContext(
        project_id=pid,
        modules=modules,
        known_routes=known_routes,
        related_tests=related,
        recent_failures=recent_failures,
        recent_incidents=recent_incidents,
        risk_score=mem.risk_score,
        risk_level=mem.risk_level,
        module_risk_hints=module_risk_hints[:5],
        hints=all_hints,
    )


def _enrich_with_risk(knowledge: ProjectKnowledge) -> ProjectKnowledge:
    try:
        from services.project_risk_service import apply_risk_to_knowledge
        return apply_risk_to_knowledge(knowledge)
    except Exception:
        logger.exception("risk enrichment failed project_id=%s", knowledge.project_id)
        return knowledge


def get_project_knowledge(project_id: str) -> Optional[ProjectKnowledge]:
    log_project_id_lookup(project_id=project_id)
    mem = get_memory((project_id or "").strip())
    log_project_id_lookup(project_id=project_id, memory_found=mem is not None)
    if not mem:
        return None
    return _enrich_with_risk(mem)
