# services/app_knowledge_graph.py
"""
Merge helpers for the per-project App Knowledge Graph (Phase 1).

Pure functions — no I/O. Deep-merge URL-keyed entities and recompute risk.
"""
from __future__ import annotations

import re
from typing import Any, Dict, Iterable, List, Optional, Tuple
from urllib.parse import urlparse

from models.project_knowledge_models import (
    KnowledgeApi,
    KnowledgeFailureEntry,
    KnowledgeForm,
    KnowledgeIncidentEntry,
    KnowledgeModule,
    KnowledgeRelatedTest,
    KnowledgeRoute,
    KnowledgeTable,
    KnowledgeWorkflow,
    ProjectKnowledge,
    _utc_now_iso,
)

_MAX_LIST = 200
_API_PATH_RE = re.compile(r"/api|/v[12]/", re.I)


def _clip(s: str, n: int = 400) -> str:
    t = (s or "").strip()
    return t if len(t) <= n else t[: n - 1] + "…"


def _norm_url(url: str) -> str:
    u = (url or "").strip()
    if not u:
        return ""
    try:
        p = urlparse(u)
        if not p.scheme or not p.netloc:
            return u.rstrip("/") or u
        path = p.path.rstrip("/") or "/"
        return f"{p.scheme.lower()}://{p.netloc.lower()}{path}"
    except Exception:
        return u


def _dedupe_key_route(r: KnowledgeRoute) -> str:
    return _norm_url(r.url)


def _dedupe_key_api(a: KnowledgeApi) -> str:
    return f"{(a.method or 'GET').upper()} {_norm_url(a.url)}"


def merge_routes(existing: List[KnowledgeRoute], incoming: Iterable[KnowledgeRoute]) -> List[KnowledgeRoute]:
    by_key: Dict[str, KnowledgeRoute] = {_dedupe_key_route(r): r for r in existing if r.url}
    for r in incoming:
        k = _dedupe_key_route(r)
        if not k:
            continue
        prev = by_key.get(k)
        if prev:
            merged = prev.model_copy(update={
                "title": r.title or prev.title,
                "page_types": list(dict.fromkeys((prev.page_types or []) + (r.page_types or [])))[:10],
                "source": r.source or prev.source,
                "last_seen_at": r.last_seen_at or prev.last_seen_at,
            })
            by_key[k] = merged
        else:
            by_key[k] = r
    return list(by_key.values())[:_MAX_LIST]


def merge_modules(existing: List[KnowledgeModule], incoming: Iterable[KnowledgeModule]) -> List[KnowledgeModule]:
    by_name: Dict[str, KnowledgeModule] = {(m.name or "").lower(): m for m in existing if m.name}
    for m in incoming:
        key = (m.name or "").lower()
        if not key:
            continue
        prev = by_name.get(key)
        if prev:
            by_name[key] = prev.model_copy(update={
                "test_count": max(prev.test_count, m.test_count),
                "routes": list(dict.fromkeys((prev.routes or []) + (m.routes or [])))[:20],
                "source": m.source or prev.source,
                "last_seen_at": m.last_seen_at or prev.last_seen_at,
            })
        else:
            by_name[key] = m
    return list(by_name.values())[:_MAX_LIST]


def merge_forms(existing: List[KnowledgeForm], incoming: Iterable[KnowledgeForm]) -> List[KnowledgeForm]:
    out: Dict[str, KnowledgeForm] = {}
    for f in list(existing) + list(incoming):
        key = f"{_norm_url(f.url)}|{(f.name or '').lower()}"
        if key == "|":
            continue
        prev = out.get(key)
        if prev:
            out[key] = prev.model_copy(update={
                "fields": list(dict.fromkeys((prev.fields or []) + (f.fields or [])))[:30],
                "source": f.source or prev.source,
                "last_seen_at": f.last_seen_at or prev.last_seen_at,
            })
        else:
            out[key] = f
    return list(out.values())[:_MAX_LIST]


def merge_tables(existing: List[KnowledgeTable], incoming: Iterable[KnowledgeTable]) -> List[KnowledgeTable]:
    out: Dict[str, KnowledgeTable] = {}
    for t in list(existing) + list(incoming):
        key = f"{_norm_url(t.url)}|{(t.name or '').lower()}"
        if key == "|":
            continue
        prev = out.get(key)
        if prev:
            out[key] = prev.model_copy(update={
                "columns": list(dict.fromkeys((prev.columns or []) + (t.columns or [])))[:30],
                "source": t.source or prev.source,
                "last_seen_at": t.last_seen_at or prev.last_seen_at,
            })
        else:
            out[key] = t
    return list(out.values())[:_MAX_LIST]


def merge_apis(existing: List[KnowledgeApi], incoming: Iterable[KnowledgeApi]) -> List[KnowledgeApi]:
    by_key: Dict[str, KnowledgeApi] = {_dedupe_key_api(a): a for a in existing if a.url}
    for a in incoming:
        k = _dedupe_key_api(a)
        if not k.strip():
            continue
        prev = by_key.get(k)
        if prev:
            by_key[k] = prev.model_copy(update={
                "source": a.source or prev.source,
                "last_seen_at": a.last_seen_at or prev.last_seen_at,
            })
        else:
            by_key[k] = a
    return list(by_key.values())[:_MAX_LIST]


def merge_workflows(existing: List[KnowledgeWorkflow], incoming: Iterable[KnowledgeWorkflow]) -> List[KnowledgeWorkflow]:
    by_name: Dict[str, KnowledgeWorkflow] = {(w.name or "").lower(): w for w in existing if w.name}
    for w in incoming:
        key = (w.name or "").lower()
        if not key:
            continue
        prev = by_name.get(key)
        if prev:
            by_name[key] = prev.model_copy(update={
                "steps": list(dict.fromkeys((prev.steps or []) + (w.steps or [])))[:15],
                "source": w.source or prev.source,
                "last_seen_at": w.last_seen_at or prev.last_seen_at,
            })
        else:
            by_name[key] = w
    return list(by_name.values())[:_MAX_LIST]


def merge_related_tests(
    existing: List[KnowledgeRelatedTest],
    incoming: Iterable[KnowledgeRelatedTest],
) -> List[KnowledgeRelatedTest]:
    by_id: Dict[str, KnowledgeRelatedTest] = {t.test_case_id: t for t in existing if t.test_case_id}
    for t in incoming:
        if not t.test_case_id:
            continue
        prev = by_id.get(t.test_case_id)
        if prev:
            by_id[t.test_case_id] = prev.model_copy(update={
                "name": t.name or prev.name,
                "module": t.module or prev.module,
                "test_type": t.test_type or prev.test_type,
                "priority": t.priority or prev.priority,
                "last_run_status": t.last_run_status or prev.last_run_status,
                "last_run_at": t.last_run_at or prev.last_run_at,
            })
        else:
            by_id[t.test_case_id] = t
    return list(by_id.values())[:_MAX_LIST]


def replace_failure_history(entries: List[KnowledgeFailureEntry]) -> List[KnowledgeFailureEntry]:
    return entries[:_MAX_LIST]


def replace_incident_history(entries: List[KnowledgeIncidentEntry]) -> List[KnowledgeIncidentEntry]:
    return entries[:50]


def compute_risk_score(
    *,
    failure_history: List[KnowledgeFailureEntry],
    incident_history: List[KnowledgeIncidentEntry],
    run_fail_rate: float = 0.0,
) -> float:
    score = 0.0
    score += min(40.0, len(failure_history) * 4.0)
    score += min(25.0, sum(e.count for e in failure_history) * 1.5)
    for inc in incident_history[:10]:
        sev = (inc.severity or "").lower()
        if sev == "critical":
            score += 8.0
        elif sev == "high":
            score += 5.0
        elif sev == "medium":
            score += 3.0
        else:
            score += 1.0
    score += min(20.0, run_fail_rate * 100.0 * 0.2)
    return round(min(100.0, score), 1)


def explorer_page_to_entities(page: Dict[str, Any], *, source: str = "explorer") -> Tuple[
    List[KnowledgeRoute],
    List[KnowledgeForm],
    List[KnowledgeTable],
    List[KnowledgeApi],
    List[Dict[str, Any]],
]:
    """Normalize ``application_explorer`` / multi-page inventory into graph entities."""
    now = _utc_now_iso()
    url = _clip(str(page.get("url") or ""), 2048)
    title = _clip(str(page.get("title") or ""), 200)
    routes: List[KnowledgeRoute] = []
    forms: List[KnowledgeForm] = []
    tables: List[KnowledgeTable] = []
    apis: List[KnowledgeApi] = []
    components: List[Dict[str, Any]] = []

    if url:
        routes.append(KnowledgeRoute(url=url, title=title, source=source, last_seen_at=now))

    for form in page.get("forms") or []:
        if not isinstance(form, dict):
            continue
        fields = [str(x) for x in (form.get("fields") or []) if x][:20]
        forms.append(KnowledgeForm(
            name=_clip(str(form.get("name") or ""), 120),
            url=url,
            fields=fields,
            source=source,
            last_seen_at=now,
        ))

    for inp in page.get("inputs") or []:
        if isinstance(inp, dict) and inp.get("selector"):
            components.append({
                "kind": "input",
                "name": _clip(str(inp.get("name") or ""), 120),
                "selector": _clip(str(inp.get("selector") or ""), 200),
                "url": url,
                "source": source,
            })
    for btn in page.get("buttons") or []:
        if isinstance(btn, dict) and btn.get("selector"):
            components.append({
                "kind": "button",
                "name": _clip(str(btn.get("name") or ""), 120),
                "selector": _clip(str(btn.get("selector") or ""), 200),
                "url": url,
                "source": source,
            })

    for link in page.get("links") or []:
        if not isinstance(link, dict):
            continue
        href = _clip(str(link.get("href") or link.get("text") or ""), 400)
        if href.startswith("http"):
            routes.append(KnowledgeRoute(
                url=href,
                title=_clip(str(link.get("text") or ""), 120),
                source=source,
                last_seen_at=now,
            ))
            if _API_PATH_RE.search(href):
                apis.append(KnowledgeApi(url=href, source=source, last_seen_at=now))

    return routes, forms, tables, apis, components[:100]


def app_map_to_entities(app_map: Dict[str, Any], *, source: str = "app_map") -> Tuple[
    List[KnowledgeRoute],
    List[KnowledgeForm],
    List[KnowledgeTable],
    List[KnowledgeWorkflow],
    List[KnowledgeRoute],
]:
    now = _utc_now_iso()
    url = _clip(str(app_map.get("final_url") or app_map.get("url") or ""), 2048)
    page_types = [str(x) for x in (app_map.get("page_type") or []) if x]
    routes = [KnowledgeRoute(url=url, title="", page_types=page_types, source=source, last_seen_at=now)] if url else []

    forms: List[KnowledgeForm] = []
    for f in app_map.get("forms") or []:
        if not isinstance(f, dict):
            continue
        fields = [str(x) for x in (f.get("fields") or f.get("field_labels") or []) if x][:20]
        forms.append(KnowledgeForm(
            name=_clip(str(f.get("name") or f.get("label") or ""), 120),
            url=url,
            fields=fields,
            source=source,
            last_seen_at=now,
        ))

    tables: List[KnowledgeTable] = []
    for t in app_map.get("tables") or []:
        if not isinstance(t, dict):
            continue
        cols = [str(x) for x in (t.get("columns") or t.get("headers") or []) if x][:20]
        tables.append(KnowledgeTable(
            name=_clip(str(t.get("name") or ""), 120),
            url=url,
            columns=cols,
            source=source,
            last_seen_at=now,
        ))

    workflows: List[KnowledgeWorkflow] = []
    for i, flow in enumerate(app_map.get("suggested_test_flows") or []):
        if not flow:
            continue
        workflows.append(KnowledgeWorkflow(
            name=_clip(f"flow_{i + 1}", 80),
            steps=[_clip(str(flow), 300)],
            source=source,
            last_seen_at=now,
        ))

    nav_routes: List[KnowledgeRoute] = []
    for nav in app_map.get("main_navigation") or []:
        if not isinstance(nav, dict):
            continue
        href = _clip(str(nav.get("href") or ""), 400)
        if href.startswith("http"):
            nav_routes.append(KnowledgeRoute(
                url=href,
                title=_clip(str(nav.get("text") or ""), 120),
                source=source,
                last_seen_at=now,
            ))

    return routes, forms, tables, workflows, nav_routes


def apply_patch(knowledge: ProjectKnowledge, patch: Dict[str, Any]) -> ProjectKnowledge:
    """Merge a partial patch into existing knowledge."""
    data = knowledge.model_dump()
    now = _utc_now_iso()
    data["updated_at"] = now

    if patch.get("routes"):
        data["routes"] = [r.model_dump() for r in merge_routes(
            [KnowledgeRoute.model_validate(x) for x in data.get("routes") or []],
            [KnowledgeRoute.model_validate(x) if isinstance(x, dict) else x for x in patch["routes"]],
        )]
    if patch.get("modules"):
        data["modules"] = [m.model_dump() for m in merge_modules(
            [KnowledgeModule.model_validate(x) for x in data.get("modules") or []],
            [KnowledgeModule.model_validate(x) if isinstance(x, dict) else x for x in patch["modules"]],
        )]
    if patch.get("forms"):
        data["forms"] = [f.model_dump() for f in merge_forms(
            [KnowledgeForm.model_validate(x) for x in data.get("forms") or []],
            [KnowledgeForm.model_validate(x) if isinstance(x, dict) else x for x in patch["forms"]],
        )]
    if patch.get("tables"):
        data["tables"] = [t.model_dump() for t in merge_tables(
            [KnowledgeTable.model_validate(x) for x in data.get("tables") or []],
            [KnowledgeTable.model_validate(x) if isinstance(x, dict) else x for x in patch["tables"]],
        )]
    if patch.get("apis"):
        data["apis"] = [a.model_dump() for a in merge_apis(
            [KnowledgeApi.model_validate(x) for x in data.get("apis") or []],
            [KnowledgeApi.model_validate(x) if isinstance(x, dict) else x for x in patch["apis"]],
        )]
    if patch.get("workflows"):
        data["workflows"] = [w.model_dump() for w in merge_workflows(
            [KnowledgeWorkflow.model_validate(x) for x in data.get("workflows") or []],
            [KnowledgeWorkflow.model_validate(x) if isinstance(x, dict) else x for x in patch["workflows"]],
        )]
    if patch.get("related_tests"):
        data["related_tests"] = [t.model_dump() for t in merge_related_tests(
            [KnowledgeRelatedTest.model_validate(x) for x in data.get("related_tests") or []],
            [KnowledgeRelatedTest.model_validate(x) if isinstance(x, dict) else x for x in patch["related_tests"]],
        )]
    if patch.get("components"):
        existing = data.get("components") or []
        incoming = patch["components"] or []
        combined = (existing + incoming)[:100]
        data["components"] = combined
    if patch.get("failure_history") is not None:
        data["failure_history"] = [e.model_dump() for e in replace_failure_history(
            [KnowledgeFailureEntry.model_validate(x) if isinstance(x, dict) else x for x in patch["failure_history"]],
        )]
    if patch.get("incident_history") is not None:
        data["incident_history"] = [e.model_dump() for e in replace_incident_history(
            [KnowledgeIncidentEntry.model_validate(x) if isinstance(x, dict) else x for x in patch["incident_history"]],
        )]
    if patch.get("metadata"):
        meta = dict(data.get("metadata") or {})
        meta.update(patch["metadata"])
        data["metadata"] = meta

    pk = ProjectKnowledge.model_validate(data)
    pk.risk_score = compute_risk_score(
        failure_history=pk.failure_history,
        incident_history=pk.incident_history,
        run_fail_rate=float((pk.metadata or {}).get("run_fail_rate") or 0.0),
    )
    return pk
