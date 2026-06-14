# services/repository_knowledge_relations.py
"""
Module ↔ route ↔ API ↔ test relationship builder (Phase B).

Pure functions — reuses canonical module logic and change-impact scoring.
"""
from __future__ import annotations

from collections import defaultdict
from typing import Dict, Iterable, List, Optional, Set, Tuple

from models.project_knowledge_models import (
    KnowledgeApi,
    KnowledgeModule,
    KnowledgeRelatedTest,
    KnowledgeRoute,
    _utc_now_iso,
)
from services.change_impact_service import _MATCH_THRESHOLD, _score_token_module
from services.module_aliases import alias_equivalent
from services.module_canonical import (
    build_module_label_map,
    canonical_module_key,
    module_display_label,
)
from services.repository_knowledge_extractors import path_tokens_from_url


def _singular(token: str) -> str:
    t = (token or "").lower()
    if len(t) > 4 and t.endswith("ies"):
        return t[:-3] + "y"
    if len(t) > 3 and t.endswith("s"):
        return t[:-1]
    return t


def _best_module_for_tokens(
    tokens: Iterable[str],
    known_modules: List[str],
) -> Tuple[str, float]:
    best_mod = ""
    best_score = 0.0
    for token in tokens:
        for mod in known_modules:
            sc = _score_token_module(token, mod)
            if sc > best_score:
                best_score = sc
                best_mod = mod
    if best_score >= _MATCH_THRESHOLD:
        return best_mod, best_score
    return "", best_score


def infer_module_for_url(url: str, known_modules: List[str]) -> str:
    tokens = path_tokens_from_url(url)
    mod, _ = _best_module_for_tokens(tokens, known_modules)
    return mod


def infer_module_for_prisma_model(model_name: str, known_modules: List[str]) -> str:
    if not model_name:
        return ""
    candidates = [
        model_name,
        _singular(model_name),
        model_name.lower(),
        _singular(model_name.lower()),
    ]
    best_mod = ""
    best_score = 0.0
    for token in candidates:
        for mod in known_modules:
            sc = _score_token_module(token, mod)
            if sc > best_score:
                best_score = sc
                best_mod = mod
    if best_score >= _MATCH_THRESHOLD:
        return best_mod
    return ""


def _module_alias_key(name: str) -> str:
    return canonical_module_key(name)


def _modules_equivalent(a: str, b: str) -> bool:
    if not a or not b:
        return False
    if _module_alias_key(a) == _module_alias_key(b):
        return True
    return alias_equivalent(a.lower(), b.lower()) or alias_equivalent(_singular(a), _singular(b))


def consolidate_modules_by_alias(modules: List[KnowledgeModule]) -> List[KnowledgeModule]:
    """Merge modules that alias-match (catalog + prisma domain entities)."""
    if not modules:
        return []
    groups: List[List[KnowledgeModule]] = []
    for mod in modules:
        placed = False
        for group in groups:
            if _modules_equivalent(mod.name, group[0].name):
                group.append(mod)
                placed = True
                break
        if not placed:
            groups.append([mod])

    raw_names = [m.name for m in modules]
    label_map = build_module_label_map(raw_names)
    merged: List[KnowledgeModule] = []

    for group in groups:
        catalog_first = sorted(
            group,
            key=lambda m: (
                0 if m.source == "catalog" else 1,
                -(m.test_count or 0),
                -len(m.fields or []),
            ),
        )
        base = catalog_first[0]
        key = canonical_module_key(base.name)
        display = module_display_label(key, labels_by_key=label_map)

        routes: List[str] = []
        apis: List[str] = []
        fields: List[str] = []
        relations: List[str] = []
        test_count = 0
        entity_type = ""
        file_path = ""
        source = base.source
        last_seen = base.last_seen_at

        for m in group:
            routes.extend(m.routes or [])
            apis.extend(m.apis or [])
            fields.extend(m.fields or [])
            relations.extend(m.relations or [])
            test_count = max(test_count, m.test_count or 0)
            if m.entity_type and not entity_type:
                entity_type = m.entity_type
            if m.file_path and not file_path:
                file_path = m.file_path
            if m.source == "catalog":
                source = "catalog"
            elif m.source == "repository" and source != "catalog":
                source = "repository"
            if m.last_seen_at:
                last_seen = m.last_seen_at

        merged.append(KnowledgeModule(
            name=display,
            test_count=test_count,
            routes=list(dict.fromkeys(routes))[:30],
            apis=list(dict.fromkeys(apis))[:30],
            source=source,
            entity_type=entity_type or (base.entity_type or ""),
            fields=list(dict.fromkeys(fields))[:40],
            relations=list(dict.fromkeys(relations))[:20],
            file_path=file_path,
            last_seen_at=last_seen or _utc_now_iso(),
        ))
    return merged


def link_entities_to_modules(
    modules: List[KnowledgeModule],
    routes: List[KnowledgeRoute],
    apis: List[KnowledgeApi],
    *,
    related_tests: Optional[List[KnowledgeRelatedTest]] = None,
) -> Tuple[List[KnowledgeModule], List[KnowledgeRoute], List[KnowledgeApi]]:
    """
    Infer module links on routes/apis and enrich modules with routes, apis, test evidence.
    """
    known_modules = [m.name for m in modules if m.name]
    for tc in related_tests or []:
        if tc.module and tc.module.strip() and tc.module not in known_modules:
            known_modules.append(tc.module.strip())

    label_map = build_module_label_map(known_modules)
    known_display = [module_display_label(canonical_module_key(m), labels_by_key=label_map) for m in known_modules]
    known_display = list(dict.fromkeys(known_display))

    linked_routes: List[KnowledgeRoute] = []
    for r in routes:
        mod = r.module or infer_module_for_url(r.url, known_display)
        linked_routes.append(r.model_copy(update={"module": mod}))

    linked_apis: List[KnowledgeApi] = []
    for a in apis:
        mod = a.module or infer_module_for_url(a.url, known_display)
        linked_apis.append(a.model_copy(update={"module": mod}))

    mod_routes: Dict[str, List[str]] = defaultdict(list)
    mod_apis: Dict[str, List[str]] = defaultdict(list)
    mod_tests: Dict[str, int] = defaultdict(int)

    for r in linked_routes:
        if r.module and r.url:
            mod_routes[r.module.lower()].append(r.url)
    for a in linked_apis:
        if a.module and a.url:
            key = f"{(a.method or 'GET').upper()} {a.url}"
            mod_apis[a.module.lower()].append(key)
    for tc in related_tests or []:
        if tc.module:
            mod_tests[tc.module.lower()] += 1

    enriched: List[KnowledgeModule] = []
    seen_mod_keys: Set[str] = set()
    for m in modules:
        key = m.name.lower()
        seen_mod_keys.add(key)
        extra_routes = mod_routes.get(key, [])
        extra_apis = mod_apis.get(key, [])
        extra_tests = mod_tests.get(key, 0)
        enriched.append(m.model_copy(update={
            "routes": list(dict.fromkeys((m.routes or []) + extra_routes))[:30],
            "apis": list(dict.fromkeys((m.apis or []) + extra_apis))[:30],
            "test_count": max(m.test_count or 0, extra_tests),
        }))

    for mod_name in known_display:
        key = mod_name.lower()
        if key in seen_mod_keys:
            continue
        routes_for = mod_routes.get(key, [])
        apis_for = mod_apis.get(key, [])
        tests_for = mod_tests.get(key, 0)
        if routes_for or apis_for or tests_for:
            enriched.append(KnowledgeModule(
                name=mod_name,
                test_count=tests_for,
                routes=list(dict.fromkeys(routes_for))[:30],
                apis=list(dict.fromkeys(apis_for))[:30],
                source="catalog",
                last_seen_at=_utc_now_iso(),
            ))

    return consolidate_modules_by_alias(enriched), linked_routes, linked_apis
