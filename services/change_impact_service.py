# services/change_impact_service.py
"""
Change Impact Service — deterministic file path → catalog module mapping.

Uses project-specific data only (catalog + System Memory). No hardcoded product
domains (Zuperio, DemoQA, etc.).
"""
from __future__ import annotations

import logging
import os
import re
from typing import Dict, List, Optional, Set, Tuple

from models.project_knowledge_models import ProjectKnowledge
from services.module_aliases import alias_equivalent

logger = logging.getLogger("vanya.change_impact")

_TOKEN_SPLIT_RE = re.compile(r"[/\\._\-\s]+")
_MIN_TOKEN_LEN = 3
_MATCH_THRESHOLD = 0.65
_DEBUG_PREFIX = "PR_ANALYSIS_DEBUG"


def _normalize(s: str) -> str:
    return (s or "").strip().lower()


def _singular(token: str) -> str:
    t = token.lower()
    if len(t) > 4 and t.endswith("ies"):
        return t[:-3] + "y"
    if len(t) > 3 and t.endswith("s"):
        return t[:-1]
    return t


def _tokenize_path(file_path: str) -> List[str]:
    path = (file_path or "").replace("\\", "/").strip()
    if not path:
        return []
    tokens: List[str] = []
    for part in _TOKEN_SPLIT_RE.split(path):
        p = part.strip()
        if len(p) >= _MIN_TOKEN_LEN:
            tokens.append(p.lower())
    base = os.path.basename(path)
    stem, _ext = os.path.splitext(base)
    if stem and len(stem) >= _MIN_TOKEN_LEN:
        tokens.append(stem.lower())
    # Strip common suffixes for component/service names
    cleaned: List[str] = []
    for t in tokens:
        cleaned.append(t)
        for suffix in ("form", "page", "service", "component", "controller", "route", "routes", "api", "view", "model", "repository", "repo", "handler"):
            if t.endswith(suffix) and len(t) > len(suffix) + 2:
                cleaned.append(t[: -len(suffix)])
    return list(dict.fromkeys(cleaned))


def _score_token_module(token: str, module: str) -> float:
    t = _normalize(token)
    m = _normalize(module)
    if not t or not m:
        return 0.0
    if t == m:
        return 1.0
    if t in m or m in t:
        return 0.85
    ts, ms = _singular(t), _singular(m)
    if ts == ms:
        return 0.8
    if ts in ms or ms in ts:
        return 0.75
    if alias_equivalent(t, m) or alias_equivalent(ts, ms):
        return 0.8
    return 0.0


def _debug_log(lines: str) -> None:
    logger.debug("%s\n%s", _DEBUG_PREFIX, lines)


def _memory_module_names(knowledge: Optional[ProjectKnowledge]) -> List[str]:
    names: Set[str] = set()
    if knowledge:
        for mod in knowledge.modules or []:
            if mod.name and mod.name.strip():
                names.add(mod.name.strip())
        for tc in knowledge.related_tests or []:
            if tc.module and tc.module.strip():
                names.add(tc.module.strip())
    return sorted(names, key=str.lower)


def _alias_match_for_pair(token: str, module: str) -> bool:
    t = _normalize(token)
    m = _normalize(module)
    if not t or not m:
        return False
    ts, ms = _singular(t), _singular(m)
    return alias_equivalent(t, m) or alias_equivalent(ts, ms)


def _known_modules(knowledge: Optional[ProjectKnowledge], catalog_modules: List[str]) -> List[str]:
    names: Set[str] = set()
    for m in catalog_modules:
        if m and m.strip():
            names.add(m.strip())
    if knowledge:
        for mod in knowledge.modules or []:
            if mod.name and mod.name.strip():
                names.add(mod.name.strip())
        for tc in knowledge.related_tests or []:
            if tc.module and tc.module.strip():
                names.add(tc.module.strip())
    return sorted(names, key=str.lower)


def _route_hints(knowledge: Optional[ProjectKnowledge]) -> Dict[str, str]:
    """Map route URL segment → module name (from knowledge.modules.routes)."""
    hints: Dict[str, str] = {}
    if not knowledge:
        return hints
    mod_routes: Dict[str, List[str]] = {}
    for mod in knowledge.modules or []:
        if mod.name:
            mod_routes[_normalize(mod.name)] = [r for r in (mod.routes or []) if r]
    for mod in knowledge.modules or []:
        mod_name = mod.name or ""
        for route in mod.routes or []:
            seg = _route_segment(route)
            if seg:
                hints[seg] = mod_name
    for route in knowledge.routes or []:
        seg = _route_segment(route.url)
        if seg and seg not in hints:
            for mod_name, routes in mod_routes.items():
                if any(seg in _normalize(r) for r in routes):
                    hints[seg] = mod_name
                    break
    return hints


def _route_segment(url: str) -> str:
    u = (url or "").strip().lower()
    if not u:
        return ""
    try:
        from urllib.parse import urlparse
        path = urlparse(u).path.strip("/")
        if not path:
            return ""
        return path.split("/")[0]
    except Exception:
        parts = [p for p in u.split("/") if p and p not in ("http:", "https:")]
        return parts[-1] if parts else ""


def map_file_to_modules(
    file_path: str,
    *,
    known_modules: List[str],
    knowledge: Optional[ProjectKnowledge] = None,
) -> List[Tuple[str, float, str]]:
    """
    Return ranked module matches for a single file path.

    Each tuple: (module_name, confidence 0–1, match_reason).
    """
    if not file_path or not known_modules:
        if file_path and logger.isEnabledFor(logging.DEBUG):
            _debug_log(f"file={file_path}\ntokens=[]\nknown_modules={known_modules!r}\nresult=unmapped (no known modules)")
        return []

    tokens = _tokenize_path(file_path)
    path_lower = file_path.replace("\\", "/").lower()
    matches: Dict[str, Tuple[float, str]] = {}

    if logger.isEnabledFor(logging.DEBUG):
        _debug_log(
            f"file={file_path}\n"
            f"tokens={tokens!r}\n"
            f"known_modules={known_modules!r}"
        )

    route_hints = _route_hints(knowledge)
    for seg, mod in route_hints.items():
        if seg and seg in path_lower:
            prev = matches.get(mod)
            if not prev or prev[0] < 0.7:
                matches[mod] = (0.7, f"route segment '{seg}'")

    for mod in known_modules:
        best_score = 0.0
        best_token = ""
        best_alias_match = False
        for token in tokens:
            sc = _score_token_module(token, mod)
            alias_match = _alias_match_for_pair(token, mod)
            if sc > best_score:
                best_score = sc
                best_token = token
                best_alias_match = alias_match
        if logger.isEnabledFor(logging.DEBUG):
            _debug_log(
                f"file={file_path}\n"
                f"tokens={tokens!r}\n"
                f"module={mod!r}\n"
                f"score={best_score}\n"
                f"alias_match={'true' if best_alias_match else 'false'}"
            )
        if best_score >= _MATCH_THRESHOLD:
            prev = matches.get(mod)
            if not prev or best_score > prev[0]:
                matches[mod] = (best_score, f"path token '{best_token}'")

    if knowledge:
        for tc in knowledge.related_tests or []:
            mod = tc.module or ""
            if not mod:
                continue
            for key in (tc.test_case_id, tc.name):
                k = _normalize(str(key or ""))
                if len(k) >= _MIN_TOKEN_LEN and k in path_lower:
                    prev = matches.get(mod)
                    if not prev or prev[0] < 0.72:
                        matches[mod] = (0.72, f"related test '{key}'")

    ranked = sorted(matches.items(), key=lambda x: (-x[1][0], x[0].lower()))
    result = [(mod, sc, reason) for mod, (sc, reason) in ranked]
    if logger.isEnabledFor(logging.DEBUG):
        _debug_log(
            f"file={file_path}\n"
            f"file_mappings={result!r}"
        )
    return result


def map_changed_files(
    changed_files: List[str],
    *,
    knowledge: Optional[ProjectKnowledge],
    catalog_modules: Optional[List[str]] = None,
) -> Dict[str, List[Tuple[str, float, str]]]:
    """Map each changed file to ranked module matches."""
    catalog = [m for m in (catalog_modules or []) if m and m.strip()]
    memory_modules = _memory_module_names(knowledge)
    modules = _known_modules(knowledge, catalog)
    if logger.isEnabledFor(logging.DEBUG):
        _debug_log(
            f"changed_files={changed_files!r}\n"
            f"catalog_modules={catalog!r}\n"
            f"memory_modules={memory_modules!r}\n"
            f"known_modules={modules!r}"
        )
    out: Dict[str, List[Tuple[str, float, str]]] = {}
    for f in changed_files or []:
        fp = (f or "").strip()
        if not fp:
            continue
        out[fp] = map_file_to_modules(fp, known_modules=modules, knowledge=knowledge)
    return out


def resolve_impacted_modules(
    file_mappings: Dict[str, List[Tuple[str, float, str]]],
) -> List[Tuple[str, List[str], float]]:
    """
    Aggregate file mappings → unique impacted modules.

    Returns: [(module, [files], best_confidence), ...] sorted by confidence desc.
    """
    agg: Dict[str, Dict] = {}
    for file_path, matches in file_mappings.items():
        if not matches:
            continue
        mod, conf, _reason = matches[0]
        entry = agg.setdefault(mod, {"files": [], "best": 0.0})
        entry["files"].append(file_path)
        entry["best"] = max(entry["best"], conf)

    result = [
        (mod, data["files"], data["best"])
        for mod, data in agg.items()
    ]
    result.sort(key=lambda x: (-x[2], x[0].lower()))
    if logger.isEnabledFor(logging.DEBUG):
        _debug_log(f"impacted_modules={result!r}")
    return result
