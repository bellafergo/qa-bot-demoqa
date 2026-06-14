# services/repository_knowledge_extractors.py
"""
Pure extractors for repository knowledge (Phase B).

No I/O — path classification and content parsing only.
"""
from __future__ import annotations

import re
from typing import Dict, List, Optional, Set, Tuple

from services.repository_knowledge_models import ParsedApiEndpoint, ParsedPrismaModel, ParsedUiRoute

_SKIP_PREFIXES = (
    "node_modules/",
    "dist/",
    ".next/",
    "build/",
    "coverage/",
    ".git/",
    "vendor/",
)

_UI_PAGE_EXTS = frozenset({".tsx", ".jsx", ".js"})
_API_ROUTE_EXTS = frozenset({".ts", ".js"})
_PYTHON_EXT = ".py"

_ROUTE_GROUP_RE = re.compile(r"\([^)]+\)/?")
_DYNAMIC_SEG_RE = re.compile(r"\[[^\]]+\]")

_NEXT_EXPORT_METHOD_RE = re.compile(
    r"export\s+(?:async\s+)?function\s+(GET|POST|PUT|PATCH|DELETE|HEAD|OPTIONS)\b",
    re.I,
)
_FASTAPI_DECORATOR_RE = re.compile(
    r"@(?:router|app)\.(get|post|put|patch|delete|head|options)\s*\(\s*[\"']([^\"']*)[\"']",
    re.I,
)
_FASTAPI_API_ROUTE_RE = re.compile(
    r"@(?:router|app)\.api_route\s*\(\s*[\"']([^\"']*)[\"'][^)]*methods\s*=\s*\[([^\]]+)\]",
    re.I,
)
_ROUTER_PREFIX_RE = re.compile(
    r"(?:APIRouter|router)\s*\([^)]*prefix\s*=\s*[\"']([^\"']+)[\"']",
    re.I,
)
_INCLUDE_ROUTER_PREFIX_RE = re.compile(
    r"include_router\s*\([^,]+,\s*prefix\s*=\s*[\"']([^\"']+)[\"']",
    re.I,
)

_PRISMA_MODEL_RE = re.compile(r"^\s*model\s+(\w+)\s*\{", re.M)
_PRISMA_FIELD_RE = re.compile(r"^\s*(\w+)\s+\w+", re.M)
_PRISMA_RELATION_TYPE_RE = re.compile(r"^\s*(\w+)\s+(\w+)\s+@relation", re.M)
_PRISMA_RELATION_FIELD_RE = re.compile(r"^\s*(\w+)\s+(\w+)(?:\[\])?\s*$", re.M)

_SQL_CREATE_TABLE_RE = re.compile(
    r"CREATE\s+TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?[`\"']?(\w+)[`\"']?",
    re.I,
)
_SQL_ALTER_TABLE_RE = re.compile(
    r"ALTER\s+TABLE\s+[`\"']?(\w+)[`\"']?",
    re.I,
)

_PRISMA_SCHEMA_PATHS = (
    "prisma/schema.prisma",
    "db/schema.prisma",
    "schema.prisma",
)


def should_skip_path(path: str) -> bool:
    p = (path or "").replace("\\", "/").strip().lower()
    if not p:
        return True
    return any(p.startswith(prefix) for prefix in _SKIP_PREFIXES)


def classify_repository_paths(paths: List[str]) -> Dict[str, List[str]]:
    """Bucket repo paths into extraction categories."""
    buckets: Dict[str, List[str]] = {
        "ui_app_router": [],
        "ui_pages_router": [],
        "ui_vite": [],
        "api_next_app": [],
        "api_next_pages": [],
        "api_python": [],
        "api_node": [],
        "prisma_schema": [],
        "sql_files": [],
        "content_fetch": [],
    }
    prisma_found: Set[str] = set()

    for raw in paths:
        path = (raw or "").replace("\\", "/").strip()
        if not path or should_skip_path(path):
            continue
        lower = path.lower()

        if lower in _PRISMA_SCHEMA_PATHS:
            buckets["prisma_schema"].append(path)
            prisma_found.add(lower)
            buckets["content_fetch"].append(path)
            continue

        if lower.endswith("/page.tsx") or lower.endswith("/page.jsx"):
            if lower.startswith("app/"):
                buckets["ui_app_router"].append(path)
            continue

        if lower.startswith("pages/"):
            if lower.startswith("pages/api/"):
                if lower.endswith((".ts", ".js")):
                    buckets["api_next_pages"].append(path)
                    buckets["content_fetch"].append(path)
            elif any(lower.endswith(ext) for ext in _UI_PAGE_EXTS):
                buckets["ui_pages_router"].append(path)
            continue

        if lower.startswith("src/pages/") or lower.startswith("src/routes/"):
            if any(lower.endswith(ext) for ext in (".tsx", ".jsx")):
                buckets["ui_vite"].append(path)
            continue

        if lower.startswith("app/api/") and lower.endswith(("/route.ts", "/route.js")):
            buckets["api_next_app"].append(path)
            buckets["content_fetch"].append(path)
            continue

        if lower.startswith("api/routes/") and lower.endswith(_PYTHON_EXT):
            buckets["api_python"].append(path)
            buckets["content_fetch"].append(path)
            continue

        if lower.startswith("routes/") and lower.endswith(_PYTHON_EXT):
            if not lower.startswith("routes/__"):
                buckets["api_python"].append(path)
                buckets["content_fetch"].append(path)
            continue

        if (lower.startswith("server/") or lower.startswith("src/server/")) and lower.endswith(".ts"):
            buckets["api_node"].append(path)
            buckets["content_fetch"].append(path)
            continue

        if lower.endswith(".sql") and "/migrations/" in lower:
            buckets["sql_files"].append(path)
            if len(buckets["sql_files"]) <= 20:
                buckets["content_fetch"].append(path)

    return buckets


def app_router_path_to_url(file_path: str) -> str:
    """Convert app/foo/(group)/bar/page.tsx → /foo/bar."""
    p = (file_path or "").replace("\\", "/").strip()
    if p.lower().startswith("app/"):
        p = p[4:]
    for ext in ("page.tsx", "page.jsx", "page.js"):
        if p.lower().endswith(ext):
            p = p[: -len(ext)]
            break
    for ext in ("/page.tsx", "/page.jsx", "/page.js"):
        if p.lower().endswith(ext):
            p = p[: -len(ext)]
            break
    p = _ROUTE_GROUP_RE.sub("", p)
    p = p.strip("/")
    if not p:
        return "/"
    segments = [s for s in p.split("/") if s]
    return "/" + "/".join(segments)


def pages_router_path_to_url(file_path: str) -> str:
    """Convert pages/companies/index.tsx → /companies."""
    p = (file_path or "").replace("\\", "/").strip()
    if p.lower().startswith("pages/"):
        p = p[6:]
    for ext in _UI_PAGE_EXTS:
        if p.lower().endswith(ext):
            p = p[: -len(ext)]
            break
    if p.lower() == "index" or p.lower().endswith("/index"):
        p = p[: -len("/index")] if p.lower().endswith("/index") else ""
    p = p.strip("/")
    if not p:
        return "/"
    return "/" + p


def vite_router_path_to_url(file_path: str) -> str:
    """Convert src/pages/Candidates.tsx or src/routes/auth/login.tsx → route path."""
    p = (file_path or "").replace("\\", "/").strip()
    for prefix in ("src/pages/", "src/routes/"):
        if p.lower().startswith(prefix):
            p = p[len(prefix):]
            break
    for ext in (".tsx", ".jsx"):
        if p.lower().endswith(ext):
            p = p[: -len(ext)]
            break
    if p.lower() == "index" or p.lower().endswith("/index"):
        p = p[: -len("/index")] if p.lower().endswith("/index") else ""
    p = p.strip("/")
    if not p:
        return "/"
    segments = []
    for seg in p.split("/"):
        seg = seg.strip()
        if not seg:
            continue
        if seg[0].isupper() and seg[1:].isalnum():
            segments.append(seg[0].lower() + seg[1:])
        else:
            segments.append(seg.lower())
    return "/" + "/".join(segments) if segments else "/"


def extract_ui_routes_from_paths(paths: List[str]) -> List[ParsedUiRoute]:
    routes: List[ParsedUiRoute] = []
    seen: Set[str] = set()
    for path in paths:
        lower = path.replace("\\", "/").lower()
        url = ""
        if lower.endswith("/page.tsx") or lower.endswith("/page.jsx"):
            if lower.startswith("app/"):
                url = app_router_path_to_url(path)
        elif lower.startswith("pages/") and not lower.startswith("pages/api/"):
            if any(lower.endswith(ext) for ext in _UI_PAGE_EXTS):
                url = pages_router_path_to_url(path)
        elif lower.startswith("src/pages/") or lower.startswith("src/routes/"):
            if any(lower.endswith(ext) for ext in (".tsx", ".jsx")):
                url = vite_router_path_to_url(path)
        if not url or url in seen:
            continue
        seen.add(url)
        title = path.rsplit("/", 1)[-1]
        routes.append(ParsedUiRoute(url=url, file_path=path, title=title))
    return routes


def next_app_api_path_to_url(file_path: str) -> str:
    p = (file_path or "").replace("\\", "/").strip()
    if p.lower().startswith("app/"):
        p = p[4:]
    for suffix in ("/route.ts", "/route.js"):
        if p.lower().endswith(suffix):
            p = p[: -len(suffix)]
            break
    p = p.strip("/")
    return "/" + p if p else "/api"


def next_pages_api_path_to_url(file_path: str) -> str:
    p = (file_path or "").replace("\\", "/").strip()
    if p.lower().startswith("pages/"):
        p = p[6:]
    for ext in _API_ROUTE_EXTS:
        if p.lower().endswith(ext):
            p = p[: -len(ext)]
            break
    p = p.strip("/")
    return "/" + p if p else "/api"


def python_api_path_to_url(file_path: str) -> str:
    p = (file_path or "").replace("\\", "/").strip()
    for prefix in ("api/routes/", "routes/"):
        if p.lower().startswith(prefix):
            stem = p[len(prefix):]
            if stem.lower().endswith(_PYTHON_EXT):
                stem = stem[: -len(_PYTHON_EXT)]
            stem = stem.strip("/")
            if stem.endswith("/__init__"):
                stem = stem[: -len("/__init__")]
            return f"/api/{stem}" if stem else "/api"
    return "/api"


def node_server_path_to_url(file_path: str) -> str:
    p = (file_path or "").replace("\\", "/").strip()
    for prefix in ("src/server/", "server/"):
        if p.lower().startswith(prefix):
            stem = p[len(prefix):]
            if stem.lower().endswith(".ts"):
                stem = stem[: -3]
            stem = stem.strip("/")
            return f"/api/{stem}" if stem else "/api"
    return "/api"


def extract_next_route_methods(content: str) -> List[str]:
    if not content:
        return []
    methods = []
    for m in _NEXT_EXPORT_METHOD_RE.findall(content):
        methods.append(m.upper())
    return list(dict.fromkeys(methods))


def _normalize_api_path(prefix: str, route: str) -> str:
    pfx = (prefix or "").strip()
    rt = (route or "").strip()
    if not rt:
        return pfx or "/api"
    if rt.startswith("http"):
        return rt
    combined = f"{pfx.rstrip('/')}/{rt.lstrip('/')}" if pfx else rt
    if not combined.startswith("/"):
        combined = "/" + combined
    combined = re.sub(r"/{2,}", "/", combined)
    return combined.rstrip("/") or "/"


def extract_fastapi_endpoints(content: str, *, fallback_url: str) -> List[Tuple[str, str]]:
    """Return list of (method, url) from FastAPI-style decorators."""
    if not content:
        return [("UNKNOWN", fallback_url)]

    prefix = ""
    for m in _ROUTER_PREFIX_RE.finditer(content):
        prefix = m.group(1)
    for m in _INCLUDE_ROUTER_PREFIX_RE.finditer(content):
        prefix = m.group(1) or prefix

    endpoints: List[Tuple[str, str]] = []

    for method, route in _FASTAPI_DECORATOR_RE.findall(content):
        url = _normalize_api_path(prefix, route)
        endpoints.append((method.upper(), url))

    for route, methods_raw in _FASTAPI_API_ROUTE_RE.findall(content):
        url = _normalize_api_path(prefix, route)
        for part in re.findall(r"[\"'](\w+)[\"']", methods_raw):
            endpoints.append((part.upper(), url))

    if not endpoints:
        return [("UNKNOWN", fallback_url)]

    seen: Set[Tuple[str, str]] = set()
    out: List[Tuple[str, str]] = []
    for item in endpoints:
        if item not in seen:
            seen.add(item)
            out.append(item)
    return out


def extract_node_express_methods(content: str, *, fallback_url: str) -> List[Tuple[str, str]]:
    if not content:
        return [("UNKNOWN", fallback_url)]
    endpoints: List[Tuple[str, str]] = []
    for m in re.finditer(
        r"(?:app|router)\.(get|post|put|patch|delete)\s*\(\s*[\"']([^\"']+)[\"']",
        content,
        re.I,
    ):
        endpoints.append((m.group(1).upper(), m.group(2)))
    if not endpoints:
        return [("UNKNOWN", fallback_url)]
    seen: Set[Tuple[str, str]] = set()
    out: List[Tuple[str, str]] = []
    for item in endpoints:
        if item not in seen:
            seen.add(item)
            out.append(item)
    return out


def extract_api_endpoints(
    file_path: str,
    content: Optional[str] = None,
) -> List[ParsedApiEndpoint]:
    lower = file_path.replace("\\", "/").lower()
    endpoints: List[ParsedApiEndpoint] = []

    if lower.startswith("app/api/") and lower.endswith(("/route.ts", "/route.js")):
        url = next_app_api_path_to_url(file_path)
        methods = extract_next_route_methods(content or "")
        if not methods:
            methods = ["UNKNOWN"]
        for method in methods:
            endpoints.append(ParsedApiEndpoint(url=url, method=method, file_path=file_path))
        return endpoints

    if lower.startswith("pages/api/"):
        url = next_pages_api_path_to_url(file_path)
        methods = extract_next_route_methods(content or "")
        if not methods:
            methods = ["UNKNOWN"]
        for method in methods:
            endpoints.append(ParsedApiEndpoint(url=url, method=method, file_path=file_path))
        return endpoints

    if lower.endswith(_PYTHON_EXT) and (
        lower.startswith("api/routes/") or lower.startswith("routes/")
    ):
        url = python_api_path_to_url(file_path)
        for method, ep_url in extract_fastapi_endpoints(content or "", fallback_url=url):
            resolved = ep_url if ep_url and ep_url not in ("/", "") else url
            if resolved == "/" and url:
                resolved = url
            endpoints.append(ParsedApiEndpoint(url=resolved, method=method, file_path=file_path))
        return endpoints

    if (lower.startswith("server/") or lower.startswith("src/server/")) and lower.endswith(".ts"):
        url = node_server_path_to_url(file_path)
        for method, ep_url in extract_node_express_methods(content or "", fallback_url=url):
            endpoints.append(ParsedApiEndpoint(url=ep_url, method=method, file_path=file_path))
        return endpoints

    return endpoints


def parse_prisma_schema(content: str, *, file_path: str = "") -> List[ParsedPrismaModel]:
    if not content:
        return []
    models: List[ParsedPrismaModel] = []
    for match in _PRISMA_MODEL_RE.finditer(content):
        model_name = match.group(1)
        start = match.end()
        next_model = _PRISMA_MODEL_RE.search(content, start)
        block = content[start: next_model.start() if next_model else len(content)]
        fields: List[str] = []
        relations: List[str] = []
        for line in block.splitlines():
            stripped = line.strip()
            if not stripped or stripped.startswith("//") or stripped.startswith("@@"):
                continue
            rel = _PRISMA_RELATION_TYPE_RE.match(stripped)
            if rel:
                fields.append(rel.group(1))
                target = rel.group(2)
                if target not in relations:
                    relations.append(target)
                continue
            rel2 = _PRISMA_RELATION_FIELD_RE.match(stripped)
            if rel2 and rel2.group(2)[0].isupper():
                fields.append(rel2.group(1))
                target = rel2.group(2)
                if target not in relations:
                    relations.append(target)
                continue
            fm = _PRISMA_FIELD_RE.match(stripped)
            if fm and fm.group(1) not in ("model",):
                fname = fm.group(1)
                if fname not in fields:
                    fields.append(fname)
        models.append(ParsedPrismaModel(name=model_name, fields=fields[:40], relations=relations[:20]))
    return models


def extract_sql_table_names(content: str) -> List[str]:
    if not content:
        return []
    tables: List[str] = []
    seen: Set[str] = set()
    for pattern in (_SQL_CREATE_TABLE_RE, _SQL_ALTER_TABLE_RE):
        for m in pattern.finditer(content):
            name = m.group(1)
            if name and name.lower() not in seen:
                seen.add(name.lower())
                tables.append(name)
    return tables


def path_tokens_from_url(url: str) -> List[str]:
    u = (url or "").strip().lower()
    if not u:
        return []
    u = u.split("?", 1)[0]
    parts = [p for p in u.split("/") if p and p not in ("api", "v1", "v2")]
    tokens: List[str] = []
    for p in parts:
        if p.startswith("[") and p.endswith("]"):
            continue
        clean = re.sub(r"[^a-z0-9_-]", "", p)
        if clean and len(clean) >= 2:
            tokens.append(clean)
    return tokens
