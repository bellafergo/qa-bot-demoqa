# tests/test_repository_knowledge_extractors.py
from __future__ import annotations

from services.repository_knowledge_extractors import (
    app_router_path_to_url,
    classify_repository_paths,
    extract_api_endpoints,
    extract_fastapi_endpoints,
    extract_next_route_methods,
    extract_sql_table_names,
    extract_ui_routes_from_paths,
    pages_router_path_to_url,
    parse_prisma_schema,
    path_tokens_from_url,
    should_skip_path,
    vite_router_path_to_url,
)


def test_should_skip_node_modules():
    assert should_skip_path("node_modules/react/index.js") is True
    assert should_skip_path("app/page.tsx") is False


def test_app_router_path_to_url():
    assert app_router_path_to_url("app/page.tsx") == "/"
    assert app_router_path_to_url("app/dashboard/page.tsx") == "/dashboard"
    assert app_router_path_to_url("app/(auth)/login/page.tsx") == "/login"
    assert app_router_path_to_url("app/candidates/[id]/page.tsx") == "/candidates/[id]"


def test_pages_router_path_to_url():
    assert pages_router_path_to_url("pages/index.tsx") == "/"
    assert pages_router_path_to_url("pages/companies/index.tsx") == "/companies"
    assert pages_router_path_to_url("pages/about.jsx") == "/about"


def test_vite_router_path_to_url():
    assert vite_router_path_to_url("src/pages/Candidates.tsx") == "/candidates"
    assert vite_router_path_to_url("src/routes/auth/login.tsx") == "/auth/login"


def test_classify_repository_paths():
    paths = [
        "app/dashboard/page.tsx",
        "pages/companies/index.tsx",
        "pages/api/health.ts",
        "src/pages/Home.jsx",
        "app/api/users/route.ts",
        "api/routes/candidates.py",
        "prisma/schema.prisma",
        "node_modules/foo/bar.ts",
        "db/migrations/001_init.sql",
    ]
    buckets = classify_repository_paths(paths)
    assert "app/dashboard/page.tsx" in buckets["ui_app_router"]
    assert "pages/companies/index.tsx" in buckets["ui_pages_router"]
    assert "pages/api/health.ts" in buckets["api_next_pages"]
    assert "src/pages/Home.jsx" in buckets["ui_vite"]
    assert "app/api/users/route.ts" in buckets["api_next_app"]
    assert "api/routes/candidates.py" in buckets["api_python"]
    assert "prisma/schema.prisma" in buckets["prisma_schema"]


def test_extract_ui_routes_from_paths_dedupes():
    paths = [
        "app/dashboard/page.tsx",
        "pages/companies/index.tsx",
        "src/pages/Candidates.tsx",
    ]
    routes = extract_ui_routes_from_paths(paths)
    urls = {r.url for r in routes}
    assert "/dashboard" in urls
    assert "/companies" in urls
    assert "/candidates" in urls


def test_extract_next_route_methods():
    content = """
    export async function GET() {}
    export function POST() {}
    """
    assert extract_next_route_methods(content) == ["GET", "POST"]


def test_extract_fastapi_endpoints_with_router_prefix():
    content = '''
router = APIRouter(prefix="/api/candidates")

@router.get("/")
async def list_candidates():
    pass

@router.post("/{id}")
async def update_candidate():
    pass
'''
    endpoints = extract_fastapi_endpoints(content, fallback_url="/api/candidates")
    methods = {(m, u) for m, u in endpoints}
    assert ("GET", "/api/candidates") in methods
    assert ("POST", "/api/candidates/{id}") in methods


def test_extract_api_endpoints_next_app():
    content = "export async function GET() {}\nexport async function POST() {}"
    eps = extract_api_endpoints("app/api/users/route.ts", content=content)
    assert len(eps) == 2
    assert eps[0].url == "/api/users"
    assert {e.method for e in eps} == {"GET", "POST"}


def test_extract_api_endpoints_fastapi_python():
    content = '''
@router.get("/items")
async def list_items():
    pass
'''
    eps = extract_api_endpoints("api/routes/items.py", content=content)
    assert len(eps) >= 1
    assert eps[0].method == "GET"


def test_parse_prisma_schema_models_fields_relations():
    schema = """
model Candidate {
  id        String   @id
  email     String
  vacancyId String
  vacancy   Vacancy  @relation(fields: [vacancyId], references: [id])
}

model Vacancy {
  id         String      @id
  title      String
  candidates Candidate[]
}
"""
    models = parse_prisma_schema(schema)
    assert len(models) == 2
    cand = next(m for m in models if m.name == "Candidate")
    assert "email" in cand.fields
    assert "Vacancy" in cand.relations


def test_extract_sql_table_names():
    sql = """
    CREATE TABLE IF NOT EXISTS candidates (
      id TEXT PRIMARY KEY
    );
    ALTER TABLE vacancies ADD COLUMN title TEXT;
    """
    tables = extract_sql_table_names(sql)
    assert "candidates" in tables
    assert "vacancies" in tables


def test_path_tokens_from_url():
    assert "candidates" in path_tokens_from_url("/api/v1/candidates/[id]")
    assert "dashboard" in path_tokens_from_url("/dashboard")
