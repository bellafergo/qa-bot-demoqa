# System Memory / App Knowledge Graph — Phase 1

## Goal

Per-project persistent memory that aggregates what Vanya already learns from Explorer, App Map, Catalog, Runs, Failure Intelligence, and Incident Investigator — **without duplicating raw stores**.

## What already exists (reuse)

| Source | Data reused | Store |
|--------|-------------|-------|
| Projects | `base_url`, name | `projects` |
| Test Catalog | modules, tests, `base_url` | `test_cases` |
| Runs | status metrics, `project_id` in meta | `test_runs` / `qa_runs` |
| Explorer | pages, forms, links | ephemeral → **ingested** into `project_knowledge` |
| App Map | forms, tables, workflows, nav | light persist + **ingested** |
| Failure Intelligence | regressions | computed on demand → **cached** in memory |
| Incident Investigator | recent investigations | `incident_investigation_runs` → **indexed** |

## New components

| File | Role |
|------|------|
| `models/project_knowledge_models.py` | Pydantic contracts |
| `services/app_knowledge_graph.py` | Pure merge + risk scoring |
| `services/project_memory_service.py` | CRUD facade |
| `services/project_knowledge_service.py` | Orchestration + ingest hooks |
| `services/db/project_knowledge_repository.py` | SQLite `project_knowledge` table |
| `api/routes/project_knowledge_routes.py` | `GET/POST /projects/{id}/knowledge` |
| `vanya-frontend/src/pages/KnowledgePage.jsx` | System Memory UI |

## Persistence

Table `project_knowledge`:

- `project_id` (PK)
- `project_name`
- `payload_json` — full `ProjectKnowledge` document
- `updated_at`

Fields inside payload: `modules`, `routes`, `apis`, `components`, `forms`, `tables`, `workflows`, `related_tests`, `failure_history`, `incident_history`, `risk_score`, `metadata`.

## Automatic feeding

1. **Explorer** — `POST /app-explorer/explore-app` with `project_id` → `ingest_explorer_result()` (live hook only)
2. **App Map** — `POST /inspect-url/map` with `project_id` → `ingest_app_map()` + light row in `test_runs`
3. **Runs** — `test_catalog_service._save_run()` → `ingest_run_completed()` (test metrics)
4. **Incidents** — after investigation → `ingest_incident_completed()`
5. **Refresh** — `POST /projects/{id}/knowledge/refresh?mode=replace` (default) rebuilds from sources

## Refresh modes (R7)

| Mode | Behaviour |
|------|-----------|
| `replace` (default) | Rebuild derived memory from current sources; drops stale routes/forms |
| `merge` | Additive patch on top of existing memory (legacy) |

UI: **Refresh memory** → `replace`; **Merge memory** → `merge`.

## Reconstruction on refresh (R1)

`refresh_project_knowledge` rebuilds from:

| Source | Store | Rebuilt fields |
|--------|-------|----------------|
| Catalog | `test_cases` | modules, related_tests, routes (base_url) |
| Browser Inspection | `test_runs` (`_browser_inspection`) | routes, forms (count), workflows, apis, components |
| Runs | `run_history_service` | `run_fail_rate` in metadata |
| Failure Intelligence | computed | failure_history |
| Incidents | `incident_investigation_runs` | incident_history |

**Not reconstructible on refresh** (documented in `metadata.reconstruction_notes`):

- **Explorer** (`/app-explorer/explore-app`) — HTML inventories are ephemeral; never written to `test_runs`
- **Full forms/tables field lists** — only bounded `app_map_summary` / counts are persisted

## Incident Investigator integration

Before diagnosis, `get_knowledge_context()` supplies module/route/failure hints appended to recommendations and stored in `meta.knowledge_context`.

## UI

Sidebar → **System Memory** / **Memoria del Sistema** → `/knowledge`

Shows: risk score, modules, routes, APIs, related tests, recent incidents, failure history.

## Phase 2 (not in MVP)

- Supabase mirror for `project_knowledge`
- Link watch diff events into memory
- OpenAPI endpoint graph ingestion
- Temporal graph (nodes/edges) instead of flat lists
