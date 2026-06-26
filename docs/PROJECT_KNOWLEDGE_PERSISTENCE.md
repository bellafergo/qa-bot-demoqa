# Project Knowledge persistence (System Memory)

## Current state

Per-project **System Memory** (`ProjectKnowledge`) is stored in **SQLite** only:

| Component | Location |
|-----------|----------|
| ORM row | `services/db/project_knowledge_repository.py` → table `project_knowledge` |
| Facade | `services/project_memory_service.py` |
| Orchestration | `services/project_knowledge_service.py` |

Catalog, projects, and test cases already use **Supabase/Postgres** when `SUPABASE_STRICT=1`.
System Memory does **not** — each API worker has its own `./data/vanya.db` (or ephemeral disk on Render).

## Risk

1. **Cold workers** — no memory row until `refresh_project_knowledge` runs (PR Analysis auto-refreshes once).
2. **Multi-instance** — instances do not share memory; refresh on worker A is invisible to worker B.
3. **Deploys** — ephemeral filesystem can wipe SQLite unless a persistent volume is mounted.
4. **Demo / enterprise UX** — Knowledge page and PR Intelligence appear “empty” after redeploy even when Supabase catalog is complete.

## Files affected by a future migration

| File | Role |
|------|------|
| `services/db/project_knowledge_repository.py` | SQLite CRUD — replace or dual-write |
| `services/project_memory_service.py` | Facade used across app |
| `services/project_knowledge_service.py` | Refresh / assemble |
| `services/pr_analysis_service.py` | `load_project_knowledge_for_analysis` |
| `api/routes/project_knowledge_routes.py` | GET/POST knowledge |
| `vanya-frontend/src/pages/KnowledgePage.jsx` | UI |

## Recommended minimal migration (not implemented)

1. Add Supabase table (example):

```sql
CREATE TABLE IF NOT EXISTS public.project_knowledge (
  project_id   TEXT PRIMARY KEY,
  project_name TEXT NOT NULL DEFAULT '',
  payload_json JSONB NOT NULL DEFAULT '{}',
  updated_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

2. Implement `ProjectKnowledgeRepositorySupabase` mirroring `catalog_repository_supabase` pattern.
3. Proxy in `project_knowledge_repository.py`: strict mode → Supabase, else SQLite.
4. One-time backfill script: read SQLite `project_knowledge`, upsert to Postgres.
5. Keep SQLite as dev fallback.

## Interim mitigations (implemented)

- PR Analysis **auto-refresh** when memory is missing (`load_project_knowledge_for_analysis`).
- **Auto `include_repository`** when GitHub + repo are connected (`project_knowledge_refresh_utils.py`).
- **Repository index diagnostic** surfaced in knowledge metadata + Knowledge UI when GitHub App env is missing.

## Validation after migration

- Refresh memory on instance A; GET `/projects/{id}/knowledge` on instance B returns same payload.
- PR Analysis on cold worker does not require full refresh if Postgres row exists.
