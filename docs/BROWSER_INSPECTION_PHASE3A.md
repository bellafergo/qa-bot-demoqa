# Browser inspection — Phase 3A (light persistence)

## What is stored

Runs are written through the existing **`persist_run_payload` → `run_store.save_run`** pipeline (SQLite `test_runs` via `run_bridge`, optional Supabase `qa_runs` mirror).

Synthetic catalog key:

- **`test_case_id`**: `_browser_inspection`  
- **`meta.source`**: `browser_inspection`  
- **`run_id` / `evidence_id`**: same as **`inspection_id`** (UUID from the live response)

Rows are **excluded from default catalog run listings** (`test_run_repo.list_runs` without `test_case_id`, and Supabase `list_qa_runs_canonical` filters) so dashboards stay focused on real test cases. The same rows are **filtered out of in-memory** ``run_store.list_runs`` (and PR/tag variants) when tagged as browser inspection.

Bounded JSON under **`meta.browser_inspection_summary`** and **`meta.app_map_summary`** (counts, truncated errors, navigation/action summaries). The Supabase `result` column receives the **same minimal dict** as the `run_store` payload — never full DOM/HTML, never `screenshot_b64`.

See **Phase 3B**: `docs/BROWSER_INSPECTION_PHASE3B.md` — `POST /browser-inspections/diff` and listing-hygiene details.

See **Phase 3C**: `docs/BROWSER_INSPECTION_PHASE3C.md` — scheduled watches and change alerts.

## API responses

`POST /inspect-url` and `POST /inspect-url/map` add optional fields:

- **`inventory_counts`** (inspect only): raw inventory sizes from Playwright.  
- **`inspection_succeeded`**: `false` when navigation failed.  
- **`persisted`**, **`persisted_run_id`**, **`persistence_warning`**: set after a best-effort persist; failures do **not** fail the HTTP request.

## History

- **`GET /browser-inspections?project_id=&limit=&source=browser_inspection`** — list persisted rows (SQLite-backed in dev; Supabase rows appear when configured and mirrored).  
- **`GET /browser-inspections/{inspection_id}`** — detail with summaries (404 if not a browser-inspection row).

## Environment

| Variable | Default | Meaning |
|----------|---------|---------|
| `VANYA_PERSIST_BROWSER_INSPECTION` | `1` | Set `0` / `false` to skip persistence entirely. |

## Future work

- Optional TTL / retention job for `_browser_inspection` rows.  
- BFS crawl reusing the same persist contract per URL.
