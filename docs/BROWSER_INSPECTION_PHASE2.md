# Browser inspection — Phase 2 (semantic app map)

## Goal

Deterministic **single-page** semantic classification on top of Phase 1 inspection: page types, patterns, navigation hints, risks, and suggested test flows — **no LLM**, no multi-page crawl, no clicks.

See **Phase 3A**: `docs/BROWSER_INSPECTION_PHASE3A.md` — light persistence + `GET /browser-inspections`.

See **Phase 3B**: `docs/BROWSER_INSPECTION_PHASE3B.md` — `POST /browser-inspections/diff`.

See **Phase 3C**: `docs/BROWSER_INSPECTION_PHASE3C.md` — scheduled watches (`/browser-inspections/watch`).

## API

`POST /inspect-url/map`

Body (Pydantic):

```json
{
  "url": "https://example.com",
  "project_id": null,
  "timeout_ms": 15000,
  "execution_mode": "cloud",
  "local_agent_id": null,
  "retention_policy": null
}
```

- **`execution_mode`**: only `"cloud"` is accepted today. Values like `local_agent` or `hybrid` fail **request validation (422)** until implemented.
- **`local_agent_id` / `retention_policy`**: echoed in `persist_hints` for future storage; ignored for execution.

## Pipeline

1. `validate_target_url` (inside `inspect_url_collect`, same SSRF rules as `/inspect-url`).
2. `run_browser_inspection` — Playwright load-only; extras include **`layout_hints`** (table counts, nav link density, search inputs, dialogs, required fields, etc.).
3. `dom_semantic_classifier.classify_semantic_map` — heuristics over inspection + inventory + hints.
4. `AppMapResponse` — bounded lists, no full DOM/HTML.

## Overlap with App Explorer

| Path | Mechanism | Use case |
|------|-----------|----------|
| `POST /app-explorer/explore` | `page.content()` + HTML parser | Legacy Explorer / drafts |
| `POST /inspect-url` | Playwright + `dom_analyzer` + console/network | Objective evidence |
| `POST /inspect-url/map` | Inspection + semantic heuristics | App map slice for one URL |

**Do not remove** `/app-explorer/*` routes. Future convergence: extract a shared **normalized inventory DTO** consumed by both HTML and Playwright paths (TODO in code).

## Cloud vs future local/hybrid

### Cloud (current)

- Backend runs Playwright.
- **Strict SSRF**: localhost, loopback, private IPs, non-http(s) blocked via `validate_target_url`.

### Future `local_agent` (design only)

- Playwright runs **on the customer network** (localhost, intranet, VPN).
- Agent sends **normalized JSON** (same shapes as inspection/map) to the cloud API.
- URLs flagged as internal never hit cloud Playwright.

### Future `hybrid`

- Mix: public surfaces via cloud, private surfaces via `local_agent` merge — **not implemented**.

## Persistence

`persist_hints` in the response documents the intended contract (`source`, `inspection_id`, `project_id`, `execution_mode`, …). **No DB migration** in Phase 2 — do not persist full DOM or `screenshot_b64`.

## Related docs

- `docs/BROWSER_INSPECTION_PHASE1.md` — install, deploy, `/inspect-url`.

## Phase 3 (suggested)

- Bounded BFS crawl reusing `validate_target_url` per discovered URL.
- Persisted `app_map` / inspection graph keyed by `inspection_id` + `project_id`.
- Draft test generation consuming `selector_candidates` + `suggested_test_flows`.
- Optional local agent registration API for hybrid mode.
