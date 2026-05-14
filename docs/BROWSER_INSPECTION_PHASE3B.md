# Browser inspection — Phase 3B (diff / UI change hints)

## Purpose

Deterministic comparison of **two persisted** browser inspections (same metadata as Phase 3A). No Playwright, no DOM/HTML in the response, no IA.

## API

`POST /browser-inspections/diff`

```json
{
  "base_inspection_id": "<uuid>",
  "target_inspection_id": "<uuid>",
  "project_id": "optional-must-match-when-set-on-rows"
}
```

- **404** if either id is missing or is not a browser-inspection row (`test_case_id` + `meta.source`).
- **400** if `project_id` in the body disagrees with a row’s `meta.project_id`, or if base and target belong to different projects.

## List hygiene (Phase 3B + 3A)

Browser inspection rows are excluded from:

- Default SQLite listings via `test_run_repo.list_runs` (`_browser_inspection` test_case_id).
- Default Supabase listings via `qa_runs_read._qa_row_matches_filters` (test_case_id **or** `meta.source == browser_inspection`).
- In-memory `run_store.list_runs` / `list_runs_for_pr` / `list_runs_for_tag` (same heuristics).

They remain visible on **`GET /browser-inspections`**.

See **Phase 3C**: `docs/BROWSER_INSPECTION_PHASE3C.md` — scheduled watches + alerts (implemented).

See **Phase 3D**: `docs/BROWSER_INSPECTION_PHASE3D.md` — visual hash diff + alert dedupe.

## Future (post–3C)

- Visual diff (screenshot hash / perceptual hash) when URLs stable.
- Tie diff output into draft generation.
