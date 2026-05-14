# Browser inspection — Phase 3E (watch baselines + operational metrics)

Extends **3C/3D** with a **fixed baseline** per watch, **compare_mode**, persisted **last_status**, **metrics** and **event history** for future monitoring UI.

## Baseline

| Field | Meaning |
|-------|---------|
| `baseline_inspection_id` | Persisted `run_id` / inspection id to diff against when `compare_mode=baseline` |
| `compare_mode` | `last` (default, same as pre-3E) or `baseline` |
| `baseline_set_at` | ISO timestamp when baseline was pinned |
| `baseline_updated_by` | Optional string (e.g. actor) from `POST …/baseline` |

**`compare_mode=baseline`**

- Each successful run diffs **target** (new inspection) vs `baseline_inspection_id` (not vs previous last).
- On the **first** successful run when baseline is empty, the inspection id is **auto-pinned** as baseline and a warning `watch_baseline_auto_pinned:…` is added to `run-now` `warnings[]` (no diff that tick).

**`POST /browser-inspections/watch/{watch_id}/baseline`**

```json
{ "inspection_id": "optional-uuid", "use_latest": true, "baseline_updated_by": "optional" }
```

- `use_latest: true` → pins `last_inspection_id` (must exist).
- Otherwise `inspection_id` is required.
- Validates inspection exists in `test_runs`, `source=browser_inspection`, URL matches watch URL (when URLs present), and `project_id` when both sides set.

## last_status (persisted on watch row)

| Value | Rule (simplified) |
|-------|-------------------|
| `disabled` | `enabled=false` |
| `never_run` | No `last_run_at` |
| `failed` | `last_run_error` set (timeout, persist error, etc.) |
| `healthy` | Ran, no effective change (`last_effective_change_level` / tick outcome `none`) |
| `changed` | Ran with material effective change level ≠ `none` |

Included on `GET /browser-inspections/watch` and `GET …/{watch_id}`.

## Metrics — `GET …/{watch_id}/metrics`

Aggregates **light** rows in `browser_inspection_watch_events` (`event_type`):

- `total_runs` — `run_completed`
- `total_diffs` — `diff_generated`
- `alerts_triggered` — `diff_generated` with `alert_triggered=1`
- `alerts_deduped` — `alert_deduped`
- `failed_runs` — `error`
- `visual_fetch_failures` — `visual_diff_skipped`
- `last_change_level`, `last_visual_change_level` — from latest diff + `visual_meta_json`
- `last_run_at`, `last_alert_at` — from watch row / diff-derived
- `current_status` — watch `last_status`

**TODO (scale):** SQL aggregates + indexes if event volume grows.

## Event history — `GET …/{watch_id}/events`

Returns newest-first items: `event_id`, `event_type`, `created_at`, `target_inspection_id`, `base_inspection_id`, `change_level`, `summary` (capped), `alert_triggered`.

**Event types:** `run_completed`, `diff_generated`, `alert_deduped`, `visual_diff_skipped`, `error`, `baseline_set` (and legacy rows default to `diff_generated`).

No DOM / HTML / screenshot b64.

## Dedupe (3D) + 3E

When Slack is suppressed by dedupe, an **`alert_deduped`** event row is inserted (small summary + optional `visual_meta` `{dedupe:true}`).

## Migrations (SQLite)

`init_catalog_db` adds columns idempotently:

- **watches:** `compare_mode`, `baseline_*`, `last_status`, `last_effective_change_level`, `last_alert_at`, `last_run_error`
- **events:** `event_type` (default `diff_generated`)

## Related docs

- `docs/BROWSER_INSPECTION_PHASE3C.md` — scheduler + watches
- `docs/BROWSER_INSPECTION_PHASE3D.md` — visual diff + dedupe env

## Phase 3F suggestions

- Alert policies per project / tenant.
- Webhook destinations beyond Slack.
- Baseline approval workflow + audit log.
- Pagination + cursors on `/events`.
