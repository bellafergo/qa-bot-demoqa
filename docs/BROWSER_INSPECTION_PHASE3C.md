# Browser inspection — Phase 3C (scheduled re-inspection & alerts)

## What it does

1. **Register watches** — URLs to re-inspect on an interval, with a **change threshold** (`low` | `medium` | `high`).
2. **Run pipeline** — each tick: Playwright inspection → **light persistence** (Phase 3A) → **diff vs last inspection** (Phase 3B) → optional **Slack alert** (small text only).
3. **Scheduler** — single-process daemon thread (MVP), not safe across multiple workers without external coordination.

## API (mounted under `/browser-inspections/watch`)

| Method | Path | Purpose |
|--------|------|---------|
| POST | `/browser-inspections/watch` | Create watch |
| GET | `/browser-inspections/watch` | List watches (`project_id`, `limit`) |
| GET | `/browser-inspections/watch/{watch_id}` | Get one |
| PATCH | `/browser-inspections/watch/{watch_id}` | Update fields |
| POST | `/browser-inspections/watch/{watch_id}/run-now` | Run immediately (`force=true` bypasses disabled guard) |

`execution_mode` is **`cloud` only** (same validation pattern as app map).

## Persistence

SQLite tables (created via `init_catalog_db`):

- `browser_inspection_watches` — config + `last_inspection_id`, `last_diff_id`, `last_run_at`.
- `browser_inspection_watch_events` — one row per diff (bounded JSON for signals).

No DOM/HTML/screenshot_b64 stored.

## Threshold → alert

- **high**: alert only when **effective** change level is `high`.
- **medium**: alert when effective level is **medium** or **high**.
- **low**: alert when effective level is **low**, **medium**, or **high`.

**Effective level** (Phase 3D): `max(metadata change_level, visual_change_level)` from the diff response so purely visual drift can still cross the threshold.

Slack kinds: `browser_inspection_regression_detected` (non-empty regression signals) or `browser_inspection_change_detected`.

## Environment

| Variable (preferred) | Legacy (deprecated, still read) | Default | Meaning |
|----------------------|----------------------------------|---------|---------|
| `VANYA_BROWSER_WATCH_SCHEDULER` | `VANYA_BIO_WATCH_SCHEDULER` | `1` | `0` disables background scheduler thread |
| `VANYA_BROWSER_WATCH_TICK_S` | `VANYA_BIO_WATCH_TICK_S` | `120` | Sleep seconds between batches (30–3600) |
| `VANYA_BROWSER_WATCH_MAX_PER_TICK` | `VANYA_BIO_WATCH_MAX_PER_TICK` | `5` | Max watches per wake (1–25) |

If both new and legacy keys are set, **the new name wins**. **TODO:** remove `VANYA_BIO_WATCH_*` after all deployments migrate.

See **Phase 3D** — `docs/BROWSER_INSPECTION_PHASE3D.md` (visual diff + alert dedupe + `VANYA_BROWSER_ALERT_DEDUPE_MINUTES`).

## TODO (distributed / prod)

- Redis lock + single leader election for multi-worker.
- Cron / external scheduler instead of in-process sleep.
- ~~Dedupe alerts per watch + time window~~ — MVP in Phase 3D (`VANYA_BROWSER_ALERT_DEDUPE_MINUTES`); distributed dedupe still TODO.

## Related docs

- `docs/BROWSER_INSPECTION_PHASE3A.md` — persistence.
- `docs/BROWSER_INSPECTION_PHASE3B.md` — diff + listing hygiene.
- `docs/BROWSER_INSPECTION_PHASE3D.md` — visual diff + dedupe (extends 3B/3C).
- `docs/BROWSER_INSPECTION_PHASE3E.md` — baselines, metrics, events.
