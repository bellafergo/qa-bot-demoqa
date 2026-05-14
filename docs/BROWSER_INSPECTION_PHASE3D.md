# Browser inspection — Phase 3D (visual diff + alert dedupe)

Lightweight **screenshot hash** comparison between two persisted `screenshot_url` values (from `browser_inspection_summary`), plus **in-process Slack alert dedupe** for scheduled watches.

## Visual diff

- **Algorithm:** 8×8 grayscale **average hash** (64 bits) via Pillow; Hamming distance → `visual_change_level` (`none` / `low` / `medium` / `high`) and `visual_similarity_score` (0–1).
- **Fetch:** HTTPS only, **host allowlist** (default: `*.cloudinary.com`, `res.cloudinary.com`). Override with `VANYA_BROWSER_SCREENSHOT_FETCH_SUFFIXES` (comma-separated hostname suffixes).
- **Limits:** `VANYA_BROWSER_VISUAL_MAX_BYTES` (default 2 MiB cap), connect/read timeouts in code (3s / 12s).
- **Integration:** `POST /browser-inspections/diff` response includes optional fields:
  - `visual_change_detected`, `visual_change_level`, `visual_hash_changed`, `visual_similarity_score`
  - On missing URLs / fetch failure: `warnings[]` entries (`visual_diff_skipped:*`), visual fields degraded (no crash).
- **Semantics:** `visual_change_suspected` is appended to `regression_signals` when visual level is `medium` or `high` (UI drift signal; not pixel-perfect regression).

Code: `services/browser_visual_diff_service.py`, `models/browser_visual_diff_models.py`, wired from `services/browser_inspection_diff_service.py`.

## Watch alert dedupe (MVP)

- **Fingerprint:** `watch_id` + effective `change_level` + sorted `regression_signals` + summary prefix + visual level + visual similarity.
- **Window:** `VANYA_BROWSER_ALERT_DEDUPE_MINUTES` (default **60**, clamped 1–1440).
- **Behavior:** Equivalent alerts inside the window are **not** sent to Slack; a warning is logged; the watch event row is still written with `alert_triggered=0` when dedupe blocks after threshold matched.
- **Scope:** Single-process memory map — **not** safe across multiple workers; see TODO for Redis / external dedupe.

Code: `services/browser_watch_alert_dedupe.py`; used from `services/browser_inspection_watch_service.py`.

## Watch env renames (backward compatible)

| New (preferred) | Legacy (deprecated) | Default |
|-----------------|---------------------|---------|
| `VANYA_BROWSER_WATCH_SCHEDULER` | `VANYA_BIO_WATCH_SCHEDULER` | `1` (on) |
| `VANYA_BROWSER_WATCH_TICK_S` | `VANYA_BIO_WATCH_TICK_S` | `120` |
| `VANYA_BROWSER_WATCH_MAX_PER_TICK` | `VANYA_BIO_WATCH_MAX_PER_TICK` | `5` |

If **both** keys exist, the **new** name wins. **TODO:** remove `VANYA_BIO_WATCH_*` after all environments migrate.

Resolver: `services/browser_watch_env.py`. Scheduler: `services/browser_inspection_watch_scheduler.py`.

## Persistence (watch events)

Optional column **`visual_meta_json`** on `browser_inspection_watch_events` (small JSON: visual flags, level, similarity, hash_changed — **no** image bytes). Added via idempotent migration in `services/db/init_db.py`.

## API (`run-now`)

`BrowserInspectionWatchRunNowResponse` may include:

- `visual_change_detected`, `visual_change_level`, `visual_similarity_score`
- `alert_dedupe_suppressed` — `true` when an alert would have fired but dedupe blocked it

See `docs/BROWSER_INSPECTION_PHASE3E.md` for baselines, metrics, and event history.

## Limitations / TODO (3E+)

- Distributed dedupe (Redis), leader lock for multi-worker schedulers.
- Perceptual hash upgrades (e.g. pHash) behind a feature flag.
- Screenshot history timeline and visual baselines per watch.
- **Remove** `VANYA_BIO_WATCH_*` env fallbacks once legacy is gone.
