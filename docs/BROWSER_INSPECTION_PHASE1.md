# Browser inspection — Phase 1 (deterministic)

## Purpose

Single-URL inspection without LLM: SSRF-safe navigation, viewport screenshot (optional Cloudinary URL), DOM-derived metadata, console/network failures, and selector candidates for future test generation.

## Install (local / CI)

```bash
cd /path/to/qa-bot-demoqa
python3 -m pip install -r requirements.txt
python3 -m playwright install chromium
```

`requirements.txt` already pins `playwright==1.49.0`. Browsers are **not** installed by `pip install` alone.

## Deploy (e.g. Render / Docker)

1. Run the same `playwright install chromium` step in the image or build command (cache this layer).
2. Ensure OS deps for Chromium match Playwright docs for your base image (often `playwright install-deps chromium` on Linux).
3. Optional: set `CLOUDINARY_*` so `screenshot_url` is a hosted HTTPS link instead of `null` (see `core.settings.HAS_CLOUDINARY`).

See **Phase 3A**: `docs/BROWSER_INSPECTION_PHASE3A.md` — optional persistence of inspection summaries + history API.

## API

`POST /inspect-url` with JSON body `{ "url": "https://…", "project_id": null, "timeout_ms": 15000 }`.

See OpenAPI `/docs` after starting the app.

See also **Phase 2**: `docs/BROWSER_INSPECTION_PHASE2.md` (`POST /inspect-url/map` — semantic single-page app map on the same inspection pipeline).

## Overlap with `/app-explorer/explore`

`/app-explorer/explore` uses HTML parsing after `page.content()` and does not expose console/network, visibility-first inventory, or Cloudinary screenshot URL. Prefer `/inspect-url` for Browser Intelligence foundations; keep `/app-explorer/explore` for existing Explorer flows until a later consolidation.

## TODO (next phases)

- Phase 3A done: light `qa_runs` / SQLite rows via `persist_run_payload` — see `docs/BROWSER_INSPECTION_PHASE3A.md`.
- Optional full-page screenshot flag (bounded size).
- Harden selector string escaping for exotic attributes.
