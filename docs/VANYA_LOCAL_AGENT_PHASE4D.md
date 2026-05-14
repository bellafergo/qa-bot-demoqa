# Vanya Local Agent — Phase 4D (Evidence upload & cloud persistence)

## Goal

After a local `browser_inspection` job, the agent uploads **one** screenshot (PNG / JPEG / WEBP, bounded size) to cloud storage (Cloudinary), then persists a **real** lightweight browser inspection run via the existing `persist_run_payload` / `persist_light_browser_inspection` pipeline. **No `screenshot_b64` in SQLite** — only `evidence_url`, optional `artifact_sha256` in meta, and bounded summaries.

## Endpoints (Bearer `agent_token`)

- `POST /agent-api/{agent_id}/artifacts/upload?inspection_id=…` — multipart field `file`; validates magic bytes vs `Content-Type`; max `LOCAL_AGENT_MAX_ARTIFACT_BYTES` (8 MiB).
- `POST /agent-api/{agent_id}/browser-inspections/persist` — JSON `LocalAgentBrowserInspectionPersistRequest` (`inspection`, optional `job_id`, `watch_id`, `project_id`, `artifact_sha256`). Strips `screenshot_b64` if present in JSON.

## Local agent flow

1. Run Playwright inspection (Phase 4C).
2. If runner returned `screenshot_b64`, decode bytes → `POST …/artifacts/upload` (failures become a **warning**; inspection still completes).
3. `POST …/browser-inspections/persist` with `inspection` JSON + `screenshot_url` from upload when available.
4. `POST …/jobs/{job_id}/result` with lightweight `result_ref` (includes optional `evidence_url`, persist flags).

## Persistence contract

- `meta.source` = `local_agent`, `meta.execution_mode` = `local_agent`, `meta.run_type` = `browser_inspection`.
- Optional: `meta.local_agent_id`, `meta.watch_id`, `meta.job_id`, `meta.artifact_sha256`.
- `CanonicalRun.source` includes `local_agent` (see `models/run_contract.py`).

## Limits / TODO

- Single artifact per inspection in this MVP; no arbitrary binaries.
- Cloudinary must be configured for uploads; otherwise upload returns **503** and persist may proceed without screenshot.
- **Phase 4E**: UI for connected agents, watch `execution_mode=local_agent`, queue visibility.

## Tests

`pytest tests/test_local_agent_artifacts.py` (and primary/regression commands from the Phase 4D ticket).
