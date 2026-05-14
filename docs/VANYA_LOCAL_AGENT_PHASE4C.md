# Vanya Local Agent — Phase 4C (Local Playwright inspection)

Phase **4C** lets the **local agent** execute `job_type: browser_inspection` using the same Playwright runner as the cloud (`runners.browser_inspector_runner.run_browser_inspection`), with **no** `app.py` import. Results are sent to Cloud as a **compact JSON** string in `result_ref` (≤512 chars): **no `screenshot_b64`** in the payload.

## Contract

### Job payload (required fields for execution)

```json
{
  "url": "http://localhost:3000",
  "timeout_ms": 15000,
  "project_id": "optional",
  "execution_mode": "local_agent"
}
```

`target_url` on the job row may be used as a fallback if `payload.url` is missing.

### Poll response (4C)

`POST /agent-api/{agent_id}/poll` now includes:

```json
{
  "jobs": [ ... ],
  "agent_capabilities": ["browser_inspection", "localhost_access"]
}
```

If capabilities are missing, the agent still honors **CLI / env** flags (`--allow-localhost`, etc.).

### Result (`result_ref`)

- `kind`: `vanya_local_agent_browser_inspection`
- Trimmed headings, links, buttons, inputs, forms, selector_candidates, console/network errors (bounded)
- `screenshot_present`, `screenshot_sha256_24` (hash of captured screenshot bytes locally — **not** the image)
- **TODO 4D**: upload screenshot/evidence artifact and persist as full `browser_inspection` run in Cloud.

## CLI / environment

| Flag | Env | Default |
|------|-----|---------|
| `--browser-enabled` / `--no-browser-enabled` | `VANYA_AGENT_BROWSER_ENABLED` | off |
| `--allow-localhost` | `VANYA_AGENT_ALLOW_LOCALHOST` | off |
| `--allow-private-ips` | `VANYA_AGENT_ALLOW_PRIVATE_IPS` | off |
| `--browser-headless` / `--no-browser-headless` | `VANYA_AGENT_BROWSER_HEADLESS` | on |
| `--browser-timeout-ms` | `VANYA_AGENT_BROWSER_TIMEOUT_MS` | 15000 |

## Playwright install (host)

```bash
pip install -r requirements.txt
python -m playwright install chromium
```

## Reuse vs duplication

- **Reuse:** `run_browser_inspection`, `normalize_raw_runner_output` (from `services.browser_inspector_service`) with `upload_hosted_screenshot=False` so the agent does not push to Cloudinary from the field.
- **New:** `local_agent/url_guard.py` for local-only URL policy; `local_agent/result_packager.py` for bounded `result_ref`.

## Limitations

- No atomic job lease / `running` state in Cloud.
- Docker image in `local_agent/Dockerfile` remains **httpx-only**; Playwright in Docker is a **TODO** (use `mcr.microsoft.com/playwright/python` + copy `runners/`, `core/`, `models/`, `services/browser_inspector_service.py` or install full repo).

## Phase 4D plan

Evidence upload, durable `qa_runs` / inspection linking, Browser Watch handoff.
