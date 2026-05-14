# Vanya Local Agent (Phase 4B CLI)

Minimal **executable agent** that validates the **Cloud ↔ Agent** channel: heartbeat, poll, and (unless `--dry-run`) a **simulated** job result. **No browser**, no Playwright, no opening `target_url`, no downloading job payloads.

## Requirements

Use the same repo virtualenv as Vanya (already includes `httpx`):

```bash
pip install -r requirements.txt
```

## Configuration

| CLI flag | Environment variable | Description |
|----------|----------------------|-------------|
| `--base-url` | `VANYA_CLOUD_URL` | Vanya Cloud origin, e.g. `https://qa-bot-demoqa.onrender.com` |
| `--agent-id` | `VANYA_AGENT_ID` | Agent UUID from `POST /local-agents/register` |
| `--agent-token` | `VANYA_AGENT_TOKEN` | Bearer secret shown **once** at registration |
| `--poll-interval` | `VANYA_AGENT_POLL_INTERVAL` | Seconds to sleep when the queue is empty (default `10`) |
| `--http-timeout` | `VANYA_AGENT_HTTP_TIMEOUT` | HTTP timeout seconds (default `30`) |
| `--agent-version` | — | Sent as `agent_version` on heartbeat (default `vanya-local-agent/4b`) |

CLI arguments override environment variables when both are set.

## Run

```bash
cd /path/to/qa-bot-demoqa
python -m local_agent.cli \
  --base-url https://qa-bot-demoqa.onrender.com \
  --agent-id <agent_id> \
  --agent-token <agent_token> \
  --poll-interval 10 \
  --once
```

**Dry-run** (heartbeat + poll, **no** `POST …/jobs/{id}/result`):

```bash
python -m local_agent.cli \
  --base-url "$VANYA_CLOUD_URL" \
  --agent-id "$VANYA_AGENT_ID" \
  --agent-token "$VANYA_AGENT_TOKEN" \
  --once \
  --dry-run
```

**Environment-only example:**

```bash
export VANYA_CLOUD_URL=https://qa-bot-demoqa.onrender.com
export VANYA_AGENT_ID=...
export VANYA_AGENT_TOKEN=...
export VANYA_AGENT_POLL_INTERVAL=10
python -m local_agent.cli --once --dry-run
```

## Security notes

- Logs show **only** `sha256:<12 hex>…<last 4 chars>` for the token — never the full secret.
- Do **not** commit tokens; do **not** write them to disk from this CLI.
- The agent **does not** execute shell commands from jobs and **does not** navigate to URLs in Phase 4B.

## Docker

From this directory (`local_agent/`, where this `README.md` lives):

```bash
docker build -t vanya-local-agent .
docker run --rm \
  -e VANYA_CLOUD_URL=https://qa-bot-demoqa.onrender.com \
  -e VANYA_AGENT_ID=YOUR_AGENT_ID \
  -e VANYA_AGENT_TOKEN=YOUR_AGENT_TOKEN \
  vanya-local-agent --once --dry-run
```

Image installs only `httpx` (no Playwright).

## Simulated result payload

Successful completions send `status: succeeded` and a short JSON `result_ref` with `message` and `artifacts: []` (see `local_agent/client.py::build_dry_run_result_ref`).

## Phase 4C preview

Next phase: execute `browser_inspection` jobs with Playwright on the LAN, then upload normalized results to Cloud.
