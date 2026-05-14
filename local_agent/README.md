# Vanya Local Agent (Phase 4B–4C)

Executable **local agent** CLI: heartbeat, poll, and job results to Vanya Cloud. **Phase 4B** validated the channel; **Phase 4C** adds optional **Playwright** execution for `browser_inspection` jobs (single navigation, no clicks/crawl), including **localhost / private IPs** when explicitly allowed.

## Requirements

```bash
pip install -r requirements.txt
```

For browser jobs (Phase 4C):

```bash
python -m playwright install chromium
```

## Configuration

| CLI flag | Environment variable | Description |
|----------|----------------------|-------------|
| `--base-url` | `VANYA_CLOUD_URL` | Vanya Cloud origin |
| `--agent-id` | `VANYA_AGENT_ID` | Agent UUID from `POST /local-agents/register` |
| `--agent-token` | `VANYA_AGENT_TOKEN` | Bearer secret (shown once at registration) |
| `--poll-interval` | `VANYA_AGENT_POLL_INTERVAL` | Sleep when queue empty (default `10`) |
| `--http-timeout` | `VANYA_AGENT_HTTP_TIMEOUT` | HTTP timeout seconds (default `30`) |
| `--agent-version` | — | `agent_version` on heartbeat (default `vanya-local-agent/4c`) |
| `--browser-enabled` / `--no-browser-enabled` | `VANYA_AGENT_BROWSER_ENABLED` | Run Playwright for `browser_inspection` jobs (default off) |
| `--allow-localhost` | `VANYA_AGENT_ALLOW_LOCALHOST` | Allow `localhost` / `127.0.0.1` targets |
| `--allow-private-ips` | `VANYA_AGENT_ALLOW_PRIVATE_IPS` | Allow literal RFC1918 / link-local IPs |
| `--browser-headless` / `--no-browser-headless` | `VANYA_AGENT_BROWSER_HEADLESS` | Chromium headless (default on) |
| `--browser-timeout-ms` | `VANYA_AGENT_BROWSER_TIMEOUT_MS` | Default nav timeout if job omits `timeout_ms` |

CLI overrides environment when both are set.

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

**Browser inspection** (localhost example — requires explicit flags and `payload.execution_mode=local_agent` on the job):

```bash
export VANYA_AGENT_BROWSER_ENABLED=1
export VANYA_AGENT_ALLOW_LOCALHOST=1
python -m local_agent.cli --base-url "$VANYA_CLOUD_URL" --agent-id "$VANYA_AGENT_ID" --agent-token "$VANYA_AGENT_TOKEN" --once
```

**Dry-run** (no `POST …/jobs/{id}/result`):

```bash
python -m local_agent.cli \
  --base-url "$VANYA_CLOUD_URL" \
  --agent-id "$VANYA_AGENT_ID" \
  --agent-token "$VANYA_AGENT_TOKEN" \
  --once \
  --dry-run
```

## Security notes

- Logs only show **redacted** token labels (`sha256:…` + last 4 chars).
- Do **not** persist agent tokens to disk.
- No shell commands from jobs; **no** link following or form submits (Playwright path matches cloud Phase 1 rules).
- Cloud SSRF / `inspect-url` **unchanged** — localhost stays blocked on the server.

## Docker

The bundled `local_agent/Dockerfile` installs **httpx only** (no browsers). For Playwright inside Docker, see **TODO** in [docs/VANYA_LOCAL_AGENT_PHASE4C.md](../docs/VANYA_LOCAL_AGENT_PHASE4C.md).

```bash
docker build -t vanya-local-agent .
docker run --rm -e VANYA_CLOUD_URL=... -e VANYA_AGENT_ID=... -e VANYA_AGENT_TOKEN=... vanya-local-agent --once --dry-run
```

## Job results

- **Unknown `job_type`:** agent posts `failed` with a clear error (no silent success).
- **`browser_inspection`:** compact JSON in `result_ref` (no `screenshot_b64`); see Phase 4C doc.

Full contract: [docs/VANYA_LOCAL_AGENT_PHASE4C.md](../docs/VANYA_LOCAL_AGENT_PHASE4C.md).
