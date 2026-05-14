# Vanya Local Agent — Phase 4B

Phase **4B** adds the first **runnable** local agent: a small Python CLI under `local_agent/` that talks to Vanya Cloud over **`/agent-api`** (heartbeat, poll, job result). It validates the control channel **without** Playwright, **without** opening `target_url`, and **without** persisting secrets to disk.

## Delivered components

| Path | Role |
|------|------|
| `local_agent/cli.py` | Argument parsing, logging bootstrap |
| `local_agent/config.py` | Env + CLI config validation |
| `local_agent/client.py` | `httpx` client, retries on transport/5xx, 401/403 handling |
| `local_agent/runner.py` | Loop: heartbeat → poll → simulated result (unless `--dry-run`) |
| `local_agent/security.py` | Token redaction for logs |
| `local_agent/README.md` | Runbook + Docker |
| `local_agent/Dockerfile` | Slim image (`httpx` only) |

## Cloud contract (unchanged from 4A)

- `POST /agent-api/{agent_id}/heartbeat`
- `POST /agent-api/{agent_id}/poll`
- `POST /agent-api/{agent_id}/jobs/{job_id}/result`

Simulated success uses `result_ref` JSON (≤512 chars) describing dry-run `message` and empty `artifacts`.

## Limitations

- No job lease / claim race hardening yet.
- No upload of screenshots or evidence.
- Admin registration still documented in `docs/VANYA_LOCAL_AGENT_PHASE4A.md`.

## Phase 4C plan

1. Spawn Playwright in-process (or sidecar) for `job_type=browser_inspection`.
2. Respect agent capabilities (`localhost_access`, `intranet_access`).
3. Normalize inspection output and map to `result_ref` / future artifact APIs.
4. Optional: backoff when Cloud returns 429.

**Update:** Phase 4C (local Playwright + bounded `result_ref`) is documented in [VANYA_LOCAL_AGENT_PHASE4C.md](VANYA_LOCAL_AGENT_PHASE4C.md).
