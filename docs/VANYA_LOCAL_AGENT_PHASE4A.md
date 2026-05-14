# Vanya Local Agent — Phase 4A (Cloud ↔ Agent Contract)

This document describes **Phase 4A only**: persistence models, security model, HTTP contract, and execution-mode hooks. It does **not** include a runnable local Playwright worker, evidence upload, or Browser Watch UI for agents.

## Architecture

| Component | Role |
|-----------|------|
| **Vanya Cloud** | Control plane: registers agents, stores hashed tokens, exposes minimal job queue contract, receives heartbeats and normalized job results. |
| **Vanya Local Agent** (future 4B+) | Execution node inside the customer network: polls cloud, pulls authorized jobs, runs inspections locally, posts results. Never exposes intranet/localhost to the public internet. |

**Prefixes**

- **`/local-agents`** — Admin / API-ready UI surface: register, list, get, patch, disable. Intended to be protected by `LOCAL_AGENT_REGISTER_SECRET` (and/or `VANYA_SERVICE_TOKEN`) in production.
- **`/agent-api`** — Agent-only endpoints: heartbeat, poll, job result. Authenticated with **per-agent Bearer token** (shown once at registration).

Paths are **JWT-exempt** at the auth middleware edge (`core/vanya_auth.py`) so agents do not use end-user Supabase credentials; authorization is enforced inside handlers.

## Security model

1. **Agent token** — Generated once at registration (`vla_…`). Stored as **SHA-256 hex** only (`token_hash`). Short **`token_fingerprint`** (first 12 hex chars) is stored for support/UI. Responses from `GET /local-agents` / `GET /local-agents/{id}` never include the raw token.
2. **Admin registration key** — Optional env `LOCAL_AGENT_REGISTER_SECRET`. When set, all `/local-agents/*` admin calls require header `X-Vanya-Local-Agent-Register-Key` **or** `X-Service-Token` matching `VANYA_SERVICE_TOKEN`.
3. **No user passwords** — Agents use machine secrets only.
4. **Revocation** — `POST /local-agents/{agent_id}/disable` or `PATCH` with `enabled: false` invalidates operational use (heartbeat/poll/result return 403).
5. **Rate limits** — When `RATE_LIMIT_ENABLED=1`, `POST` under `/local-agents` and `/agent-api/` use the **explorer** tier (`core/rate_limit_middleware.py`).

## Data model (SQLite)

Tables: `local_agents`, `local_agent_jobs`. Created via SQLAlchemy `Base.metadata.create_all` (`services/db/init_db.py`). Same pattern can map to Supabase/Postgres later (replace engine URL; keep column semantics).

**Job queue (minimal)** — `job_type` (e.g. `browser_inspection`), `target_url`, small `payload_json` (max 4096 bytes), statuses `queued` / `running` / `succeeded` / `failed` / `cancelled`, optional `result_ref`. Phase 4A does not enqueue jobs from product flows automatically; tests may insert rows via `local_agent_repo.insert_job`.

## Execution mode: `cloud` vs `local_agent` (Browser Watch)

- **Create watch** — Still **cloud-only** (`execution_mode` must be `cloud` in `BrowserInspectionWatchCreate`).
- **PATCH watch** — May set `execution_mode` to `local_agent` for watches that will run on an agent in a later phase.
- **URL validation** — If the effective mode is `local_agent`, PATCH may set **http(s) URLs** that are not allowed in cloud SSRF checks (e.g. localhost) via a light validator (`services/browser_inspection_watch_service.py`). **Cloud** paths (`inspect-url`, cloud watch ticks) still use `validate_target_url` — **localhost is not opened from Render/cloud**.
- **Scheduler / run-now** — Watches with `execution_mode=local_agent` are **skipped** in `tick_due_watches` and **rejected** in `execute_watch_tick` (unless `force=True`), with a clear error directing to the Local Agent roadmap.

## Endpoints (summary)

| Method | Path | Auth |
|--------|------|------|
| POST | `/local-agents/register` | Admin key / service token if secret set |
| GET | `/local-agents` | Same |
| GET | `/local-agents/{agent_id}` | Same |
| PATCH | `/local-agents/{agent_id}` | Same |
| POST | `/local-agents/{agent_id}/disable` | Same |
| POST | `/agent-api/{agent_id}/heartbeat` | `Authorization: Bearer <agent_token>` |
| POST | `/agent-api/{agent_id}/poll` | Bearer |
| POST | `/agent-api/{agent_id}/jobs/{job_id}/result` | Bearer |

## Example: register + heartbeat

```bash
export REG_KEY=your-long-random-secret   # if LOCAL_AGENT_REGISTER_SECRET is set
curl -sS -X POST "$BASE/local-agents/register" \
  -H "Content-Type: application/json" \
  -H "X-Vanya-Local-Agent-Register-Key: $REG_KEY" \
  -d '{"project_id":"acme-storefront","name":"store-lan-1","capabilities":["browser_inspection","localhost_access"],"version":"4a.0"}'
```

Response (truncated):

```json
{
  "agent_id": "8f2c…",
  "agent_token": "vla_…",
  "token_fingerprint": "a1b2c3d4e5f6",
  "project_id": "acme-storefront",
  "name": "store-lan-1",
  "capabilities": ["browser_inspection", "localhost_access"],
  "version": "4a.0",
  "created_at": "2026-05-11T12:00:00+00:00"
}
```

```bash
curl -sS -X POST "$BASE/agent-api/$AGENT_ID/heartbeat" \
  -H "Authorization: Bearer $AGENT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"agent_version":"4a.0"}'
```

## What Phase 4A does **not** do

- No local Playwright runner, no browser navigation from the agent binary.
- No automatic enqueue of watch ticks as `local_agent_jobs`.
- No evidence pipeline integration for agent uploads.
- No full admin UI (API only).

## Roadmap (short)

| Phase | Scope |
|-------|--------|
| **4B** | Minimal CLI/Docker agent: heartbeat + poll loop. |
| **4C** | Execute `browser_inspection` jobs locally with Playwright. |
| **4D** | Upload normalized evidence; `result_ref` / artifact contract. |
| **4E** | UI: connected agents, status, fingerprints. |
| **4F** | Browser Watch: choose `local_agent` execution with agent selection. |

## cURL quick reference

```bash
# List agents for a project
curl -sS "$BASE/local-agents?project_id=acme-storefront" -H "X-Vanya-Local-Agent-Register-Key: $REG_KEY"

# Poll jobs (empty list if none queued)
curl -sS -X POST "$BASE/agent-api/$AGENT_ID/poll" \
  -H "Authorization: Bearer $AGENT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"limit":10}'

# Post job result
curl -sS -X POST "$BASE/agent-api/$AGENT_ID/jobs/$JOB_ID/result" \
  -H "Authorization: Bearer $AGENT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"status":"succeeded","result_ref":"s3://bucket/key.json"}'
```

## Limitations / TODOs

- In-memory rate limits are not shared across workers.
- Admin routes are middleware-public; **always set `LOCAL_AGENT_REGISTER_SECRET` in production** (or restrict at the edge with API gateway / mTLS).
- Job claiming / leasing and `running` transitions are minimal; 4B+ will define atomic claim semantics.
