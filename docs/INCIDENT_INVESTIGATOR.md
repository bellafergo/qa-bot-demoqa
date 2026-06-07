# Autonomous Incident Investigator (MVP)

## Overview

Users describe an incident in natural language. Vanya:

1. Infers or validates a target URL (project `base_url`, keywords, or explicit URL)
2. Opens the page with Playwright (read-only — no form submits)
3. Captures screenshot, console errors, failed requests, HTTP 4xx/5xx
4. Returns a heuristic diagnosis and persists the investigation run

## API

| Method | Path | Description |
|--------|------|-------------|
| POST | `/incidents/investigate` | Run investigation |
| GET | `/incidents/runs` | List history (`?project_id=&limit=`) |
| GET | `/incidents/runs/{id}` | Get one run |

### POST body

```json
{
  "incident_description": "Dashboard se queda cargando",
  "target_url": "https://app.example.com/dashboard",
  "project_id": "demo",
  "module": "/dashboard",
  "max_steps": 5,
  "credentials_mode": "none",
  "allow_destructive_actions": false,
  "timeout_ms": 30000
}
```

## Reused components

- `runners/browser_inspector_runner.py` — Playwright probe
- `services/browser_inspector_service.py` — screenshot upload (Cloudinary)
- `core/target_url_validation.py` — SSRF policy
- `services/incident_diagnosis.py` — heuristic RCA rules

## Persistence

SQLite table `incident_investigation_runs` (payload JSON). Supabase parity deferred — no schema change to `qa_runs`.

## UI

Frontend route: `/incidents` — sidebar **Incident Investigator**

## Local test

```bash
pytest tests/test_incident_investigator.py -q
curl -X POST http://localhost:8000/incidents/investigate \
  -H "Content-Type: application/json" \
  -d '{"incident_description":"Login page shows console error","target_url":"https://example.com"}'
```

## Limitations (MVP)

- Synchronous only (blocks until Playwright finishes)
- No LLM reasoning — rules-based diagnosis
- No authenticated sessions (`credentials_mode` must be `none`)
- No destructive actions
- SQLite persistence only (no Supabase table yet)
- Single-page navigation (no multi-step replay)

## Next steps

- Async job + polling for long investigations
- Link investigations to `qa_runs` / Failure Intelligence clusters
- LLM-enhanced diagnosis using captured evidence
- Trigger from Runs page / Browser Watch alerts
- Supabase mirror table for multi-instance deploys
