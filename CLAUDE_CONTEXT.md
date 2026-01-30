Project: Vanya QA Bot (FastAPI + Playwright + Supabase)
Core flows:
- POST /execute_steps and /execute_heb trigger Playwright runs
- Runs are stored by evidence_id via services/run_store(_supabase).py
- UI polls GET /runs/{evidence_id}
- GitHub PR agent lives in services/pr_agent.py and routes/webhooks.py
Constraints:
- Avoid breaking the UI contract: status, evidence_id, evidence_url, report_url, duration_ms
Current issues:
- Supabase project sometimes paused causing psycopg2 OperationalError on /threads
Goal of review:
- Find correctness issues, architecture risks, cyclic imports, error handling gaps, security issues (secrets/logging), performance and flakiness risks.
