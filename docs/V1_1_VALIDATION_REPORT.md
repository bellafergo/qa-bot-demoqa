# V1.1 Validation Readiness Report

**Date:** 2026-06-10  
**Sprint:** Vanya V1.1 — Hardening & Validation  
**Branch baseline:** `main` @ OBS-02A merge

---

## 1. Dashboard API Count

When a project is selected, `DashboardPage.load()` issues **16 HTTP requests**:

| # | Endpoint | Batch | Error handling (post-V1.1) |
|---|----------|-------|---------------------------|
| 1 | `GET /dashboard/summary?project_id=` | Initial | `summaryError` banner |
| 2 | `GET /dashboard/recent-runs` | Initial | `runsError` on section |
| 3 | `GET /dashboard/recent-jobs` | Initial | `jobsError` on section |
| 4 | `GET /execution/status` (exec) | Initial | `execError` on scheduler card |
| 5 | `GET /platform/observability` | Intel | `trackIntel` → visible error |
| 6 | `GET /projects/{id}/knowledge` | Side | 404 → false; other errors silent |
| 7 | `GET /projects/{id}/quality-trends` | Intel | Not surfaced in dedicated section* |
| 8 | `GET /projects/{id}/early-degradation` | Intel | Not surfaced in dedicated section* |
| 9 | `GET /projects/{id}/value-dashboard` | Intel | `DashboardSectionState` error |
| 10 | `GET /projects/{id}/executive-impact` | Intel | `DashboardSectionState` error |
| 11 | `GET /projects/{id}/business-risk` | Intel | `DashboardSectionState` error |
| 12 | `GET /servicenow/intelligence` | Intel | `DashboardSectionState` error |
| 13 | `GET /qmetry/coverage` | Intel | `DashboardSectionState` error |
| 14 | `GET /qmetry/recommendations` | Intel | Inline on recommendations card |
| 15 | `GET /failure-intelligence/summary` | Parallel | `fiError` on failures widget |
| 16 | `GET /analytics/runs/dashboard` | Parallel | `analyticsError` on trends widget |

\*Quality trends and early degradation render inline cards; failures still fall back to hidden state if API fails.

**Embedded in summary (no extra call):** onboarding checklist, release readiness compositor fields.

**Without project selected:** 5 calls (summary global, runs, jobs, exec status, platform observability) + failure intel + analytics.

---

## 2. Slowest Endpoints (code analysis)

| Endpoint | Risk | Reason |
|----------|------|--------|
| `GET /projects/{id}/value-dashboard` | **High** | N+1: lists up to 100 incident report IDs then `get()` each (`value_dashboard_service._load_incident_reports`) |
| `GET /dashboard/summary` | **Medium** | Loads up to 500 runs from Supabase + SQLite merge when enabled |
| `GET /analytics/runs/dashboard` | **Medium** | Same 500-run scan + aggregation |
| `GET /qmetry/coverage` | **Medium** | External connector latency + catalog crosswalk |
| `GET /servicenow/intelligence` | **Medium** | Correlation over stored incidents + connector metadata |
| `GET /projects/{id}/executive-impact` | **Low–Medium** | Aggregates historical snapshots |
| `GET /failure-intelligence/summary` | **Low** | Summary-only query |

No server-side timing instrumentation exists; rankings are from static analysis and E2E audit observations.

---

## 3. N+1 Query Risks

| Location | Pattern | Severity |
|----------|---------|----------|
| `services/value_dashboard_service._load_incident_reports` | `list_reports` + N × `get(report_id)` | **Critical** |
| `services/coverage_service._load_data` | Bulk catalog + bulk run status (OK) | Low |
| `services/dashboard_service.get_summary` | Single bulk run list | Low |
| Executive impact / business risk builders | Read aggregated tables | Low |

**Primary fix candidate (post-sprint):** batch-load incident reports or store denormalized value-dashboard counters.

---

## 4. Integration Validation Flow

| Step | Path | Status |
|------|------|--------|
| Register connector | `POST /admin/connectors/register` | Admin API |
| Submit validation | `POST /validation` | Creates `EnterpriseValidationRequest` |
| List validations | `GET /validation` | Read-only history |
| SSO validate | `POST /security/sso/validate` | Config check only |
| Frontend Integrations page | Manual test + status badges | User-facing |

Validation is **request/record based** — no automatic re-validation cron. Failed integrations surface in connector status and dashboard empty states (CTA → `/integrations`).

---

## 5. Missing Loading States (remaining)

| Surface | Loading | Error | Empty |
|---------|---------|-------|-------|
| Executive Impact | ✅ Section | ✅ Section | ✅ |
| Value Dashboard | ✅ Section | ✅ Section | ✅ |
| Business Risk | ✅ Section | ✅ Section | ✅ |
| Platform Observability | ✅ Section | ✅ Section | ✅ |
| ServiceNow Intelligence | ✅ Section | ✅ Section | ✅ |
| QMetry Coverage | ✅ Section | ✅ Section | ✅ |
| Quality Trends card | ⚠️ Partial | ❌ Hidden | ✅ |
| Early Degradation card | ⚠️ Partial | ❌ Hidden | ✅ |
| Recommended Tests | ⚠️ Partial | ❌ Silent | ✅ |
| Project Knowledge probe | ❌ None | ❌ Silent | N/A |
| Scheduled Reports panel | ✅ | ✅ | ✅ |

---

## 6. Remaining Untranslated Strings

Audit method: grep for hardcoded UI strings in `vanya-frontend/src/pages` and `components`.

| Location | Example | Priority |
|----------|---------|----------|
| `DashboardPage.jsx` inline styles / fallbacks | Minimal — most via `t()` | Low |
| `ExecutionPage.jsx` | Some status strings | Medium |
| Dev-only console messages | N/A for prod | Low |

**Fixed in V1.1:** `common.close` (EN + ES) for incident preview modals.

**Recommendation:** run i18n extraction script before GA; no systematic untranslated-string linter exists.

---

## 7. Remaining Duplicated Metrics

| Metric | Surfaces (post-H2) | Status |
|--------|-------------------|--------|
| Pass rate | Health strip, Risk card, Runs analytics chart, Value dashboard | **3×** (mini panel removed) |
| Failure intelligence | Failure distribution chart, Risk card (flaky count), Failure intel API notes | **2–3×** |
| Jira blockers | Jira intelligence section only | **Reduced** (removed from Exec Impact + Value Dashboard) |
| Coverage | QMetry coverage section + catalog donut (labeled "catalog coverage") | **2×** (intentional distinction) |
| Release readiness | Release readiness card + Executive Brief rollup | **2×** (rollup is summary) |

---

## Test Coverage Added (V1.1)

- `executiveImpactViewUtils.test.js` — load error vs insufficient history
- `dashboardSectionStateUtils.test.js` — error/empty/cold project
- `executiveBriefViewUtils.test.js` — rollup from existing VMs

---

## Related Documents

- [RBAC Enforcement Audit](./RBAC_ENFORCEMENT_AUDIT.md)
