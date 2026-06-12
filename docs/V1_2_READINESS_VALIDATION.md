# docs/V1_2_READINESS_VALIDATION.md
# V1.2 Readiness Sprint — Validation

**Date:** 2026-06-10

---

## 1. RBAC Verification Matrix

Enforcement is **active when** `VANYA_AUTH_ENABLED=1`, `SUPABASE_URL` is set, and `VANYA_RBAC_ENFORCEMENT` is not `0`.

| Role | VIEW_DASHBOARD | VIEW_INCIDENTS | VIEW_RELEASE_INTEL | VIEW_REPORTS | SEND_REPORTS | MANAGE_INTEGRATIONS | MANAGE_SECURITY | APPROVE_ACTIONS | Write ops (default) |
|------|----------------|----------------|--------------------|--------------|--------------|--------------------|-----------------|-----------------|---------------------|
| VIEWER | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ |
| QA_ENGINEER | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ✅ |
| QA_MANAGER | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ |
| RELEASE_MANAGER | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ | ✅ |
| ADMIN | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Service token | ✅ (all) | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

### Role resolution order

1. Service token → `ADMIN`
2. JWT claim `vanya_role` (top-level, `app_metadata`, or `user_metadata`)
3. `VANYA_RBAC_ROLE_BY_USER_ID` env map
4. `VANYA_RBAC_ROLE_BY_EMAIL` env map
5. Default → `VIEWER`

### Enforcement mechanisms

- **Middleware:** `RbacEnforcementMiddleware` — path-based permission checks + VIEWER write block
- **Audit:** `ACCESS_DENIED` events recorded on 403
- **API:** `GET /security/me` — returns resolved role + permissions
- **Frontend:** `RbacProvider` gates nav items and routes by permission

### Example env configuration

```bash
VANYA_AUTH_ENABLED=1
SUPABASE_URL=https://your-project.supabase.co
VANYA_RBAC_ENFORCEMENT=1
VANYA_RBAC_ROLE_BY_EMAIL=admin@corp.com=ADMIN,qa.lead@corp.com=QA_MANAGER
```

---

## 2. Value Dashboard — Query Count Before/After

| Scenario | Before (V1.1) | After (V1.2) |
|----------|---------------|--------------|
| SQLite only, N reports | **1 + N** (list + N×get) | **1** (`list_full_reports`) |
| SQLite + Supabase, N reports | **2 + 2N** | **2** (1 SQLite + 1 Supabase bulk) |
| Max (N=100) SQLite | **101 queries** | **1 query** |
| Max (N=100) dual store | **202 queries** | **2 queries** |

Implementation: `incident_report_repository.list_full_reports()` + `_list_full_sqlite()` + `list_incident_reports_full_supabase()`.

---

## 3. Top 10 Slowest Endpoints

Endpoint timing is collected in-process via `PerformanceMiddleware` and reported at:

**`GET /platform/performance/slow-endpoints?limit=10`**

Each record includes: `endpoint`, `method`, `duration_ms`, `timestamp`, `status_code`.

Response ranks endpoints by **average duration** over the rolling in-memory window (max 5,000 requests).

Response header: `x-process-time-ms` on all non-static API responses.

### Expected slow endpoints (static analysis — populate after traffic)

| Rank | Endpoint | Expected avg | Notes |
|------|----------|--------------|-------|
| 1 | `GET /projects/{id}/value-dashboard` | High | Bulk load improved; still aggregates 100 reports |
| 2 | `GET /dashboard/summary` | Medium | 500-run scan when Supabase enabled |
| 3 | `GET /analytics/runs/dashboard` | Medium | Same run aggregation |
| 4 | `GET /qmetry/coverage` | Medium | Connector latency |
| 5 | `GET /servicenow/intelligence` | Medium | Correlation work |
| 6 | `GET /projects/{id}/executive-impact` | Low–Medium | Historical aggregation |
| 7 | `GET /failure-intelligence/summary` | Low | Summary query |
| 8 | `GET /platform/observability` | Low | Audit aggregation |
| 9 | `GET /projects/{id}/business-risk` | Low | Stored intelligence |
| 10 | `GET /projects/{id}/release-readiness` | Low | Compositor read |

Run the report endpoint after representative dashboard load to get live rankings.

---

## Tests

```bash
python3 -m pytest tests/test_rbac_enforcement.py tests/test_value_dashboard_bulk_load.py tests/test_endpoint_timing.py tests/test_rbac_service.py -q
```
