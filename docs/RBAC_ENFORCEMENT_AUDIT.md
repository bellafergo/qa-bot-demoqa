# RBAC Enforcement Audit — V1.1 (C6)

**Date:** 2026-06-10  
**Scope:** Verify whether role-based access control is actively enforced when authentication is enabled.

## Summary

| Layer | Auth enabled (`VANYA_AUTH_ENABLED=1`) | RBAC enforcement |
|-------|--------------------------------------|------------------|
| API middleware | JWT required on non-public routes | **Not applied** |
| Route permissions | None | **Not applied** |
| Frontend UI gating | None | **Not applied** |
| RBAC readiness API | Reports catalog only | `enforcement_enabled: false` |

**Conclusion:** Authentication and RBAC are **separate concerns** in V1. When auth is enabled, users must present a valid JWT, but **all authenticated users receive identical access**. RBAC is foundation-only (roles/permissions catalog) with enforcement explicitly disabled.

## Backend Findings

### 1. RBAC service (`services/rbac_service.py`)

- `resolve_user_role()` always returns `VIEWER` for every user; no persistent role assignments.
- `build_rbac_readiness_report()` hardcodes `enforcement_enabled = False`.
- Module docstring states: *"No authorization enforcement or access denial."*

### 2. Auth middleware (`core/auth_middleware.py`)

- When `auth_enforcement_enabled()` is false (default), all requests pass through without JWT.
- When auth is enabled, middleware validates Bearer JWT and email domain policy only.
- **No call** to `resolve_user_role()`, permission checks, or role-based route guards.

### 3. API routes

- No `@require_permission` or equivalent decorator exists on any route.
- Security routes expose read-only RBAC catalog:
  - `GET /security/rbac` — readiness report
  - `GET /security/roles`, `GET /security/permissions`
- Sensitive operations (integrations, report send, settings) are **not** gated by role.

### 4. App startup (`app.py`)

- Imports `auth_enforcement_enabled` for middleware wiring only.
- No RBAC middleware registered.

## Frontend Findings

### 1. Settings / Security UI

- RBAC panel displays readiness report from `GET /security/rbac`.
- Shows "Enforcement disabled" badge when `enforcement_enabled` is false.
- No client-side route guards based on role or permission.

### 2. Navigation

- All sidebar routes (Dashboard, Incidents, Integrations, Settings, Reports) are visible to every user regardless of role.

## Gap Matrix

| Expected capability (V1 roadmap) | Implemented | Enforced |
|-----------------------------------|-------------|----------|
| Role catalog (5 roles) | Yes | N/A |
| Permission catalog (9 permissions) | Yes | N/A |
| User → role assignment | No | No |
| API permission checks | No | No |
| UI feature gating | No | No |
| Audit on denied access | No | No |

## Recommendations (post-V1.1 — not in sprint scope)

1. Add `require_permission(permission_id)` dependency for FastAPI routes.
2. Persist user-role mappings (DB or IdP groups).
3. Set `enforcement_enabled` from env flag once assignments exist.
4. Gate frontend nav/actions using permissions from `/security/me` (endpoint does not exist yet).

## Verification Steps Performed

1. Code review of `rbac_service.py`, `auth_middleware.py`, `app.py`, `api/routes/security_routes.py`.
2. Grep for `enforcement_enabled`, `resolve_user_role`, permission decorators — none on routes.
3. Frontend review of `rbacViewUtils.js` and Settings security panel.
4. Existing test `tests/test_rbac_service.py` confirms `enforcement_enabled is False`.
