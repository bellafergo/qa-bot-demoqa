# GitHub Integration — SaaS GitHub App

Project-scoped GitHub connection for PR Analysis v1. **No PAT storage.** Installation access tokens are minted server-side and cached in memory only.

No automatic PR comments. No webhooks in v1.

## 1. Technical recommendation

| Approach | Verdict |
|----------|---------|
| **GitHub App** | **Recommended** — server-to-server JWT, per-installation tokens, multi-tenant, webhook-ready, least privilege |
| **OAuth App** | Secondary — user-delegated access; better when acting as the user; defer to v2 |
| **Manual PAT** | **Rejected for SaaS** — plain text at rest; legacy read-only path with `needs_migration` |

## 2. Audit of existing assets

| Asset | Path | SaaS role |
|-------|------|-----------|
| `project.settings.github` | `project_settings_service`, `project_github_settings_service` | Persist installation + repo metadata (no tokens) |
| `installation_id` | Same block | Links GitHub App installation → `project_id` |
| `github_project_context.py` | Credential resolution | App installation tokens + legacy PAT fallback |
| `github_app_service.py` | App JWT + installation tokens | Core SaaS auth |
| `github_installation_token_cache.py` | In-memory TTL cache | Short-lived tokens, never persisted |
| `POST /github/pr/fetch` | `github_routes.py` | Legacy URL fetch (uses project credentials) |
| `GET /github/app/setup` | `github_routes.py` | Post-install callback |
| Webhooks + `pr_agent.py` | `webhooks.py` | **Not used** in v1 |
| `GITHUB_WEBHOOK_SECRET` | `core/settings.py` | Reserved for future `pull_request` webhooks |

## 3. Persisted model (`project.settings.github`)

```json
{
  "provider": "github_app",
  "enabled": true,
  "installation_id": "12345",
  "owner": "acme",
  "repo": "my-app",
  "default_branch": "main",
  "repo_url": "https://github.com/acme/my-app",
  "connected_by": "user@example.com",
  "connected_at": "2026-06-06T12:00:00+00:00",
  "repo_selected_at": "2026-06-06T12:05:00+00:00",
  "last_validated_at": "2026-06-06T12:05:00+00:00",
  "permissions": {
    "contents": "read",
    "pull_requests": "read",
    "metadata": "read"
  }
}
```

**Never stored:** installation access token, App JWT, PAT (cleared on App connect).

### Migration from MVP PAT

| Legacy field | Action |
|--------------|--------|
| `github_token` | Detected → `provider: legacy_pat`, `needs_migration: true`; reconnect via App clears token |
| `installation_id` (manual in ProjectModal) | Reused if present after App connect |
| `owner`, `repo`, `default_branch` | Preserved when selecting repo again |

Flow: `GET status` → if `needs_migration` → install App → `connect-app` → `select-repository`.

## 4. Required environment variables

| Variable | Required | Description |
|----------|----------|-------------|
| `GITHUB_APP_ID` | Yes | GitHub App ID |
| `GITHUB_APP_SLUG` | Yes | App slug for install URL |
| `GITHUB_APP_PRIVATE_KEY` or `GITHUB_APP_PRIVATE_KEY_PATH` | Yes | PEM private key (never logged) |
| `GITHUB_APP_SETUP_URL` | Recommended | Callback URL (e.g. `https://api.example.com/github/app/setup`) |
| `GITHUB_API_BASE` | No | Default `https://api.github.com` |
| `GITHUB_HTTP_TIMEOUT_S` | No | Default `30` |
| `GITHUB_WEBHOOK_SECRET` | Future | Webhook HMAC |
| `GITHUB_TOKEN` | Deprecated | Legacy global PAT only |

### GitHub App permissions (minimum)

- Repository: Contents (read), Pull requests (read), Metadata (read)

## 5. API endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | `/projects/{id}/github/install-url` | GitHub App install URL (`state=project_id`) |
| POST | `/projects/{id}/github/connect-app` | Link `installation_id` to project |
| GET | `/projects/{id}/github/repositories` | List authorized repos for installation |
| POST | `/projects/{id}/github/select-repository` | Bind owner/repo + validate |
| GET | `/projects/{id}/github/status` | Connection status (no tokens) |
| GET | `/projects/{id}/github/branches` | List branches |
| GET | `/projects/{id}/github/pull-requests` | Open PRs |
| GET | `/projects/{id}/github/pull-requests/{n}/files` | Changed files |
| POST | `/projects/{id}/github/pull-requests/{n}/analyze` | Files → PR Analysis v1 |
| GET | `/github/app/setup` | Setup callback (`installation_id`, `state=project_id`) |

Removed: `POST /projects/{id}/github/connect` (PAT).

## 6. Security

- Installation tokens generated server-side only; optional in-memory cache with expiry
- Tokens never returned in API responses
- Tokens never logged (log `installation_id` / `project_id` only)
- GitHub errors sanitized (401/403/404/429)
- `mask_settings_for_api` never exposes raw tokens; legacy PAT shows `{present, deprecated}` only

## 7. UI (`/pr-analysis`)

- **Conectar GitHub** → opens App install URL
- Status: conectado / instalación vinculada / no conectado
- Selector de repositorio autorizado
- Lista de PRs abiertos + **Analizar PR**
- No PAT field

## 8. Files

| File | Role |
|------|------|
| `models/github_integration_models.py` | Request/response contracts |
| `services/github_app_service.py` | App JWT + installation API |
| `services/github_installation_token_cache.py` | Token cache |
| `services/project_github_settings_service.py` | Connect / select / status |
| `services/github_integration_service.py` | PRs, files, analyze |
| `services/github_project_context.py` | Credential resolution |
| `api/routes/github_project_routes.py` | Project endpoints |
| `api/routes/github_routes.py` | Setup callback + legacy PR fetch |
| `vanya-frontend/src/pages/PRAnalysisPage.jsx` | SaaS UI |

## 9. Limitations (v1)

1. In-memory token cache per worker (not shared across instances — acceptable for v1)
2. Open PRs only
3. No webhook-triggered analysis
4. No PR comments
5. Manual installation ID step if setup callback not configured
6. Legacy PAT projects must reconnect via App

## 10. Future

- `pull_request` webhook → `analyze_pull_request()`
- OAuth for user-scoped actions
- Encrypted credential store / Redis token cache
- Auto PR comments via `pr_agent.py`
