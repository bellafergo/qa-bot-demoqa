# PR Analysis v1

Deterministic pull-request change impact analysis scoped to a catalog project.
Consumes **System Memory** and **Risk Engine** outputs — does not recalculate risk.

## Phase 1 audit

| Capability | Existing | Reused in v1 |
|------------|----------|--------------|
| PR keyword analysis | `POST /pr-analysis/analyze` | Legacy fallback (no project) |
| Module suggest from paths | `POST /risk-selection/suggest-modules` | Logic absorbed into `change_impact_service` |
| GitHub PR fetch | `POST /github/pr/fetch` + webhooks | UI only — not required for v1 |
| Project GitHub settings | `project.settings.github` | Not used in v1 |
| System Memory | `GET /projects/{id}/knowledge` | **Primary** module + route context |
| Risk Engine | `module_risks`, `recommended_tests` | **Consumed as-is** |
| Test catalog | `catalog_repo.all_modules_for_project` | Module name source of truth |
| Draft test templates | `pr_analysis_service._DRAFT_TEMPLATES` | Legacy flow only |

### Gaps (v1)

- No AST / import-graph analysis (token + route heuristic only)
- No automatic GitHub PR ingestion into v1 endpoint
- Unmapped files reported in reasoning (no ML inference)

## Design

```
changed_files[]
    → change_impact_service.map_changed_files()
        → catalog modules + knowledge.modules + routes + related_tests
    → resolve_impacted_modules()
    → pr_analysis_service.analyze_for_project()
        → knowledge.risk_score / risk_level (no recalc)
        → filter knowledge.module_risks by impacted
        → filter knowledge.recommended_tests by impacted
```

## API

`POST /projects/{project_id}/pr-analysis`

```json
{
  "changed_files": [
    "src/components/CandidateForm.tsx",
    "src/services/candidate_service.py"
  ],
  "branch": "feature/candidates",
  "pr_id": "PR-42",
  "title": "Fix candidate form validation"
}
```

Response: `ProjectPRAnalysisReport` with `impacted_modules`, `risk_score`, `risk_level`, `recommended_tests`, `reasoning`.

## Files

- `services/change_impact_service.py` — file → module mapping
- `services/pr_analysis_service.py` — `analyze_for_project()`
- `models/pr_analysis_models.py` — `ProjectPRAnalysisReport`
- `api/routes/project_knowledge_routes.py` — endpoint
- `vanya-frontend/src/pages/PRAnalysisPage.jsx` — manual file list UI

## Limitations (v1)

1. Token/substring matching — may miss renamed or deeply nested modules
2. Requires System Memory refresh for accurate risk data
3. Global risk is project-level from Risk Engine, not PR-specific
4. Legacy `/pr-analysis/analyze` still uses domain keywords when no project selected

## GitHub automatic PR (future)

Would need:

1. Webhook `pull_request` → resolve `project_id` from repo settings
2. Fetch changed files from GitHub API (already partial in `pr_agent`)
3. Call `POST /projects/{id}/pr-analysis` with file list
4. Optional: comment on PR with recommended tests
