# Risk Engine v1

Explainable heuristic risk assessment for Vanya projects. No AI. Computed on demand from existing stores — no new raw tables.

## Phase 1 audit (data sources)

| Source | Reused signals | Persistence |
|--------|----------------|-------------|
| `project_knowledge` | `failure_history`, `incident_history`, `related_tests`, `modules`, cached `run_fail_rate` | SQLite `project_knowledge` (derived cache) |
| `test_runs` / `qa_runs` | Run status, pass rate, per-module outcomes | SQLite + Supabase |
| `failure_intelligence` | Regressions, flaky tests, flip rate | On-demand from runs |
| `incident_investigation_runs` | Severity, description, module, recency | SQLite |
| `browser_inspection` | Watch regression signals (future weighting) | Bounded summaries in `test_runs` |
| `test_cases` (catalog) | Module, priority, type, coverage | SQLite / Supabase |

**Not duplicated:** raw runs, FI outputs, or incident payloads are read at compute time.

## Formula (v1)

### Project `risk_score` (0–100)

| Factor | Max pts | Rule |
|--------|---------|------|
| Regressions | 12 | `min(12, count × 4)` |
| Failure frequency | 12 | `min(12, entries×2 + total_failures×0.4)` |
| Flaky tests | 10 | `min(10, suspected_flaky × 3.5)` |
| Incident severity | 18 | critical=6, high=4, medium=2.5, low=1 per incident |
| Success rate | 18 | `(100 − pass_rate) × 0.18` |
| Coverage gaps | 10 | modules with failures but &lt;3 tests |
| Recency | 20 | recent incidents×4 + recent failures×2 (≤7 days) |

### `risk_level`

| Score | Level |
|-------|-------|
| 0–24 | LOW |
| 25–49 | MEDIUM |
| 50–74 | HIGH |
| 75–100 | CRITICAL |

### Module `module_risk_score`

Per module: regressions, failures, flaky, incidents, pass rate, low coverage, recency (caps 100).

### `recommended_tests`

Top risky modules → catalog tests scored by regression (+8), flaky (+6), recent fail (+5), priority (+0–5).

## API

- `GET /projects/{id}/knowledge` — includes `risk_score`, `risk_level`, `module_risks`, `recommended_tests`, `risk_explanation`
- `GET /projects/{id}/risk` — on-demand `RiskAssessment` only

## Files

- `models/risk_engine_models.py`
- `services/risk_engine_service.py` — pure scoring
- `services/project_risk_service.py` — orchestration + knowledge enrichment
- `tests/test_risk_engine.py`

## Next phase recommendation

**PR Analysis** should consume `GET /projects/{id}/risk` and `module_risks` to prioritize tests for changed files/modules. Optional Risk Engine v1.1: weight `browser_inspection` watch regression signals and expose per-route risk.
