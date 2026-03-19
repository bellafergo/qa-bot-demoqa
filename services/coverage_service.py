# services/coverage_service.py
"""
Coverage Intelligence Service
==============================

Analyzes the test catalog and run history to produce:
  - per-module coverage scores
  - identification of never-run tests
  - detection of missing test scenario types
  - actionable recommendations

Coverage score = executed_tests / total_tests  (0.0 – 1.0)
"Executed" means the test has at least one entry in run history.

No new DB tables are required — all data comes from the existing
catalog and test-run repositories.
"""
from __future__ import annotations

import logging
from collections import defaultdict
from typing import Dict, List, Optional, Set, Tuple

from models.coverage_models import CoverageResult

logger = logging.getLogger("vanya.coverage")


# ── Constants ─────────────────────────────────────────────────────────────────

_ALL_TEST_TYPES = {"smoke", "regression", "functional", "negative", "e2e"}

_TYPE_LABEL: Dict[str, str] = {
    "smoke":       "smoke tests (basic sanity)",
    "regression":  "regression tests",
    "functional":  "functional tests",
    "negative":    "negative / error-path tests",
    "e2e":         "end-to-end tests",
}

# Minimum types we recommend every module to have
_RECOMMENDED_TYPES: List[str] = ["smoke", "negative"]


# ── Recommendation builders ───────────────────────────────────────────────────

def _build_recommendations(
    result: CoverageResult,
    missing_types: List[str],
) -> List[str]:
    recs: List[str] = []

    if result.total_tests == 0:
        recs.append("No tests registered for this module. Add at least one smoke test.")
        return recs

    if result.never_run_tests > 0:
        pct = int(result.never_run_tests / result.total_tests * 100)
        recs.append(
            f"{result.never_run_tests} test(s) ({pct}%) have never been executed. "
            "Run them to establish a baseline."
        )

    if result.coverage_score < 0.5:
        recs.append(
            f"Coverage score is low ({result.coverage_score:.0%}). "
            "Increase test execution frequency or add tests to the orchestrator schedule."
        )
    elif result.coverage_score < 0.8:
        recs.append(
            f"Coverage score is moderate ({result.coverage_score:.0%}). "
            "Consider running all catalog tests more frequently."
        )

    for t in missing_types:
        label = _TYPE_LABEL.get(t, t)
        recs.append(f"Missing {label}. Consider adding {t} tests for this module.")

    if result.failed_tests > 0 and result.executed_tests > 0:
        fail_pct = int(result.failed_tests / result.executed_tests * 100)
        recs.append(
            f"{result.failed_tests} test(s) are currently failing ({fail_pct}% of executed). "
            "Investigate failures to restore coverage confidence."
        )

    if not recs:
        recs.append(
            f"Coverage looks healthy ({result.coverage_score:.0%}). "
            "Keep monitoring for regressions."
        )

    return recs


# ── Core computation ──────────────────────────────────────────────────────────

def _compute_module_coverage(
    module: str,
    tc_ids: List[str],
    tc_types: Dict[str, str],              # test_case_id → type
    latest_status: Dict[str, str],         # test_case_id → last run status ("pass"/"fail"/"error"/None)
) -> CoverageResult:
    """Build a CoverageResult for one module from pre-fetched data."""
    total_tests    = len(tc_ids)
    executed_tests = sum(1 for tid in tc_ids if latest_status.get(tid) is not None)
    never_run      = total_tests - executed_tests
    passed_tests   = sum(1 for tid in tc_ids if latest_status.get(tid) == "pass")
    failed_tests   = sum(1 for tid in tc_ids
                         if latest_status.get(tid) in ("fail", "error"))

    coverage_score = (executed_tests / total_tests) if total_tests > 0 else 0.0

    # Types present in the catalog for this module
    present_types: Set[str] = {tc_types[tid] for tid in tc_ids if tid in tc_types}
    missing_types = [t for t in _RECOMMENDED_TYPES if t not in present_types]

    result = CoverageResult(
        module          = module,
        total_tests     = total_tests,
        executed_tests  = executed_tests,
        passed_tests    = passed_tests,
        failed_tests    = failed_tests,
        never_run_tests = never_run,
        coverage_score  = round(coverage_score, 4),
        missing_test_types = missing_types,
    )
    result.recommendations = _build_recommendations(result, missing_types)
    return result


# ── Service ───────────────────────────────────────────────────────────────────

class CoverageService:

    def get_summary_aggregate(self):
        """
        Return the aggregate coverage summary consumed by the frontend CoveragePage.
        Transforms the per-module List[CoverageResult] into the shape the UI expects.
        """
        from models.coverage_models import CoverageModuleSummary, CoverageSummaryResponse

        per_module = self.get_summary()

        total_tests    = sum(r.total_tests    for r in per_module)
        total_executed = sum(r.executed_tests for r in per_module)
        overall_pct    = round(total_executed / total_tests * 100) if total_tests > 0 else 0
        uncovered      = sum(1 for r in per_module if r.executed_tests == 0)

        modules = [
            CoverageModuleSummary(
                module          = r.module,
                coverage_pct    = round(r.coverage_score * 100),
                test_case_count = r.total_tests,
                flows_covered   = r.executed_tests,
                gaps            = r.recommendations,
                notes           = None,
            )
            for r in per_module
        ]

        return CoverageSummaryResponse(
            overall_coverage_pct       = overall_pct,
            total_test_cases           = total_tests,
            total_flows_discovered     = total_executed,
            uncovered_flows_count      = uncovered,
            modules                    = modules,
            discovered_pages           = [],
            uncovered_flow_suggestions = [],
        )

    def get_summary(self) -> List[CoverageResult]:
        """Return coverage metrics for every module in the catalog."""
        tc_by_module, tc_types, latest_status = self._load_data()

        results: List[CoverageResult] = []
        for module, tc_ids in sorted(tc_by_module.items()):
            results.append(
                _compute_module_coverage(module, tc_ids, tc_types, latest_status)
            )

        if not results:
            logger.info("coverage: catalog is empty — no modules found")

        return results

    def generate_tests_for_gaps(self, module: str):
        """
        Generate draft test suggestions based on coverage gaps for a module.

        Reuses _DRAFT_TEMPLATES from pr_analysis_service (keyed by domain/module).
        Falls back to a generic smoke test when no template is found.
        Never modifies the catalog.
        """
        from models.coverage_models import CoverageTestGenerationResponse
        from models.pr_analysis_models import DraftTestSuggestion
        from services.pr_analysis_service import _DRAFT_TEMPLATES

        result   = self.get_module_coverage(module)
        mod_key  = module.lower().strip()
        templates = _DRAFT_TEMPLATES.get(mod_key, [])

        # Derive a brief gap signal for source_signal
        gap_signal = (
            result.recommendations[0]
            if result.recommendations
            else f"coverage gap in {module}"
        )

        suggestions: List[DraftTestSuggestion] = []

        for tmpl in templates:
            suggestions.append(DraftTestSuggestion(
                name                 = tmpl["name"],
                module               = module,
                rationale            = tmpl["rationale"],
                suggested_steps      = list(tmpl.get("suggested_steps", [])),
                suggested_assertions = list(tmpl.get("suggested_assertions", [])),
                source_signal        = f"coverage gap: {gap_signal[:80]}",
                confidence           = tmpl.get("confidence", "medium"),
            ))

        # Generic fallback when no domain template exists
        if not suggestions:
            suggestions.append(DraftTestSuggestion(
                name                 = f"{module} — basic smoke test",
                module               = module,
                rationale            = (
                    f"No templates found for module '{module}'. "
                    "Add a smoke test to establish a baseline."
                ),
                suggested_steps      = [
                    {"action": "goto",    "value": f"{{{{base_url}}}}/{mod_key}"},
                    {"action": "wait_ms", "ms": 1000},
                ],
                suggested_assertions = [
                    {"type": "url_contains", "value": f"/{mod_key}"},
                ],
                source_signal        = "coverage gap: no tests registered",
                confidence           = "low",
            ))

        logger.info(
            "coverage: generated %d draft suggestion(s) for module '%s' (score=%.0f%%)",
            len(suggestions), module, result.coverage_score * 100,
        )

        return CoverageTestGenerationResponse(
            module          = module,
            gap_summary     = result.recommendations,
            suggested_tests = [s.model_dump() for s in suggestions],
        )

    def get_module_coverage(self, module: str) -> CoverageResult:
        """Return coverage metrics for a specific module."""
        tc_by_module, tc_types, latest_status = self._load_data(module=module)

        tc_ids = tc_by_module.get(module, [])

        # Also try case-insensitive / partial match if exact key is missing
        if not tc_ids:
            m_lower = module.lower()
            for key, ids in tc_by_module.items():
                if key.lower() == m_lower or m_lower in key.lower():
                    tc_ids = ids
                    module = key
                    break

        return _compute_module_coverage(module, tc_ids, tc_types, latest_status)

    # ── Data loading ──────────────────────────────────────────────────────────

    def _load_data(
        self,
        module: Optional[str] = None,
    ) -> Tuple[
        Dict[str, List[str]],   # module → [test_case_id, …]
        Dict[str, str],          # test_case_id → type
        Dict[str, str],          # test_case_id → last run status (or None)
    ]:
        """Load catalog + run history in bulk and return structured lookups."""
        from services.db.catalog_repository import catalog_repo
        from services.db.test_run_repository import test_run_repo

        # Fetch active test cases (optionally filtered by module)
        if module:
            all_tests = catalog_repo.list_test_cases(
                module=module, status="active", limit=2000
            )
        else:
            all_tests = catalog_repo.list_test_cases(status="active", limit=2000)

        tc_by_module: Dict[str, List[str]] = defaultdict(list)
        tc_types: Dict[str, str] = {}

        for tc in all_tests:
            mod = (tc.module or "unknown").strip()
            tc_by_module[mod].append(tc.test_case_id)
            tc_types[tc.test_case_id] = tc.type

        # Fetch recent runs and keep only the latest per test_case_id
        all_runs = test_run_repo.list_runs(limit=5000)
        latest_status: Dict[str, str] = {}

        # list_runs returns most-recent first (ORDER BY executed_at DESC)
        for run in all_runs:
            if run.test_case_id not in latest_status:
                latest_status[run.test_case_id] = run.status

        logger.debug(
            "coverage: loaded %d test cases across %d modules, %d runs",
            len(all_tests), len(tc_by_module), len(all_runs),
        )

        return dict(tc_by_module), tc_types, latest_status


# ── Module-level singleton ────────────────────────────────────────────────────

coverage_service = CoverageService()
