# services/failure_intelligence_service.py
"""
Failure Intelligence Service
==============================

Provides on-demand failure analysis over persisted test run history:

1. Failure Clustering
   Groups failed runs by (RCA category, impacted layer, module).
   Uses rca_service.analyze() — no pattern duplication.

2. Flaky Test Detection
   Heuristic flip-rate analysis: a test is suspected flaky when it
   alternates between pass and fail/error with flip_rate >= threshold
   across at least FLAKY_MIN_RUNS recent runs.

3. Recurrent Regression Detection
   Identifies test cases that have failed >= REGRESSION_MIN_FAILURES times
   within their most recent REGRESSION_WINDOW runs.

4. Summary Aggregation
   Combines the above into a single FailureIntelligenceSummary.

Data sources:
  - test_run_repo  (SQLite run history)
  - catalog_repo   (test case metadata for module mapping)
  - rca_service    (deterministic RCA, reused as-is)

No new persistence layer — all computed on demand.
"""
from __future__ import annotations

import hashlib
import logging
from typing import Dict, List, Optional, Tuple

from models.failure_intelligence_models import (
    FailureCluster,
    FailureIntelligenceSummary,
    FlakyTestSignal,
    RegressionPattern,
)

logger = logging.getLogger("vanya.failure_intelligence")


# ── Configuration ──────────────────────────────────────────────────────────────

FLAKY_WINDOW         = 10    # examine the last N runs per test case
FLAKY_MIN_RUNS       = 4     # minimum runs to flag as flaky
FLAKY_FLIP_THRESHOLD = 0.4   # minimum flip rate to suspect flakiness
CLUSTER_LIMIT        = 200   # max recent runs to load for clustering
REGRESSION_MIN_FAILURES = 2  # failures needed to report a regression pattern
REGRESSION_WINDOW    = 20    # recent run window for regression detection


# ── Private helpers ────────────────────────────────────────────────────────────

def _compute_flip_rate(statuses: List[str]) -> float:
    """
    Fraction of adjacent status pairs that are a pass↔non-pass transition.
    statuses must be ordered most-recent-first (list_runs returns DESC).
    """
    if len(statuses) < 2:
        return 0.0
    transitions = sum(
        1 for a, b in zip(statuses, statuses[1:])
        if (a == "pass") != (b == "pass")
    )
    return round(transitions / (len(statuses) - 1), 4)


def _cluster_id(category: str, layer: str, module: str) -> str:
    h = hashlib.md5(f"{category}:{layer}:{module}".encode()).hexdigest()[:8]
    return f"CL-{h}"


# ── Service class ─────────────────────────────────────────────────────────────

class FailureIntelligenceService:

    # ── Clustering ─────────────────────────────────────────────────────────────

    def get_clusters(
        self,
        limit: int = CLUSTER_LIMIT,
        module: Optional[str] = None,
        root_cause_category: Optional[str] = None,
    ) -> List[FailureCluster]:
        """
        Load recent failed/error runs, run RCA on each, then group by
        (root_cause_category, impacted_layer, module).

        Optional filters narrow down returned clusters.
        """
        from services.db.test_run_repository import test_run_repo
        from services.db.catalog_repository import catalog_repo
        from services.rca_service import rca_service

        all_runs     = test_run_repo.list_runs(limit=limit)
        failed_runs  = [r for r in all_runs if r.status in ("fail", "error")]

        # Build tc_id → module lookup
        tc_module: Dict[str, str] = {}
        try:
            for tc_id, mod in catalog_repo.all_modules():
                tc_module[tc_id] = mod
        except Exception:
            logger.debug("failure_intelligence: could not load module map")

        # Cluster map: (category, layer, module) → [(run, rca_result)]
        cluster_map: Dict[Tuple, List] = {}

        for run in failed_runs:
            run_module = tc_module.get(run.test_case_id, "unknown")
            if module and run_module != module:
                continue

            try:
                rca      = rca_service.analyze(run)
                category = rca.root_cause_category
                layer    = rca.impacted_layer
            except Exception:
                rca      = None
                category = "unknown"
                layer    = "unknown"

            if root_cause_category and category != root_cause_category:
                continue

            key = (category, layer, run_module)
            cluster_map.setdefault(key, []).append((run, rca))

        # Build FailureCluster objects
        clusters: List[FailureCluster] = []
        for (category, layer, mod), items in cluster_map.items():
            run_ids = [r.run_id for r, _ in items]
            rep_tc  = items[0][0].test_case_id

            # Aggregate signal types by frequency
            sig_counter: Dict[str, int] = {}
            for _, rca in items:
                if rca and rca.evidence_signals:
                    for sig in rca.evidence_signals:
                        sig_counter[sig.signal_type] = sig_counter.get(sig.signal_type, 0) + 1
            common_signals = [
                s for s, _ in sorted(sig_counter.items(), key=lambda x: -x[1])
            ][:5]

            first_rca      = next((r for _, r in items if r), None)
            probable_cause = (
                first_rca.probable_cause if first_rca
                else f"Repeated {category.replace('_', ' ')} failures in module '{mod}'"
            )
            confidence     = first_rca.confidence if first_rca else "low"
            n              = len(run_ids)
            summary = (
                f"{n} {category.replace('_', ' ')} failure(s) in module '{mod}' "
                f"(layer: {layer}). {probable_cause[:80]}."
            )

            clusters.append(FailureCluster(
                cluster_id                  = _cluster_id(category, layer, mod),
                root_cause_category         = category,
                impacted_layer              = layer,
                module                      = mod,
                representative_test_case_id = rep_tc,
                run_ids                     = run_ids,
                total_failures              = n,
                common_signals              = common_signals,
                probable_cause              = probable_cause,
                confidence                  = confidence,
                summary                     = summary,
            ))

        clusters.sort(key=lambda c: -c.total_failures)
        return clusters

    # ── Flaky detection ────────────────────────────────────────────────────────

    def get_flaky_tests(
        self,
        window:         int   = FLAKY_WINDOW,
        min_runs:       int   = FLAKY_MIN_RUNS,
        flip_threshold: float = FLAKY_FLIP_THRESHOLD,
    ) -> List[FlakyTestSignal]:
        """
        Analyse the most recent `window` runs per test case.

        A test is suspected flaky when:
          - total_runs >= min_runs
          - both passes and fail/errors exist
          - flip_rate >= flip_threshold

        flaky_score ∈ [0, 1] combines flip_rate and pass/fail balance.
        """
        from services.db.test_run_repository import test_run_repo

        runs_by_tc = test_run_repo.count_runs_by_test_case()
        signals: List[FlakyTestSignal] = []

        for tc_id in runs_by_tc:
            recent = test_run_repo.list_runs(test_case_id=tc_id, limit=window)
            if not recent:
                continue

            statuses    = [r.status for r in recent]
            total       = len(statuses)
            pass_count  = statuses.count("pass")
            fail_count  = statuses.count("fail")
            error_count = statuses.count("error")
            non_pass    = fail_count + error_count

            flip_rate   = _compute_flip_rate(statuses)

            # flaky_score is high when flip_rate is high AND passes/failures balanced
            pass_frac   = pass_count / total if total > 0 else 0.0
            fail_frac   = non_pass   / total if total > 0 else 0.0
            flaky_score = round(flip_rate * min(pass_frac, fail_frac) * 2, 4)

            suspected_flaky = (
                total     >= min_runs
                and pass_count > 0
                and non_pass   > 0
                and flip_rate  >= flip_threshold
            )

            if suspected_flaky:
                notes = (
                    f"Alternates pass/fail across {total} recent runs "
                    f"(flip rate: {flip_rate:.2f}, score: {flaky_score:.2f})."
                )
            elif non_pass == total:
                notes = f"Consistently failing — {non_pass}/{total} runs failed."
            elif pass_count == total:
                notes = f"Consistently passing — all {pass_count} runs passed."
            else:
                notes = (
                    f"Low flip rate ({flip_rate:.2f}) — not suspected flaky "
                    f"({pass_count} pass, {non_pass} fail/error of {total})."
                )

            signals.append(FlakyTestSignal(
                test_case_id    = tc_id,
                total_runs      = total,
                pass_count      = pass_count,
                fail_count      = fail_count,
                error_count     = error_count,
                flip_rate       = flip_rate,
                flaky_score     = flaky_score,
                suspected_flaky = suspected_flaky,
                notes           = notes,
            ))

        signals.sort(key=lambda s: (-int(s.suspected_flaky), -s.flaky_score))
        return signals

    # ── Regression detection ────────────────────────────────────────────────────

    def get_regressions(
        self,
        min_failures: int = REGRESSION_MIN_FAILURES,
        window:       int = REGRESSION_WINDOW,
    ) -> List[RegressionPattern]:
        """
        Detect test cases that have failed >= min_failures times within
        their most recent `window` runs.
        """
        from services.db.test_run_repository import test_run_repo
        from services.db.catalog_repository import catalog_repo
        from services.rca_service import rca_service

        tc_module: Dict[str, str] = {}
        try:
            for tc_id, mod in catalog_repo.all_modules():
                tc_module[tc_id] = mod
        except Exception:
            pass

        runs_by_tc = test_run_repo.count_runs_by_test_case()
        patterns: List[RegressionPattern] = []

        for tc_id, status_counts in runs_by_tc.items():
            # Quick pre-filter: skip if aggregate totals can't qualify
            total_agg_failures = status_counts.get("fail", 0) + status_counts.get("error", 0)
            if total_agg_failures < min_failures:
                continue

            recent = test_run_repo.list_runs(test_case_id=tc_id, limit=window)
            failed = [r for r in recent if r.status in ("fail", "error")]
            if len(failed) < min_failures:
                continue

            latest_cause = "unknown"
            try:
                rca = rca_service.analyze(failed[0])
                latest_cause = rca.root_cause_category
            except Exception:
                pass

            mod = tc_module.get(tc_id, "unknown")
            patterns.append(RegressionPattern(
                pattern_id        = f"REG-{tc_id}",
                test_case_id      = tc_id,
                module            = mod,
                repeated_failures = len(failed),
                latest_root_cause = latest_cause,
                affected_runs     = [r.run_id for r in failed],
                summary           = (
                    f"Test '{tc_id}' in module '{mod}' has failed "
                    f"{len(failed)} time(s) in recent runs. "
                    f"Latest RCA category: {latest_cause}."
                ),
            ))

        patterns.sort(key=lambda p: -p.repeated_failures)
        return patterns

    # ── Summary ────────────────────────────────────────────────────────────────

    def get_summary(self) -> FailureIntelligenceSummary:
        """Aggregate all failure intelligence metrics into one summary object."""
        from services.db.test_run_repository import test_run_repo

        run_counts   = test_run_repo.count_by_status()
        total_failed = run_counts.get("fail", 0) + run_counts.get("error", 0)

        clusters    = self.get_clusters()
        flaky       = self.get_flaky_tests()
        regressions = self.get_regressions()

        flaky_count = sum(1 for f in flaky if f.suspected_flaky)

        # Aggregate failure count per RCA category across clusters
        top_categories: Dict[str, int] = {}
        for c in clusters:
            top_categories[c.root_cause_category] = (
                top_categories.get(c.root_cause_category, 0) + c.total_failures
            )

        notes_parts = []
        if flaky_count:
            notes_parts.append(f"{flaky_count} test(s) suspected flaky.")
        if regressions:
            notes_parts.append(f"{len(regressions)} recurrent regression(s) detected.")
        if clusters:
            biggest = clusters[0]
            notes_parts.append(
                f"Largest cluster: {biggest.root_cause_category} in "
                f"'{biggest.module}' ({biggest.total_failures} failures)."
            )
        if not notes_parts:
            notes_parts.append("No significant failure patterns detected.")

        return FailureIntelligenceSummary(
            total_failed_runs            = total_failed,
            total_clusters               = len(clusters),
            flaky_tests_count            = flaky_count,
            recurrent_regressions_count  = len(regressions),
            top_failure_categories       = top_categories,
            notes                        = " ".join(notes_parts),
        )


# ── Module-level singleton ─────────────────────────────────────────────────────

failure_intelligence_service = FailureIntelligenceService()
