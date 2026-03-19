# services/risk_selection_service.py
"""
Risk-Based Test Selection Service
===================================

Selects which tests to run based on:
  1. Module match (changed_modules signal)
  2. Test priority (critical > high > medium > low)
  3. Historical failure rate (recent failing tests scored higher)
  4. Business risk level of the module (flow-based)

Scoring (additive, higher = more important to run):
  - Module match:        +10 per matched changed module
  - Priority critical:  +8
  - Priority high:      +5
  - Priority medium:    +2
  - Priority low:       +0
  - Recent failure:     +4  (latest run status = fail or error)
  - Business-critical module (checkout/auth): +3
  - Smoke / regression type: +1
"""
from __future__ import annotations

import logging
from typing import Dict, List, Optional, Set, Tuple

import re

from models.risk_selection_models import (
    ModuleMatch,
    RiskSelectionRequest,
    RiskSelectionResult,
    SelectedTest,
    SelectAndRunResult,
    SuggestModulesRequest,
    SuggestModulesResult,
)

logger = logging.getLogger("vanya.risk_selection")

# ── Constants ─────────────────────────────────────────────────────────────────

_PRIORITY_SCORE: Dict[str, int] = {
    "critical": 8,
    "high":     5,
    "medium":   2,
    "low":      0,
}

_PRIORITY_ORDER: List[str] = ["critical", "high", "medium", "low"]

# Modules considered business-critical (checkout/auth paths)
_HIGH_RISK_MODULE_KEYWORDS: List[str] = [
    "checkout", "auth", "login", "payment", "order", "cart",
]

_RECENT_FAILURE_BONUS = 4
_MODULE_MATCH_BONUS   = 10
_HIGH_RISK_MODULE_BONUS = 3
_PREFERRED_TYPE_BONUS = 1


def _is_high_risk_module(module: str) -> bool:
    m = module.lower()
    return any(kw in m for kw in _HIGH_RISK_MODULE_KEYWORDS)


def _build_reason(
    matched_modules: List[str],
    priority: str,
    had_recent_failure: bool,
    high_risk_module: bool,
    tc_type: str,
) -> str:
    parts: List[str] = []
    if matched_modules:
        parts.append(f"module match ({', '.join(matched_modules)})")
    if priority in ("critical", "high"):
        parts.append(f"{priority} priority")
    if had_recent_failure:
        parts.append("recent failure")
    if high_risk_module:
        parts.append("business-critical module")
    if tc_type in ("smoke", "regression"):
        parts.append(f"{tc_type} test type")
    return "; ".join(parts) if parts else "general coverage"


# ── Service ───────────────────────────────────────────────────────────────────

class RiskSelectionService:

    def select(self, req: RiskSelectionRequest) -> RiskSelectionResult:
        """Score and rank all active tests, return top max_tests."""
        from services.db.catalog_repository import catalog_repo
        from services.db.test_run_repository import test_run_repo

        # Load catalog and runs
        all_tests = catalog_repo.list_test_cases(status="active", limit=5000)
        all_runs  = test_run_repo.list_runs(limit=5000)

        # Build latest-status lookup (list_runs returns most-recent-first)
        latest_status: Dict[str, str] = {}
        for run in all_runs:
            if run.test_case_id not in latest_status:
                latest_status[run.test_case_id] = run.status

        changed_set: Set[str] = {m.lower() for m in (req.changed_modules or [])}

        scored: List[Tuple[int, SelectedTest]] = []

        for tc in all_tests:
            # Priority filter (optional)
            if req.priority and tc.priority != req.priority:
                continue

            tc_module = (tc.module or "unknown").lower()

            # --- Scoring ---
            score = 0
            matched: List[str] = []

            # 1. Module match (required gate when changed_modules provided)
            for cm in changed_set:
                if cm == tc_module or cm in tc_module or tc_module in cm:
                    score += _MODULE_MATCH_BONUS
                    matched.append(cm)

            # When changed_modules were specified, exclude tests with no module match
            if changed_set and score == 0:
                continue

            # 2. Priority
            score += _PRIORITY_SCORE.get(tc.priority or "low", 0)

            # 3. Historical failure
            last_status = latest_status.get(tc.test_case_id)
            had_failure = last_status in ("fail", "error")
            if had_failure:
                score += _RECENT_FAILURE_BONUS

            # 4. Business-critical module bonus
            hrm = _is_high_risk_module(tc_module)
            if hrm:
                score += _HIGH_RISK_MODULE_BONUS

            # 5. Preferred type (smoke/regression)
            if (tc.type or "").lower() in ("smoke", "regression"):
                score += _PREFERRED_TYPE_BONUS

            reason = _build_reason(matched, tc.priority or "low", had_failure, hrm, tc.type or "")

            scored.append((
                score,
                SelectedTest(
                    test_case_id     = tc.test_case_id,
                    name             = tc.name,
                    module           = tc.module or "unknown",
                    type             = tc.type or "smoke",
                    priority         = tc.priority or "low",
                    selection_score  = score,
                    selection_reason = reason,
                ),
            ))

        # Sort: descending score, then priority order, then name
        _prio_rank = {p: i for i, p in enumerate(_PRIORITY_ORDER)}

        scored.sort(
            key=lambda x: (
                -x[0],
                _prio_rank.get(x[1].priority, 99),
                x[1].name,
            )
        )

        selected = [st for _, st in scored[: req.max_tests]]

        # Build reasoning summary
        if not selected:
            reasoning = "No tests matched the given changed modules and priority filter."
        else:
            total_avail = len(scored)
            capped = total_avail > req.max_tests
            modules_hit = sorted({s.module for s in selected})
            reasoning = (
                f"Selected {len(selected)} test(s)"
                + (f" (capped from {total_avail} candidates)" if capped else "")
                + (f" covering modules: {', '.join(modules_hit)}." if modules_hit else ".")
            )
            if req.changed_modules:
                reasoning += f" Changed modules: {', '.join(req.changed_modules)}."

        return RiskSelectionResult(
            selected_tests = selected,
            total_selected = len(selected),
            reasoning      = reasoning,
        )

    def select_and_run(self, req: RiskSelectionRequest) -> SelectAndRunResult:
        """Select tests and immediately enqueue a suite job via the orchestrator."""
        selection = self.select(req)

        if not selection.selected_tests:
            return SelectAndRunResult(selection=selection, enqueued=False)

        tc_ids = [st.test_case_id for st in selection.selected_tests]

        try:
            from services.catalog_orchestrator import orchestrator_service
            job = orchestrator_service.enqueue_suite(test_case_ids=tc_ids)
            return SelectAndRunResult(
                selection           = selection,
                orchestrator_job_id = job.job_id,
                enqueued            = True,
            )
        except Exception:
            logger.exception("risk_selection: enqueue_suite failed")
            return SelectAndRunResult(selection=selection, enqueued=False)


    def suggest_modules(self, req: SuggestModulesRequest) -> SuggestModulesResult:
        """
        Map PR signals (inferred domain names + changed file paths) to real
        catalog module names using case-insensitive substring matching.

        Logic (deterministic, no ML):
          1. For each catalog module, check if any inferred domain name is a
             substring of the module name, or vice-versa.
          2. For each changed file path, extract meaningful tokens (≥4 chars),
             then check if any token appears in a catalog module name.
        """
        from services.db.catalog_repository import catalog_repo

        # Unique catalog module names (de-duplicated, non-empty)
        all_pairs = catalog_repo.all_modules()          # [(test_case_id, module), ...]
        catalog_modules: List[str] = sorted({m for _, m in all_pairs if m and m.strip()})

        suggested: set = set()

        # 1. Match inferred domain names against catalog module names (bidirectional substring)
        for domain in req.inferred_modules:
            d = domain.strip().lower()
            if not d:
                continue
            for cat_mod in catalog_modules:
                cat_l = cat_mod.lower()
                if d in cat_l or cat_l in d:
                    suggested.add(cat_mod)

        # 2. Match changed file path tokens against catalog module names
        matched_files: List[ModuleMatch] = []
        unmatched_files: List[str] = []

        for f in req.changed_files:
            # Extract tokens: split on path separators, dots, underscores, hyphens
            tokens = [t.lower() for t in re.split(r"[/\\._ -]", f) if len(t) >= 4]
            file_matched = False
            for cat_mod in catalog_modules:
                cat_l = cat_mod.lower()
                for token in tokens:
                    if token in cat_l or cat_l in token:
                        suggested.add(cat_mod)
                        matched_files.append(ModuleMatch(file=f, matched_module=cat_mod))
                        file_matched = True
                        break
                if file_matched:
                    break
            if not file_matched:
                unmatched_files.append(f)

        return SuggestModulesResult(
            suggested_modules=sorted(suggested),
            matched_files=matched_files,
            unmatched_files=unmatched_files,
        )


# ── Module-level singleton ────────────────────────────────────────────────────

risk_selection_service = RiskSelectionService()
