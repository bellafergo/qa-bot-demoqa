# services/coverage_expansion_service.py
"""
Coverage Expansion Service
===========================

Compares discovered UI flows against the existing Test Catalog to identify:
  - covered flows   — likely already tested (module+name overlap)
  - uncovered flows — discovered but under-represented in catalog

Generates draft UI tests for uncovered flows by reusing the existing
TestGenerationService templates with exploration-sourced signals.

Coverage heuristic:
  A flow is considered "covered" if the catalog has at least
  _MIN_TESTS_FOR_COVERAGE active UI tests in the same module AND
  at least one existing test name or step URL partially overlaps
  with the flow's path keywords.
"""
from __future__ import annotations

import logging
import re
from collections import defaultdict
from typing import Any, Dict, List, Optional, Set, Tuple

from models.exploration_models import (
    CoverageExpansionResult,
    DiscoveredFlow,
    ExplorationRequest,
)

logger = logging.getLogger("vanya.coverage_expansion")

# Minimum catalog tests in a module for a flow to be considered "covered"
_MIN_TESTS_FOR_COVERAGE = 2


# ── Path keyword extractor ────────────────────────────────────────────────────

def _url_keywords(url: str) -> Set[str]:
    """Extract meaningful path segments from a URL."""
    try:
        path = re.sub(r"https?://[^/]+", "", url)
        segments = re.split(r"[/\-_?&=]+", path.lower())
        # Filter out numbers, short tokens, and common noise
        return {s for s in segments if len(s) > 2 and not s.isdigit()}
    except Exception:
        return set()


def _name_keywords(name: str) -> Set[str]:
    """Extract keywords from a test name or flow name."""
    words = re.split(r"[\s\-_/→>]+", (name or "").lower())
    return {w for w in words if len(w) > 2}


# ── Coverage checker ──────────────────────────────────────────────────────────

def _build_catalog_index() -> Dict[str, Any]:
    """
    Build an in-memory coverage index from the active UI test catalog.

    Returns:
      {
        module → {
          "count": int,
          "names": List[str],
          "url_keywords": Set[str],   # aggregated from all test step URLs
        }
      }
    """
    from services.db.catalog_repository import catalog_repo

    tests = catalog_repo.list_test_cases(status="active", test_type="ui", limit=5000)
    index: Dict[str, Dict[str, Any]] = defaultdict(lambda: {
        "count": 0, "names": [], "url_keywords": set()
    })

    for tc in tests:
        mod = (tc.module or "ui").lower()
        index[mod]["count"]  += 1
        index[mod]["names"].append(tc.name.lower())

        # Extract URL keywords from goto steps
        for step in tc.steps:
            action = (step.action or "").lower()
            if action in ("goto",):
                val = step.value or step.url or ""
                index[mod]["url_keywords"] |= _url_keywords(val)

    return dict(index)


def _flow_is_covered(flow: DiscoveredFlow, catalog_index: Dict[str, Any]) -> bool:
    """
    Determine whether a discovered flow is likely already covered.

    Heuristic:
      1. The flow's module must appear in the catalog with >= MIN_TESTS_FOR_COVERAGE tests.
      2. At least one URL keyword from the flow path overlaps with catalog URL keywords
         OR the flow's name keywords overlap with existing test names.
    """
    module = flow.inferred_module.lower()
    entry  = catalog_index.get(module)

    if entry is None or entry["count"] < _MIN_TESTS_FOR_COVERAGE:
        return False

    # Keyword overlap from URLs
    flow_url_kw = _url_keywords(flow.start_url) | _url_keywords(flow.end_url)
    if flow_url_kw & entry["url_keywords"]:
        return True

    # Name keyword overlap with existing test names
    flow_name_kw = _name_keywords(flow.name)
    for test_name in entry["names"]:
        test_kw = _name_keywords(test_name)
        if flow_name_kw & test_kw:
            return True

    # If module is well-covered (>= 5 tests), consider all its flows covered
    if entry["count"] >= 5:
        return True

    return False


# ── Draft generation from uncovered flows ────────────────────────────────────

def _generate_drafts_for_flow(flow: DiscoveredFlow, max_drafts: int = 2) -> List[Dict[str, Any]]:
    """
    Generate draft UI test suggestions for an uncovered flow.

    Reuses TestGenerationService with exploration-specific signals.
    """
    from models.test_generation_models import TestGenerationRequest
    from services.test_generation_service import generation_service

    try:
        req = TestGenerationRequest(
            title            = flow.name,
            module           = flow.inferred_module,
            prompt           = (
                f"Discovered UI flow: '{flow.name}' "
                f"navigating from {flow.start_url} to {flow.end_url}."
            ),
            source           = "exploration",
            max_drafts       = max_drafts,
        )
        result = generation_service.generate(req)
        drafts = []
        for d in result.drafts:
            d_dict = d.model_dump()
            d_dict["source_signal"] = "exploration"
            drafts.append(d_dict)
        return drafts
    except Exception as exc:
        logger.warning("coverage_expansion: draft generation failed for flow %s — %s", flow.flow_id, exc)
        return []


# ── Service ───────────────────────────────────────────────────────────────────

class CoverageExpansionService:

    def expand(
        self,
        req:               ExplorationRequest,
        exploration_result = None,   # optional pre-built ExplorationResult
    ) -> CoverageExpansionResult:
        """
        Run exploration (or use a provided result) and compare against catalog.

        Returns covered/uncovered flow analysis + optional draft suggestions.
        """
        from services.exploration_service import exploration_service

        if exploration_result is None:
            exploration_result = exploration_service.explore(req)

        flows = exploration_result.discovered_flows
        notes: List[str] = list(exploration_result.notes)

        if not flows:
            notes.append("No flows discovered — nothing to compare against catalog.")
            return CoverageExpansionResult(
                discovered_flows = flows,
                covered_flows    = [],
                uncovered_flows  = [],
                coverage_ratio   = 0.0,
                suggested_drafts = [],
                notes            = notes,
            )

        catalog_index = _build_catalog_index()
        covered: List[DiscoveredFlow]   = []
        uncovered: List[DiscoveredFlow] = []

        for flow in flows:
            if _flow_is_covered(flow, catalog_index):
                covered.append(flow)
            else:
                uncovered.append(flow)

        coverage_ratio = round(len(covered) / len(flows), 4) if flows else 0.0

        logger.info(
            "coverage_expansion: %d flows total — %d covered, %d uncovered (ratio=%.2f)",
            len(flows), len(covered), len(uncovered), coverage_ratio,
        )

        # Generate draft suggestions for uncovered flows
        suggested_drafts: List[Dict[str, Any]] = []
        if req.generate_draft_tests:
            for flow in uncovered:
                drafts = _generate_drafts_for_flow(flow, max_drafts=2)
                suggested_drafts.extend(drafts)
            if suggested_drafts:
                notes.append(
                    f"Generated {len(suggested_drafts)} draft suggestion(s) for "
                    f"{len(uncovered)} uncovered flow(s)."
                )

        if uncovered:
            modules = sorted({f.inferred_module for f in uncovered})
            notes.append(f"Uncovered modules: {', '.join(modules)}.")

        return CoverageExpansionResult(
            discovered_flows = flows,
            covered_flows    = covered,
            uncovered_flows  = uncovered,
            coverage_ratio   = coverage_ratio,
            suggested_drafts = suggested_drafts,
            notes            = notes,
        )


# ── Module-level singleton ────────────────────────────────────────────────────

coverage_expansion_service = CoverageExpansionService()
