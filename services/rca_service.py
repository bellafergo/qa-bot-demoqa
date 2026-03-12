# services/rca_service.py
"""
Root Cause Analysis (RCA) Service
===================================

Analyzes failed test runs and classifies the most probable cause of failure
using deterministic rule-based heuristics applied to:
  - runner log lines
  - step-level error messages and statuses
  - run metadata

No LLM, no external services, no new DB tables required.
Analysis is computed on demand from persisted TestRun records.

Classification priority (higher index = lower priority, checked last):
  auth_issue > api_failure > selector_issue > timeout_issue >
  assertion_issue > navigation_issue > data_issue > environment_issue > unknown
"""
from __future__ import annotations

import logging
import re
from typing import Dict, List, Optional, Tuple

from models.rca_models import (
    ImpactedLayer,
    RCAAnalysisResult,
    RCAConfidence,
    RCAEvidenceSignal,
    RootCauseCategory,
)
from models.test_run import TestRun

logger = logging.getLogger("vanya.rca")


# ── Pattern definitions ───────────────────────────────────────────────────────
#
# Each entry: (compiled_regex, signal_type_label, default_confidence)
# Evaluated case-insensitively against every log line and step error.

_PAT_TIMEOUT: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"timeout", re.I),                     "timeout",                  "high"),
    (re.compile(r"timed out", re.I),                   "timed_out",                "high"),
    (re.compile(r"navigation timeout", re.I),           "navigation_timeout",       "high"),
    (re.compile(r"exceeded.*wait", re.I),               "exceeded_wait",            "medium"),
    (re.compile(r"waiting.*selector.*timeout", re.I),   "waiting_for_selector",     "high"),
    (re.compile(r"page\.waitfor", re.I),                "waitfor_call",             "medium"),
]

_PAT_SELECTOR: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"selector not found", re.I),           "selector_not_found",       "high"),
    (re.compile(r"locator not found", re.I),            "locator_not_found",        "high"),
    (re.compile(r"element not (visible|found)", re.I),  "element_not_visible",      "high"),
    (re.compile(r"no element matching", re.I),          "no_element_matching",      "high"),
    (re.compile(r"strict mode violation", re.I),        "strict_mode_violation",    "medium"),
    (re.compile(r"\[RESOLVE_FAIL\]", re.I),             "resolve_fail",             "high"),
    (re.compile(r"unable to find.*element", re.I),      "unable_to_find_element",   "high"),
    (re.compile(r"can'?t find.*element", re.I),         "cant_find_element",        "high"),
    (re.compile(r"waiting for.*selector", re.I),        "waiting_for_selector",     "medium"),
]

_PAT_ASSERTION: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"assertionerror", re.I),               "assertion_error",          "high"),
    (re.compile(r"assertion.*fail", re.I),              "assertion_failure",        "high"),
    (re.compile(r"assert.*fail", re.I),                 "assert_fail",              "high"),
    (re.compile(r"texto no encontrado", re.I),          "text_not_found",           "high"),
    (re.compile(r"text not found", re.I),               "text_not_found",           "high"),
    (re.compile(r"expected.*not in", re.I),             "expected_not_in",          "medium"),
    (re.compile(r"assert_text_contains.*fail", re.I),   "assert_text_fail",         "high"),
    (re.compile(r"assert_url_contains.*fal", re.I),     "assert_url_fail",          "high"),
    (re.compile(r"url.*fail.*contains", re.I),          "url_contains_fail",        "medium"),
    (re.compile(r"expected contiene", re.I),            "text_not_found",           "high"),
]

_PAT_API: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"status 5\d\d", re.I),                "http_5xx",                 "high"),
    (re.compile(r"http.?5\d\d", re.I),                 "http_5xx",                 "high"),
    (re.compile(r"500 internal", re.I),                "http_500",                 "high"),
    (re.compile(r"502 bad gateway", re.I),             "http_502",                 "high"),
    (re.compile(r"503 service", re.I),                 "http_503",                 "high"),
    (re.compile(r"connection refused", re.I),           "connection_refused",       "high"),
    (re.compile(r"connection error", re.I),             "connection_error",         "medium"),
    (re.compile(r"failed to fetch", re.I),              "fetch_failed",             "medium"),
    (re.compile(r"network error", re.I),                "network_error",            "medium"),
    (re.compile(r"response validation failed", re.I),   "response_validation",      "high"),
    (re.compile(r"net::err_", re.I),                    "net_err",                  "high"),
]

_PAT_AUTH: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"unauthorized", re.I),                 "unauthorized",             "high"),
    (re.compile(r"forbidden", re.I),                    "forbidden",                "high"),
    (re.compile(r"\b401\b", re.I),                      "http_401",                 "high"),
    (re.compile(r"\b403\b", re.I),                      "http_403",                 "high"),
    (re.compile(r"login failed", re.I),                 "login_failed",             "high"),
    (re.compile(r"token expired", re.I),                "token_expired",            "high"),
    (re.compile(r"session expired", re.I),              "session_expired",          "high"),
    (re.compile(r"access denied", re.I),                "access_denied",            "high"),
    (re.compile(r"authentication failed", re.I),        "auth_failed",              "high"),
]

_PAT_NAVIGATION: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"redirect loop", re.I),                "redirect_loop",            "high"),
    (re.compile(r"page did not load", re.I),            "page_not_loaded",          "high"),
    (re.compile(r"navigation failed", re.I),            "navigation_failed",        "high"),
    (re.compile(r"goto.*fail", re.I),                   "goto_fail",                "medium"),
    (re.compile(r"url.*not.*match", re.I),              "url_mismatch",             "medium"),
    (re.compile(r"err_name_not_resolved", re.I),        "dns_error",                "high"),
    (re.compile(r"err_connection_refused", re.I),       "conn_refused_nav",         "high"),
    (re.compile(r"net::err_connection", re.I),          "net_connection_err",       "high"),
]

_PAT_DATA: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"missing test data", re.I),            "missing_test_data",        "high"),
    (re.compile(r"invalid fixture", re.I),              "invalid_fixture",          "high"),
    (re.compile(r"null.*required", re.I),               "null_required_field",      "medium"),
    (re.compile(r"no records found", re.I),             "no_records_found",         "medium"),
    (re.compile(r"data not found", re.I),               "data_not_found",           "medium"),
    (re.compile(r"missing.*field", re.I),               "missing_field",            "medium"),
    (re.compile(r"required.*empty", re.I),              "required_field_empty",     "medium"),
    (re.compile(r"no such.*record", re.I),              "no_such_record",           "medium"),
]

_PAT_ENVIRONMENT: List[Tuple[re.Pattern, str, RCAConfidence]] = [
    (re.compile(r"service unavailable", re.I),          "service_unavailable",      "high"),
    (re.compile(r"database.*error", re.I),              "database_error",           "high"),
    (re.compile(r"config.*missing", re.I),              "config_missing",           "medium"),
    (re.compile(r"env.*not set", re.I),                 "env_not_set",              "medium"),
    (re.compile(r"deployment.*fail", re.I),             "deployment_fail",          "medium"),
]

# Ordered pattern groups: (patterns, category, layer)
# Classification checks them in order; first match with highest score wins.
_CATEGORY_PATTERNS: List[Tuple[
    List[Tuple[re.Pattern, str, RCAConfidence]],
    RootCauseCategory,
    ImpactedLayer,
]] = [
    (_PAT_AUTH,         "auth_issue",         "auth"),
    (_PAT_API,          "api_failure",        "api"),
    (_PAT_SELECTOR,     "selector_issue",     "ui"),
    (_PAT_TIMEOUT,      "timeout_issue",      "ui"),
    (_PAT_ASSERTION,    "assertion_issue",    "ui"),
    (_PAT_NAVIGATION,   "navigation_issue",   "ui"),
    (_PAT_DATA,         "data_issue",         "data"),
    (_PAT_ENVIRONMENT,  "environment_issue",  "environment"),
]


# ── Recommendations ───────────────────────────────────────────────────────────

_RECOMMENDATIONS: Dict[str, str] = {
    "selector_issue":    "Review the locator strategy or recent UI changes around the failing element. Consider updating selectors or adding data-testid attributes.",
    "timeout_issue":     "Inspect loading delays, network latency, or backend response times. Consider increasing step timeouts or adding explicit waits.",
    "assertion_issue":   "Verify expected text, URL fragments, or element visibility. Check for recent content changes or conditional rendering.",
    "api_failure":       "Inspect backend/service logs and API health for the failing endpoint. Check for recent deployments or connectivity issues.",
    "auth_issue":        "Verify test credentials, token expiry, and session configuration. Check for auth service availability and test account status.",
    "data_issue":        "Review test data setup and required fixture entities. Ensure prerequisite data exists before test execution.",
    "navigation_issue":  "Check that the target URL is correct and reachable. Inspect for redirect loops, DNS issues, or environment routing problems.",
    "environment_issue": "Verify the test environment is healthy: services are running, config is complete, and infrastructure is available.",
    "unknown":           "Inspect the full run log for error details. No specific pattern was matched — manual review is recommended.",
}

_PROBABLE_CAUSES: Dict[str, str] = {
    "selector_issue":    "A UI element could not be located. The selector may be stale, the page may have changed, or the element may not have rendered.",
    "timeout_issue":     "A step exceeded its allowed time. The page or element may be loading slowly, or a backend call is delayed.",
    "assertion_issue":   "A test assertion did not match the actual page state. Expected content or URL was not found.",
    "api_failure":       "A backend service returned an error (5xx) or was unreachable. The API may be down or overloaded.",
    "auth_issue":        "An authentication or authorization check failed. Test credentials may be invalid, expired, or lacking required permissions.",
    "data_issue":        "Required test data was missing or invalid. A fixture, record, or field needed by the test was not available.",
    "navigation_issue":  "The browser failed to navigate to the expected page. The URL may be wrong, or the server may be unreachable.",
    "environment_issue": "An infrastructure or configuration issue prevented the test from running. The environment may be misconfigured or unavailable.",
    "unknown":           "The failure cause could not be determined from available signals. Manual investigation is recommended.",
}


# ── Helpers ───────────────────────────────────────────────────────────────────

def _scan_text(
    text: str,
    patterns: List[Tuple[re.Pattern, str, RCAConfidence]],
    source: str,
) -> List[RCAEvidenceSignal]:
    """Return all evidence signals found in *text* for the given pattern list."""
    signals: List[RCAEvidenceSignal] = []
    for pat, signal_type, conf in patterns:
        m = pat.search(text)
        if m:
            signals.append(RCAEvidenceSignal(
                signal_type = signal_type,
                value       = text[:200].strip(),
                source      = source,
                confidence  = conf,
            ))
    return signals


def _aggregate_confidence(signals: List[RCAEvidenceSignal]) -> RCAConfidence:
    """Aggregate signal confidences into a single level."""
    if not signals:
        return "low"
    highs   = sum(1 for s in signals if s.confidence == "high")
    mediums = sum(1 for s in signals if s.confidence == "medium")
    if highs >= 2 or (highs >= 1 and mediums >= 1):
        return "high"
    if highs == 1 or mediums >= 1:
        return "medium"
    return "low"


# ── Main service ──────────────────────────────────────────────────────────────

class RCAService:

    def analyze_run_id(self, run_id: str) -> RCAAnalysisResult:
        """Load run from DB and analyze it."""
        from services.db.test_run_repository import test_run_repo
        run = test_run_repo.get_run(run_id)
        if run is None:
            raise ValueError(f"Run '{run_id}' not found")
        return self.analyze(run)

    def analyze(self, run: TestRun) -> RCAAnalysisResult:
        """Analyze a TestRun directly and return RCA result."""

        # Passed runs need no analysis
        if run.status == "pass":
            return RCAAnalysisResult(
                run_id              = run.run_id,
                root_cause_category = "unknown",
                probable_cause      = "Test passed — no failure to analyze.",
                confidence          = "high",
                impacted_layer      = "unknown",
                evidence_signals    = [],
                recommendation      = "No action required. This run passed.",
                summary             = f"Run {run.run_id} passed. RCA is not applicable.",
            )

        all_signals = self._extract_signals(run)
        category, layer, matched_signals = self._classify(all_signals)
        confidence = _aggregate_confidence(matched_signals)
        if not matched_signals:
            confidence = "low"

        recommendation = _RECOMMENDATIONS.get(category, _RECOMMENDATIONS["unknown"])
        probable_cause = self._build_probable_cause(run, category, matched_signals)
        summary        = self._build_summary(run, category, confidence, probable_cause)

        logger.info(
            "rca: run=%s status=%s category=%s confidence=%s signals=%d",
            run.run_id, run.status, category, confidence, len(matched_signals),
        )

        return RCAAnalysisResult(
            run_id              = run.run_id,
            root_cause_category = category,
            probable_cause      = probable_cause,
            confidence          = confidence,
            impacted_layer      = layer,
            evidence_signals    = matched_signals,
            recommendation      = recommendation,
            summary             = summary,
        )

    # ── Signal extraction ─────────────────────────────────────────────────────

    def _extract_signals(self, run: TestRun) -> Dict[str, List[RCAEvidenceSignal]]:
        """
        Scan all text sources and return a dict keyed by category name.
        Each value is the list of evidence signals found for that category.
        """
        # Build a flat list of text blobs with their sources
        texts: List[Tuple[str, str]] = []

        for line in run.logs:
            texts.append((str(line), "log"))

        for step in run.steps_result:
            if step.get("status") in ("failed", "error"):
                err = str(step.get("error") or "")
                if err:
                    texts.append((err, "step_error"))
                action = str(step.get("action") or "")
                if action:
                    texts.append((f"{action}: {err}", "step_error"))

        # Meta fields
        for key in ("runner_reason", "runner_outcome"):
            val = run.meta.get(key)
            if val:
                texts.append((str(val), "meta"))

        # Scan all texts against each category's patterns
        result: Dict[str, List[RCAEvidenceSignal]] = {}
        for patterns, category, _layer in _CATEGORY_PATTERNS:
            category_signals: List[RCAEvidenceSignal] = []
            for text, source in texts:
                category_signals.extend(_scan_text(text, patterns, source))
            if category_signals:
                result[category] = category_signals

        return result

    # ── Classification ────────────────────────────────────────────────────────

    def _classify(
        self,
        signals_by_category: Dict[str, List[RCAEvidenceSignal]],
    ) -> Tuple[RootCauseCategory, ImpactedLayer, List[RCAEvidenceSignal]]:
        """
        Return (category, layer, evidence_signals) by applying priority order.
        The highest-priority category with at least one signal wins.
        """
        for _patterns, category, layer in _CATEGORY_PATTERNS:
            signals = signals_by_category.get(category, [])
            if signals:
                return category, layer, signals

        return "unknown", "unknown", []

    # ── Builders ──────────────────────────────────────────────────────────────

    def _build_probable_cause(
        self,
        run: TestRun,
        category: RootCauseCategory,
        signals: List[RCAEvidenceSignal],
    ) -> str:
        base = _PROBABLE_CAUSES.get(category, _PROBABLE_CAUSES["unknown"])

        # Enrich with the first concrete error message
        first_err = next(
            (s.value[:120] for s in signals if s.source in ("step_error", "log")),
            None,
        )
        if first_err:
            return f"{base} Evidence: \"{first_err.strip()[:120]}\""
        return base

    def _build_summary(
        self,
        run: TestRun,
        category: RootCauseCategory,
        confidence: RCAConfidence,
        probable_cause: str,
    ) -> str:
        label = category.replace("_", " ").title()
        tc = run.test_case_id or "unknown"
        return (
            f"Run {run.run_id} (test: {tc}) failed with status '{run.status}'. "
            f"RCA category: {label} (confidence: {confidence}). "
            f"{probable_cause[:200]}"
        )


# ── Module-level singleton ────────────────────────────────────────────────────

rca_service = RCAService()
