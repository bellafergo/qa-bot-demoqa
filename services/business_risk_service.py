# services/business_risk_service.py
"""
Business Risk Scoring Service
==============================

Translates technical test failures into business impact signals by:

  1. Loading the TestRun record
  2. Running (or reusing) RCA to understand the failure category
  3. Mapping the impacted test/module to a business flow
  4. Applying deterministic risk rules per flow
  5. Returning a structured BusinessRiskResult

Risk is always deterministic and rule-based in the first version.
No LLM or external services are required.

Business flow → risk level mapping:
  checkout          → critical
  authentication    → high
  product_discovery → medium
  user_profile      → medium
  internal          → low
  unknown           → low
"""
from __future__ import annotations

import logging
from typing import Dict, List, Optional, Tuple

from models.business_risk_models import BRConfidence, BusinessRiskResult, RiskLevel
from models.test_run import TestRun

logger = logging.getLogger("vanya.business_risk")


# ── Flow keyword → business flow ──────────────────────────────────────────────
#
# Checked in order against module name and test name (case-insensitive).
# First keyword that matches wins.

_FLOW_KEYWORDS: List[Tuple[str, str]] = [
    # checkout / payment / orders — highest business value
    ("checkout",  "checkout"),
    ("cart",      "checkout"),
    ("payment",   "checkout"),
    ("order",     "checkout"),
    ("billing",   "checkout"),
    ("purchase",  "checkout"),
    ("shipping",  "checkout"),
    ("invoice",   "checkout"),
    ("coupon",    "checkout"),
    ("stripe",    "checkout"),
    ("paypal",    "checkout"),
    # authentication
    ("login",     "authentication"),
    ("auth",      "authentication"),
    ("logout",    "authentication"),
    ("session",   "authentication"),
    ("password",  "authentication"),
    ("signup",    "authentication"),
    ("register",  "authentication"),
    ("token",     "authentication"),
    ("credential","authentication"),
    ("oauth",     "authentication"),
    # product discovery / catalog
    ("search",    "product_discovery"),
    ("product",   "product_discovery"),
    ("catalog",   "product_discovery"),
    ("inventory", "product_discovery"),
    ("browse",    "product_discovery"),
    ("sku",       "product_discovery"),
    ("listing",   "product_discovery"),
    # user profile / account
    ("profile",   "user_profile"),
    ("settings",  "user_profile"),
    ("account",   "user_profile"),
    ("user",      "user_profile"),
    ("preference","user_profile"),
    ("subscription","user_profile"),
    # internal / admin
    ("admin",     "internal"),
    ("dashboard", "internal"),
    ("config",    "internal"),
    ("internal",  "internal"),
    ("migration", "internal"),
    ("report",    "internal"),
]

# ── Risk level per business flow ──────────────────────────────────────────────

_FLOW_RISK: Dict[str, RiskLevel] = {
    "checkout":          "critical",
    "authentication":    "high",
    "product_discovery": "medium",
    "user_profile":      "medium",
    "internal":          "low",
    "unknown":           "low",
}

# ── Impact summaries ──────────────────────────────────────────────────────────

_FLOW_IMPACT: Dict[str, str] = {
    "checkout":          "Users may be unable to complete purchases. Revenue is directly at risk.",
    "authentication":    "Users may be unable to log in or access the platform.",
    "product_discovery": "Users may be unable to find or browse products.",
    "user_profile":      "Users may experience issues updating personal information or preferences.",
    "internal":          "Internal functionality affected. End-user experience may not be impacted.",
    "unknown":           "Business impact could not be determined from available test signals.",
}

# ── Priority recommendations ──────────────────────────────────────────────────

_RISK_RECOMMENDATION: Dict[str, str] = {
    "critical": "Fix immediately — this failure blocks a revenue-critical user flow.",
    "high":     "Fix today — this failure prevents users from accessing the platform.",
    "medium":   "Schedule fix soon — this failure degrades the user experience.",
    "low":      "Monitor — this failure has limited user-facing impact.",
}


# ── Flow detection ────────────────────────────────────────────────────────────

def _detect_flow(text: str) -> Optional[str]:
    """Return the first matching business flow for the given text, or None."""
    lower = text.lower().replace("-", " ").replace("_", " ").replace("/", " ")
    for keyword, flow in _FLOW_KEYWORDS:
        if keyword in lower:
            return flow
    return None


def _infer_flow(
    module: Optional[str],
    test_name: Optional[str],
    rca_category: Optional[str],
) -> Tuple[str, BRConfidence]:
    """
    Infer the business flow from available signals.

    Priority:
      1. Module name (most specific — set by the test author)
      2. Test case name
      3. RCA category (indirect signal)

    Returns (flow, confidence).
    """
    # 1. Explicit module
    if module:
        flow = _detect_flow(module)
        if flow:
            return flow, "high"

    # 2. Test name
    if test_name:
        flow = _detect_flow(test_name)
        if flow:
            return flow, "medium"

    # 3. RCA category as indirect signal
    if rca_category:
        rca_flow = _detect_flow(rca_category)
        if rca_flow:
            return rca_flow, "low"
        # Direct category → flow fallback
        if rca_category == "auth_issue":
            return "authentication", "medium"
        if rca_category == "api_failure":
            return "unknown", "low"

    return "unknown", "low"


# ── Service ───────────────────────────────────────────────────────────────────

class BusinessRiskService:

    def analyze_run_id(self, run_id: str) -> BusinessRiskResult:
        """Load run from DB and analyze business risk."""
        from services.db.test_run_repository import test_run_repo
        run = test_run_repo.get_run(run_id)
        if run is None:
            raise ValueError(f"Run '{run_id}' not found")
        return self.analyze(run)

    def analyze(self, run: TestRun) -> BusinessRiskResult:
        """Compute business risk for a TestRun."""
        from services.rca_service import rca_service

        # Get RCA — works even for passed runs
        rca = rca_service.analyze(run)

        # Module from run metadata (set by test_catalog_service when executing)
        module    = run.meta.get("tc_module") or ""
        test_name = run.test_name or ""

        flow, confidence = _infer_flow(
            module      = module or None,
            test_name   = test_name or None,
            rca_category = rca.root_cause_category,
        )

        risk_level      = _FLOW_RISK.get(flow, "low")
        impact_summary  = _FLOW_IMPACT.get(flow, _FLOW_IMPACT["unknown"])
        recommendation  = _RISK_RECOMMENDATION[risk_level]

        # For passed runs there is no real risk — downgrade regardless of flow
        if run.status == "pass":
            risk_level     = "low"
            impact_summary = "Test passed — no business impact."
            recommendation = _RISK_RECOMMENDATION["low"]
            confidence     = "high"

        logger.info(
            "business_risk: run=%s status=%s flow=%s risk=%s confidence=%s",
            run.run_id, run.status, flow, risk_level, confidence,
        )

        return BusinessRiskResult(
            run_id                  = run.run_id,
            risk_level              = risk_level,
            affected_business_flow  = flow,
            impact_summary          = impact_summary,
            priority_recommendation = recommendation,
            confidence              = confidence,
            rca_category            = rca.root_cause_category,
            test_module             = module or None,
        )


# ── Module-level singleton ────────────────────────────────────────────────────

business_risk_service = BusinessRiskService()
