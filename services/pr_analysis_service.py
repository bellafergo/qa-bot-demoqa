# services/pr_analysis_service.py
"""
PR Impact Analysis Service
==========================

Analyzes a pull request / diff and produces:
  - impacted modules (heuristic keyword matching)
  - risk level (deterministic scoring)
  - matched test cases from the existing catalog
  - optional draft test suggestions

No LLM required for the core flow — all rules are deterministic.
The only optional LLM touchpoint is draft test enrichment, which is
not implemented here and can be added as a decorator layer.

Upgrade path:
  - swap _infer_modules() with a real AST / import-graph analyzer
  - swap _score_risk() with a trained risk classifier
  - swap _generate_drafts() with an LLM-backed suggestion engine
"""
from __future__ import annotations

import logging
import re
from typing import Dict, List, Optional, Set, Tuple

from models.pr_analysis_models import (
    DraftTestSuggestion,
    PRAnalysisRequest,
    PRAnalysisResult,
)

logger = logging.getLogger("vanya.pr_analysis")


# ── Keyword → domain mapping ──────────────────────────────────────────────────
#
# Each key is a canonical domain name.
# Its value is the set of keywords that signal that domain.
# Matching is case-insensitive substring search.

_DOMAIN_KEYWORDS: Dict[str, List[str]] = {
    "auth":      ["login", "auth", "password", "session", "token", "oauth",
                  "signup", "register", "credential", "logout", "sso", "2fa",
                  "forgot-password", "reset-password"],
    "checkout":  ["checkout", "cart", "payment", "order", "purchase",
                  "billing", "shipping", "coupon", "discount", "promo",
                  "stripe", "paypal", "invoice"],
    "inventory": ["inventory", "product", "catalog", "sku", "stock",
                  "item", "listing", "warehouse", "category", "price"],
    "account":   ["user", "profile", "account", "settings", "preferences",
                  "avatar", "subscription", "plan", "tier"],
    "navigation":["nav", "router", "routing", "redirect", "menu",
                  "breadcrumb", "sidebar", "header", "footer", "url"],
    "forms":     ["form", "input", "field", "textarea", "select", "checkbox",
                  "radio", "submit", "validate", "validation", "textbox"],
    "api":       ["api", "endpoint", "controller", "route", "rest",
                  "graphql", "webhook", "response", "request", "schema"],
    "ui":        ["component", "modal", "dialog", "dropdown", "button",
                  "table", "pagination", "tooltip", "notification", "alert"],
    "database":  ["migration", "schema", "db", "database", "sql",
                  "model", "repository", "seed", "fixture", "alembic"],
    "search":    ["search", "filter", "sort", "query", "autocomplete",
                  "facet", "elasticsearch", "index"],
    "reporting": ["report", "dashboard", "metric", "chart", "analytics",
                  "export", "csv", "pdf"],
    "demoqa":    ["demoqa", "textbox", "checkbox", "radiobutton", "webtable",
                  "droppable", "draggable", "resizable"],
    "elements":  ["element", "checkbox", "dynamic", "dropdown", "upload",
                  "download", "iframe", "frame"],
}

# HIGH risk keywords anywhere in the input
_HIGH_RISK_KEYWORDS = [
    "login", "auth", "password", "session", "checkout", "payment",
    "migration", "schema", "billing", "security", "vulnerability",
    "permission", "role", "access", "token", "encryption",
]

# Risk file-path patterns
_HIGH_RISK_PATHS = [
    r"migration", r"schema\.py", r"settings\.py", r"config\.py",
    r"core/", r"common/", r"shared/", r"middleware",
    r"auth", r"login", r"checkout", r"payment",
]

_MEDIUM_RISK_PATHS = [
    r"controller", r"service", r"api/", r"route", r"endpoint",
    r"form", r"validator", r"selector", r"locator",
]

_LOW_RISK_PATHS = [
    r"test_", r"_test\.", r"spec\.", r"\.md$", r"readme",
    r"docs/", r"\.txt$", r"\.rst$",
]

# Diff patterns
_DELETION_HEAVY_RATIO = 0.6   # if >60% of diff lines are deletions → high risk
_MANY_FILES_THRESHOLD  = 15   # more than this many changed files → high risk

# Priority score for matched test cases (used to sort)
_PRIORITY_SCORE = {"critical": 4, "high": 3, "medium": 2, "low": 1}
_TYPE_SCORE     = {"smoke": 4, "regression": 3, "e2e": 2, "functional": 1, "negative": 0}


# ── Draft test templates ──────────────────────────────────────────────────────
#
# Each entry is a dict with the base template for a DraftTestSuggestion.
# They are keyed by domain name.

_DRAFT_TEMPLATES: Dict[str, List[Dict]] = {
    "auth": [
        {
            "name": "Login valid user",
            "rationale": "Auth change detected — verify login happy path still works.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/login"},
                {"action": "input", "target": "username field", "value": "{{test_user}}"},
                {"action": "input", "target": "password field", "value": "{{test_password}}"},
                {"action": "click", "target": "login button"},
            ],
            "suggested_assertions": [
                {"type": "url_contains",  "value": "/dashboard"},
                {"type": "text_visible",  "value": "Welcome"},
            ],
            "source_signal": "auth/login change",
            "confidence": "high",
        },
        {
            "name": "Login with invalid password shows error",
            "rationale": "Negative auth path must still reject bad credentials.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/login"},
                {"action": "input", "target": "username field", "value": "{{test_user}}"},
                {"action": "input", "target": "password field", "value": "wrong-password"},
                {"action": "click", "target": "login button"},
            ],
            "suggested_assertions": [
                {"type": "text_visible", "value": "invalid"},
            ],
            "source_signal": "auth/login change",
            "confidence": "high",
        },
        {
            "name": "Logout redirects to login page",
            "rationale": "Session management should survive auth refactors.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/login"},
                {"action": "input", "target": "username field", "value": "{{test_user}}"},
                {"action": "input", "target": "password field", "value": "{{test_password}}"},
                {"action": "click", "target": "login button"},
                {"action": "click", "target": "logout button"},
            ],
            "suggested_assertions": [
                {"type": "url_contains", "value": "/login"},
            ],
            "source_signal": "session/logout change",
            "confidence": "medium",
        },
    ],
    "checkout": [
        {
            "name": "Checkout happy path completes order",
            "rationale": "Checkout change — verify full purchase flow.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/cart"},
                {"action": "click", "target": "proceed to checkout"},
                {"action": "input", "target": "card number field", "value": "4242424242424242"},
                {"action": "click", "target": "place order button"},
            ],
            "suggested_assertions": [
                {"type": "text_visible", "value": "Order confirmed"},
                {"type": "url_contains", "value": "/confirmation"},
            ],
            "source_signal": "checkout/payment change",
            "confidence": "high",
        },
        {
            "name": "Invalid card shows validation error",
            "rationale": "Payment validation must reject bad card numbers.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/checkout"},
                {"action": "input", "target": "card number field", "value": "0000000000000000"},
                {"action": "click", "target": "place order button"},
            ],
            "suggested_assertions": [
                {"type": "text_visible", "value": "invalid card"},
            ],
            "source_signal": "payment validation change",
            "confidence": "medium",
        },
    ],
    "forms": [
        {
            "name": "Form submits and shows confirmation",
            "rationale": "Form change — verify submit flow still works.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/form"},
                {"action": "input", "target": "name field",  "value": "Test User"},
                {"action": "input", "target": "email field", "value": "test@example.com"},
                {"action": "click", "target": "submit button"},
            ],
            "suggested_assertions": [
                {"type": "text_visible", "value": "submitted"},
            ],
            "source_signal": "form change detected",
            "confidence": "medium",
        },
        {
            "name": "Required field validation on empty submit",
            "rationale": "Validation rules may have changed.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/form"},
                {"action": "click", "target": "submit button"},
            ],
            "suggested_assertions": [
                {"type": "text_visible", "value": "required"},
            ],
            "source_signal": "form validation change",
            "confidence": "medium",
        },
    ],
    "inventory": [
        {
            "name": "Product listing page loads",
            "rationale": "Inventory change — verify product list renders.",
            "suggested_steps": [
                {"action": "goto",    "value": "{{base_url}}/products"},
                {"action": "wait_ms", "ms": 1000},
            ],
            "suggested_assertions": [
                {"type": "text_visible",    "value": "Products"},
                {"type": "element_visible", "target": ".product-card"},
            ],
            "source_signal": "inventory/catalog change",
            "confidence": "medium",
        },
    ],
    "navigation": [
        {
            "name": "Navigation links render correctly",
            "rationale": "Routing change — verify main nav is intact.",
            "suggested_steps": [
                {"action": "goto", "value": "{{base_url}}/"},
            ],
            "suggested_assertions": [
                {"type": "element_visible", "target": "nav"},
                {"type": "text_visible",    "value": "Home"},
            ],
            "source_signal": "navigation/routing change",
            "confidence": "low",
        },
    ],
    "account": [
        {
            "name": "User profile page loads after login",
            "rationale": "Account change — verify profile is accessible.",
            "suggested_steps": [
                {"action": "goto",  "value": "{{base_url}}/login"},
                {"action": "input", "target": "username", "value": "{{test_user}}"},
                {"action": "input", "target": "password", "value": "{{test_password}}"},
                {"action": "click", "target": "login button"},
                {"action": "goto",  "value": "{{base_url}}/profile"},
            ],
            "suggested_assertions": [
                {"type": "text_visible", "value": "Profile"},
            ],
            "source_signal": "account/profile change",
            "confidence": "medium",
        },
    ],
}


# ── Helpers ───────────────────────────────────────────────────────────────────

def _lower_tokens(text: str) -> str:
    """Lower-case and normalize text for keyword matching."""
    return text.lower().replace("-", " ").replace("_", " ").replace("/", " ")


def _contains_any(haystack: str, needles: List[str]) -> bool:
    h = _lower_tokens(haystack)
    return any(n in h for n in needles)


def _match_domain_keywords(text: str) -> Set[str]:
    """Return all domain names whose keywords appear in text."""
    h = _lower_tokens(text)
    return {domain for domain, kws in _DOMAIN_KEYWORDS.items() if any(k in h for k in kws)}


# ── Main service ──────────────────────────────────────────────────────────────

class PRAnalysisService:

    # ── Entry points ──────────────────────────────────────────────────────────

    def analyze(self, req: PRAnalysisRequest) -> PRAnalysisResult:
        """Full impact analysis: infer modules, score risk, match tests, optionally draft."""
        modules      = self._infer_modules(req)
        risk, reasons = self._score_risk(req, modules)
        matched_ids, match_reasons = self._match_tests(modules, req)
        drafts: List[DraftTestSuggestion] = []
        if req.generate_draft_tests:
            drafts = self._generate_drafts(modules, req)

        job_id: Optional[str] = None
        if req.auto_enqueue and matched_ids:
            job_id = self._enqueue(matched_ids, req)

        return self._build_result(req, modules, risk, reasons, matched_ids, match_reasons, drafts, job_id)

    # ── Module inference ──────────────────────────────────────────────────────

    def _infer_modules(self, req: PRAnalysisRequest) -> List[str]:
        """
        Infer impacted semantic domains from all input signals.

        Returns a deduplicated, sorted list of domain names such as
        "auth", "checkout", "forms", plus any values in req.changed_modules.
        """
        found: Set[str] = set()

        # 1. Changed file paths
        for path in req.changed_files:
            found |= _match_domain_keywords(path)

        # 2. PR title and description
        for text in (req.title or "", req.description or ""):
            if text:
                found |= _match_domain_keywords(text)

        # 3. Diff text (heavier — scan for keyword hits)
        if req.diff_text:
            found |= _match_domain_keywords(req.diff_text[:4000])  # cap to avoid huge scans

        # 4. Explicit caller-provided modules (always included, lowercased)
        for m in req.changed_modules:
            found.add(m.lower().strip())

        return sorted(found)

    # ── Risk scoring ──────────────────────────────────────────────────────────

    def _score_risk(
        self,
        req: PRAnalysisRequest,
        modules: List[str],
    ) -> Tuple[str, List[str]]:
        """
        Return (risk_level, [reason, ...]) using deterministic rules.
        """
        reasons: List[str] = []
        score = 0   # 0 = low, 1-2 = medium, 3+ = high

        # ── HIGH risk signals ─────────────────────────────────────────────────
        high_domains = {"auth", "checkout", "database"}
        overlap = high_domains & set(modules)
        if overlap:
            reasons.append(f"High-risk domains affected: {', '.join(sorted(overlap))}")
            score += 3

        n_files = len(req.changed_files)
        if n_files > _MANY_FILES_THRESHOLD:
            reasons.append(f"{n_files} files changed (threshold: {_MANY_FILES_THRESHOLD})")
            score += 2

        high_path_hits = [
            p for p in req.changed_files
            if any(re.search(pat, p, re.IGNORECASE) for pat in _HIGH_RISK_PATHS)
        ]
        if high_path_hits:
            reasons.append(
                f"High-risk file paths: {', '.join(high_path_hits[:5])}"
                + (" …" if len(high_path_hits) > 5 else "")
            )
            score += 2

        if req.diff_text:
            lines = req.diff_text.splitlines()
            del_lines = sum(1 for l in lines if l.startswith("-") and not l.startswith("---"))
            add_lines = sum(1 for l in lines if l.startswith("+") and not l.startswith("+++"))
            total = del_lines + add_lines
            if total > 0 and del_lines / total > _DELETION_HEAVY_RATIO:
                reasons.append(
                    f"Deletion-heavy diff: {del_lines} deletions vs {add_lines} additions"
                )
                score += 2

        # Title / description high-risk keywords
        combined_text = " ".join(filter(None, [req.title, req.description]))
        if _contains_any(combined_text, _HIGH_RISK_KEYWORDS):
            hit_words = [k for k in _HIGH_RISK_KEYWORDS if k in _lower_tokens(combined_text)]
            reasons.append(f"High-risk keywords in PR text: {', '.join(hit_words[:5])}")
            score += 1

        # ── MEDIUM risk signals ───────────────────────────────────────────────
        medium_domains = {"forms", "api", "navigation", "account", "ui"}
        med_overlap = medium_domains & set(modules)
        if med_overlap and score < 3:
            reasons.append(f"Medium-risk domains affected: {', '.join(sorted(med_overlap))}")
            score += 1

        med_path_hits = [
            p for p in req.changed_files
            if any(re.search(pat, p, re.IGNORECASE) for pat in _MEDIUM_RISK_PATHS)
        ]
        if med_path_hits and score < 3:
            reasons.append(
                f"Service/API paths changed: {', '.join(med_path_hits[:3])}"
                + (" …" if len(med_path_hits) > 3 else "")
            )
            score += 1

        # Multiple files in a single module
        if n_files > 3 and score < 3:
            reasons.append(f"{n_files} files changed across modules")
            score += 1

        # ── LOW risk signals ──────────────────────────────────────────────────
        all_low = all(
            any(re.search(pat, p, re.IGNORECASE) for pat in _LOW_RISK_PATHS)
            for p in req.changed_files
        ) if req.changed_files else False

        if all_low and score == 0:
            reasons.append("All changed files appear to be docs/tests (low risk)")

        # ── Final mapping ─────────────────────────────────────────────────────
        if score >= 3:
            level = "high"
        elif score >= 1:
            level = "medium"
        else:
            level = "low"

        if not reasons:
            reasons.append("No specific risk signals detected")

        return level, reasons

    # ── Test matching ─────────────────────────────────────────────────────────

    def _match_tests(
        self, modules: List[str], req: PRAnalysisRequest,
    ) -> Tuple[List[str], Dict[str, str]]:
        """
        Match active catalog test cases relevant to inferred modules.

        Matching priority (each test can score multiple points):
          +4  exact module match (module name contains a domain keyword)
          +3  tag match (any tag contains a domain keyword)
          +2  test name keyword match
          +1  type=smoke or priority=critical/high (boost for coverage priorities)

        Returns (test_case_ids sorted by descending score, per-id reason dict).
        """
        from services.test_catalog_service import catalog_service

        all_tests = catalog_service.list_test_cases(status="active", limit=500)
        if not all_tests or not modules:
            return [], {}

        # Build a flat set of all keywords associated with the inferred domains
        domain_kws: Set[str] = set()
        for d in modules:
            domain_kws.update(_DOMAIN_KEYWORDS.get(d, [d]))   # fallback: use domain itself
            domain_kws.add(d)

        scored:  List[Tuple[int, str]] = []
        reasons: Dict[str, str]        = {}

        for tc in all_tests:
            score = 0
            reason = ""
            tc_module_lower = _lower_tokens(tc.module)
            tc_name_lower   = _lower_tokens(tc.name)
            tc_tags_lower   = {_lower_tokens(t) for t in tc.tags}

            # Module match
            if any(kw in tc_module_lower for kw in domain_kws):
                score += 4
                hit_domain = next(
                    (d for d in modules
                     if d in tc_module_lower
                     or any(k in tc_module_lower for k in _DOMAIN_KEYWORDS.get(d, []))),
                    modules[0],
                )
                reason = f"covers {hit_domain} flow"

            # Tag match
            if any(kw in tag for tag in tc_tags_lower for kw in domain_kws):
                score += 3
                if not reason:
                    tag_domain = next(
                        (d for d in modules
                         if any(kw in tag for tag in tc_tags_lower
                                for kw in _DOMAIN_KEYWORDS.get(d, [d]))),
                        modules[0],
                    )
                    reason = f"related to {tag_domain} flow"

            # Name keyword match
            if any(kw in tc_name_lower for kw in domain_kws):
                score += 2
                if not reason:
                    name_domain = next(
                        (d for d in modules
                         if any(kw in tc_name_lower for kw in _DOMAIN_KEYWORDS.get(d, [d]))),
                        modules[0],
                    )
                    reason = f"related to {name_domain} functionality"

            # Coverage priority boost — only applied when there is already a match
            if score > 0:
                if tc.type in ("smoke", "regression"):
                    score += 1
                if tc.priority in ("critical", "high"):
                    score += 1

            if score > 0:
                scored.append((score, tc.test_case_id))
                reasons[tc.test_case_id] = reason or f"relevant to {modules[0]} and related functionality"

        scored.sort(key=lambda x: -x[0])
        return [tc_id for _, tc_id in scored], reasons

    # ── Draft generation ──────────────────────────────────────────────────────

    def _generate_drafts(
        self,
        modules: List[str],
        req: PRAnalysisRequest,
    ) -> List[DraftTestSuggestion]:
        """
        Return draft test suggestions for affected domains.
        These are NOT inserted into the catalog — they are proposals only.
        """
        seen_names: Set[str] = set()
        drafts: List[DraftTestSuggestion] = []

        for domain in modules:
            templates = _DRAFT_TEMPLATES.get(domain, [])
            for tmpl in templates:
                if tmpl["name"] in seen_names:
                    continue
                seen_names.add(tmpl["name"])
                signal = tmpl.get("source_signal", domain)
                if req.changed_files:
                    # Enrich source_signal with the first matching file
                    first = next(
                        (f for f in req.changed_files
                         if any(k in _lower_tokens(f) for k in _DOMAIN_KEYWORDS.get(domain, []))),
                        None,
                    )
                    if first:
                        signal = f"{signal} (changed: {first})"

                drafts.append(DraftTestSuggestion(
                    name                 = tmpl["name"],
                    module               = domain,
                    rationale            = tmpl["rationale"],
                    suggested_steps      = list(tmpl.get("suggested_steps", [])),
                    suggested_assertions = list(tmpl.get("suggested_assertions", [])),
                    source_signal        = signal,
                    confidence           = tmpl.get("confidence", "medium"),
                ))

        return drafts

    # ── Orchestrator enqueue ──────────────────────────────────────────────────

    def _enqueue(self, matched_ids: List[str], req: PRAnalysisRequest) -> Optional[str]:
        try:
            from services.catalog_orchestrator import orchestrator_service
            job = orchestrator_service.enqueue_suite(
                test_case_ids=matched_ids,
                environment=req.environment or "default",
            )
            logger.info("pr_analysis: enqueued job %s for PR %s", job.job_id, req.pr_id)
            return job.job_id
        except Exception:
            logger.exception("pr_analysis: orchestrator enqueue failed")
            return None

    # ── Result builder ────────────────────────────────────────────────────────

    def _build_result(
        self,
        req:           PRAnalysisRequest,
        modules:       List[str],
        risk:          str,
        reasons:       List[str],
        matched_ids:   List[str],
        match_reasons: Dict[str, str],
        drafts:        List[DraftTestSuggestion],
        job_id:        Optional[str],
    ) -> PRAnalysisResult:
        n = len(matched_ids)

        if n == 0:
            summary = (
                f"No catalog tests matched the inferred modules ({', '.join(modules) or 'none'})."
                " Consider adding test cases or expanding the PR description."
            )
        else:
            summary = (
                f"Found {n} relevant test case(s) across modules: "
                f"{', '.join(modules)}. "
                f"Risk level: {risk}."
            )
            if job_id:
                summary += f" Enqueued as orchestrator job {job_id}."
            if drafts:
                summary += f" Generated {len(drafts)} draft test suggestion(s)."

        # Overall confidence: high if we had clear file/module signals
        if modules and len(req.changed_files) > 0:
            confidence = "high"
        elif modules:
            confidence = "medium"
        else:
            confidence = "low"

        return PRAnalysisResult(
            pr_id               = req.pr_id,
            inferred_modules    = modules,
            inferred_risk_level = risk,
            risk_reasons        = reasons,
            matched_test_case_ids = matched_ids,
            matched_tests_count = n,
            test_match_reasons  = match_reasons,
            suggested_new_tests = drafts,
            orchestrator_job_id = job_id,
            summary             = summary,
            confidence          = confidence,
        )


# Module-level singleton
pr_analysis_service = PRAnalysisService()
