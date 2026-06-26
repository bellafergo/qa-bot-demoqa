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
from typing import Dict, List, Optional, Set, Tuple  # noqa: F401 — Set used in analyze_for_project

from models.pr_analysis_models import (
    DraftTestSuggestion,
    FileChangeClassification,
    FileModuleMapping,
    ImpactedModuleReport,
    PRAnalysisRequest,
    PRAnalysisResult,
    PRRecommendedTest,
    ProjectPRAnalysisReport,
    ProjectPRAnalysisRequest,
    PRRiskSignal,
)
from models.risk_engine_models import ModuleRisk

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

# PR change surface — classify changed paths as UI vs API/backend (substring/regex)
_UI_SURFACE_PATTERNS = [
    r"vanya-frontend",
    r"(?:^|/)src/components/",
    r"(?:^|/)components/",
    r"(?:^|/)src/pages/",
    r"(?:^|/)pages/",
    r"(?:^|/)styles?/",
    r"(?:^|/)frontend/",
    r"(?:^|/)public/",
    r"\.tsx$",
    r"\.jsx$",
    r"\.vue$",
    r"(?:^|/)ui/",
    r"\.css$",
    r"\.scss$",
    r"tailwind",
]

_API_SURFACE_PATTERNS = [
    r"(?:^|/)api/",
    r"(?:^|/)routes/",
    r"(?:^|/)controllers?/",
    r"(?:^|/)middleware/",
    r"(?:^|/)services/",
    r"(?:^|/)backend/",
    r"(?:^|/)server/",
    r"graphql",
    r"openapi",
    r"swagger",
    r"\.controller\.",
    r"_routes?\.",
]


def _classify_change_surface(req: PRAnalysisRequest) -> str:
    """
    Infer whether the PR primarily touches UI, API/backend, or both, from paths.
    Falls back to \"mixed\" when there are no path signals or both kinds appear.
    """
    any_ui = False
    any_api = False
    for path in req.changed_files or []:
        p = path.replace("\\", "/")
        if any(re.search(pat, p, re.IGNORECASE) for pat in _UI_SURFACE_PATTERNS):
            any_ui = True
        if any(re.search(pat, p, re.IGNORECASE) for pat in _API_SURFACE_PATTERNS):
            any_api = True
    if any_ui and any_api:
        return "mixed"
    if any_ui:
        return "ui"
    if any_api:
        return "api"
    return "mixed"


def _split_recommendations_by_test_type(
    matched_ids: List[str],
    types_by_id: Dict[str, str],
) -> Tuple[List[str], List[str]]:
    """API tests first list; second list is ui + desktop (non-api). Preserves matched_ids order within each bucket."""
    api_ids: List[str] = []
    ui_ids: List[str] = []
    for tc_id in matched_ids:
        tt = (types_by_id.get(tc_id) or "ui").strip().lower()
        if tt == "api":
            api_ids.append(tc_id)
        else:
            ui_ids.append(tc_id)
    return api_ids, ui_ids


def _order_enqueue_test_ids(matched_ids: List[str], types_by_id: Dict[str, str]) -> List[str]:
    """API catalog tests first, then UI/desktop, preserving score order within each group."""
    api_ids, ui_ids = _split_recommendations_by_test_type(matched_ids, types_by_id)
    return api_ids + ui_ids


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


def _best_changed_file_for_domain(domain: str, changed_files: List[str]) -> str:
    """
    Return a short filename from changed_files that is clearly related to domain.
    Uses basename if the path is long. Returns "" if nothing matches.
    """
    import os
    kws = _DOMAIN_KEYWORDS.get(domain, [domain])
    for path in changed_files:
        base = os.path.basename(path)
        if any(k in _lower_tokens(base) for k in kws):
            # Keep it short: just the basename
            return base
    return ""


def _match_domain_keywords(text: str) -> Set[str]:
    """Return all domain names whose keywords appear in text."""
    h = _lower_tokens(text)
    return {domain for domain, kws in _DOMAIN_KEYWORDS.items() if any(k in h for k in kws)}


# ── Main service ──────────────────────────────────────────────────────────────

def _project_baseline_risk_fields(knowledge: Optional["ProjectKnowledge"]) -> Dict[str, object]:
    """Project baseline from System Memory — not PR-specific."""
    if knowledge:
        return {
            "project_risk_score": float(knowledge.risk_score or 0.0),
            "project_risk_level": str(knowledge.risk_level or "LOW"),
        }
    return {
        "project_risk_score": 0.0,
        "project_risk_level": "LOW",
    }


def _compose_and_filter_pr_outputs(
    *,
    file_classifications: List[FileChangeClassification],
    impacted_modules: List[ImpactedModuleReport],
    project_risk_score: float,
    module_risks: List[ModuleRisk],
    recommended_raw: List[PRRecommendedTest],
    unmatched_files: List[str],
) -> Tuple[Dict[str, object], List[PRRecommendedTest], List[PRRecommendedTest], List[str], List[PRRiskSignal]]:
    from services.pr_risk_composer_service import compose_pr_risk
    from services.pr_test_policy_service import filter_recommended_tests_for_pr

    risk = compose_pr_risk(
        file_classifications=file_classifications,
        impacted_modules=impacted_modules,
        project_risk_score=project_risk_score,
        module_risks=module_risks,
        recommended_tests=recommended_raw,
        unmatched_files=unmatched_files,
    )
    policy = filter_recommended_tests_for_pr(
        recommended_raw,
        file_classifications,
        impacted_modules,
        risk.pr_risk_score,
    )
    extra_reasoning = list(risk.reasoning) + list(policy.policy_reasons)
    pr_fields = {
        "pr_risk_score": risk.pr_risk_score,
        "pr_risk_level": risk.pr_risk_level,
        "risk_score": risk.pr_risk_score,
        "risk_level": risk.pr_risk_level,
    }
    return pr_fields, policy.recommended_tests, policy.recommended_tests_raw, extra_reasoning, risk.risk_signals


class PRAnalysisService:

    # ── Entry points ──────────────────────────────────────────────────────────

    def analyze(self, req: PRAnalysisRequest) -> PRAnalysisResult:
        """Full impact analysis: infer modules, score risk, match tests, optionally draft."""
        modules       = self._infer_modules(req)
        risk, reasons = self._score_risk(req, modules)
        change_surface = _classify_change_surface(req)
        matched_ids, match_reasons, types_by_id = self._match_tests(modules, req)
        rec_api, rec_ui = _split_recommendations_by_test_type(matched_ids, types_by_id)
        drafts: List[DraftTestSuggestion] = []
        if req.generate_draft_tests:
            drafts = self._generate_drafts(modules, req)

        job_id: Optional[str] = None
        if req.auto_enqueue and matched_ids:
            ordered = _order_enqueue_test_ids(matched_ids, types_by_id)
            job_id = self._enqueue(ordered, req)

        return self._build_result(
            req,
            modules,
            risk,
            reasons,
            matched_ids,
            match_reasons,
            drafts,
            job_id,
            change_surface=change_surface,
            recommended_api_tests=rec_api,
            recommended_ui_tests=rec_ui,
        )

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
    ) -> Tuple[List[str], Dict[str, str], Dict[str, str]]:
        """
        Match active catalog test cases relevant to inferred modules.

        Matching priority (each test can score multiple points):
          +4  exact module match (module name contains a domain keyword)
          +3  tag match (any tag contains a domain keyword)
          +2  test name keyword match
          +1  type=smoke or priority=critical/high (boost for coverage priorities)

        Returns (test_case_ids sorted by descending score, per-id reason dict,
        test_case_id → catalog test_type for bucketing).
        """
        from services.test_catalog_service import catalog_service

        all_tests = catalog_service.list_test_cases(status="active", limit=500)
        if not all_tests or not modules:
            return [], {}, {}

        # Build a flat set of all keywords associated with the inferred domains
        domain_kws: Set[str] = set()
        for d in modules:
            domain_kws.update(_DOMAIN_KEYWORDS.get(d, [d]))   # fallback: use domain itself
            domain_kws.add(d)

        scored:  List[Tuple[int, str]] = []
        reasons: Dict[str, str]        = {}
        types_by_id: Dict[str, str]    = {}

        for tc in all_tests:
            score = 0
            reason        = ""
            domain_for_cf = modules[0]   # structural domain used for file lookup
            tc_module_lower = _lower_tokens(tc.module)
            tc_name_lower   = _lower_tokens(tc.name)
            tc_tags_lower   = {_lower_tokens(t) for t in tc.tags}

            # Module match
            if any(kw in tc_module_lower for kw in domain_kws):
                score += 4
                hit_domain    = next(
                    (d for d in modules
                     if d in tc_module_lower
                     or any(k in tc_module_lower for k in _DOMAIN_KEYWORDS.get(d, []))),
                    modules[0],
                )
                reason        = f"covers {hit_domain} flow"
                domain_for_cf = hit_domain

            # Tag match
            if any(kw in tag for tag in tc_tags_lower for kw in domain_kws):
                score += 3
                if not reason:
                    tag_domain    = next(
                        (d for d in modules
                         if any(kw in tag for tag in tc_tags_lower
                                for kw in _DOMAIN_KEYWORDS.get(d, [d]))),
                        modules[0],
                    )
                    reason        = f"related to {tag_domain} flow"
                    domain_for_cf = tag_domain

            # Name keyword match
            if any(kw in tc_name_lower for kw in domain_kws):
                score += 2
                if not reason:
                    name_domain   = next(
                        (d for d in modules
                         if any(kw in tc_name_lower for kw in _DOMAIN_KEYWORDS.get(d, [d]))),
                        modules[0],
                    )
                    reason        = f"related to {name_domain} functionality"
                    domain_for_cf = name_domain

            # Coverage priority boost — only applied when there is already a match
            if score > 0:
                if tc.type in ("smoke", "regression"):
                    score += 1
                if tc.priority in ("critical", "high"):
                    score += 1

            if score > 0:
                # Enrich reason with the most relevant changed file, if any
                if reason and req.changed_files:
                    cf = _best_changed_file_for_domain(domain_for_cf, req.changed_files)
                    if cf:
                        reason = f"{reason} ({cf} changed)"

                scored.append((score, tc.test_case_id))
                reasons[tc.test_case_id] = reason or f"relevant to {modules[0]} and related functionality"
                types_by_id[tc.test_case_id] = (
                    getattr(tc, "test_type", None) or "ui"
                ).strip().lower()

        scored.sort(key=lambda x: -x[0])
        return [tc_id for _, tc_id in scored], reasons, types_by_id

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
                project_id=getattr(req, "project_id", None),
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
        *,
        change_surface: str = "mixed",
        recommended_api_tests: Optional[List[str]] = None,
        recommended_ui_tests: Optional[List[str]] = None,
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

        rec_api = list(recommended_api_tests or [])
        rec_ui = list(recommended_ui_tests or [])
        cs = change_surface if change_surface in ("ui", "api", "mixed") else "mixed"

        return PRAnalysisResult(
            pr_id               = req.pr_id,
            inferred_modules    = modules,
            inferred_risk_level = risk,
            risk_reasons        = reasons,
            change_surface       = cs,
            matched_test_case_ids = matched_ids,
            matched_tests_count = n,
            recommended_api_tests = rec_api,
            recommended_ui_tests = rec_ui,
            test_match_reasons  = match_reasons,
            suggested_new_tests = drafts,
            orchestrator_job_id = job_id,
            summary             = summary,
            confidence          = confidence,
        )

    # ── Project-scoped v1 (System Memory + Risk Engine) ─────────────────────

    def analyze_for_project(
        self,
        project_id: str,
        req: ProjectPRAnalysisRequest,
    ) -> ProjectPRAnalysisReport:
        """
        PR Analysis v1: map changed files → catalog modules using System Memory,
        then consume ``module_risks`` and ``recommended_tests`` from Risk Engine
        without recalculating project risk.
        """
        from services.change_classification_service import classify_changed_files
        from services.change_impact_service import map_changed_files, resolve_impacted_modules
        from services.db.catalog_repository import catalog_repo
        from services.pr_analysis_project_debug import log_project_id_lookup
        from services.project_knowledge_service import _resolve_project_name

        pid = (project_id or "").strip()
        if not pid:
            raise ValueError("project_id is required")

        log_project_id_lookup(project_id=project_id)

        changed = [f.strip() for f in (req.changed_files or []) if f and f.strip()]
        if not changed:
            raise ValueError("changed_files must contain at least one path")

        file_classifications = classify_changed_files(changed, req.file_patches)

        project_name = _resolve_project_name(pid)
        knowledge, refresh_attempted = load_project_knowledge_for_analysis(pid)
        log_project_id_lookup(project_id=pid, memory_found=knowledge is not None)

        catalog_modules = sorted({
            m for _, m in (catalog_repo.all_modules_for_project(pid) or []) if m and m.strip()
        })

        if not knowledge:
            reasoning = [
                "No System Memory found for this project.",
                "Refresh memory at /knowledge before running PR Analysis.",
            ]
            if refresh_attempted:
                reasoning.append(
                    "Automatic System Memory refresh did not produce project knowledge."
                )
            pr_fields, filtered_tests, raw_tests, composer_reasoning, risk_signals = _compose_and_filter_pr_outputs(
                file_classifications=file_classifications,
                impacted_modules=[],
                project_risk_score=0.0,
                module_risks=[],
                recommended_raw=[],
                unmatched_files=changed,
            )
            reasoning.extend(composer_reasoning)
            baseline = _project_baseline_risk_fields(None)
            return ProjectPRAnalysisReport(
                project_id=pid,
                project_name=project_name,
                changed_files_count=len(changed),
                file_classifications=file_classifications,
                reasoning=reasoning,
                summary=f"Analyzed {len(changed)} file(s) — no project knowledge available.",
                recommended_tests=filtered_tests,
                recommended_tests_raw=raw_tests,
                risk_signals=risk_signals,
                memory_available=False,
                **baseline,
                **pr_fields,
            )

        file_map = map_changed_files(
            changed,
            knowledge=knowledge,
            catalog_modules=catalog_modules,
        )
        impacted_raw = resolve_impacted_modules(file_map)

        risk_by_mod = {m.module.lower(): m for m in (knowledge.module_risks or []) if m.module}
        file_mappings: List[FileModuleMapping] = []
        for fp, matches in file_map.items():
            if matches:
                mod, conf, reason = matches[0]
                file_mappings.append(FileModuleMapping(
                    file_path=fp, module=mod, confidence=round(conf, 2), reason=reason,
                ))

        impacted_modules: List[ImpactedModuleReport] = []
        impacted_names: Set[str] = set()
        for mod, files, conf in impacted_raw:
            impacted_names.add(mod.lower())
            mr = risk_by_mod.get(mod.lower())
            reasons: List[str] = [f"Matched {len(files)} changed file(s) (confidence {conf:.0%})"]
            if mr:
                if mr.regression_count:
                    reasons.append(f"{mr.regression_count} regression(s) in module")
                if mr.flaky_count:
                    reasons.append(f"{mr.flaky_count} flaky test(s) in module")
                if mr.incident_count:
                    reasons.append(f"{mr.incident_count} incident(s) in module")
                if mr.pass_rate is not None:
                    reasons.append(f"{mr.pass_rate:.0f}% module pass rate")
            impacted_modules.append(ImpactedModuleReport(
                module=mod,
                module_risk_score=mr.module_risk_score if mr else 0.0,
                module_risk_level=mr.module_risk_level if mr else "LOW",
                matched_files=files,
                reasons=reasons,
            ))

        unmatched = [f for f, m in file_map.items() if not m]

        recommended: List[PRRecommendedTest] = []
        seen_tc: Set[str] = set()
        for rec in knowledge.recommended_tests or []:
            if (rec.module or "").lower() in impacted_names and rec.test_case_id:
                if rec.test_case_id not in seen_tc:
                    seen_tc.add(rec.test_case_id)
                    recommended.append(PRRecommendedTest(
                        test_case_id=rec.test_case_id,
                        name=rec.name or rec.test_case_id,
                        module=rec.module or "",
                        reason=rec.reason or "Risk Engine recommendation",
                    ))

        if len(recommended) < 3:
            for tc in knowledge.related_tests or []:
                if (tc.module or "").lower() not in impacted_names:
                    continue
                if not tc.test_case_id or tc.test_case_id in seen_tc:
                    continue
                status = (tc.last_run_status or "").lower()
                if status in ("fail", "error", "failed") or len(recommended) < 5:
                    seen_tc.add(tc.test_case_id)
                    reason = "related test in impacted module"
                    if status in ("fail", "error", "failed"):
                        reason = f"recent {status} in impacted module"
                    recommended.append(PRRecommendedTest(
                        test_case_id=tc.test_case_id,
                        name=tc.name or tc.test_case_id,
                        module=tc.module or "",
                        reason=reason,
                    ))
                if len(recommended) >= 12:
                    break

        from services.pr_risk_composer_service import dominant_change_class

        if not recommended:
            dom = dominant_change_class(file_classifications)
            if dom in ("schema", "config"):
                for rec in knowledge.recommended_tests or []:
                    if not rec.test_case_id:
                        continue
                    blob = f"{rec.name} {rec.reason}".lower()
                    if any(k in blob for k in ("api", "regression", "integration", "smoke", "critical")):
                        recommended.append(PRRecommendedTest(
                            test_case_id=rec.test_case_id,
                            name=rec.name or rec.test_case_id,
                            module=rec.module or "",
                            reason=rec.reason or f"catalog {dom} change fallback",
                        ))
                    if len(recommended) >= 6:
                        break

        reasoning: List[str] = []
        if knowledge.risk_explanation:
            reasoning.extend(knowledge.risk_explanation[:5])
        for im in impacted_modules[:5]:
            if im.module_risk_level in ("HIGH", "CRITICAL"):
                reasoning.append(
                    f"Impacted module '{im.module}' has {im.module_risk_level} risk "
                    f"({im.module_risk_score:.0f}/100)"
                )
        if unmatched:
            reasoning.append(
                f"{len(unmatched)} file(s) could not be mapped to a catalog module "
                f"(add tests or refresh System Memory)"
            )

        comment_only = [c.file_path for c in file_classifications if c.primary_class == "comments"]
        if comment_only:
            reasoning.append(
                f"Change classification: {len(comment_only)} file(s) with comment-only diff"
            )

        recommended_raw = recommended[:12]

        baseline = _project_baseline_risk_fields(knowledge)
        pr_fields, filtered_tests, raw_tests, composer_reasoning, risk_signals = _compose_and_filter_pr_outputs(
            file_classifications=file_classifications,
            impacted_modules=impacted_modules,
            project_risk_score=float(baseline["project_risk_score"]),
            module_risks=knowledge.module_risks or [],
            recommended_raw=recommended_raw,
            unmatched_files=unmatched,
        )
        reasoning.extend(composer_reasoning)

        mod_labels = [im.module for im in impacted_modules[:5]]
        summary_parts = [
            f"Analyzed {len(changed)} file(s) for {project_name}.",
        ]
        if mod_labels:
            summary_parts.append(f"Impacted modules: {', '.join(mod_labels)}.")
        summary_parts.append(
            f"Project risk baseline {baseline['project_risk_level']} "
            f"({baseline['project_risk_score']:.0f}/100)."
        )
        summary_parts.append(
            f"PR change risk {pr_fields['pr_risk_level']} "
            f"({pr_fields['pr_risk_score']:.0f}/100)."
        )
        if filtered_tests:
            summary_parts.append(
                f"Recommended {len(filtered_tests)} test(s) after PR test policy "
                f"({len(raw_tests)} from Risk Engine memory)."
            )

        return ProjectPRAnalysisReport(
            project_id=pid,
            project_name=project_name,
            changed_files_count=len(changed),
            impacted_modules=impacted_modules,
            file_mappings=file_mappings,
            file_classifications=file_classifications,
            recommended_tests=filtered_tests,
            recommended_tests_raw=raw_tests,
            risk_signals=risk_signals,
            reasoning=reasoning[:14],
            summary=" ".join(summary_parts),
            memory_available=True,
            **baseline,
            **pr_fields,
        )


def load_project_knowledge_for_analysis(
    project_id: str,
) -> tuple[Optional["ProjectKnowledge"], bool]:
    """
    Load System Memory for PR Analysis.

    When SQLite has no row (common on cold Render workers), rebuild via refresh
    from catalog/runs/incidents before giving up.
    """
    from models.project_knowledge_models import ProjectKnowledge, ProjectKnowledgeRefreshRequest
    from services.pr_analysis_project_debug import log_memory_refresh_event
    from services.project_knowledge_service import get_project_knowledge, refresh_project_knowledge

    pid = (project_id or "").strip()
    knowledge = get_project_knowledge(pid)
    if knowledge:
        return knowledge, False

    log_memory_refresh_event("memory_missing_before_refresh", project_id=pid)
    refresh_attempted = True
    try:
        refresh_project_knowledge(pid, ProjectKnowledgeRefreshRequest(mode="replace"))
        log_memory_refresh_event("refresh_attempted", project_id=pid)
    except Exception:
        logger.exception("pr_analysis: automatic memory refresh failed project_id=%s", pid)
        log_memory_refresh_event("refresh_failed", project_id=pid)
        return None, refresh_attempted

    knowledge = get_project_knowledge(pid)
    log_memory_refresh_event(
        "memory_found_after_refresh",
        project_id=pid,
        memory_found=knowledge is not None,
    )
    return knowledge, refresh_attempted


# Module-level singleton
pr_analysis_service = PRAnalysisService()
