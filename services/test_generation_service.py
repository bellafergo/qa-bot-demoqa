# services/test_generation_service.py
"""
Test Generation Service
=======================

Generates structured draft test cases from:
  - Natural language prompts
  - Requirement / user story text
  - Module signals (explicit or inferred from PR analysis)

Generation is fully deterministic (template-based) for the first version.
No LLM dependency is required. The service can be upgraded with an LLM
layer on top of the same interface.

Draft tests are NOT inserted into the catalog automatically.
Call approve() to persist selected drafts via TestCatalogService.
"""
from __future__ import annotations

import logging
import re
from typing import Dict, List, Optional, Set, Tuple

from models.test_generation_models import (
    ApproveDraftRequest,
    ApproveDraftResponse,
    DraftGeneratedTest,
    TestGenerationRequest,
    TestGenerationResponse,
)

logger = logging.getLogger("vanya.test_generation")


# ── Valid catalog values ───────────────────────────────────────────────────────

_VALID_TYPES     = {"smoke", "regression", "functional", "negative", "e2e"}
_VALID_PRIORITIES = {"low", "medium", "high", "critical"}


def _norm_type(t: Optional[str]) -> str:
    v = (t or "functional").lower().strip()
    return v if v in _VALID_TYPES else "functional"


def _norm_priority(p: Optional[str]) -> str:
    v = (p or "medium").lower().strip()
    return v if v in _VALID_PRIORITIES else "medium"


# ── Domain keyword detection ───────────────────────────────────────────────────

_DOMAIN_SIGNALS: Dict[str, List[str]] = {
    "auth": [
        "login", "auth", "authentication", "password", "session", "token",
        "logout", "signup", "register", "credential", "oauth", "sso", "2fa",
        "forgot password", "reset password", "sign in", "sign up",
    ],
    "checkout": [
        "checkout", "cart", "payment", "order", "purchase", "billing",
        "shipping", "stripe", "paypal", "invoice", "coupon", "discount",
        "promo", "place order", "credit card", "debit card",
    ],
    "search": [
        "search", "filter", "sort", "query", "autocomplete", "facet",
        "elasticsearch", "find", "lookup",
    ],
    "inventory": [
        "inventory", "product", "catalog", "sku", "stock", "item",
        "listing", "warehouse", "category", "price", "availability",
    ],
    "account": [
        "user", "profile", "account", "settings", "preferences",
        "subscription", "plan", "tier", "avatar",
    ],
    "forms": [
        "form", "input", "field", "textarea", "select", "checkbox",
        "radio", "submit", "validate", "validation", "textbox",
    ],
    "navigation": [
        "nav", "navigation", "router", "routing", "redirect", "menu",
        "breadcrumb", "sidebar", "header", "footer", "link",
    ],
}

# Domain priority: if multiple match, prefer higher-value domains for test gen
_DOMAIN_PRIORITY = ["auth", "checkout", "inventory", "search", "account", "forms", "navigation"]


def _detect_domains(text: str) -> Set[str]:
    """Return domain names whose keywords appear in the lowercased text."""
    h = text.lower().replace("-", " ").replace("_", " ").replace("/", " ")
    return {domain for domain, kws in _DOMAIN_SIGNALS.items() if any(k in h for k in kws)}


def _infer_domains(req: TestGenerationRequest) -> List[str]:
    """Collect all relevant domain names from request signals, ordered by priority."""
    found: Set[str] = set()

    # Explicit module hint
    if req.module:
        m = req.module.lower().strip()
        # Map the explicit module to a known domain if possible
        for domain, kws in _DOMAIN_SIGNALS.items():
            if m == domain or any(k in m for k in [domain] + kws[:3]):
                found.add(domain)
                break
        else:
            # Unknown domain — add verbatim so templates can fall back to generic
            found.add(m)

    # changed_modules from PR signals
    for mod in req.changed_modules:
        found |= _detect_domains(mod)
        # Also treat the module name itself as a hint
        found |= _detect_domains(mod.replace("-", " "))

    # Free-text signals
    for text in (req.prompt or "", req.requirement_text or "", req.title or ""):
        if text:
            found |= _detect_domains(text)

    # Sort by domain priority, unknowns at the end
    known = [d for d in _DOMAIN_PRIORITY if d in found]
    unknown = sorted(found - set(_DOMAIN_PRIORITY))
    return known + unknown


# ── Test templates ────────────────────────────────────────────────────────────
#
# Each entry: name, type, priority, rationale, steps, assertions, confidence
# Steps use catalog format: action / target / value
# Assertions use catalog format: type / value / target

_TEMPLATES: Dict[str, List[Dict]] = {

    "auth": [
        {
            "name": "Login valid user",
            "type": "smoke", "priority": "high", "confidence": "high",
            "rationale": "Verify the main login happy path works with valid credentials.",
            "steps": [
                {"action": "goto",  "value": "/login"},
                {"action": "input", "target": "username field", "value": "valid_user"},
                {"action": "input", "target": "password field", "value": "valid_password"},
                {"action": "click", "target": "login button"},
            ],
            "assertions": [
                {"type": "url_contains",  "value": "dashboard"},
                {"type": "text_visible",  "value": "Welcome"},
            ],
        },
        {
            "name": "Login with invalid password shows error",
            "type": "negative", "priority": "high", "confidence": "high",
            "rationale": "Authentication must reject incorrect credentials and show an error.",
            "steps": [
                {"action": "goto",  "value": "/login"},
                {"action": "input", "target": "username field", "value": "valid_user"},
                {"action": "input", "target": "password field", "value": "wrong_password"},
                {"action": "click", "target": "login button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "invalid"},
            ],
        },
        {
            "name": "Missing password validation",
            "type": "negative", "priority": "medium", "confidence": "medium",
            "rationale": "The login form must require a password before submission.",
            "steps": [
                {"action": "goto",  "value": "/login"},
                {"action": "input", "target": "username field", "value": "valid_user"},
                {"action": "click", "target": "login button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "required"},
            ],
        },
        {
            "name": "Logout after login redirects to login page",
            "type": "smoke", "priority": "medium", "confidence": "high",
            "rationale": "Session management: logout must clear session and return to login.",
            "steps": [
                {"action": "goto",  "value": "/login"},
                {"action": "input", "target": "username field", "value": "valid_user"},
                {"action": "input", "target": "password field", "value": "valid_password"},
                {"action": "click", "target": "login button"},
                {"action": "click", "target": "logout button"},
            ],
            "assertions": [
                {"type": "url_contains", "value": "login"},
            ],
        },
        {
            "name": "Session redirect after login",
            "type": "functional", "priority": "medium", "confidence": "medium",
            "rationale": "After login the user should land on the expected post-auth page.",
            "steps": [
                {"action": "goto",  "value": "/login"},
                {"action": "input", "target": "username field", "value": "valid_user"},
                {"action": "input", "target": "password field", "value": "valid_password"},
                {"action": "click", "target": "login button"},
            ],
            "assertions": [
                {"type": "url_contains",        "value": "dashboard"},
                {"type": "element_visible",  "target": "nav"},
            ],
        },
    ],

    "checkout": [
        {
            "name": "Checkout happy path completes order",
            "type": "smoke", "priority": "critical", "confidence": "high",
            "rationale": "The full purchase flow must succeed end-to-end after checkout changes.",
            "steps": [
                {"action": "goto",  "value": "/cart"},
                {"action": "click", "target": "proceed to checkout"},
                {"action": "input", "target": "card number field", "value": "4242424242424242"},
                {"action": "input", "target": "expiry field", "value": "12/26"},
                {"action": "input", "target": "cvv field", "value": "123"},
                {"action": "click", "target": "place order button"},
            ],
            "assertions": [
                {"type": "text_visible",    "value": "Order confirmed"},
                {"type": "url_contains",    "value": "confirmation"},
            ],
        },
        {
            "name": "Invalid payment card shows validation error",
            "type": "negative", "priority": "high", "confidence": "high",
            "rationale": "Payment validation must reject bad card numbers.",
            "steps": [
                {"action": "goto",  "value": "/checkout"},
                {"action": "input", "target": "card number field", "value": "0000000000000000"},
                {"action": "click", "target": "place order button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "invalid card"},
            ],
        },
        {
            "name": "Cart total recalculates correctly",
            "type": "functional", "priority": "medium", "confidence": "medium",
            "rationale": "Price totals and discounts must remain accurate after cart/checkout changes.",
            "steps": [
                {"action": "goto",  "value": "/cart"},
                {"action": "input", "target": "quantity field", "value": "2"},
                {"action": "click", "target": "update cart button"},
            ],
            "assertions": [
                {"type": "element_visible", "target": ".cart-total"},
            ],
        },
        {
            "name": "Order confirmation visible after purchase",
            "type": "smoke", "priority": "high", "confidence": "medium",
            "rationale": "Confirmation page must render with order details after successful payment.",
            "steps": [
                {"action": "goto",  "value": "/checkout/confirmation"},
            ],
            "assertions": [
                {"type": "text_visible",    "value": "Thank you"},
                {"type": "element_visible", "target": ".order-id"},
            ],
        },
        {
            "name": "Empty cart checkout is blocked",
            "type": "negative", "priority": "medium", "confidence": "medium",
            "rationale": "Users should not be able to proceed to checkout with an empty cart.",
            "steps": [
                {"action": "goto",  "value": "/cart"},
                {"action": "click", "target": "proceed to checkout"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "empty"},
            ],
        },
    ],

    "search": [
        {
            "name": "Search returns expected item",
            "type": "smoke", "priority": "high", "confidence": "high",
            "rationale": "Core search must return relevant results for a known query.",
            "steps": [
                {"action": "goto",  "value": "/"},
                {"action": "input", "target": "search field", "value": "test item"},
                {"action": "click", "target": "search button"},
            ],
            "assertions": [
                {"type": "element_visible", "target": ".search-result"},
                {"type": "text_visible",    "value": "test item"},
            ],
        },
        {
            "name": "Empty search result shows informative message",
            "type": "negative", "priority": "medium", "confidence": "high",
            "rationale": "No-results state must show a clear message instead of a blank page.",
            "steps": [
                {"action": "goto",  "value": "/"},
                {"action": "input", "target": "search field", "value": "xyzzy_nonexistent_item_99999"},
                {"action": "click", "target": "search button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "no results"},
            ],
        },
        {
            "name": "Product detail opens from search result",
            "type": "functional", "priority": "medium", "confidence": "medium",
            "rationale": "Clicking a search result must navigate to the correct product detail page.",
            "steps": [
                {"action": "goto",  "value": "/"},
                {"action": "input", "target": "search field", "value": "test item"},
                {"action": "click", "target": "search button"},
                {"action": "click", "target": "first search result"},
            ],
            "assertions": [
                {"type": "url_contains",    "value": "product"},
                {"type": "element_visible", "target": ".product-detail"},
            ],
        },
        {
            "name": "Inventory change is visible in search results",
            "type": "functional", "priority": "low", "confidence": "low",
            "rationale": "Stock status updates must propagate to search listings.",
            "steps": [
                {"action": "goto",  "value": "/products"},
            ],
            "assertions": [
                {"type": "element_visible", "target": ".product-card"},
            ],
        },
        {
            "name": "Invalid SKU search shows no results",
            "type": "negative", "priority": "medium", "confidence": "medium",
            "rationale": "An invalid SKU search must not crash and must return no-results state.",
            "steps": [
                {"action": "goto",  "value": "/"},
                {"action": "input", "target": "search field", "value": "INVALID-SKU-000"},
                {"action": "click", "target": "search button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "no results"},
            ],
        },
    ],

    "inventory": [
        {
            "name": "Product listing page loads",
            "type": "smoke", "priority": "high", "confidence": "high",
            "rationale": "Product listing must render correctly after inventory/catalog changes.",
            "steps": [
                {"action": "goto",    "value": "/products"},
                {"action": "wait_ms", "ms": 1000},
            ],
            "assertions": [
                {"type": "text_visible",    "value": "Products"},
                {"type": "element_visible", "target": ".product-card"},
            ],
        },
        {
            "name": "Product detail page displays correct info",
            "type": "functional", "priority": "medium", "confidence": "medium",
            "rationale": "Product detail view must show accurate name, price, and stock status.",
            "steps": [
                {"action": "goto", "value": "/products/1"},
            ],
            "assertions": [
                {"type": "element_visible", "target": ".product-name"},
                {"type": "element_visible", "target": ".product-price"},
            ],
        },
        {
            "name": "Out of stock product cannot be added to cart",
            "type": "negative", "priority": "medium", "confidence": "low",
            "rationale": "Unavailable products must block cart addition.",
            "steps": [
                {"action": "goto",  "value": "/products/out-of-stock"},
                {"action": "click", "target": "add to cart button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "out of stock"},
            ],
        },
    ],

    "account": [
        {
            "name": "Update profile happy path",
            "type": "smoke", "priority": "high", "confidence": "high",
            "rationale": "Profile update must save correctly after account/user changes.",
            "steps": [
                {"action": "goto",  "value": "/profile"},
                {"action": "input", "target": "display name field", "value": "Test User Updated"},
                {"action": "click", "target": "save profile button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "saved"},
            ],
        },
        {
            "name": "Required field validation on profile update",
            "type": "negative", "priority": "medium", "confidence": "medium",
            "rationale": "Profile form must block submission when required fields are empty.",
            "steps": [
                {"action": "goto",  "value": "/profile"},
                {"action": "input", "target": "display name field", "value": ""},
                {"action": "click", "target": "save profile button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "required"},
            ],
        },
        {
            "name": "Password change with valid current password",
            "type": "functional", "priority": "high", "confidence": "high",
            "rationale": "Password change flow must succeed with the correct current password.",
            "steps": [
                {"action": "goto",  "value": "/settings/security"},
                {"action": "input", "target": "current password", "value": "current_pass"},
                {"action": "input", "target": "new password",     "value": "new_pass_123!"},
                {"action": "input", "target": "confirm password", "value": "new_pass_123!"},
                {"action": "click", "target": "change password button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "updated"},
            ],
        },
        {
            "name": "Unauthorized access to settings is blocked",
            "type": "negative", "priority": "critical", "confidence": "high",
            "rationale": "Unauthenticated users must not access protected account pages.",
            "steps": [
                {"action": "goto", "value": "/settings"},
            ],
            "assertions": [
                {"type": "url_contains", "value": "login"},
            ],
        },
    ],

    "forms": [
        {
            "name": "Form submits and shows confirmation",
            "type": "smoke", "priority": "medium", "confidence": "medium",
            "rationale": "Form submission happy path must work correctly after form changes.",
            "steps": [
                {"action": "goto",  "value": "/form"},
                {"action": "input", "target": "name field",  "value": "Test User"},
                {"action": "input", "target": "email field", "value": "test@example.com"},
                {"action": "click", "target": "submit button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "submitted"},
            ],
        },
        {
            "name": "Required field validation on empty submit",
            "type": "negative", "priority": "medium", "confidence": "medium",
            "rationale": "Empty required fields must prevent form submission and show errors.",
            "steps": [
                {"action": "goto",  "value": "/form"},
                {"action": "click", "target": "submit button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "required"},
            ],
        },
        {
            "name": "Invalid email format is rejected",
            "type": "negative", "priority": "medium", "confidence": "medium",
            "rationale": "Email fields must validate format before form submission.",
            "steps": [
                {"action": "goto",  "value": "/form"},
                {"action": "input", "target": "email field", "value": "not-an-email"},
                {"action": "click", "target": "submit button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "valid email"},
            ],
        },
    ],

    "navigation": [
        {
            "name": "Navigation links render correctly",
            "type": "smoke", "priority": "medium", "confidence": "medium",
            "rationale": "Routing/nav changes must not break the main navigation structure.",
            "steps": [
                {"action": "goto", "value": "/"},
            ],
            "assertions": [
                {"type": "element_visible", "target": "nav"},
                {"type": "text_visible",    "value": "Home"},
            ],
        },
        {
            "name": "Redirect to correct page after navigation",
            "type": "functional", "priority": "low", "confidence": "low",
            "rationale": "Router changes must preserve all existing redirect rules.",
            "steps": [
                {"action": "goto", "value": "/"},
                {"action": "click", "target": "first nav link"},
            ],
            "assertions": [
                {"type": "element_visible", "target": "main"},
            ],
        },
    ],

    "_generic": [
        {
            "name": "Page renders successfully",
            "type": "smoke", "priority": "low", "confidence": "low",
            "rationale": "Basic smoke test: the target page must load without errors.",
            "steps": [
                {"action": "goto", "value": "/"},
            ],
            "assertions": [
                {"type": "element_visible", "target": "body"},
            ],
        },
        {
            "name": "Required fields validation prevents empty submission",
            "type": "negative", "priority": "low", "confidence": "low",
            "rationale": "Generic validation: empty form submission should be blocked.",
            "steps": [
                {"action": "goto",  "value": "/"},
                {"action": "click", "target": "submit button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "required"},
            ],
        },
        {
            "name": "Submit action succeeds for valid input",
            "type": "functional", "priority": "low", "confidence": "low",
            "rationale": "Generic happy path: valid input must result in a successful action.",
            "steps": [
                {"action": "goto",  "value": "/"},
                {"action": "input", "target": "main input field", "value": "test value"},
                {"action": "click", "target": "submit button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "success"},
            ],
        },
        {
            "name": "Invalid input is rejected with informative message",
            "type": "negative", "priority": "low", "confidence": "low",
            "rationale": "Generic negative path: invalid input must produce a clear error.",
            "steps": [
                {"action": "goto",  "value": "/"},
                {"action": "input", "target": "main input field", "value": "!!!invalid!!!"},
                {"action": "click", "target": "submit button"},
            ],
            "assertions": [
                {"type": "text_visible", "value": "error"},
            ],
        },
    ],
}


# ── Generation service ────────────────────────────────────────────────────────

class TestGenerationService:

    def generate(self, req: TestGenerationRequest) -> TestGenerationResponse:
        """
        Generate draft test cases from all signals in the request.

        Strategy:
          1. Infer affected domains from text/module signals
          2. Pull templates for each domain (deduplicated by name)
          3. Override type/priority when explicitly requested
          4. Apply max_drafts limit
          5. Fall back to generic templates when no domain matches
        """
        domains = _infer_domains(req)
        notes:  List[str] = []
        source_signal = self._describe_source(req, domains)

        if domains:
            strategy = f"template-based:{'+'.join(domains[:3])}"
        else:
            strategy = "template-based:generic"
            notes.append("No specific domain detected — using generic test templates.")

        drafts = self._collect_drafts(req, domains, source_signal, notes)
        drafts = drafts[: req.max_drafts]

        logger.info(
            "test_generation: generated %d drafts for domains=%s strategy=%s",
            len(drafts), domains, strategy,
        )

        return TestGenerationResponse(
            drafts=drafts,
            total_drafts=len(drafts),
            generation_strategy=strategy,
            notes=notes,
        )

    def approve(self, req: ApproveDraftRequest) -> ApproveDraftResponse:
        """
        Persist approved draft tests into the Test Catalog.

        Each draft gets a unique TC-GEN-XXXX ID.
        Conflicts (already-existing IDs or names) cause the draft to be skipped.
        """
        from models.test_case import TestCaseCreate
        from services.test_catalog_service import catalog_service

        created: List[str] = []
        skipped: List[str] = []

        gen_ids = self._allocate_gen_ids(len(req.drafts))
        status = "active" if req.activate else "inactive"

        for draft, tc_id in zip(req.drafts, gen_ids):
            try:
                payload = TestCaseCreate(
                    test_case_id = tc_id,
                    name         = draft.name,
                    module       = draft.module,
                    type         = _norm_type(draft.type),
                    priority     = _norm_priority(draft.priority),
                    status       = status,
                    tags         = ["generated"],
                    steps        = draft.steps,
                    assertions   = draft.assertions,
                )
                catalog_service.create_test_case(payload)
                created.append(tc_id)
                logger.info("test_generation: approved draft '%s' → %s", draft.name, tc_id)
            except ValueError as e:
                logger.warning("test_generation: skipped draft '%s' — %s", draft.name, e)
                skipped.append(draft.draft_id)
            except Exception:
                logger.exception("test_generation: unexpected error approving '%s'", draft.name)
                skipped.append(draft.draft_id)

        return ApproveDraftResponse(
            created_test_case_ids = created,
            skipped_draft_ids     = skipped,
            total_created         = len(created),
            total_skipped         = len(skipped),
        )

    # ── Internal helpers ──────────────────────────────────────────────────────

    def _collect_drafts(
        self,
        req: TestGenerationRequest,
        domains: List[str],
        source_signal: str,
        notes: List[str],
    ) -> List[DraftGeneratedTest]:
        seen_names: Set[str] = set()
        drafts: List[DraftGeneratedTest] = []

        template_domains = domains if domains else ["_generic"]

        for domain in template_domains:
            templates = _TEMPLATES.get(domain, [])
            if not templates:
                notes.append(f"No templates found for domain '{domain}' — skipped.")
                continue

            for tmpl in templates:
                if tmpl["name"] in seen_names:
                    continue
                seen_names.add(tmpl["name"])

                tc_type     = _norm_type(req.type or tmpl["type"])
                tc_priority = _norm_priority(req.priority or tmpl["priority"])
                tc_module   = req.module or domain
                confidence  = tmpl["confidence"]

                # Downgrade confidence to low for multi-domain generic fallback
                if domain == "_generic":
                    confidence = "low"

                drafts.append(DraftGeneratedTest(
                    name         = tmpl["name"],
                    module       = tc_module,
                    type         = tc_type,
                    priority     = tc_priority,
                    rationale    = tmpl["rationale"],
                    steps        = list(tmpl.get("steps", [])),
                    assertions   = list(tmpl.get("assertions", [])),
                    confidence   = confidence,
                    source_signal = source_signal,
                ))

        # If no templates produced drafts (all unknown domains) → fall back to generic
        if not drafts:
            notes.append("No domain-specific templates matched — falling back to generic tests.")
            for tmpl in _TEMPLATES["_generic"]:
                if tmpl["name"] in seen_names:
                    continue
                seen_names.add(tmpl["name"])
                drafts.append(DraftGeneratedTest(
                    name          = tmpl["name"],
                    module        = req.module or "general",
                    type          = _norm_type(req.type or tmpl["type"]),
                    priority      = _norm_priority(req.priority or tmpl["priority"]),
                    rationale     = tmpl["rationale"],
                    steps         = list(tmpl.get("steps", [])),
                    assertions    = list(tmpl.get("assertions", [])),
                    confidence    = "low",
                    source_signal = source_signal,
                ))

        return drafts

    def _describe_source(self, req: TestGenerationRequest, domains: List[str]) -> str:
        parts: List[str] = []
        if req.source:
            parts.append(req.source)
        if req.title:
            parts.append(f"title: {req.title[:60]}")
        elif req.prompt:
            parts.append(f"prompt: {req.prompt[:60]}")
        elif req.requirement_text:
            parts.append(f"req: {req.requirement_text[:60]}")
        if domains:
            parts.append(f"domains: {', '.join(domains[:3])}")
        return " | ".join(parts)

    def _allocate_gen_ids(self, count: int) -> List[str]:
        """Generate the next N available TC-GEN-XXXX IDs from the catalog."""
        from services.db.catalog_repository import catalog_repo

        all_tcs = catalog_repo.list_test_cases(status=None, limit=10000)
        nums: List[int] = []
        for tc in all_tcs:
            if tc.test_case_id.startswith("TC-GEN-"):
                suffix = tc.test_case_id[7:]  # after "TC-GEN-"
                if suffix.isdigit():
                    nums.append(int(suffix))

        start = max(nums) + 1 if nums else 1
        return [f"TC-GEN-{start + i:04d}" for i in range(count)]


# ── Module-level singleton ────────────────────────────────────────────────────

generation_service = TestGenerationService()
