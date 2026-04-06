# core/site_profiles.py
"""
Registered site / demo execution profiles — keeps demo-specific behavior out of
scattered string checks in the core.

A profile applies when ``base_url`` (or any URL string) contains one of
``host_suffixes`` (substring match, case-insensitive).

Adding a real customer profile later: register a new ``SiteExecutionProfile``
with host_suffixes for their staging domain and optional ``semantic_targets`` /
``llm_execute_appendix`` — no need to edit step_compiler conditionals.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional, Tuple


@dataclass(frozen=True)
class SiteExecutionProfile:
    """Bundled behavior for a known public demo or a customer site."""

    profile_id: str
    host_suffixes: Tuple[str, ...]
    # (kind, name) -> partial target {primary, fallbacks} for semantic_step_builder
    semantic_targets: Dict[Tuple[str, str], Dict[str, Any]] = field(default_factory=dict)
    # Natural-language login parse in step_compiler (deterministic prompt → steps)
    enable_nl_login_parse: bool = False
    # When True, compiler may inject known demo credentials for side-effect flows
    inject_demo_credentials: bool = False
    demo_username: str = ""
    demo_password: str = ""
    login_error_selector: str = ""
    post_login_url_contains: str = ""
    post_login_inventory_assert_selector: str = ""
    # Substrings in user prompt → treat as execute target without URL (intent_router)
    intent_markers_without_url: Tuple[str, ...] = ()
    # Appended to EXECUTE system prompt when base_url matches (LLM fallback)
    llm_execute_appendix: str = ""


# ── Sauce Demo (public training site) ─────────────────────────────────────────
_SAUCEDEMO_SEMANTIC: Dict[Tuple[str, str], Dict[str, Any]] = {
    ("input", "username"): {
        "primary": "#user-name",
        "fallbacks": [
            {"type": "css", "value": "input[name='user-name']"},
            {"type": "css", "value": "input[data-test='username']"},
            {"type": "name", "value": "user-name"},
            {"type": "placeholder", "value": "Username"},
            {"type": "label", "value": "Username"},
        ],
    },
    ("input", "password"): {
        "primary": "#password",
        "fallbacks": [
            {"type": "css", "value": "input[name='password']"},
            {"type": "css", "value": "input[type='password']"},
            {"type": "placeholder", "value": "Password"},
            {"type": "label", "value": "Password"},
        ],
    },
    ("button", "login"): {
        "primary": "#login-button",
        "fallbacks": [
            {"type": "css", "value": "button[type='submit']"},
            {"type": "css", "value": "input[type='submit']"},
            {"type": "text", "value": "Login"},
            {"type": "role", "value": {"role": "button", "name": "Login"}},
        ],
    },
    ("text", "error"): {
        "primary": "[data-test='error']",
        "fallbacks": [
            {"type": "css", "value": ".error-message-container"},
            {"type": "css", "value": "[data-testid='error']"},
        ],
    },
}

_SAUCEDEMO_LLM_APPENDIX = """
SITE-SPECIFIC RULES (training demo — only when Base URL is this site)
When domain includes "saucedemo.com", ALWAYS use these stable selectors:

LOGIN:
- username field:       #user-name
- password field:       #password
- login button:         #login-button
- login error:          [data-test="error"]
- login success:        .inventory_list

ADD TO CART (CRITICAL — "Add to cart" appears multiple times; NEVER use text alone):
- Sauce Labs Backpack:  [data-test="add-to-cart-sauce-labs-backpack"]
- Sauce Labs Bike Light: [data-test="add-to-cart-sauce-labs-bike-light"]
- Sauce Labs Bolt T-Shirt: [data-test="add-to-cart-sauce-labs-bolt-t-shirt"]
- Sauce Labs Fleece Jacket: [data-test="add-to-cart-sauce-labs-fleece-jacket"]
- Generic add-to-cart (last resort): .btn_inventory  (only when product is unambiguous)
- NEVER use get_by_text("Add to cart") when multiple products exist.

CART / NAVIGATION:
- Cart icon:            .shopping_cart_link
- Cart badge (count):   .shopping_cart_badge

CHECKOUT FLOW:
- Checkout button (cart page):  [data-test="checkout"]
- First name field:             [data-test="firstName"]
- Last name field:              [data-test="lastName"]
- Postal code field:            [data-test="postalCode"]
- Continue button:              [data-test="continue"]
- Finish button:                [data-test="finish"]
- Checkout summary container:   .checkout_summary_container
- Order complete message:       .complete-header or [data-test="complete-header"]
""".strip()


PROFILE_SAUCEDEMO = SiteExecutionProfile(
    profile_id="saucedemo",
    host_suffixes=("saucedemo.com",),
    semantic_targets=_SAUCEDEMO_SEMANTIC,
    enable_nl_login_parse=True,
    inject_demo_credentials=True,
    demo_username="standard_user",
    demo_password="secret_sauce",
    login_error_selector="[data-test='error']",
    post_login_url_contains="inventory.html",
    post_login_inventory_assert_selector=".inventory_list",
    intent_markers_without_url=("saucedemo", "saucedemo.com"),
    llm_execute_appendix=_SAUCEDEMO_LLM_APPENDIX,
)

REGISTERED_PROFILES: Tuple[SiteExecutionProfile, ...] = (PROFILE_SAUCEDEMO,)


def resolve_site_profile(url: Optional[str]) -> Optional[SiteExecutionProfile]:
    """Return the first registered profile whose host suffix matches ``url``."""
    if not url:
        return None
    u = url.lower()
    for prof in REGISTERED_PROFILES:
        if any(s in u for s in prof.host_suffixes):
            return prof
    return None


def semantic_override_for_url(
    url: Optional[str], kind: str, name: str
) -> Optional[Dict[str, Any]]:
    """Partial target dict (primary/fallbacks) or None."""
    prof = resolve_site_profile(url)
    if not prof:
        return None
    key = ((kind or "").strip().lower(), (name or "").strip().lower())
    return prof.semantic_targets.get(key)


def collect_intent_markers_from_profiles() -> List[str]:
    """Markers merged into intent_router (execute without explicit URL)."""
    out: List[str] = []
    for prof in REGISTERED_PROFILES:
        out.extend(prof.intent_markers_without_url)
    return out


def profile_llm_appendix(url: Optional[str]) -> str:
    """Extra EXECUTE system prompt text for the matched profile, or empty."""
    prof = resolve_site_profile(url)
    if not prof or not prof.llm_execute_appendix:
        return ""
    return prof.llm_execute_appendix.strip()


def iter_registered_profiles() -> Iterable[SiteExecutionProfile]:
    return REGISTERED_PROFILES
