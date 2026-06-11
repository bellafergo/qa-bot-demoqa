# services/capability_mapping_service.py
"""
Shared deterministic business capability mapping (ROI-01C / QMETRY-01B).

Single source of truth for module keywords and free-text capability resolution.
No AI, embeddings, or scoring — mapping only.
"""
from __future__ import annotations

import re
from typing import Dict, Optional, Tuple

# Token → capability for free-text fields (Jira summaries, journey names, etc.)
_CAPABILITY_MAP: Dict[str, str] = {
    "payments": "Revenue Collection",
    "payment": "Revenue Collection",
    "checkout": "Customer Purchase Flow",
    "authentication": "Customer Access",
    "auth": "Customer Access",
    "orders": "Order Processing",
    "order": "Order Processing",
    "inventory": "Inventory Accuracy",
    "candidates": "Recruiting Operations",
    "candidate": "Recruiting Operations",
    "recruiting": "Recruiting Operations",
}

# Longer tokens first for deterministic substring matching on module / test names.
_MODULE_KEYWORD_RULES: Tuple[Tuple[str, str, str], ...] = (
    ("checkout", "Checkout", "Customer Purchase Flow"),
    ("payments", "Payments", "Revenue Collection"),
    ("payment", "Payment", "Revenue Collection"),
    ("authentication", "Authentication", "Customer Access"),
    ("login", "Login", "Customer Access"),
    ("orders", "Orders", "Order Processing"),
    ("order", "Order", "Order Processing"),
    ("inventory", "Inventory", "Inventory Accuracy"),
    ("recruiting", "Recruiting", "Recruiting Operations"),
    ("candidate", "Candidate", "Recruiting Operations"),
)

_KNOWN_CAPABILITIES: Tuple[str, ...] = (
    "Revenue Collection",
    "Customer Purchase Flow",
    "Customer Access",
    "Order Processing",
    "Inventory Accuracy",
    "Recruiting Operations",
)


def _normalize_key(text: str) -> str:
    return re.sub(r"[^a-z0-9]+", " ", (text or "").strip().lower()).strip()


def map_text_to_capability(text: str) -> Optional[str]:
    """Map free text to a business capability using token/substring rules."""
    key = _normalize_key(text)
    if not key:
        return None
    for token, capability in _CAPABILITY_MAP.items():
        if token in key.split():
            return capability
        if token in key:
            return capability
    for token, capability in _CAPABILITY_MAP.items():
        if key.startswith(token) or key.endswith(token):
            return capability
    return None


def match_module_keyword(text: str) -> Optional[Tuple[str, str, str]]:
    """Match module keyword rules; return (module_label, capability, match_reason)."""
    haystack = (text or "").lower()
    if not haystack:
        return None
    for keyword, module, capability in _MODULE_KEYWORD_RULES:
        if keyword in haystack:
            return module, capability, f'Test case name contains "{keyword.title()}"'
    return None


def map_module_to_capability(text: str) -> Optional[str]:
    """Map module or test-case text to a business capability via keyword rules."""
    hit = match_module_keyword(text)
    return hit[1] if hit else None


def module_label_for_text(text: str) -> Optional[str]:
    """Return display module label when text matches a keyword rule."""
    hit = match_module_keyword(text)
    return hit[0] if hit else None


def known_capabilities() -> Tuple[str, ...]:
    return _KNOWN_CAPABILITIES


def known_module_keywords() -> Tuple[Tuple[str, str, str], ...]:
    return _MODULE_KEYWORD_RULES
