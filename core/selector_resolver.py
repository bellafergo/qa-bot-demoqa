# core/selector_resolver.py
"""
Resolves intent strings ("username field", "login button") to real Playwright
selector strategies using a DOM inventory captured by dom_analyzer.

Resolution priority per element type:
  Inputs:  testid > ariaLabel > label > placeholder > name > id
  Buttons: testid > ariaLabel > text > value > name > id
  Links:   ariaLabel > text > id

Returns a resolution dict or None when nothing matches above the min threshold.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Tuple

# Words stripped from intent before keyword matching
_NOISE = frozenset({
    "field", "input", "box", "textbox", "button", "link", "the", "a", "an",
    "on", "for", "my", "in", "of", "with", "to", "form", "element", "el",
    "la", "click", "fill", "type", "press", "enter",
    # NOTE: "submit" intentionally omitted — it can be both an action AND an element identifier
})

_MIN_SCORE = 40  # minimum score (0-100) to accept a resolution


# ── Keyword helpers ────────────────────────────────────────────────────────────

def _keywords(text: str) -> List[str]:
    """Normalize intent → list of meaningful lowercase keywords."""
    words = re.findall(r"[a-z0-9]+", text.lower())
    return [w for w in words if w not in _NOISE and len(w) >= 2]


def _score(candidate: str, keywords: List[str]) -> int:
    """Score a candidate field value against intent keywords. Returns 0-100."""
    if not candidate or not keywords:
        return 0
    c = candidate.lower().strip()
    c_words = set(re.findall(r"[a-z0-9]+", c))
    kw_set = set(keywords)

    # Full exact match
    if c == " ".join(keywords):
        return 100
    # All keywords present in candidate
    if kw_set.issubset(c_words):
        return 90
    # First keyword exact match
    if keywords[0] == c:
        return 85
    # Each keyword scored individually
    total = 0
    for kw in keywords:
        if kw in c_words:
            total += 70
        elif kw in c:
            total += 40
        elif any(kw in cw for cw in c_words):
            total += 20
    return min(100, total)


def _best_match(
    items: List[Dict[str, Any]],
    fields: List[str],
    keywords: List[str],
) -> Optional[Tuple[Dict[str, Any], str, int]]:
    """
    Find best matching item by scoring the given fields against keywords.
    Returns (item, field_name, score) or None.
    """
    best_item, best_field, best_score = None, None, 0
    for item in items:
        for field in fields:
            val = item.get(field)
            if not val or not isinstance(val, str):
                continue
            s = _score(val, keywords)
            if s > best_score:
                best_score, best_item, best_field = s, item, field
    if best_score >= _MIN_SCORE:
        return best_item, best_field, best_score
    return None


# ── Per-element-type resolvers ─────────────────────────────────────────────────

def _resolve_input(inputs: List[Dict[str, Any]], keywords: List[str]) -> Optional[Dict[str, Any]]:
    fields = ["testid", "ariaLabel", "label", "placeholder", "name", "id"]
    result = _best_match(inputs, fields, keywords)
    if not result:
        return None
    item, field, score = result

    if field == "testid" and item.get("testid"):
        return {"strategy": "testid", "value": item["testid"], "score": score,
                "element_type": "input", "source_field": "testid"}
    if field == "ariaLabel" and item.get("ariaLabel"):
        return {"strategy": "label", "value": item["ariaLabel"], "score": score,
                "element_type": "input", "source_field": "ariaLabel"}
    if field == "label" and item.get("label"):
        return {"strategy": "label", "value": item["label"], "score": score,
                "element_type": "input", "source_field": "label"}
    if field == "placeholder" and item.get("placeholder"):
        return {"strategy": "placeholder", "value": item["placeholder"], "score": score,
                "element_type": "input", "source_field": "placeholder"}
    if field == "name" and item.get("name"):
        return {"strategy": "name", "value": item["name"], "score": score,
                "element_type": "input", "source_field": "name"}
    if field == "id" and item.get("id"):
        return {"strategy": "css", "value": f"#{item['id']}", "score": score,
                "element_type": "input", "source_field": "id"}
    return None


def _resolve_button(buttons: List[Dict[str, Any]], keywords: List[str]) -> Optional[Dict[str, Any]]:
    fields = ["testid", "ariaLabel", "text", "value", "name", "id"]
    result = _best_match(buttons, fields, keywords)
    if not result:
        return None
    item, field, score = result

    if field == "testid" and item.get("testid"):
        return {"strategy": "testid", "value": item["testid"], "score": score,
                "element_type": "button", "source_field": "testid"}
    if field == "ariaLabel" and item.get("ariaLabel"):
        return {"strategy": "label", "value": item["ariaLabel"], "score": score,
                "element_type": "button", "source_field": "ariaLabel"}
    txt = item.get("text") or item.get("value") or ""
    if field in ("text", "value") and txt:
        return {"strategy": "role", "value": txt, "score": score,
                "element_type": "button", "source_field": field}
    if field == "name" and item.get("name"):
        return {"strategy": "name", "value": item["name"], "score": score,
                "element_type": "button", "source_field": "name"}
    if field == "id" and item.get("id"):
        return {"strategy": "css", "value": f"#{item['id']}", "score": score,
                "element_type": "button", "source_field": "id"}
    return None


def _resolve_link(links: List[Dict[str, Any]], keywords: List[str]) -> Optional[Dict[str, Any]]:
    fields = ["ariaLabel", "text", "id"]
    result = _best_match(links, fields, keywords)
    if not result:
        return None
    item, field, score = result

    if field == "ariaLabel" and item.get("ariaLabel"):
        return {"strategy": "label", "value": item["ariaLabel"], "score": score,
                "element_type": "link", "source_field": "ariaLabel"}
    if field == "text" and item.get("text"):
        return {"strategy": "text", "value": item["text"], "score": score,
                "element_type": "link", "source_field": "text"}
    if field == "id" and item.get("id"):
        return {"strategy": "css", "value": f"#{item['id']}", "score": score,
                "element_type": "link", "source_field": "id"}
    return None


# ── Public API ────────────────────────────────────────────────────────────────

def resolve_intent(
    dom_inventory: Dict[str, Any],
    action: str,
    intent: str,
) -> Optional[Dict[str, Any]]:
    """
    Resolve a natural-language intent string to a selector strategy.

    Args:
        dom_inventory: Output of dom_analyzer.extract_dom_inventory()
        action: Playwright action ("fill", "click", "press", "assert_visible", …)
        intent: Human description of the element ("username field", "login button", …)

    Returns:
        Resolution dict or None if nothing matched.
    """
    if not intent or not dom_inventory:
        return None
    kws = _keywords(intent)
    if not kws:
        return None

    if action in ("fill", "press"):
        return _resolve_input(dom_inventory.get("inputs") or [], kws)

    if action == "click":
        result = _resolve_button(dom_inventory.get("buttons") or [], kws)
        if not result:
            result = _resolve_link(dom_inventory.get("links") or [], kws)
        return result

    if action in ("assert_visible", "assert_not_visible"):
        for resolver, key in (
            (_resolve_button, "buttons"),
            (_resolve_input, "inputs"),
            (_resolve_link, "links"),
        ):
            result = resolver(dom_inventory.get(key) or [], kws)
            if result:
                return result

    return None


def build_playwright_target(resolution: Dict[str, Any]) -> Dict[str, Any]:
    """
    Convert a resolution dict to a generic_steps target dict compatible with
    resolve_locator() in services/selector_healer.py.
    """
    strategy = resolution.get("strategy", "")
    value = resolution.get("value", "")

    if strategy == "testid":
        primary = f"[data-testid='{value}']"
        fallbacks = [{"type": "testid", "value": value}]
    elif strategy == "label":
        primary = f"text={value}"
        fallbacks = [{"type": "label", "value": value}]
    elif strategy == "placeholder":
        primary = f"[placeholder='{value}']"
        fallbacks = [{"type": "placeholder", "value": value}]
    elif strategy == "name":
        primary = f"[name='{value}']"
        fallbacks = [{"type": "name", "value": value}]
    elif strategy == "role":
        primary = f"text={value}"
        fallbacks = [{"type": "role", "role": "button", "name": value}]
    elif strategy == "text":
        primary = f"text={value}"
        fallbacks = [{"type": "text", "value": value}]
    else:  # css or unknown
        primary = value
        fallbacks = []

    return {
        "primary": primary,
        "fallbacks": fallbacks,
        "intent": resolution.get("source_field", ""),
        "resolved_by": "selector_resolver",
        "resolution_score": resolution.get("score", 0),
        "element_type": resolution.get("element_type", ""),
    }


def is_intent_only(selector: str) -> bool:
    """
    Returns True if the selector looks like a natural-language intent
    rather than a real CSS/XPath selector.
    Selectors with #, ., [, ], :, >, +, ~, = are treated as real CSS.
    """
    if not selector:
        return False
    css_chars = set("#.[]:>+~=\"'()/\\")
    return not any(c in css_chars for c in selector)
