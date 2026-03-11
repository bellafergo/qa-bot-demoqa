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
                "element_type": "input", "source_field": "testid", "element": item}
    if field == "ariaLabel" and item.get("ariaLabel"):
        return {"strategy": "label", "value": item["ariaLabel"], "score": score,
                "element_type": "input", "source_field": "ariaLabel", "element": item}
    if field == "label" and item.get("label"):
        return {"strategy": "label", "value": item["label"], "score": score,
                "element_type": "input", "source_field": "label", "element": item}
    if field == "placeholder" and item.get("placeholder"):
        return {"strategy": "placeholder", "value": item["placeholder"], "score": score,
                "element_type": "input", "source_field": "placeholder", "element": item}
    if field == "name" and item.get("name"):
        return {"strategy": "name", "value": item["name"], "score": score,
                "element_type": "input", "source_field": "name", "element": item}
    if field == "id" and item.get("id"):
        return {"strategy": "css", "value": f"#{item['id']}", "score": score,
                "element_type": "input", "source_field": "id", "element": item}
    return None


def _resolve_button(buttons: List[Dict[str, Any]], keywords: List[str]) -> Optional[Dict[str, Any]]:
    fields = ["testid", "ariaLabel", "text", "value", "name", "id"]
    result = _best_match(buttons, fields, keywords)
    if not result:
        return None
    item, field, score = result

    if field == "testid" and item.get("testid"):
        return {"strategy": "testid", "value": item["testid"], "score": score,
                "element_type": "button", "source_field": "testid", "element": item}
    if field == "ariaLabel" and item.get("ariaLabel"):
        return {"strategy": "label", "value": item["ariaLabel"], "score": score,
                "element_type": "button", "source_field": "ariaLabel", "element": item}
    txt = item.get("text") or item.get("value") or ""
    if field in ("text", "value") and txt:
        return {"strategy": "role", "value": txt, "score": score,
                "element_type": "button", "source_field": field, "element": item}
    if field == "name" and item.get("name"):
        return {"strategy": "name", "value": item["name"], "score": score,
                "element_type": "button", "source_field": "name", "element": item}
    if field == "id" and item.get("id"):
        return {"strategy": "css", "value": f"#{item['id']}", "score": score,
                "element_type": "button", "source_field": "id", "element": item}
    return None


def _resolve_link(links: List[Dict[str, Any]], keywords: List[str]) -> Optional[Dict[str, Any]]:
    fields = ["ariaLabel", "text", "id"]
    result = _best_match(links, fields, keywords)
    if not result:
        return None
    item, field, score = result

    if field == "ariaLabel" and item.get("ariaLabel"):
        return {"strategy": "label", "value": item["ariaLabel"], "score": score,
                "element_type": "link", "source_field": "ariaLabel", "element": item}
    if field == "text" and item.get("text"):
        return {"strategy": "text", "value": item["text"], "score": score,
                "element_type": "link", "source_field": "text", "element": item}
    if field == "id" and item.get("id"):
        return {"strategy": "css", "value": f"#{item['id']}", "score": score,
                "element_type": "link", "source_field": "id", "element": item}
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

    When the resolution includes an 'element' dict (populated by the resolvers),
    builds a comprehensive 9-priority fallback chain using all available element
    attributes so the healer can try each in order before failing:

      1. data-testid / data-test
      2. aria-label
      3. role + text   (buttons / links)
      4. label association
      5. placeholder
      6. name attribute
      7. id → CSS #id
      8. visible text  (buttons / links)
      9. safe CSS type/name fallback
    """
    strategy = resolution.get("strategy", "")
    value = resolution.get("value", "")
    element = resolution.get("element") or {}
    element_type = resolution.get("element_type", "")

    # ── Primary selector ──────────────────────────────────────────────────────
    if strategy == "testid":
        primary = f"[data-testid='{value}']"
    elif strategy in ("label",):
        primary = f"text={value}"
    elif strategy == "placeholder":
        primary = f"[placeholder='{value}']"
    elif strategy == "name":
        primary = f"[name='{value}']"
    elif strategy in ("role", "text"):
        # Scope button/link roles to their element type so we never match a
        # heading that happens to contain the same text (e.g. "Login Page" h2
        # would match text=Login, clicking it instead of the submit button).
        if strategy == "role" and element_type == "button":
            primary = f"button:has-text('{value}')"
        elif strategy == "role" and element_type == "link":
            primary = f"a:has-text('{value}')"
        else:
            primary = f"text={value}"
    else:  # css or unknown
        primary = value

    # ── Comprehensive fallback chain from element attributes ──────────────────
    # Each fallback type corresponds to one of the 9 priority levels.
    # Dedup by (type, key) to avoid repeating the primary or duplicates.
    fallbacks: List[Dict[str, Any]] = []
    _seen: set = set()

    def _add(fb_type: str, fb_val: Any, dedup_key: str = "") -> None:
        key = dedup_key or f"{fb_type}:{fb_val}"
        if key in _seen or not fb_val:
            return
        _seen.add(key)
        fallbacks.append({"type": fb_type, "value": fb_val})

    # Seed dedup with primary so we never repeat it
    _seen.add(f"{strategy}:{value}")
    _seen.add(f"css:{primary}")

    # 1. testid
    if element.get("testid"):
        _add("testid", element["testid"])
        _add("css", f"[data-test='{element['testid']}']",
             dedup_key=f"css:data-test:{element['testid']}")

    # 2. aria-label
    if element.get("ariaLabel"):
        _add("label", element["ariaLabel"], dedup_key=f"label:aria:{element['ariaLabel']}")

    # 3. role + visible text  (buttons and links only)
    if element_type in ("button", "link"):
        txt = (element.get("text") or element.get("value") or "").strip()
        if txt:
            role = "button" if element_type == "button" else "link"
            _add("role", {"role": role, "name": txt},
                 dedup_key=f"role:{element_type}:{txt}")

    # 4. label association
    if element.get("label"):
        _add("label", element["label"], dedup_key=f"label:assoc:{element['label']}")

    # 5. placeholder
    if element.get("placeholder"):
        _add("placeholder", element["placeholder"])

    # 6. name attribute
    if element.get("name"):
        _add("name", element["name"])

    # 7. id → CSS #id
    if element.get("id"):
        _add("css", f"#{element['id']}", dedup_key=f"css:id:{element['id']}")

    # 8. visible text  (buttons and links only)
    if element_type in ("button", "link"):
        txt = (element.get("text") or element.get("value") or "").strip()
        if txt:
            _add("text", txt, dedup_key=f"text:{txt}")

    # 9. safe CSS type/name fallback  (inputs)
    if element_type == "input":
        if element.get("name"):
            _add("css", f"input[name='{element['name']}']",
                 dedup_key=f"css:input:name:{element['name']}")
        if element.get("type") and element.get("type") not in ("text", "hidden"):
            _add("css", f"input[type='{element['type']}']",
                 dedup_key=f"css:input:type:{element['type']}")

    # If element data was sparse, fall back to a single type-matched fallback
    if not fallbacks:
        if strategy == "testid":
            fallbacks = [{"type": "testid", "value": value}]
        elif strategy == "label":
            fallbacks = [{"type": "label", "value": value}]
        elif strategy == "placeholder":
            fallbacks = [{"type": "placeholder", "value": value}]
        elif strategy == "name":
            fallbacks = [{"type": "name", "value": value}]
        elif strategy == "role":
            fallbacks = [{"type": "role", "value": {"role": "button", "name": value}}]
        elif strategy == "text":
            fallbacks = [{"type": "text", "value": value}]

    return {
        "primary": primary,
        "fallbacks": fallbacks,
        "intent": resolution.get("source_field", ""),
        "resolved_by": "selector_resolver",
        "resolution_score": resolution.get("score", 0),
        "element_type": element_type,
    }


def build_well_known_form_target(action: str, intent: str) -> Optional[Dict[str, Any]]:
    """
    For common form fields (username, password, login button) return a target dict
    with multiple CSS/role fallbacks so selector_healer can try them sequentially.

    Called as a last resort when DOM inventory resolution fails or is unavailable.
    Returns None if the intent does not match any known pattern.
    """
    kws = set(_keywords(intent))
    if not kws:
        return None

    # ── Password field (fill / press) ─────────────────────────────────────────
    if action in ("fill", "press") and kws & {"password", "pass", "contraseña",
                                               "contrasena", "pwd", "passwd"}:
        return {
            "primary": "#password",
            "fallbacks": [
                {"type": "css",         "value": "input[name='password']"},
                {"type": "css",         "value": "input[type='password']"},
                {"type": "placeholder", "value": "Password"},
                {"type": "label",       "value": "Password"},
            ],
        }

    # ── Username / email field (fill / press) ─────────────────────────────────
    if action in ("fill", "press") and kws & {"username", "user", "email",
                                               "usuario", "correo"}:
        return {
            "primary": "#username",
            "fallbacks": [
                {"type": "css",         "value": "input[name='username']"},
                {"type": "css",         "value": "input[name='user']"},
                {"type": "css",         "value": "input[name='email']"},
                {"type": "css",         "value": "input[type='email']"},
                {"type": "css",         "value": "input[type='text']"},
                {"type": "css",         "value": "input[placeholder*='user']"},
                {"type": "placeholder", "value": "Username"},
                {"type": "placeholder", "value": "Email"},
                {"type": "label",       "value": "Username"},
                {"type": "label",       "value": "Email"},
            ],
        }

    # ── Login / submit button (click) ─────────────────────────────────────────
    if action == "click" and kws & {"login", "signin", "sign", "submit",
                                     "entrar", "ingresar", "acceder"}:
        return {
            "primary": "button[type='submit']",
            "fallbacks": [
                {"type": "css",  "value": "button:has-text('Login')"},
                {"type": "css",  "value": "button:has-text('Sign in')"},
                {"type": "css",  "value": "input[type='submit']"},
                {"type": "text", "value": "Login"},
                {"type": "text", "value": "Sign in"},
                {"type": "text", "value": "Sign In"},
                {"type": "text", "value": "Log in"},
                {"type": "role", "value": {"role": "button", "name": "Login"}},
                {"type": "role", "value": {"role": "button", "name": "Sign in"}},
            ],
        }

    return None


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
