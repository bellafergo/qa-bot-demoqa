# services/suggested_tests.py
"""
Suggested Tests — generates structured QA test suggestions from a page inventory.

    suggest_tests_from_inventory(inventory: dict) -> list[dict]

Receives the dict produced by services/application_explorer.analyze_html()
or explore_page() and applies five deterministic rules to emit test suggestions.

Rules (MVP)
-----------
1. Form with ≥1 field and ≥1 button
   → {form_name}_valid_submission  (reason: form_detected, priority: high)

2. Per input in any qualifying form (same gate as rule 1)
   → {form_name}_missing_{field}   (reason: required_field_detected, priority: high)

3. Any button whose name contains a search keyword
   → {button_slug}_empty_search    (reason: search_button_detected, priority: medium)

4. Any links present
   → navigation_smoke              (reason: links_detected, priority: low)

5. No duplicates — test_name is the deduplication key.

Output per suggestion:
    {
        "test_name": str,   # lowercase, snake_case, deterministic
        "reason":    str,   # one of the four reason strings above
        "priority":  str,   # "high" | "medium" | "low"
    }

Results are sorted: high → medium → low.
Pure module — no I/O, no side effects.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Set

# ── Constants ──────────────────────────────────────────────────────────────────

_PRIORITY_ORDER: Dict[str, int] = {"high": 0, "medium": 1, "low": 2}

# Keywords that identify a search button (checked against lowercased button name)
_SEARCH_KEYWORDS: frozenset = frozenset({"search", "buscar", "find", "busca"})


# ── Public API ─────────────────────────────────────────────────────────────────

def suggest_tests_from_inventory(inventory: Any) -> List[Dict[str, Any]]:
    """
    Return a deduplicated, priority-sorted list of test suggestions derived
    from *inventory*.

    *inventory* must be a dict as returned by application_explorer.
    Any other type returns an empty list.
    """
    if not isinstance(inventory, dict):
        return []

    seen:        Set[str]            = set()
    suggestions: List[Dict[str, Any]] = []

    def _add(test_name: str, reason: str, priority: str) -> None:
        name = _slug(test_name)
        if not name or name in seen:
            return
        seen.add(name)
        suggestions.append({"test_name": name, "reason": reason, "priority": priority})

    forms   = [f for f in (inventory.get("forms")   or []) if isinstance(f, dict)]
    buttons = [b for b in (inventory.get("buttons") or []) if isinstance(b, dict)]
    links   = [lnk for lnk in (inventory.get("links") or []) if isinstance(lnk, dict)]

    # ── Rules 1 & 2: forms ────────────────────────────────────────────────────
    for form in forms:
        fname       = _slug(form.get("name") or "form") or "form"
        fields      = [f for f in (form.get("fields")  or []) if _s(f)]
        form_btns   = [b for b in (form.get("buttons") or []) if _s(b)]

        if not fields or not form_btns:
            continue  # form must have both to qualify

        # Rule 1 — valid submission
        _add(f"{fname}_valid_submission", "form_detected", "high")

        # Rule 2 — missing field per input
        for field in fields:
            _add(f"{fname}_missing_{_slug(field)}", "required_field_detected", "high")

    # ── Rule 3: search buttons ─────────────────────────────────────────────────
    for btn in buttons:
        bname = _s(btn.get("name")).lower()
        if any(kw in bname for kw in _SEARCH_KEYWORDS):
            slug = _slug(btn.get("name") or "search")
            _add(f"{slug}_empty_search", "search_button_detected", "medium")

    # ── Rule 4: navigation smoke ──────────────────────────────────────────────
    if links:
        _add("navigation_smoke", "links_detected", "low")

    suggestions.sort(key=lambda s: _PRIORITY_ORDER.get(s["priority"], 1))
    return suggestions


# ── Helpers ────────────────────────────────────────────────────────────────────

def _slug(v: Any) -> str:
    """Lowercase snake_case, max 80 chars. Safe on any input."""
    try:
        s = re.sub(r"[^a-z0-9]+", "_", str(v).lower().strip())
        return s.strip("_")[:80]
    except Exception:
        return ""


def _s(v: Any) -> str:
    try:
        return str(v).strip() if v is not None else ""
    except Exception:
        return ""
