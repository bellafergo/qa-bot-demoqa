# services/test_draft_generator.py
"""
Test Draft Generator for Vanya.

    generate_test_drafts(inventory, suggestions) -> list[dict]
    exploration_fallback_draft(url) -> dict | None

Converts suggested tests (from suggested_tests.py) into structured, human-
readable draft test cases.  Drafts use descriptive placeholders for fill
values — nothing is executed.

exploration_fallback_draft() builds a minimal goto + assert_visible(body) draft
when pages were crawled but no heuristic produced steps (e.g. empty DOM signals).

Output schema per draft:
    {
        "test_name": str,       # snake_case, deterministic
        "status":    "draft",
        "priority":  str,       # "high" | "medium" | "low"
        "reason":    str,       # origin signal
        "steps":     list[dict] # Playwright-compatible step dicts
    }

Step values use placeholders like "<username>", "<password>", "<value>"
so the draft is immediately readable and reviewable.

Selectors are resolved from the inventory when available; a best-effort
fallback selector is generated when not.

Pure module — no I/O, no Playwright, no side effects.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Set

# ── Placeholder heuristics ────────────────────────────────────────────────────

_PLACEHOLDER_HINTS: List[tuple] = [
    ("password", "<password>"),
    ("pass",     "<password>"),
    ("email",    "<email>"),
    ("mail",     "<email>"),
    ("username", "<username>"),
    ("user",     "<username>"),
    ("phone",    "<phone>"),
    ("zip",      "<zip_code>"),
    ("postal",   "<zip_code>"),
    ("name",     "<name>"),
    ("search",   "<search_query>"),
    ("query",    "<search_query>"),
    ("message",  "<message>"),
    ("comment",  "<comment>"),
]


# ── Public API ─────────────────────────────────────────────────────────────────

def generate_test_drafts(
    inventory:   Dict[str, Any],
    suggestions: List[Dict[str, Any]],
) -> List[Dict[str, Any]]:
    """
    Return a list of draft test dicts, one per unique suggestion.

    Suggestions with no buildable steps are silently skipped.
    Duplicates (by test_name) are silently deduplicated.
    """
    if not isinstance(inventory, dict):
        inventory = {}
    if not isinstance(suggestions, list):
        return []

    url = _s(inventory.get("url"))

    seen:   Set[str]            = set()
    drafts: List[Dict[str, Any]] = []

    for suggestion in suggestions:
        if not isinstance(suggestion, dict):
            continue

        name = _s(suggestion.get("test_name"))
        if not name or name in seen:
            continue

        steps = _build_draft_steps(suggestion, inventory, url)
        if not steps:
            continue

        seen.add(name)
        drafts.append({
            "test_name": name,
            "status":    "draft",
            "priority":  _s(suggestion.get("priority")) or "medium",
            "reason":    _s(suggestion.get("reason")),
            "steps":     steps,
        })

    return drafts


def exploration_fallback_draft(url: str) -> Optional[Dict[str, Any]]:
    """
    Minimal smoke when exploration returned page inventories but no buildable
    heuristic drafts (empty inputs/buttons/links/forms in parsed HTML).
    """
    u = _s(url)
    if not u:
        return None
    return {
        "test_name": "exploration_landing_smoke",
        "status":    "draft",
        "priority":  "low",
        "reason":    "exploration_fallback",
        "steps":     [
            {"action": "goto", "url": u},
            {"action": "assert_visible", "selector": "body"},
        ],
    }


# ── Step builders (pure) ───────────────────────────────────────────────────────

def _build_draft_steps(
    suggestion: Dict[str, Any],
    inventory:  Dict[str, Any],
    url:        str,
) -> List[Dict[str, Any]]:
    reason    = _s(suggestion.get("reason"))
    test_name = _s(suggestion.get("test_name"))

    if reason == "form_detected":
        return _draft_valid_submission(test_name, inventory, url)

    if reason == "required_field_detected":
        return _draft_missing_field(test_name, inventory, url)

    if reason == "search_button_detected":
        return _draft_empty_search(test_name, inventory, url)

    if reason == "links_detected":
        return _draft_navigation_smoke(url)

    if reason == "standalone_inputs_detected":
        return _draft_standalone_inputs(inventory, url)

    if reason == "spa_nav_button_detected":
        return _draft_button_probe(test_name, inventory, url, "nav_probe_")

    if reason == "visible_button_detected":
        return _draft_button_probe(test_name, inventory, url, "ui_probe_")

    return []


def _draft_valid_submission(
    test_name: str,
    inventory: Dict[str, Any],
    url:       str,
) -> List[Dict[str, Any]]:
    form_slug = test_name.removesuffix("_valid_submission")
    form      = _find_form(form_slug, inventory)
    if form is None:
        return []

    inp_map = _input_selector_map(inventory)
    btn_map = _button_selector_map(inventory)

    steps: List[Dict[str, Any]] = [{"action": "goto", "url": url}]

    for field_name in (form.get("fields") or []):
        sel = _resolve_input_selector(field_name, inp_map)
        steps.append({"action": "fill", "selector": sel, "value": _placeholder(field_name)})

    btn_names = form.get("buttons") or []
    if btn_names:
        btn_sel = btn_map.get(btn_names[0]) or btn_map.get(_slug(btn_names[0])) \
                  or _fallback_button_selector(btn_names[0])
        steps.append({"action": "click", "selector": btn_sel})

    steps.append({"action": "assert_visible", "selector": "body"})
    return steps


def _draft_missing_field(
    test_name: str,
    inventory: Dict[str, Any],
    url:       str,
) -> List[Dict[str, Any]]:
    form, missing_field = _find_form_and_missing_field(test_name, inventory)
    if form is None or missing_field is None:
        return []

    inp_map     = _input_selector_map(inventory)
    btn_map     = _button_selector_map(inventory)
    missing_sel = _resolve_input_selector(missing_field, inp_map)

    steps: List[Dict[str, Any]] = [{"action": "goto", "url": url}]

    for field_name in (form.get("fields") or []):
        if field_name == missing_field:
            continue
        sel = _resolve_input_selector(field_name, inp_map)
        steps.append({"action": "fill", "selector": sel, "value": _placeholder(field_name)})

    btn_names = form.get("buttons") or []
    if btn_names:
        btn_sel = btn_map.get(btn_names[0]) or btn_map.get(_slug(btn_names[0])) \
                  or _fallback_button_selector(btn_names[0])
        steps.append({"action": "click", "selector": btn_sel})

    steps.append({"action": "assert_visible", "selector": missing_sel})
    return steps


def _draft_empty_search(
    test_name: str,
    inventory: Dict[str, Any],
    url:       str,
) -> List[Dict[str, Any]]:
    btn_slug = test_name.removesuffix("_empty_search")
    btn_map  = _button_selector_map(inventory)

    btn_sel = btn_map.get(btn_slug)
    if not btn_sel:
        # scan buttons directly for a match by slug
        for btn in (inventory.get("buttons") or []):
            if isinstance(btn, dict) and _slug(_s(btn.get("name"))) == btn_slug:
                btn_sel = _s(btn.get("selector")) or _fallback_button_selector(_s(btn.get("name")))
                break

    if not btn_sel:
        return []

    return [
        {"action": "goto",           "url":      url},
        {"action": "click",          "selector": btn_sel},
        {"action": "assert_visible", "selector": "body"},
    ]


def _draft_navigation_smoke(url: str) -> List[Dict[str, Any]]:
    return [
        {"action": "goto",           "url":      url},
        {"action": "assert_visible", "selector": "body"},
    ]


def _draft_standalone_inputs(
    inventory: Dict[str, Any],
    url:       str,
) -> List[Dict[str, Any]]:
    if not url:
        return []
    inputs_list = [
        inp for inp in (inventory.get("inputs") or [])
        if isinstance(inp, dict) and _s(inp.get("selector"))
    ]
    if not inputs_list:
        return []

    first  = inputs_list[0]
    sel    = _s(first.get("selector"))
    fname  = _s(first.get("name")) or "field"
    steps: List[Dict[str, Any]] = [{"action": "goto", "url": url}]
    steps.append({"action": "assert_visible", "selector": sel})
    steps.append({"action": "fill", "selector": sel, "value": _placeholder(fname)})
    steps.append({"action": "assert_visible", "selector": "body"})
    return steps


def _draft_button_probe(
    test_name: str,
    inventory: Dict[str, Any],
    url:       str,
    prefix:    str,
) -> List[Dict[str, Any]]:
    if not url or not test_name.startswith(prefix):
        return []
    bslug = test_name[len(prefix):]
    btn_sel: Optional[str] = None
    for btn in (inventory.get("buttons") or []):
        if not isinstance(btn, dict):
            continue
        if _slug(_s(btn.get("name"))) == bslug:
            btn_sel = _s(btn.get("selector")) or _fallback_button_selector(_s(btn.get("name")))
            break
    if not btn_sel:
        return []
    return [
        {"action": "goto",           "url":      url},
        {"action": "click",          "selector": btn_sel},
        {"action": "assert_visible", "selector": "body"},
    ]


# ── Inventory helpers (pure) ──────────────────────────────────────────────────

def _input_selector_map(inventory: Dict[str, Any]) -> Dict[str, str]:
    m: Dict[str, str] = {}
    for inp in (inventory.get("inputs") or []):
        if not isinstance(inp, dict):
            continue
        name = _s(inp.get("name"))
        sel  = _s(inp.get("selector"))
        if name and sel:
            m[name]        = sel
            m[_slug(name)] = sel
    return m


def _button_selector_map(inventory: Dict[str, Any]) -> Dict[str, str]:
    m: Dict[str, str] = {}
    for btn in (inventory.get("buttons") or []):
        if not isinstance(btn, dict):
            continue
        name = _s(btn.get("name"))
        sel  = _s(btn.get("selector"))
        if name and sel:
            m[name]        = sel
            m[_slug(name)] = sel
    return m


def _find_form(form_slug: str, inventory: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    for form in (inventory.get("forms") or []):
        if isinstance(form, dict) and _slug(_s(form.get("name"))) == form_slug:
            return form
    return None


def _find_form_and_missing_field(
    test_name: str,
    inventory: Dict[str, Any],
) -> tuple:
    for form in (inventory.get("forms") or []):
        if not isinstance(form, dict):
            continue
        fslug  = _slug(_s(form.get("name")))
        prefix = f"{fslug}_missing_"
        if not test_name.startswith(prefix):
            continue
        field_slug = test_name[len(prefix):]
        for field_name in (form.get("fields") or []):
            if _slug(field_name) == field_slug:
                return form, field_name
    return None, None


def _resolve_input_selector(field_name: str, inp_map: Dict[str, str]) -> str:
    """Return selector from map, or a best-effort fallback."""
    return (
        inp_map.get(field_name)
        or inp_map.get(_slug(field_name))
        or f'[name="{field_name}"]'
    )


def _fallback_button_selector(btn_name: str) -> str:
    return f'button:has-text("{btn_name}")'


# ── Placeholder & slug helpers ────────────────────────────────────────────────

def _placeholder(field_name: str) -> str:
    low = _slug(field_name)
    for hint, value in _PLACEHOLDER_HINTS:
        if hint in low:
            return value
    clean = _s(field_name).strip("<>").strip() or "value"
    return f"<{clean}>"


def _slug(v: Any) -> str:
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
