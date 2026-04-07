# services/suggested_tests.py
"""
Suggested Tests — generates structured QA test suggestions from a page inventory.

    suggest_tests_from_inventory(inventory: dict) -> list[dict]

Receives the dict produced by services/application_explorer.analyze_html()
or explore_page() and applies five deterministic rules to emit test suggestions.

Rules (MVP + modern SPA fallbacks)
----------------------------------
1. Form with ≥1 field and ≥1 button
   → {form_name}_valid_submission  (reason: form_detected, priority: high)

2. Per input in any qualifying form (same gate as rule 1)
   → {form_name}_missing_{field}   (reason: required_field_detected, priority: high)

3. Any button whose name contains a search keyword
   → {button_slug}_empty_search    (reason: search_button_detected, priority: medium)

4. Any links present
   → navigation_smoke              (reason: links_detected, priority: low)

5. Standalone inputs (inputs exist, but no qualifying form with fields+buttons)
   → standalone_inputs_smoke       (reason: standalone_inputs_detected, priority: medium)

6. Buttons whose label matches typical app navigation (sidebar / modules / SPA)
   → nav_probe_{slug}              (reason: spa_nav_button_detected, priority: low)
   Skips buttons already tied to a qualifying form (submit) and search-keyword buttons.

7. When there are no classic links, up to two other visible buttons
   → ui_probe_{slug}               (reason: visible_button_detected, priority: low)
   Same skips as (6).

8. No duplicates — test_name is the deduplication key.

Output per suggestion:
    {
        "test_name": str,   # lowercase, snake_case, deterministic
        "reason":    str,   # e.g. form_detected, links_detected, standalone_inputs_detected, …
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

# Typical SPA / shell navigation labels (substring or token match; excludes form submit via skip set)
_NAV_PHRASES: tuple = (
    "sign in", "sign out", "log out", "log in", "my account", "my profile",
    "cerrar sesión", "iniciar sesión",
)
_NAV_TOKENS: frozenset = frozenset({
    "home", "dashboard", "inbox", "settings", "profile", "account", "menu",
    "logout", "signup", "register", "cart", "checkout", "orders", "products",
    "catalog", "users", "team", "workspace", "projects", "reports", "analytics",
    "billing", "notifications", "help", "docs", "search",  # "search" often nav in apps
    "inicio", "cuenta", "ajustes", "configuracion", "configuración", "carrito", "pedidos",
    "productos", "usuarios", "equipo", "proyectos", "panel",
})

_MAX_NAV_PROBE_BUTTONS = 5
_MAX_UI_PROBE_BUTTONS  = 2


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

    has_qualifying = bool(_qualifying_forms(forms))

    # ── Rule 5: inputs without a classic complete form ─────────────────────────
    standalone_inputs = [
        inp for inp in (inventory.get("inputs") or [])
        if isinstance(inp, dict) and _s(inp.get("selector"))
    ]
    if standalone_inputs and not has_qualifying:
        _add("standalone_inputs_smoke", "standalone_inputs_detected", "medium")

    # Buttons used as submit in qualifying forms — not treated as SPA chrome
    skip_btn_slugs = _submit_button_slugs_from_qualifying_forms(forms)

    # ── Rules 6 & 7: SPA / visible buttons (no <a href> navigation) ───────────
    nav_added = 0
    ui_added  = 0
    has_links = bool(links)

    for btn in buttons:
        if not isinstance(btn, dict):
            continue
        bname = _s(btn.get("name"))
        if not bname:
            continue
        bslug = _slug(bname)
        if not bslug or bslug in skip_btn_slugs:
            continue
        low = bname.lower()
        if any(kw in low for kw in _SEARCH_KEYWORDS):
            continue

        if nav_added < _MAX_NAV_PROBE_BUTTONS and _matches_nav_heuristic(bname):
            _add(f"nav_probe_{bslug}", "spa_nav_button_detected", "low")
            nav_added += 1
            continue

        if (not has_links) and ui_added < _MAX_UI_PROBE_BUTTONS:
            _add(f"ui_probe_{bslug}", "visible_button_detected", "low")
            ui_added += 1

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


def _qualifying_forms(forms: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for form in forms:
        fields = [f for f in (form.get("fields") or []) if _s(f)]
        btns   = [b for b in (form.get("buttons") or []) if _s(b)]
        if fields and btns:
            out.append(form)
    return out


def _submit_button_slugs_from_qualifying_forms(forms: List[Dict[str, Any]]) -> Set[str]:
    slugs: Set[str] = set()
    for form in _qualifying_forms(forms):
        for b in form.get("buttons") or []:
            su = _slug(_s(b))
            if su:
                slugs.add(su)
    return slugs


def _matches_nav_heuristic(button_label: str) -> bool:
    low = button_label.lower()
    if any(ph in low for ph in _NAV_PHRASES):
        return True
    try:
        tokens = set(re.findall(r"[a-záéíóúñü0-9]+", low))
    except Exception:
        tokens = set()
    return bool(tokens & _NAV_TOKENS)
