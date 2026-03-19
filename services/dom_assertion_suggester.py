# services/dom_assertion_suggester.py
"""
Suggest QA assertions for a draft from real, observed DOM evidence.

Strategy (no LLM — fully deterministic):
  1. Extract the first goto URL from the draft's steps.
  2. Navigate to that URL with a headless Playwright browser.
  3. Capture the rendered DOM inventory (headings, inputs, buttons, links)
     using the existing core/dom_analyzer module.
  4. Derive up to 3 assertions from reliable, low-fragility signals:
       - assert_url_contains  — confirmed URL path segment after navigation
       - assert_text_contains — h1/h2 heading text visible on the page
       - assert_visible       — input or button with a data-testid attribute
  5. Deduplicate against the draft's existing assertions.
  6. Return at most 3 new suggestions.

All returned assertions use only runner-supported types:
  assert_visible | assert_not_visible | assert_url_contains | assert_text_contains
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger("vanya.dom_suggester")

# Minimum heading text length to be worth asserting on
_MIN_HEADING_LEN = 4

# Trivial path segments that don't make useful URL assertions
_TRIVIAL_SEGMENTS = frozenset({"index", "home", "main", "app", "page", "web"})


# ── Public entry point ────────────────────────────────────────────────────────

def suggest_from_dom(draft) -> Dict[str, Any]:
    """
    Analyse a live URL from *draft.steps* and propose QA assertions.

    Returns a dict that matches the SuggestAssertionsResponse schema:
      { suggested_assertions, rationale, confidence, note }

    Never raises — always returns a valid dict.
    """
    url = _extract_goto_url(draft.steps or [])
    if not url:
        return {
            "suggested_assertions": [],
            "rationale": "",
            "confidence": "low",
            "note": "No goto URL found in draft steps — cannot fetch DOM.",
        }

    inventory, final_url = _fetch_inventory(url)
    if inventory is None:
        return {
            "suggested_assertions": [],
            "rationale": "",
            "confidence": "low",
            "note": f"Could not load URL '{url}'. The page may be unavailable.",
        }

    candidates: List[dict] = []
    signals: List[str] = []

    # ── Signal 1: URL path segment from confirmed post-navigation URL ────────
    segment = _url_path_segment(final_url or url)
    if segment:
        candidates.append({"action": "assert_url_contains", "value": segment})
        signals.append(f"URL path '{segment}'")

    # ── Signal 2: h1/h2 heading text visible on the page ────────────────────
    for h in (inventory.get("headings") or []):
        if h.get("tag") in ("h1", "h2"):
            text = (h.get("text") or "").strip()
            if len(text) >= _MIN_HEADING_LEN:
                candidates.append({
                    "action":   "assert_text_contains",
                    "selector": "body",
                    "text":     text[:80],
                })
                signals.append(f"{h['tag']} '{text[:40]}'")
                break  # one heading assertion is enough

    # ── Signal 3: data-testid on a visible input or button ───────────────────
    testid_found = False
    for item in (inventory.get("inputs") or []) + (inventory.get("buttons") or []):
        if item.get("testid") and item.get("visible") and not testid_found:
            selector = f'[data-testid="{item["testid"]}"]'
            candidates.append({"action": "assert_visible", "selector": selector})
            signals.append(f"testid='{item['testid']}'")
            testid_found = True
            break

    # ── Deduplicate against existing draft assertions ────────────────────────
    existing_keys = {_akey(a) for a in (draft.assertions or [])}
    new_assertions = [a for a in candidates if _akey(a) not in existing_keys][:3]

    if not new_assertions:
        return {
            "suggested_assertions": [],
            "rationale": (
                f"Observed initial page load of {final_url or url} — "
                "all detected patterns are already covered by existing assertions."
            ),
            "confidence": "medium",
            "note": "",
        }

    rationale = (
        f"Observed initial page load of {final_url or url} — "
        "not post-flow state. Suggestions based on what was visible on that page."
    )
    if signals:
        rationale += f" Signals: {', '.join(signals[:3])}."

    return {
        "suggested_assertions": new_assertions,
        "rationale":  rationale,
        "confidence": "high" if len(new_assertions) >= 2 else "medium",
        "note":       "",
    }


# ── Helpers ───────────────────────────────────────────────────────────────────

def _extract_goto_url(steps: List[Dict[str, Any]]) -> Optional[str]:
    """Return the first http(s) URL found in a goto step."""
    for step in steps:
        if (step.get("action") or "").lower() == "goto":
            url = step.get("url") or step.get("value") or ""
            if url.startswith("http"):
                return url
    return None


def _url_path_segment(url: str) -> Optional[str]:
    """Return the last non-trivial path segment of a URL."""
    try:
        from urllib.parse import urlparse
        path = urlparse(url).path.rstrip("/")
        if "/" in path:
            segment = path.rsplit("/", 1)[-1]
        else:
            segment = path.lstrip("/")
        if len(segment) >= 3 and segment not in _TRIVIAL_SEGMENTS:
            return segment
    except Exception:
        pass
    return None


def _akey(a: dict) -> str:
    return (
        f"{a.get('action')}:"
        f"{a.get('selector', '')}:"
        f"{a.get('value', '')}:"
        f"{a.get('text', '')}"
    )


def _fetch_inventory(url: str) -> Tuple[Optional[Dict[str, Any]], Optional[str]]:
    """
    Launch a headless browser, navigate to *url*, and capture the DOM inventory
    using core.dom_analyzer (headings, inputs, buttons, links, selects).

    Returns (inventory_dict, final_url) or (None, None) on any error.
    """
    try:
        from playwright.sync_api import sync_playwright
        from core.dom_analyzer import extract_dom_inventory

        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            try:
                page = browser.new_page()
                page.goto(url, timeout=20_000, wait_until="domcontentloaded")
                final_url = page.url
                inventory = extract_dom_inventory(page)
                return inventory, final_url
            finally:
                browser.close()
    except Exception as exc:
        logger.warning("dom_suggester: failed to fetch '%s' — %s", url, exc)
        return None, None
