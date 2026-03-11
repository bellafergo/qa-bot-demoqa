# core/dom_analyzer.py
"""
Extracts a compact, serializable DOM inventory from a live Playwright page.
No dependencies beyond Playwright (already a runner dep). Safe to import in tests.
"""
from __future__ import annotations

import logging
from typing import Any, Dict

logger = logging.getLogger("vanya.dom_analyzer")

# JavaScript run inside the browser page to extract element metadata
_JS_EXTRACT = """
() => {
    function vis(el) {
        try {
            return el.offsetParent !== null &&
                   el.style.display !== 'none' &&
                   el.style.visibility !== 'hidden';
        } catch(e) { return true; }
    }
    function labelFor(el) {
        try {
            if (el.id) {
                var lb = document.querySelector('label[for="' + el.id + '"]');
                if (lb) return lb.textContent.trim().slice(0, 60);
            }
            var p = el.closest('label');
            if (p) {
                var txt = p.textContent.trim();
                var val = el.value || '';
                return txt.replace(val, '').trim().slice(0, 60) || null;
            }
            return el.getAttribute('aria-label') || null;
        } catch(e) { return null; }
    }

    var inv = { inputs: [], buttons: [], links: [], headings: [], selects: [] };

    document.querySelectorAll('input, textarea').forEach(function(el) {
        inv.inputs.push({
            tag:         el.tagName.toLowerCase(),
            type:        el.type || null,
            id:          el.id || null,
            name:        el.name || null,
            placeholder: el.placeholder || null,
            testid:      (el.dataset && el.dataset.testid) ? el.dataset.testid : null,
            ariaLabel:   el.getAttribute('aria-label') || null,
            label:       labelFor(el),
            visible:     vis(el),
        });
    });

    document.querySelectorAll(
        'button, input[type="submit"], input[type="button"], [role="button"]'
    ).forEach(function(el) {
        var txt = (el.value || el.textContent || '').trim().slice(0, 80);
        inv.buttons.push({
            tag:       el.tagName.toLowerCase(),
            text:      txt,
            id:        el.id || null,
            name:      el.name || null,
            type:      el.type || null,
            testid:    (el.dataset && el.dataset.testid) ? el.dataset.testid : null,
            ariaLabel: el.getAttribute('aria-label') || null,
            value:     el.value || null,
            visible:   vis(el),
        });
    });

    document.querySelectorAll('a[href]').forEach(function(el) {
        inv.links.push({
            text:      el.textContent.trim().slice(0, 80),
            href:      el.getAttribute('href') || null,
            id:        el.id || null,
            ariaLabel: el.getAttribute('aria-label') || null,
        });
    });

    document.querySelectorAll('h1,h2,h3,h4').forEach(function(el) {
        inv.headings.push({
            tag:  el.tagName.toLowerCase(),
            text: el.textContent.trim().slice(0, 120),
        });
    });

    document.querySelectorAll('select').forEach(function(el) {
        var opts = [];
        el.querySelectorAll('option').forEach(function(o) {
            opts.push({ value: o.value, text: o.textContent.trim() });
        });
        inv.selects.push({
            id:        el.id || null,
            name:      el.name || null,
            ariaLabel: el.getAttribute('aria-label') || null,
            label:     labelFor(el),
            options:   opts.slice(0, 20),
            visible:   vis(el),
        });
    });

    return inv;
}
"""


def extract_dom_inventory(page) -> Dict[str, Any]:
    """
    Extract compact DOM inventory from a live Playwright page.
    Returns dict with: inputs, buttons, links, headings, selects.
    Never raises — returns empty inventory on any error.
    """
    try:
        inventory = page.evaluate(_JS_EXTRACT)
        if not isinstance(inventory, dict):
            return _empty()
        return inventory
    except Exception as e:
        logger.warning("dom_analyzer: extract_dom_inventory failed: %s", e)
        return _empty()


def _empty() -> Dict[str, Any]:
    return {"inputs": [], "buttons": [], "links": [], "headings": [], "selects": []}


def summarize_inventory(inventory: Dict[str, Any]) -> str:
    """
    Compact human-readable summary of the DOM inventory for LLM prompt injection.
    """
    lines: list[str] = []

    headings = inventory.get("headings") or []
    if headings:
        lines.append("## Headings")
        for h in headings[:5]:
            lines.append(f"  [{h.get('tag')}] {h.get('text')}")

    inputs = inventory.get("inputs") or []
    if inputs:
        lines.append("## Inputs")
        for inp in inputs[:20]:
            parts = []
            for k in ("label", "ariaLabel", "placeholder", "name", "id", "testid"):
                v = inp.get(k)
                if v:
                    parts.append(f"{k}={v!r}")
            t = inp.get("type") or inp.get("tag") or ""
            vis = "visible" if inp.get("visible") else "hidden"
            lines.append(f"  [{t}/{vis}] " + " | ".join(parts))

    buttons = inventory.get("buttons") or []
    if buttons:
        lines.append("## Buttons")
        for btn in buttons[:15]:
            txt = btn.get("text") or btn.get("value") or ""
            parts = [f"text={txt!r}"] if txt else []
            for k in ("id", "name", "testid", "ariaLabel"):
                v = btn.get(k)
                if v:
                    parts.append(f"{k}={v!r}")
            vis = "visible" if btn.get("visible") else "hidden"
            lines.append(f"  [btn/{vis}] " + " | ".join(parts))

    return "\n".join(lines) if lines else "(no DOM elements found)"
