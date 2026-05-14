# services/dom_semantic_classifier.py
"""
Deterministic semantic classification from inspection inventory + layout hints.

No LLM, no network beyond what inspection already captured.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Set
from urllib.parse import urlparse

from models.app_map_models import PageType
from models.browser_inspection_models import BrowserInspectionResult

# ── Keyword buckets (lowercase matching) ─────────────────────────────────────

_LOGIN_BUTTON = re.compile(
    r"\b(log\s*in|sign\s*in|login|authenticate|submit|continue)\b",
    re.I,
)
_DESTRUCTIVE = re.compile(r"\b(delete|remove|erase|destroy|unsubscribe)\b", re.I)
_ECOMMERCE = re.compile(r"\b(shop|store|cart|basket|checkout|product|buy|pricing)\b", re.I)
_CHECKOUT = re.compile(r"\b(checkout|payment|billing|place\s*order|order\s*confirm)\b", re.I)
_PROFILE = re.compile(r"\b(settings|account|profile|preferences|my\s*account)\b", re.I)
_ERROR_H1 = re.compile(r"\b(404|403|500|error|not\s*found|forbidden|unavailable)\b", re.I)
_FILTER = re.compile(r"\b(filter|facet|sort\s*by|category)\b", re.I)


def _lower_blob(*parts: str) -> str:
    return " ".join(p.lower() for p in parts if p).strip()


def _layout_hints(extras: Dict[str, Any]) -> Dict[str, int]:
    h = extras.get("layout_hints") if isinstance(extras, dict) else None
    if not isinstance(h, dict):
        return {}
    out: Dict[str, int] = {}
    for k in (
        "table_count",
        "nav_linkish",
        "search_inputs",
        "dialog_like",
        "required_fields",
        "password_inputs",
        "filter_like_inputs",
    ):
        try:
            v = h.get(k)
            out[k] = int(v) if v is not None else 0
        except Exception:
            out[k] = 0
    return out


def _text_blob(
    inspection: BrowserInspectionResult,
    inv: Dict[str, Any],
) -> str:
    parts: List[str] = [inspection.title or "", inspection.final_url or ""]
    for h in inspection.headings:
        parts.append(str(h.get("text") or ""))
    for ln in inspection.links:
        parts.append(str(ln.get("text") or ""))
        parts.append(str(ln.get("href") or ""))
    for b in inspection.buttons:
        parts.append(str(b.get("text") or ""))
    for inp in inspection.inputs:
        parts.append(str(inp.get("placeholder") or ""))
        parts.append(str(inp.get("name") or ""))
        parts.append(str(inp.get("aria_label") or ""))
    for sel in inv.get("selects") or []:
        if isinstance(sel, dict):
            parts.append(str(sel.get("label") or ""))
            parts.append(str(sel.get("name") or ""))
    return _lower_blob(*parts)


def classify_semantic_map(
    inspection: BrowserInspectionResult,
    inv: Dict[str, Any],
    extras: Dict[str, Any],
) -> Dict[str, Any]:
    """
    Return components for ``AppMapResponse`` (except ids/timestamps filled by builder).

    Keys: page_type_scores (internal), page_type, confidence, detected_patterns,
    main_navigation, primary_actions, forms, tables, search_elements,
    risk_notes, suggested_test_flows.
    """
    hints = _layout_hints(extras)
    blob = _text_blob(inspection, inv)
    scores: Dict[PageType, int] = {t: 0 for t in (
        "login_page",
        "dashboard",
        "landing_page",
        "form_page",
        "crud_table",
        "search_interface",
        "ecommerce_page",
        "checkout_page",
        "profile_settings",
        "error_page",
        "unknown",
    )}

    patterns: List[str] = []
    n_links = len(inspection.links)
    n_inputs = len(inspection.inputs)
    n_forms = len(inspection.forms)
    max_form_fields = 0
    for f in inspection.forms:
        if isinstance(f, dict):
            max_form_fields = max(max_form_fields, int(f.get("field_count") or 0))

    # ── error_page ───────────────────────────────────────────────────────────
    st = inspection.status_code
    if st is not None and st >= 400:
        scores["error_page"] += 10
        patterns.append("http_error_status")
    for h in inspection.headings:
        t = str(h.get("text") or "")
        if _ERROR_H1.search(t):
            scores["error_page"] += 6
            patterns.append("error_heading_signal")
            break

    # ── login_page ─────────────────────────────────────────────────────────────
    pw = hints.get("password_inputs", 0)
    if pw >= 1:
        scores["login_page"] += 6
        patterns.append("password_input_present")
    if any(str(i.get("type") or "").lower() == "email" for i in inspection.inputs):
        scores["login_page"] += 2
        patterns.append("email_input_present")
    if _LOGIN_BUTTON.search(blob):
        scores["login_page"] += 4
        patterns.append("login_cta_language")
    if "/login" in blob or "/signin" in blob or "/auth" in blob:
        scores["login_page"] += 3
        patterns.append("login_path_signal")

    # ── search_interface ───────────────────────────────────────────────────────
    si = hints.get("search_inputs", 0)
    if si >= 1:
        scores["search_interface"] += 7
        patterns.append("role_or_type_search")
    if any(str(i.get("type") or "").lower() == "search" for i in inspection.inputs):
        scores["search_interface"] += 5
        patterns.append("input_type_search")
    if re.search(r"\bsearch\b", blob) and n_inputs <= 8:
        scores["search_interface"] += 2
        patterns.append("search_vocab_light")

    # ── filters ───────────────────────────────────────────────────────────────
    if hints.get("filter_like_inputs", 0) >= 1 or _FILTER.search(blob):
        patterns.append("filter_controls_suspected")
        scores["dashboard"] += 1
        scores["crud_table"] += 1

    # ── tables / crud ────────────────────────────────────────────────────────
    tc = hints.get("table_count", 0)
    if tc >= 1:
        scores["crud_table"] += 5 + min(tc, 3)
        patterns.append("html_table_present")
    btn_blob = _lower_blob(*(str(b.get("text") or "") for b in inspection.buttons))
    if tc >= 1 and _DESTRUCTIVE.search(btn_blob):
        scores["crud_table"] += 3
        patterns.append("destructive_row_action_language")

    # ── dashboard ─────────────────────────────────────────────────────────────
    nav = hints.get("nav_linkish", 0)
    if nav >= 8 and n_links >= 12:
        scores["dashboard"] += 6
        patterns.append("dense_nav_links")
    if n_links >= 15 and n_forms <= 4 and max_form_fields <= 6:
        scores["dashboard"] += 3
        patterns.append("many_links_few_complex_forms")

    # ── landing_page ──────────────────────────────────────────────────────────
    h1s = [h for h in inspection.headings if str(h.get("tag") or "").lower() == "h1"]
    if h1s and n_inputs <= 4 and n_forms <= 2 and n_links >= 6:
        scores["landing_page"] += 5
        patterns.append("hero_like_landing_signal")

    # ── form_page ─────────────────────────────────────────────────────────────
    if max_form_fields >= 5 or n_inputs >= 7:
        scores["form_page"] += 6
        patterns.append("large_form_signal")
    if n_forms >= 2 and n_inputs >= 5:
        scores["form_page"] += 2
        patterns.append("multi_form_signal")

    # ── ecommerce / checkout / profile ───────────────────────────────────────
    if _ECOMMERCE.search(blob):
        scores["ecommerce_page"] += 5
        patterns.append("ecommerce_vocab")
    if _CHECKOUT.search(blob):
        scores["checkout_page"] += 7
        patterns.append("checkout_vocab")
    if _PROFILE.search(blob):
        scores["profile_settings"] += 5
        patterns.append("profile_settings_vocab")

    # ── modal / empty / required ──────────────────────────────────────────────
    if hints.get("dialog_like", 0) >= 1:
        patterns.append("dialog_or_modal_present")
    rq = hints.get("required_fields", 0)
    if rq >= 1:
        patterns.append("required_fields_present")
        scores["form_page"] += 1

    # ── unknown baseline ──────────────────────────────────────────────────────
    scores["unknown"] = 1

    # Pick page types: top scores, keep if >= 3 or best >= 2
    ranked = sorted(scores.items(), key=lambda kv: -kv[1])
    best_score = ranked[0][1] if ranked else 0
    threshold = 3 if best_score >= 6 else 2
    picked: List[PageType] = []
    for pt, sc in ranked:
        if pt == "unknown":
            continue
        if sc >= threshold and sc >= best_score * 0.45:
            picked.append(pt)
        if len(picked) >= 3:
            break
    if not picked:
        picked = ["unknown"]

    if best_score >= 10:
        confidence: str = "high"
    elif best_score >= 5:
        confidence = "medium"
    else:
        confidence = "low"

    main_nav = []
    for ln in inspection.links[:12]:
        tx = str(ln.get("text") or "").strip()
        href = str(ln.get("href") or "").strip()
        if tx and href:
            main_nav.append({"text": tx[:200], "href": href[:2000]})

    primary: List[Dict[str, Any]] = []
    cta = re.compile(r"^(submit|save|apply|continue|next|pay|confirm|add\s*to\s*cart)\b", re.I)
    for b in inspection.buttons:
        tx = str(b.get("text") or "").strip()
        if tx and (cta.search(tx) or len(tx) <= 24):
            primary.append({"text": tx[:200], "id": b.get("id"), "testid": b.get("testid")})
        if len(primary) >= 12:
            break

    tables_out: List[Dict[str, Any]] = []
    for i in range(min(tc, 5)):
        tables_out.append({"index": i, "hint": "table_detected_in_dom"})

    search_el: List[Dict[str, Any]] = []
    for i in inspection.inputs:
        t = str(i.get("type") or "").lower()
        nm = str(i.get("name") or "").lower()
        ph = str(i.get("placeholder") or "").lower()
        if t == "search" or "search" in nm or "search" in ph:
            search_el.append(
                {
                    "type": i.get("type"),
                    "name": i.get("name"),
                    "placeholder": i.get("placeholder"),
                    "id": i.get("id"),
                }
            )

    risks: List[str] = []
    if _DESTRUCTIVE.search(btn_blob):
        risks.append("destructive_action_language_in_buttons")
    try:
        host = urlparse(inspection.final_url).hostname or ""
        ext = 0
        for ln in inspection.links:
            href = str(ln.get("href") or "")
            if href.startswith("http") and host:
                oh = urlparse(href).hostname or ""
                if oh and oh != host:
                    ext += 1
        if ext >= 5:
            risks.append(f"external_absolute_links_count={ext}")
    except Exception:
        pass
    if hints.get("dialog_like", 0) >= 1:
        risks.append("modal_dialog_present_verify_focus_trap")

    flows = _suggested_flows(picked, patterns, rq)

    # Dedupe patterns preserving order
    seen_p: Set[str] = set()
    patterns_u: List[str] = []
    for p in patterns:
        if p not in seen_p:
            seen_p.add(p)
            patterns_u.append(p)

    return {
        "page_type": picked,
        "confidence": confidence,
        "detected_patterns": patterns_u,
        "main_navigation": main_nav,
        "primary_actions": primary,
        "forms": [dict(f) for f in inspection.forms],
        "tables": tables_out,
        "search_elements": search_el,
        "risk_notes": risks,
        "suggested_test_flows": flows,
    }


def _suggested_flows(
    page_types: List[PageType],
    patterns: List[str],
    required_ct: int,
) -> List[str]:
    flows: List[str] = []
    pt = page_types[0] if page_types else "unknown"
    if pt == "login_page":
        flows.append("Assert login form visible; negative path with invalid credentials.")
    if pt == "dashboard":
        flows.append("Smoke: open each primary nav destination and assert no 5xx.")
    if pt == "form_page":
        flows.append("Validate required fields and error messages on empty submit (without submitting if destructive).")
    if "html_table_present" in patterns:
        flows.append("Table: sort/filter if present; assert row count stable after refresh.")
    if pt == "search_interface":
        flows.append("Search: empty query, special characters, and happy-path keyword.")
    if pt == "checkout_page":
        flows.append("Checkout: verify totals line and payment section visibility (no real payment).")
    if pt == "ecommerce_page":
        flows.append("Catalog: navigate to product detail from listing card or search result.")
    if pt == "error_page":
        flows.append("Error page: assert status messaging and recovery links.")
    if required_ct >= 1 and "form_page" in page_types:
        flows.append("Required fields: leave mandatory fields empty and assert inline validation.")
    if not flows:
        flows.append("Generic: capture baseline screenshot and console cleanliness after load.")
    # Cap
    return flows[:8]
