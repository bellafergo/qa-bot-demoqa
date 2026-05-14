# services/browser_inspector_service.py
"""
Browser inspection orchestration (Phase 1 — deterministic, no LLM).

Reuses:
  - ``core.target_url_validation.validate_target_url`` (SSRF / scheme / DNS policy)
  - ``runners.browser_inspector_runner.run_browser_inspection`` (Playwright)
  - ``services.cloudinary_service.upload_screenshot_b64_url`` (optional hosted PNG)

Does **not** reuse ``application_explorer.explore_page`` — that path parses static HTML
after ``page.content()`` and omits visibility, console, network, and screenshots.

Light persistence (Phase 3A): ``POST /inspect-url`` and ``/inspect-url/map`` call
``services.browser_inspection_persistence`` → ``persist_run_payload`` when enabled
(``VANYA_PERSIST_BROWSER_INSPECTION``, default on). See ``docs/BROWSER_INSPECTION_PHASE3A.md``.
"""
from __future__ import annotations

import logging
import re
import uuid
from typing import Any, Dict, List, Optional

from models.browser_inspection_models import (
    BrowserInspectionResult,
    InspectUrlRequest,
    SelectorCandidate,
)

logger = logging.getLogger("vanya.browser_inspector")

_MAX_LINKS = 80
_MAX_BUTTONS = 80
_MAX_INPUTS = 80
_MAX_SELECTOR_CANDIDATES = 50


def _attr_escape(s: str, *, max_len: int = 120) -> str:
    t = " ".join(str(s).split()).strip()[:max_len]
    return t.replace("\\", "\\\\").replace('"', '\\"')


def _visible(row: Dict[str, Any]) -> bool:
    return bool(row.get("visible", True))


def _headings_h1_h3(inventory: Dict[str, Any]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for h in inventory.get("headings") or []:
        if not isinstance(h, dict):
            continue
        tag = str(h.get("tag") or "").lower()
        if tag not in ("h1", "h2", "h3"):
            continue
        txt = str(h.get("text") or "").strip()[:300]
        if txt:
            out.append({"tag": tag, "text": txt})
        if len(out) >= 40:
            break
    return out


def _trim_inputs(rows: List[Any]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for r in rows or []:
        if not isinstance(r, dict):
            continue
        if not _visible(r):
            continue
        item = {
            "tag": r.get("tag"),
            "type": r.get("type"),
            "id": r.get("id"),
            "name": r.get("name"),
            "placeholder": r.get("placeholder"),
            "testid": r.get("testid"),
            "aria_label": r.get("ariaLabel"),
            "label": r.get("label"),
        }
        out.append({k: v for k, v in item.items() if v})
        if len(out) >= _MAX_INPUTS:
            break
    return out


def _trim_buttons(rows: List[Any]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for r in rows or []:
        if not isinstance(r, dict):
            continue
        if not _visible(r):
            continue
        txt = str(r.get("text") or r.get("value") or "").strip()[:200]
        item = {
            "tag": r.get("tag"),
            "text": txt or None,
            "id": r.get("id"),
            "name": r.get("name"),
            "type": r.get("type"),
            "testid": r.get("testid"),
            "aria_label": r.get("ariaLabel"),
        }
        out.append({k: v for k, v in item.items() if v})
        if len(out) >= _MAX_BUTTONS:
            break
    return out


def _trim_links(rows: List[Any]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for r in rows or []:
        if not isinstance(r, dict):
            continue
        href = str(r.get("href") or "").strip()
        if not href or href.startswith("#") or href.lower().startswith("javascript:"):
            continue
        text = str(r.get("text") or "").strip()[:200]
        item = {
            "text": text or None,
            "href": href[:2000],
            "id": r.get("id"),
            "aria_label": r.get("ariaLabel"),
        }
        out.append({k: v for k, v in item.items() if v})
        if len(out) >= _MAX_LINKS:
            break
    return out


def _forms_from_extras(extras: Dict[str, Any]) -> List[Dict[str, Any]]:
    raw = extras.get("forms") or []
    if not isinstance(raw, list):
        return []
    out: List[Dict[str, Any]] = []
    for f in raw:
        if isinstance(f, dict):
            out.append(f)
        if len(out) >= 30:
            break
    return out


def _images_from_extras(extras: Dict[str, Any]) -> List[Dict[str, Any]]:
    raw = extras.get("images_without_alt") or []
    if not isinstance(raw, list):
        return []
    return [x for x in raw if isinstance(x, dict)][:40]


def _perf_from_extras(extras: Dict[str, Any]) -> Dict[str, Any]:
    p = extras.get("performance")
    return p if isinstance(p, dict) else {}


def _selector_for_input(r: Dict[str, Any]) -> Optional[tuple[str, str]]:
    tid = r.get("testid")
    if tid:
        return "testid", f'[data-testid="{_attr_escape(str(tid), max_len=120)}"]'
    eid = r.get("id")
    if eid:
        eid_s = str(eid).strip()
        if re.match(r"^[A-Za-z][\w\-:.]*$", eid_s):
            return "id", f"#{eid_s}"
    al = r.get("aria_label")
    if al:
        return "aria", f'[aria-label="{_attr_escape(str(al))}"]'
    nm = r.get("name")
    if nm:
        tag = str(r.get("tag") or "input").lower()
        return "name", f'{tag}[name="{_attr_escape(str(nm))}"]'
    return None


def _selector_for_button(r: Dict[str, Any]) -> Optional[tuple[str, str]]:
    tid = r.get("testid")
    if tid:
        return "testid", f'[data-testid="{_attr_escape(str(tid), max_len=120)}"]'
    eid = r.get("id")
    if eid:
        eid_s = str(eid).strip()
        if re.match(r"^[A-Za-z][\w\-:.]*$", eid_s):
            return "id", f"#{eid_s}"
    al = r.get("aria_label")
    if al:
        return "aria", f'[aria-label="{_attr_escape(str(al))}"]'
    nm = r.get("name")
    if nm:
        return "name", f'button[name="{_attr_escape(str(nm))}"]'
    txt = str(r.get("text") or "").strip()
    if 0 < len(txt) <= 60:
        return "role", f'button:has-text("{_attr_escape(txt, max_len=60)}")'
    return None


def _selector_for_link(r: Dict[str, Any]) -> Optional[tuple[str, str]]:
    tid = r.get("testid")
    if tid:
        return "testid", f'[data-testid="{_attr_escape(str(tid), max_len=120)}"]'
    eid = r.get("id")
    if eid:
        eid_s = str(eid).strip()
        if re.match(r"^[A-Za-z][\w\-:.]*$", eid_s):
            return "id", f"#{eid_s}"
    href = str(r.get("href") or "").strip()
    if href and not href.startswith("#"):
        return "href", f'a[href="{_attr_escape(href, max_len=500)}"]'
    al = r.get("aria_label")
    if al:
        return "aria", f'a[aria-label="{_attr_escape(str(al))}"]'
    return None


def build_selector_candidates(
    inputs: List[Dict[str, Any]],
    buttons: List[Dict[str, Any]],
    links: List[Dict[str, Any]],
    *,
    max_items: int = _MAX_SELECTOR_CANDIDATES,
) -> List[SelectorCandidate]:
    seen: set[str] = set()
    out: List[SelectorCandidate] = []

    def push(kind: str, pri_sel: Optional[tuple[str, str]], label: Optional[str]) -> bool:
        if len(out) >= max_items:
            return False
        if not pri_sel:
            return True
        pri, sel = pri_sel
        if sel in seen:
            return True
        seen.add(sel)
        out.append(SelectorCandidate(kind=kind, selector=sel, priority=pri, label=label))
        return len(out) < max_items

    for r in buttons:
        if not push("button", _selector_for_button(r), (str(r.get("text") or r.get("aria_label") or r.get("name") or "")[:120] or None)):
            return out
    for r in inputs:
        if not push(
            "input",
            _selector_for_input(r),
            (str(r.get("label") or r.get("aria_label") or r.get("placeholder") or r.get("name") or "")[:120] or None),
        ):
            return out
    for r in links:
        if not push("link", _selector_for_link(r), (str(r.get("text") or r.get("aria_label") or "")[:120] or None)):
            return out
    return out


def inspect_url_collect(
    req: InspectUrlRequest,
) -> tuple[BrowserInspectionResult, Dict[str, Any], Dict[str, Any]]:
    """
    Run inspection and return the public result plus raw inventory / extras for
    downstream consumers (e.g. Phase 2 app map) without a second Playwright pass.
    """
    from core.settings import settings
    from core.target_url_validation import validate_target_url

    from runners.browser_inspector_runner import run_browser_inspection

    warnings: List[str] = []
    validated = validate_target_url(req.url.strip())

    raw = run_browser_inspection(url=validated, timeout_ms=int(req.timeout_ms))
    inv = raw.get("inventory") or {}
    if not isinstance(inv, dict):
        inv = {}
    extras = raw.get("extras") or {}
    if not isinstance(extras, dict):
        extras = {}

    headings = _headings_h1_h3(inv)
    inputs = _trim_inputs(inv.get("inputs"))
    buttons = _trim_buttons(inv.get("buttons"))
    links = _trim_links(inv.get("links"))
    forms = _forms_from_extras(extras)
    bad_img = _images_from_extras(extras)
    performance = _perf_from_extras(extras)

    selector_candidates = build_selector_candidates(inputs, buttons, links)

    inspection_id = str(uuid.uuid4())
    screenshot_url: Optional[str] = None

    b64 = raw.get("screenshot_b64")
    if b64 and getattr(settings, "HAS_CLOUDINARY", False):
        try:
            from services.cloudinary_service import upload_screenshot_b64_url

            screenshot_url = upload_screenshot_b64_url(
                str(b64),
                evidence_id=f"insp-{inspection_id}",
                folder="vanya/browser_inspection",
                tags=["vanya", "browser_inspection"],
            )
        except Exception as e:
            warnings.append(f"screenshot_upload_skipped: {type(e).__name__}")
            logger.warning("browser_inspector: Cloudinary upload failed: %s", e)
    elif b64:
        warnings.append("screenshot_url_unavailable: configure Cloudinary for hosted screenshots")

    if raw.get("navigation_error"):
        warnings.append(f"navigation: {raw['navigation_error']}")

    for line in raw.get("screenshot_logs") or []:
        if "failed" in str(line).lower():
            warnings.append(str(line)[:300])
            break

    inspection_succeeded = not bool(raw.get("navigation_error"))
    inventory_counts = {
        "headings_count": len(inv.get("headings") or []),
        "links_count": len(inv.get("links") or []),
        "buttons_count": len(inv.get("buttons") or []),
        "inputs_count": len(inv.get("inputs") or []),
        "forms_count": len(extras.get("forms") or []),
        "images_without_alt_count": len(extras.get("images_without_alt") or []),
        "selector_candidates_count": len(selector_candidates),
    }

    result = BrowserInspectionResult(
        inspection_id=inspection_id,
        url=req.url.strip(),
        final_url=str(raw.get("final_url") or validated),
        title=str(raw.get("title") or ""),
        status_code=raw.get("status_code"),
        screenshot_url=screenshot_url,
        console_errors=list(raw.get("console_errors") or []),
        network_errors=list(raw.get("network_errors") or []),
        headings=headings,
        links=links,
        buttons=buttons,
        inputs=inputs,
        forms=forms,
        images_without_alt=bad_img,
        selector_candidates=selector_candidates,
        performance=performance,
        warnings=warnings,
        inventory_counts=inventory_counts,
        inspection_succeeded=inspection_succeeded,
    )
    return result, inv, extras


def inspect_url(req: InspectUrlRequest) -> BrowserInspectionResult:
    r, _, _ = inspect_url_collect(req)
    return r

