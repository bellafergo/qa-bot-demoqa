# runners/browser_inspector_runner.py
"""
Deterministic Playwright runner for URL inspection (Phase 1).

- Single navigation to the validated URL (no link following, no clicks).
- Fresh browser context (no persisted storage/cookies between runs).
- Controlled ``page.evaluate`` snippets only (no arbitrary user JS).

This module is intentionally separate from ``runners.generic_steps`` (test execution)
and from ``services.application_explorer.explore_page`` (HTML-parser inventory).
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

logger = logging.getLogger("vanya.browser_inspector_runner")

# Caps to keep payloads bounded (product rule: no full DOM in API).
_MAX_CONSOLE = 80
_MAX_NETWORK_FAIL = 80
_MAX_HEADINGS = 40
_MAX_FORMS = 30
_MAX_BAD_IMG = 40


_JS_EXTRAS = """
() => {
    function vis(el) {
        try {
            return el.offsetParent !== null &&
                   el.style.display !== 'none' &&
                   el.style.visibility !== 'hidden';
        } catch(e) { return true; }
    }
    var forms = [];
    document.querySelectorAll('form').forEach(function(f) {
        if (forms.length >= __MAX_FORMS__) return;
        var n = 0;
        try {
            n = f.querySelectorAll('input,select,textarea').length;
        } catch(e) { n = 0; }
        var act = '';
        try { act = f.action || ''; } catch(e2) { act = ''; }
        forms.push({
            id: f.id || null,
            name: f.name || null,
            action: act.slice(0, 300),
            method: (f.method || 'get').toLowerCase(),
            field_count: n,
        });
    });
    var images = [];
    document.querySelectorAll('img').forEach(function(img) {
        if (images.length >= __MAX_BAD_IMG__) return;
        if (!vis(img)) return;
        var alt = img.getAttribute('alt');
        if (alt === null || String(alt).trim() === '') {
            var src = '';
            try { src = (img.currentSrc || img.src || ''); } catch(e) { src = ''; }
            images.push({ src: src.slice(0, 300) });
        }
    });
    var perf = {};
    try {
        var t = performance.timing;
        if (t && t.navigationStart && t.domContentLoadedEventEnd) {
            perf.domContentLoaded_ms = Math.max(0, t.domContentLoadedEventEnd - t.navigationStart);
        }
        if (t && t.navigationStart && t.loadEventEnd) {
            perf.loadEventEnd_ms = Math.max(0, t.loadEventEnd - t.navigationStart);
        }
    } catch (e) {}
    var hints = {
        table_count: document.querySelectorAll('table').length,
        nav_linkish: document.querySelectorAll('nav a, header a, [role="navigation"] a').length,
        search_inputs: document.querySelectorAll('input[type="search"], [role="search"] input').length,
        dialog_like: document.querySelectorAll('[role="dialog"], [aria-modal="true"]').length,
        required_fields: document.querySelectorAll('input[required], select[required], textarea[required]').length,
        password_inputs: document.querySelectorAll('input[type="password"]').length,
        filter_like_inputs: document.querySelectorAll('input[list], select[multiple]').length,
    };
    return { forms: forms, images_without_alt: images, performance: perf, layout_hints: hints };
}
"""


def _patch_js_extras(max_forms: int, max_img: int) -> str:
    return (
        _JS_EXTRAS.replace("__MAX_FORMS__", str(max_forms))
        .replace("__MAX_BAD_IMG__", str(max_img))
    )


def run_browser_inspection(*, url: str, timeout_ms: int, headless: bool = True) -> Dict[str, Any]:
    """
    Run a Chromium inspection against *url* (caller validates URL for its context).

    Returns a plain dict for ``browser_inspector_service`` / local agent to normalize.
    """
    from playwright.sync_api import sync_playwright

    from core.dom_analyzer import extract_dom_inventory
    from runners.screenshot import take_screenshot_robust

    console_errors: List[Dict[str, Any]] = []
    network_errors: List[Dict[str, Any]] = []

    def on_console(msg) -> None:
        try:
            if msg.type != "error":
                return
            if len(console_errors) >= _MAX_CONSOLE:
                return
            text = (msg.text or "")[:2000]
            loc = ""
            try:
                loc_obj = msg.location
                if loc_obj:
                    loc = f"{loc_obj.get('url', '')}:{loc_obj.get('lineNumber', '')}"
            except Exception:
                pass
            console_errors.append({"text": text, "location": loc or None})
        except Exception:
            pass

    def on_request_failed(request) -> None:
        try:
            if len(network_errors) >= _MAX_NETWORK_FAIL:
                return
            failure = request.failure
            ft = getattr(failure, "text", None) or (str(failure) if failure else "")
            network_errors.append(
                {
                    "url": (request.url or "")[:2000],
                    "failure": (ft or "")[:500],
                }
            )
        except Exception:
            pass

    http_error_responses: List[Dict[str, Any]] = []

    def on_response(response) -> None:
        try:
            if len(http_error_responses) >= _MAX_NETWORK_FAIL:
                return
            status = int(response.status)
            if status < 400:
                return
            req = response.request
            http_error_responses.append(
                {
                    "url": (response.url or "")[:2000],
                    "status": status,
                    "method": (req.method if req else "")[:20],
                }
            )
        except Exception:
            pass

    title = ""
    final_url = url
    status_code: Optional[int] = None
    inventory: Dict[str, Any] = {}
    extras: Dict[str, Any] = {}
    screenshot_b64: Optional[str] = None
    shot_logs: List[str] = []
    nav_error: Optional[str] = None

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=bool(headless))
        try:
            context = browser.new_context(accept_downloads=False)
            page = context.new_page()
            page.on("console", on_console)
            page.on("requestfailed", on_request_failed)
            page.on("response", on_response)
            try:
                resp = page.goto(str(url), timeout=int(timeout_ms), wait_until="domcontentloaded")
                if resp is not None:
                    try:
                        status_code = int(resp.status)
                    except Exception:
                        status_code = None
            except Exception as e:
                nav_error = f"{type(e).__name__}: {e}"
                logger.warning("browser_inspector: goto failed url=%s err=%s", url, nav_error)

            try:
                title = (page.title() or "").strip()
            except Exception:
                title = ""
            try:
                final_url = (page.url or url).strip()
            except Exception:
                final_url = url

            inventory = extract_dom_inventory(page)
            try:
                extras = page.evaluate(_patch_js_extras(_MAX_FORMS, _MAX_BAD_IMG)) or {}
            except Exception as e:
                logger.warning("browser_inspector: extras evaluate failed: %s", e)
                extras = {}

            screenshot_b64, shot_logs = take_screenshot_robust(page)

            context.close()
        finally:
            browser.close()

    return {
        "url": url,
        "final_url": final_url,
        "title": title,
        "status_code": status_code,
        "inventory": inventory,
        "extras": extras if isinstance(extras, dict) else {},
        "screenshot_b64": screenshot_b64,
        "screenshot_logs": shot_logs,
        "console_errors": console_errors,
        "network_errors": network_errors,
        "http_error_responses": http_error_responses,
        "navigation_error": nav_error,
    }
