# services/application_explorer.py
"""
Application Explorer — single-page element inventory for QA.

Public API
----------
    analyze_html(html_content, url="", title="") -> dict
        Pure function. Parses an HTML string and returns structured element
        inventory. No I/O, no side effects. Useful for testing and offline use.

    explore_page(url) -> dict
        Opens *url* with a headless Playwright browser, extracts the rendered
        HTML, then delegates to analyze_html(). Returns the same schema.

Output schema
-------------
    {
        "url":     str,
        "title":   str,
        "inputs":  [{"name": str, "selector": str}, ...],
        "buttons": [{"name": str, "selector": str}, ...],
        "links":   [{"text": str, "selector": str}, ...],
        "forms":   [{"name": str, "fields": [str], "buttons": [str]}, ...],
    }
"""
from __future__ import annotations

from html.parser import HTMLParser
from typing import Any, Dict, List, Optional, Tuple

# ── Constants ──────────────────────────────────────────────────────────────────

_SKIP_INPUT_TYPES  = frozenset({"hidden", "image"})
_BUTTON_INPUT_TYPES = frozenset({"submit", "button", "reset"})
_SKIP_HREF_PREFIXES = ("javascript:", "mailto:", "tel:")


# ── Public API ─────────────────────────────────────────────────────────────────

def analyze_html(
    html_content: str,
    url:   str = "",
    title: str = "",
) -> Dict[str, Any]:
    """
    Parse *html_content* and return a structured element inventory dict.

    *url* and *title* are used as fallbacks if not found in the HTML.
    Never raises — returns partial data on parse errors.
    """
    parser = _PageParser()
    try:
        parser.feed(html_content or "")
    except Exception:
        pass

    parsed_title, inputs, buttons, links, forms = parser.result()
    return {
        "url":     url,
        "title":   parsed_title or title,
        "inputs":  inputs,
        "buttons": buttons,
        "links":   links,
        "forms":   forms,
    }


def explore_page(url: str) -> Dict[str, Any]:
    """
    Navigate to *url* with a headless Chromium browser and return an element
    inventory via analyze_html().

    Requires playwright to be installed (`pip install playwright` +
    `playwright install chromium`).
    """
    from core.target_url_validation import validate_target_url

    url = validate_target_url(url)

    from playwright.sync_api import sync_playwright  # lazy import — not required for pure usage

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        try:
            page = browser.new_page()
            page.goto(str(url), timeout=20_000, wait_until="domcontentloaded")
            html_content = page.content()
            page_title   = page.title() or ""
            page_url     = page.url or url
        finally:
            browser.close()

    return analyze_html(html_content, url=page_url, title=page_title)


def explore_page_in_session(page, url: str) -> Dict[str, Any]:
    """
    Like explore_page() but uses an existing Playwright page (shared session/cookies).
    """
    from core.target_url_validation import validate_target_url

    v = validate_target_url(url)
    page.goto(str(v), timeout=20_000, wait_until="domcontentloaded")
    html_content = page.content()
    page_title = page.title() or ""
    page_url = page.url or str(v)
    return analyze_html(html_content, url=page_url, title=page_title)


# ── HTML Parser ────────────────────────────────────────────────────────────────

class _PageParser(HTMLParser):
    """
    Single-pass HTML parser that collects inputs, buttons, links, and forms.

    Uses a form-stack to associate contained elements with their parent form.
    """

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        # title
        self._in_title: bool = False
        self._title_buf: List[str] = []
        # element accumulators
        self._inputs:  List[Dict[str, str]] = []
        self._buttons: List[Dict[str, str]] = []
        self._links:   List[Dict[str, str]] = []
        self._forms:   List[Dict[str, Any]] = []
        # form nesting
        self._form_stack: List[Dict[str, Any]] = []
        self._form_counter = 0
        # button text accumulation
        self._in_button: bool = False
        self._btn_attrs: Dict[str, str] = {}
        self._btn_buf:   List[str] = []
        # link text accumulation
        self._in_link: bool = False
        self._lnk_attrs: Dict[str, str] = {}
        self._lnk_buf:   List[str] = []
        # global counters for fallback selectors
        self._input_idx  = 0
        self._button_idx = 0
        self._link_idx   = 0

    # ── event handlers ────────────────────────────────────────────────────────

    def handle_starttag(self, tag: str, attrs: list) -> None:
        a = dict(attrs)
        tag = tag.lower()

        if tag == "title":
            self._in_title = True

        elif tag == "form":
            self._form_counter += 1
            form: Dict[str, Any] = {
                "_attrs":   a,
                "_index":   self._form_counter,
                "_inputs":  [],
                "_buttons": [],
            }
            self._forms.append(form)
            self._form_stack.append(form)

        elif tag == "input":
            self._process_input(a)

        elif tag == "button":
            self._in_button = True
            self._btn_attrs = a
            self._btn_buf   = []

        elif tag == "a":
            href = a.get("href") or ""
            skip = (
                not href
                or href.startswith("#")
                or any(href.lower().startswith(p) for p in _SKIP_HREF_PREFIXES)
            )
            if not skip:
                self._in_link = True
                self._lnk_attrs = a
                self._lnk_buf   = []

    def handle_endtag(self, tag: str) -> None:
        tag = tag.lower()

        if tag == "title":
            self._in_title = False

        elif tag == "form":
            if self._form_stack:
                self._form_stack.pop()

        elif tag == "button":
            if self._in_button:
                text = _clean("".join(self._btn_buf))
                self._process_button(self._btn_attrs, text)
                self._in_button = False
                self._btn_attrs = {}
                self._btn_buf   = []

        elif tag == "a":
            if self._in_link:
                text = _clean("".join(self._lnk_buf))
                self._process_link(self._lnk_attrs, text)
                self._in_link = False
                self._lnk_attrs = {}
                self._lnk_buf   = []

    def handle_data(self, data: str) -> None:
        if self._in_title:
            self._title_buf.append(data)
        if self._in_button:
            self._btn_buf.append(data)
        if self._in_link:
            self._lnk_buf.append(data)

    # ── element processors ────────────────────────────────────────────────────

    def _process_input(self, a: Dict[str, str]) -> None:
        itype = (a.get("type") or "text").lower()

        if itype in _SKIP_INPUT_TYPES:
            return

        if itype in _BUTTON_INPUT_TYPES:
            # treat as a button
            value = a.get("value") or ""
            aria  = a.get("aria-label") or ""
            eid   = a.get("id") or ""
            ename = a.get("name") or ""
            self._button_idx += 1
            idx  = self._button_idx
            name = _clean(value or aria or eid or ename) or f"button_{idx}"
            if eid:
                selector = f"#{eid}"
            elif ename:
                selector = f'input[name="{ename}"]'
            else:
                selector = f'input[type="{itype}"]'
            btn = {"name": name, "selector": selector}
            self._buttons.append(btn)
            if self._form_stack:
                self._form_stack[-1]["_buttons"].append(name)
            return

        # regular input
        self._input_idx += 1
        idx         = self._input_idx
        eid         = a.get("id") or ""
        ename       = a.get("name") or ""
        placeholder = a.get("placeholder") or ""
        aria        = a.get("aria-label") or ""
        name        = _clean(eid or ename or placeholder or aria) or f"{itype}_{idx}"

        if eid:
            selector = f"#{eid}"
        elif ename:
            selector = f'input[name="{ename}"]'
        else:
            selector = f'input[type="{itype}"]:nth-of-type({idx})'

        inp = {"name": name, "selector": selector}
        self._inputs.append(inp)
        if self._form_stack:
            self._form_stack[-1]["_inputs"].append(name)

    def _process_button(self, a: Dict[str, str], text: str) -> None:
        self._button_idx += 1
        idx  = self._button_idx
        eid  = a.get("id") or ""
        ename = a.get("name") or ""
        aria = a.get("aria-label") or ""
        name = _clean(text or aria or eid or ename) or f"button_{idx}"

        if eid:
            selector = f"#{eid}"
        elif ename:
            selector = f'button[name="{ename}"]'
        else:
            selector = f"button:nth-of-type({idx})"

        btn = {"name": name, "selector": selector}
        self._buttons.append(btn)
        if self._form_stack:
            self._form_stack[-1]["_buttons"].append(name)

    def _process_link(self, a: Dict[str, str], text: str) -> None:
        self._link_idx += 1
        idx  = self._link_idx
        href = a.get("href") or ""
        eid  = a.get("id") or ""
        aria = a.get("aria-label") or ""
        lbl  = _clean(text or aria or _href_label(href)) or f"link_{idx}"

        if eid:
            selector = f"#{eid}"
        elif href:
            selector = f'a[href="{href}"]'
        else:
            selector = f"a:nth-of-type({idx})"

        self._links.append({"text": lbl[:120], "selector": selector})

    # ── result ────────────────────────────────────────────────────────────────

    def result(
        self,
    ) -> Tuple[str, List, List, List, List]:
        title = _clean("".join(self._title_buf))
        forms: List[Dict[str, Any]] = []
        for f in self._forms:
            fa    = f["_attrs"]
            fid   = fa.get("id") or ""
            fname = fa.get("name") or ""
            fact  = _href_label(fa.get("action") or "")
            fback = f"form_{f['_index']}"
            forms.append({
                "name":    _clean(fid or fname or fact) or fback,
                "fields":  list(f["_inputs"]),
                "buttons": list(f["_buttons"]),
            })
        return title, self._inputs, self._buttons, self._links, forms


# ── Helpers ────────────────────────────────────────────────────────────────────

def _clean(s: Any) -> str:
    """Collapse whitespace, strip, limit length."""
    try:
        return " ".join(str(s).split()).strip()
    except Exception:
        return ""


def _href_label(href: str) -> str:
    """Last non-empty path segment from a URL/path string."""
    try:
        # strip query + fragment
        bare = str(href).split("?")[0].split("#")[0]
        parts = [p for p in bare.rstrip("/").split("/") if p]
        return parts[-1] if parts else ""
    except Exception:
        return ""
