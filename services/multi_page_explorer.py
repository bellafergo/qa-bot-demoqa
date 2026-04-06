# services/multi_page_explorer.py
"""
Multi-page Application Explorer for Vanya.

Public API
----------
    explore_app(start_url, max_pages=5) -> dict   [I/O — uses Playwright]

    # Pure helpers (no I/O — fully testable):
    normalize_url(url, base="")  -> str
    is_internal(url, base_url)   -> bool
    is_navigable(raw_href)       -> bool
    extract_links(inventory, base_url) -> list[str]

Design
------
BFS from start_url up to max_pages pages.
Each page is inventoried by the existing explore_page() function.
Internal links are extracted from each inventory and added to the queue.
Pages that fail to load are recorded in "errors" and skipped.
"""
from __future__ import annotations

import re
from collections import deque
from typing import Any, Dict, List
from urllib.parse import urljoin, urlparse, urlunparse

# Import at module level so tests can patch services.multi_page_explorer.explore_page.
# Playwright is only invoked when explore_page() is actually called — no I/O at import time.
from core.target_url_validation import TargetURLNotAllowed, validate_target_url
from services.application_explorer import explore_page  # noqa: E402


# ── Pure helpers ──────────────────────────────────────────────────────────────

def normalize_url(url: str, base: str = "") -> str:
    """
    Resolve *url* relative to *base* (when given), strip the fragment,
    normalise scheme/host to lowercase, and remove a trailing slash from
    the path (except for the root "/").

    Returns "" if the result has no scheme or host (i.e. unresolvable).
    """
    try:
        resolved = urljoin(base, url.strip()) if base else url.strip()
        p = urlparse(resolved)
        if not p.scheme or not p.netloc:
            return ""
        path = p.path.rstrip("/") or "/"
        return urlunparse((
            p.scheme.lower(),
            p.netloc.lower(),
            path,
            p.params,
            p.query,
            "",          # drop fragment
        ))
    except Exception:
        return ""


def _domain(url: str) -> str:
    """Return the lowercased netloc of a URL, or '' on failure."""
    try:
        return urlparse(url).netloc.lower()
    except Exception:
        return ""


def is_internal(url: str, base_url: str) -> bool:
    """True when *url* belongs to the same domain (netloc) as *base_url*."""
    d = _domain(url)
    return bool(d) and d == _domain(base_url)


def is_navigable(raw: str) -> bool:
    """
    True when *raw* is a href string worth following:
    - not empty
    - not mailto:, tel:, javascript:
    - not a pure anchor (#...)
    """
    if not raw or not raw.strip():
        return False
    low = raw.strip().lower()
    for skip in ("mailto:", "tel:", "javascript:", "#"):
        if low.startswith(skip):
            return False
    return True


def _href_from_selector(selector: str) -> str:
    """
    Extract the href value from an attribute selector like
    ``a[href="/path"]`` or ``a[href='https://example.com']``.
    Returns "" if the pattern does not match.
    """
    m = re.search(r'a\[href=["\']([^"\']+)["\']', selector)
    return m.group(1) if m else ""


def extract_links(inventory: Dict[str, Any], base_url: str) -> List[str]:
    """
    From a page inventory dict, return a deduplicated list of normalized
    internal URLs that are worth visiting next.

    Uses the ``links`` list in the inventory.  Each link item has a
    ``selector`` like ``a[href="/login"]``; the href is extracted and
    resolved against *base_url*.
    """
    seen: set     = set()
    result: List[str] = []

    for link in (inventory.get("links") or []):
        if not isinstance(link, dict):
            continue
        raw_href = _href_from_selector(str(link.get("selector") or ""))
        if not is_navigable(raw_href):
            continue
        abs_url = normalize_url(raw_href, base_url)
        if abs_url and is_internal(abs_url, base_url) and abs_url not in seen:
            seen.add(abs_url)
            result.append(abs_url)

    return result


# ── Main function (I/O) ───────────────────────────────────────────────────────

def explore_app(start_url: str, max_pages: int = 5) -> Dict[str, Any]:
    """
    Explore up to *max_pages* pages of the same domain, starting from
    *start_url*.

    Algorithm: BFS — each page's links are added to the queue; already-
    visited URLs and non-internal URLs are skipped.

    Pages that raise during exploration are recorded in ``errors`` and
    do not interrupt the overall traversal.

    Returns::

        {
            "start_url":     str,
            "visited_count": int,
            "pages":         [inventory, ...],
            "errors":        [{"url": ..., "error": ...}, ...],
        }
    """
    try:
        vstart = validate_target_url(start_url)
    except TargetURLNotAllowed as exc:
        return {
            "start_url":     start_url,
            "visited_count": 0,
            "pages":         [],
            "errors":        [{"url": start_url, "error": str(exc)}],
        }

    start = normalize_url(vstart)
    if not start:
        return {
            "start_url":     start_url,
            "visited_count": 0,
            "pages":         [],
            "errors":        [{"url": start_url, "error": "Could not parse start URL"}],
        }

    max_pages = max(1, int(max_pages))

    visited: set        = set()
    queue:   deque      = deque([start])
    pages:   List       = []
    errors:  List       = []

    while queue and len(visited) < max_pages:
        url = queue.popleft()
        if url in visited:
            continue
        visited.add(url)

        try:
            inventory = explore_page(url)
        except Exception as exc:
            errors.append({"url": url, "error": str(exc)})
            continue

        pages.append(inventory)

        # Enqueue new internal links (skip if already queued or visited)
        queued = set(queue)
        for link_url in extract_links(inventory, start):
            if link_url not in visited and link_url not in queued:
                queue.append(link_url)
                queued.add(link_url)

    return {
        "start_url":     start_url,
        "visited_count": len(visited),
        "pages":         pages,
        "errors":        errors,
    }
