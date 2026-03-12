# services/exploration_service.py
"""
Exploration Service
====================

Bounded, deterministic exploration of a web application.

Architecture:
  - Pure analytical functions (page classification, module inference, safety)
    are testable without a browser.
  - ExplorationService.explore() uses Playwright for real exploration.
  - ExplorationService.explore_from_pages() accepts pre-built PageData
    for testing and mocking.

Exploration strategy: BFS from start_url, bounded by max_pages and
max_click_depth. Unsafe interactive elements are skipped.

Safety blocklist prevents destructive/irreversible actions.
"""
from __future__ import annotations

import logging
import re
from collections import deque
from typing import Any, Dict, List, Optional, Set, Tuple
from urllib.parse import urljoin, urlparse

from models.exploration_models import (
    DiscoveredAction,
    DiscoveredFlow,
    DiscoveredPage,
    ExplorationRequest,
    ExplorationResult,
)

logger = logging.getLogger("vanya.exploration")


# ── Safety filter ─────────────────────────────────────────────────────────────

_UNSAFE_KEYWORDS: List[str] = [
    "delete", "remove", "logout", "log out", "sign out", "signout",
    "pay now", "submit payment", "confirm purchase", "confirm order",
    "place order", "checkout", "buy now", "pay", "drop", "destroy",
    "cancel subscription", "delete account", "deactivate", "terminate",
    "unsubscribe", "clear all", "reset all",
]


def is_unsafe_action(text: str) -> bool:
    """Return True if the text suggests a destructive or irreversible action."""
    lower = text.lower().strip()
    return any(kw in lower for kw in _UNSAFE_KEYWORDS)


# ── Page type classification ──────────────────────────────────────────────────

_PAGE_TYPE_RULES: List[Tuple[str, List[str], List[str]]] = [
    # (page_type, url_patterns, title_patterns)
    ("login",     ["login", "signin", "sign-in", "auth", "session"],
                  ["log in", "sign in", "login", "authentication"]),
    ("dashboard", ["dashboard", "home", "overview", "main", "portal", "admin"],
                  ["dashboard", "overview", "home", "welcome"]),
    ("settings",  ["settings", "preferences", "profile", "account", "config", "security"],
                  ["settings", "preferences", "profile", "account"]),
    ("form",      ["new", "create", "edit", "add", "register", "signup", "contact", "feedback", "apply"],
                  ["create", "add new", "edit", "register", "contact", "form"]),
    ("detail",    [],  # detected by URL pattern /\d+/ or /{id}
                  ["detail", "view", "item", "product", "article"]),
    ("listing",   ["list", "search", "browse", "catalog", "products", "results", "explore", "directory"],
                  ["search", "results", "list", "catalog", "browse", "products", "all"]),
]


def classify_page_type(url: str, title: str, text: str = "") -> str:
    """Heuristic page type classification from URL, title, and visible text."""
    url_lower   = url.lower()
    title_lower = title.lower()
    combined    = f"{url_lower} {title_lower} {text.lower()}"

    # Detail page: URL ends in numeric ID or UUID-like segment
    if re.search(r"/\d{1,10}(/|$)", url) or re.search(r"/[a-f0-9-]{8,}(/|$)", url):
        return "detail"

    for page_type, url_patterns, title_patterns in _PAGE_TYPE_RULES:
        if any(p in url_lower for p in url_patterns):
            return page_type
        if any(p in title_lower for p in title_patterns):
            return page_type

    return "unknown"


# ── Module inference ──────────────────────────────────────────────────────────

_MODULE_KEYWORD_MAP: List[Tuple[List[str], str]] = [
    (["login", "logout", "signin", "signup", "sign-in", "sign-out",
      "auth", "authentication", "password", "session", "token", "oauth",
      "register", "forgot-password", "reset-password", "credential"], "auth"),
    (["checkout", "cart", "payment", "billing", "invoice",
      "order", "purchase", "pay", "subscription", "promo"], "checkout"),
    (["product", "catalog", "category", "search", "inventory",
      "item", "sku", "stock", "listing", "browse", "shop"], "catalog"),
    (["user", "profile", "account", "settings", "preference",
      "me", "my-account", "customer", "member", "security"], "account"),
    (["admin", "management", "config", "console", "analytics",
      "report", "audit", "panel", "backoffice"], "admin"),
    (["file", "upload", "download", "media", "asset",
      "image", "attachment", "document"], "files"),
    (["notification", "message", "alert", "inbox",
      "email", "sms", "webhook"], "notifications"),
]


def infer_module(url: str, title: str, action_labels: Optional[List[str]] = None) -> str:
    """Infer a QA module name from URL path, page title, and action labels."""
    combined = f"{url} {title} {' '.join(action_labels or [])}".lower()
    combined = re.sub(r"[-_/]", " ", combined)

    for keywords, module in _MODULE_KEYWORD_MAP:
        if any(kw in combined for kw in keywords):
            return module
    return "ui"


# ── URL helpers ───────────────────────────────────────────────────────────────

def _same_domain(url: str, base_url: str, allowed_domain: Optional[str] = None) -> bool:
    """Check if url belongs to the same domain as base_url (or allowed_domain)."""
    try:
        base_host = urlparse(base_url).netloc.lower()
        url_host  = urlparse(url).netloc.lower()
        if allowed_domain:
            return allowed_domain.lower() in url_host
        # Strip www. for comparison
        return url_host == base_host or url_host.lstrip("www.") == base_host.lstrip("www.")
    except Exception:
        return False


def _normalize_url(href: str, base_url: str) -> Optional[str]:
    """Resolve a potentially relative href to an absolute URL."""
    if not href:
        return None
    try:
        if href.startswith(("#", "javascript:", "mailto:", "tel:")):
            return None
        if href.startswith("http"):
            return href.split("#")[0].rstrip("/") or href
        resolved = urljoin(base_url, href)
        return resolved.split("#")[0].rstrip("/") or resolved
    except Exception:
        return None


# ── Internal page data container ──────────────────────────────────────────────

class PageData:
    """Internal representation of a captured page — used during exploration."""

    def __init__(
        self,
        url:          str,
        title:        str,
        depth:        int,
        links:        List[Dict[str, str]],    # [{href, text}]
        buttons:      List[Dict[str, str]],    # [{text, href, tag}]
        visible_text: str = "",
    ):
        self.url          = url
        self.title        = title
        self.depth        = depth
        self.links        = links
        self.buttons      = buttons
        self.visible_text = visible_text


# ── Analysis: convert PageData → DiscoveredPage + flows ──────────────────────

def _build_discovered_page(pd: PageData) -> DiscoveredPage:
    """Convert raw PageData into a DiscoveredPage model."""
    page_type = classify_page_type(pd.url, pd.title, pd.visible_text)
    actions: List[DiscoveredAction] = []
    outgoing: List[str] = []

    for link in pd.links:
        label = (link.get("text") or link.get("href") or "").strip()
        href  = link.get("href", "")
        unsafe = is_unsafe_action(label)
        actions.append(DiscoveredAction(
            action_type = "link",
            label       = label[:100],
            target_url  = href or None,
            is_unsafe   = unsafe,
        ))
        if href and not unsafe:
            outgoing.append(href)

    for btn in pd.buttons:
        label  = (btn.get("text") or "").strip()
        target = btn.get("href") or None
        unsafe = is_unsafe_action(label)
        if label:
            actions.append(DiscoveredAction(
                action_type = "button" if btn.get("tag", "").lower() == "button" else "submit",
                label       = label[:100],
                target_url  = target,
                is_unsafe   = unsafe,
            ))

    # Key elements: top non-empty visible snippets
    key_elems = [a.label for a in actions if a.label and not a.is_unsafe][:5]

    return DiscoveredPage(
        url               = pd.url,
        title             = pd.title,
        depth             = pd.depth,
        page_type         = page_type,
        discovered_actions = actions,
        outgoing_links    = list(dict.fromkeys(outgoing))[:20],
        key_elements      = key_elems,
        confidence        = 1.0,
    )


def build_flows(
    pages: List[DiscoveredPage],
    transitions: List[Tuple[str, str, str]],   # (from_url, to_url, action_label)
) -> List[DiscoveredFlow]:
    """
    Build DiscoveredFlow objects from page transitions.

    Each transition (from_url → to_url via action_label) becomes a flow.
    """
    url_to_page: Dict[str, DiscoveredPage] = {p.url: p for p in pages}
    flows: List[DiscoveredFlow] = []

    for from_url, to_url, action_label in transitions:
        from_page = url_to_page.get(from_url)
        to_page   = url_to_page.get(to_url)
        if from_page is None or to_page is None:
            continue

        # Gather action labels for module inference
        labels = [action_label, from_page.title, to_page.title, from_url, to_url]
        module = infer_module(to_url, to_page.title, labels)
        depth  = to_page.depth

        flow_name = (
            f"{action_label or 'Navigate'} → {to_page.title or to_url}"
        ).strip()

        flows.append(DiscoveredFlow(
            name            = flow_name[:120],
            start_url       = from_url,
            end_url         = to_url,
            actions         = [action_label] if action_label else ["navigate"],
            depth           = depth,
            inferred_module = module,
            confidence      = 0.8,
        ))

    return flows


# ── Service ───────────────────────────────────────────────────────────────────

class ExplorationService:

    # ── Public API ────────────────────────────────────────────────────────────

    def explore(self, req: ExplorationRequest) -> ExplorationResult:
        """
        Explore a web application using a real Playwright browser.

        Opens a headless Chromium browser, performs BFS navigation from
        start_url, bounded by max_pages and max_click_depth.
        """
        notes: List[str] = []
        try:
            from playwright.sync_api import sync_playwright  # noqa
            with sync_playwright() as p:
                browser = p.chromium.launch(headless=True)
                try:
                    pages_data = self._bfs_explore(req, browser, notes)
                finally:
                    browser.close()
        except Exception as exc:
            logger.exception("exploration: browser exploration failed")
            return ExplorationResult(
                start_url = req.start_url,
                notes     = [f"Exploration failed: {type(exc).__name__}: {exc}"],
            )

        return self._build_result(req, pages_data, notes)

    def explore_from_pages(
        self,
        req:             ExplorationRequest,
        raw_pages:       List[Dict[str, Any]],
        transitions:     Optional[List[Tuple[str, str, str]]] = None,
    ) -> ExplorationResult:
        """
        Analyze pre-captured page data without launching a browser.

        raw_pages: list of dicts with keys: url, title, depth, links, buttons,
                   visible_text (optional).
        transitions: list of (from_url, to_url, action_label) if known.
                     If None, auto-inferred from parent-child depth.

        Used in tests and for pipeline injection.
        """
        pds: List[PageData] = [
            PageData(
                url          = p["url"],
                title        = p.get("title", ""),
                depth        = p.get("depth", 0),
                links        = p.get("links", []),
                buttons      = p.get("buttons", []),
                visible_text = p.get("visible_text", ""),
            )
            for p in raw_pages
        ]
        return self._build_result(req, pds, [], transitions=transitions)

    # ── Playwright BFS ────────────────────────────────────────────────────────

    def _bfs_explore(
        self,
        req:     ExplorationRequest,
        browser: Any,
        notes:   List[str],
    ) -> List[PageData]:
        """BFS over the application, collecting PageData for each visited URL."""
        visited: Set[str]   = set()
        queue:   deque      = deque()
        pages:   List[PageData] = []

        queue.append((req.start_url, 0))

        while queue and len(pages) < req.max_pages:
            url, depth = queue.popleft()
            if url in visited or depth > req.max_click_depth:
                continue
            visited.add(url)

            pd = self._visit_page(browser, url, depth, req, notes)
            if pd is None:
                continue
            pages.append(pd)

            # Enqueue outgoing links
            for link in pd.links:
                href = link.get("href", "")
                if href and href not in visited:
                    if req.include_external_links or _same_domain(
                        href, req.start_url, req.allowed_domain
                    ):
                        queue.append((href, depth + 1))

        return pages

    def _visit_page(
        self,
        browser: Any,
        url:     str,
        depth:   int,
        req:     ExplorationRequest,
        notes:   List[str],
    ) -> Optional[PageData]:
        """Navigate to a URL and capture page data."""
        try:
            page = browser.new_page()
            try:
                page.goto(url, timeout=12000, wait_until="domcontentloaded")
                title = page.title() or ""
                links   = self._extract_links(page, req)
                buttons = self._extract_buttons(page)
                text    = self._extract_visible_text(page)
                return PageData(url=url, title=title, depth=depth,
                                links=links, buttons=buttons, visible_text=text)
            finally:
                page.close()
        except Exception as exc:
            notes.append(f"Could not visit {url}: {exc}")
            return None

    def _extract_links(self, page: Any, req: ExplorationRequest) -> List[Dict[str, str]]:
        try:
            raw = page.evaluate("""
                () => Array.from(document.querySelectorAll('a[href]'))
                    .map(a => ({
                        href: a.href || '',
                        text: (a.textContent || a.getAttribute('aria-label') || '').trim().slice(0, 100)
                    }))
                    .filter(a => a.href.startsWith('http'))
                    .slice(0, 60)
            """)
            result = []
            for item in (raw or []):
                href = _normalize_url(item.get("href", ""), req.start_url) or ""
                if href:
                    result.append({"href": href, "text": item.get("text", "")})
            return result
        except Exception:
            return []

    def _extract_buttons(self, page: Any) -> List[Dict[str, str]]:
        try:
            raw = page.evaluate("""
                () => Array.from(document.querySelectorAll(
                    'button, input[type="submit"], [role="button"], nav a, header a'
                ))
                .map(el => ({
                    tag:  el.tagName || '',
                    text: (el.textContent || el.value || el.getAttribute('aria-label') || '').trim().slice(0, 80),
                    href: el.getAttribute('href') || ''
                }))
                .filter(el => el.text)
                .slice(0, 40)
            """)
            return raw or []
        except Exception:
            return []

    def _extract_visible_text(self, page: Any) -> str:
        try:
            text = page.evaluate("""
                () => (document.body ? document.body.innerText : '').slice(0, 500)
            """)
            return (text or "").strip()
        except Exception:
            return ""

    # ── Build result ──────────────────────────────────────────────────────────

    def _build_result(
        self,
        req:         ExplorationRequest,
        pages_data:  List[PageData],
        notes:       List[str],
        transitions: Optional[List[Tuple[str, str, str]]] = None,
    ) -> ExplorationResult:
        """Convert raw PageData list into a full ExplorationResult."""
        discovered_pages: List[DiscoveredPage] = [_build_discovered_page(pd) for pd in pages_data]

        # Auto-infer transitions if not provided
        if transitions is None:
            transitions = self._infer_transitions(pages_data)

        flows = build_flows(discovered_pages, transitions)

        # Apply module override
        if req.module_override:
            for f in flows:
                f.inferred_module = req.module_override

        notes_out = list(notes)
        if not discovered_pages:
            notes_out.append("No pages were discovered.")

        return ExplorationResult(
            start_url        = req.start_url,
            discovered_pages = discovered_pages,
            discovered_flows = flows,
            total_pages      = len(discovered_pages),
            total_flows      = len(flows),
            notes            = notes_out,
        )

    @staticmethod
    def _infer_transitions(pages_data: List[PageData]) -> List[Tuple[str, str, str]]:
        """
        Infer (from_url, to_url, label) transitions from BFS parent-child relationships.

        Uses the fact that a page at depth D was discovered via a link from depth D-1.
        """
        # Build depth → urls map
        depth_urls: Dict[int, List[str]] = {}
        for pd in pages_data:
            depth_urls.setdefault(pd.depth, []).append(pd.url)

        # Build url → PageData map
        url_to_pd: Dict[str, PageData] = {pd.url: pd for pd in pages_data}

        transitions: List[Tuple[str, str, str]] = []

        for pd in pages_data:
            if pd.depth == 0:
                continue
            parent_depth = pd.depth - 1
            parents = depth_urls.get(parent_depth, [])

            # Find the parent that had this URL as an outgoing link
            for parent_url in parents:
                parent_pd = url_to_pd.get(parent_url)
                if parent_pd is None:
                    continue
                for link in parent_pd.links:
                    href = link.get("href", "")
                    if href == pd.url:
                        label = link.get("text", "") or "navigate"
                        transitions.append((parent_url, pd.url, label))
                        break

        return transitions


# ── Module-level singleton ────────────────────────────────────────────────────

exploration_service = ExplorationService()
