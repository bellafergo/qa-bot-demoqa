# tests/test_multi_page_explorer.py
"""
Tests for services/multi_page_explorer.py

All tests target pure helpers — no browser, no I/O.
explore_app() integration tests use unittest.mock to stub explore_page.

Run: .venv/bin/python -m pytest tests/test_multi_page_explorer.py -v
"""
import os
import sys
from unittest.mock import patch, call

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.multi_page_explorer import (
    normalize_url,
    is_internal,
    is_navigable,
    _href_from_selector,
    extract_links,
    explore_app,
)


# ══════════════════════════════════════════════════════════════
# 1. normalize_url
# ══════════════════════════════════════════════════════════════

class TestNormalizeUrl:

    def test_absolute_url_unchanged(self):
        assert normalize_url("https://example.com/path") == "https://example.com/path"

    def test_strips_fragment(self):
        assert normalize_url("https://example.com/path#section") == "https://example.com/path"

    def test_strips_trailing_slash(self):
        assert normalize_url("https://example.com/path/") == "https://example.com/path"

    def test_root_path_preserved(self):
        result = normalize_url("https://example.com/")
        assert result == "https://example.com/"

    def test_root_no_slash(self):
        result = normalize_url("https://example.com")
        assert result in ("https://example.com/", "https://example.com")

    def test_lowercase_scheme(self):
        assert normalize_url("HTTPS://example.com/path").startswith("https://")

    def test_lowercase_host(self):
        assert "example.com" in normalize_url("https://EXAMPLE.COM/path")

    def test_relative_url_resolved_against_base(self):
        result = normalize_url("/login", "https://example.com/home")
        assert result == "https://example.com/login"

    def test_relative_url_no_leading_slash(self):
        result = normalize_url("about", "https://example.com/")
        assert "example.com" in result

    def test_empty_url_returns_empty(self):
        assert normalize_url("") == ""

    def test_fragment_only_returns_empty(self):
        assert normalize_url("#anchor") == ""

    def test_preserves_query_string(self):
        url = "https://example.com/search?q=test"
        assert normalize_url(url) == url

    def test_invalid_url_returns_empty(self):
        assert normalize_url("not_a_url") == ""

    def test_double_fragment_stripped(self):
        result = normalize_url("https://example.com/page#a#b")
        assert "#" not in result


# ══════════════════════════════════════════════════════════════
# 2. is_internal
# ══════════════════════════════════════════════════════════════

class TestIsInternal:

    BASE = "https://example.com"

    def test_same_domain_is_internal(self):
        assert is_internal("https://example.com/login", self.BASE)

    def test_different_domain_not_internal(self):
        assert not is_internal("https://other.com/page", self.BASE)

    def test_subdomain_not_internal(self):
        # subdomain is a different netloc
        assert not is_internal("https://sub.example.com/page", self.BASE)

    def test_same_domain_different_path_is_internal(self):
        assert is_internal("https://example.com/a/b/c", self.BASE)

    def test_empty_url_not_internal(self):
        assert not is_internal("", self.BASE)

    def test_domain_case_insensitive(self):
        assert is_internal("https://EXAMPLE.COM/page", self.BASE)

    def test_http_vs_https_same_domain(self):
        # different scheme but same netloc → internal (netloc only check)
        assert is_internal("http://example.com/page", self.BASE)


# ══════════════════════════════════════════════════════════════
# 3. is_navigable
# ══════════════════════════════════════════════════════════════

class TestIsNavigable:

    def test_normal_path_is_navigable(self):
        assert is_navigable("/login")

    def test_absolute_url_is_navigable(self):
        assert is_navigable("https://example.com/page")

    def test_mailto_not_navigable(self):
        assert not is_navigable("mailto:admin@example.com")

    def test_tel_not_navigable(self):
        assert not is_navigable("tel:+1234567890")

    def test_javascript_not_navigable(self):
        assert not is_navigable("javascript:void(0)")

    def test_pure_anchor_not_navigable(self):
        assert not is_navigable("#section")

    def test_anchor_prefix_not_navigable(self):
        assert not is_navigable("#top")

    def test_empty_string_not_navigable(self):
        assert not is_navigable("")

    def test_none_like_whitespace_not_navigable(self):
        assert not is_navigable("   ")

    def test_case_insensitive_mailto(self):
        assert not is_navigable("MAILTO:foo@bar.com")

    def test_case_insensitive_javascript(self):
        assert not is_navigable("JavaScript:alert(1)")

    def test_relative_path_is_navigable(self):
        assert is_navigable("../about")


# ══════════════════════════════════════════════════════════════
# 4. _href_from_selector
# ══════════════════════════════════════════════════════════════

class TestHrefFromSelector:

    def test_double_quote_selector(self):
        assert _href_from_selector('a[href="/login"]') == "/login"

    def test_single_quote_selector(self):
        assert _href_from_selector("a[href='/about']") == "/about"

    def test_absolute_url_in_selector(self):
        assert _href_from_selector('a[href="https://example.com/page"]') == "https://example.com/page"

    def test_no_match_returns_empty(self):
        assert _href_from_selector("button.login-btn") == ""

    def test_empty_selector_returns_empty(self):
        assert _href_from_selector("") == ""

    def test_extracts_first_match(self):
        result = _href_from_selector('a[href="/first"]')
        assert result == "/first"

    def test_selector_with_path_and_query(self):
        assert _href_from_selector('a[href="/search?q=test"]') == "/search?q=test"


# ══════════════════════════════════════════════════════════════
# 5. extract_links
# ══════════════════════════════════════════════════════════════

_HOME_INV = {
    "url":   "https://example.com",
    "title": "Home",
    "links": [
        {"text": "Login",    "selector": 'a[href="/login"]'},
        {"text": "About",    "selector": 'a[href="/about"]'},
        {"text": "External", "selector": 'a[href="https://other.com/page"]'},
        {"text": "Anchor",   "selector": 'a[href="#section"]'},
        {"text": "Mail",     "selector": 'a[href="mailto:hi@example.com"]'},
    ],
    "inputs": [], "buttons": [], "forms": [],
}

_BASE = "https://example.com"


class TestExtractLinks:

    def test_returns_list(self):
        assert isinstance(extract_links(_HOME_INV, _BASE), list)

    def test_internal_links_included(self):
        links = extract_links(_HOME_INV, _BASE)
        assert any("/login" in l for l in links)
        assert any("/about" in l for l in links)

    def test_external_links_excluded(self):
        links = extract_links(_HOME_INV, _BASE)
        assert not any("other.com" in l for l in links)

    def test_anchors_excluded(self):
        links = extract_links(_HOME_INV, _BASE)
        assert not any("#" in l for l in links)

    def test_mailto_excluded(self):
        links = extract_links(_HOME_INV, _BASE)
        assert not any("mailto" in l for l in links)

    def test_deduplication(self):
        inv = {
            "links": [
                {"text": "A", "selector": 'a[href="/login"]'},
                {"text": "B", "selector": 'a[href="/login"]'},  # duplicate
            ]
        }
        links = extract_links(inv, _BASE)
        assert links.count(next(l for l in links if "/login" in l)) == 1

    def test_non_dict_items_skipped(self):
        inv = {"links": [None, "bad", 42, {"text": "OK", "selector": 'a[href="/ok"]'}]}
        links = extract_links(inv, _BASE)
        assert len(links) == 1

    def test_empty_inventory_returns_empty(self):
        assert extract_links({}, _BASE) == []

    def test_no_links_key_returns_empty(self):
        assert extract_links({"inputs": []}, _BASE) == []

    def test_links_are_normalized(self):
        inv = {"links": [{"text": "X", "selector": 'a[href="/login/"]'}]}
        links = extract_links(inv, _BASE)
        assert links and links[0] == "https://example.com/login"


# ══════════════════════════════════════════════════════════════
# 6. explore_app — output contract (mocked explore_page)
# ══════════════════════════════════════════════════════════════

_PAGE_HOME = {
    "url": "https://example.com", "title": "Home",
    "links": [{"text": "Login", "selector": 'a[href="/login"]'}],
    "inputs": [], "buttons": [], "forms": [],
}
_PAGE_LOGIN = {
    "url": "https://example.com/login", "title": "Login",
    "links": [],
    "inputs": [{"name": "username", "selector": "#username"}],
    "buttons": [{"name": "Submit", "selector": "#submit"}],
    "forms": [],
}


class TestExploreAppContract:

    def test_returns_dict(self):
        with patch("services.multi_page_explorer.explore_page", return_value=_PAGE_HOME):
            result = explore_app("https://example.com", max_pages=1)
        assert isinstance(result, dict)

    def test_has_required_keys(self):
        with patch("services.multi_page_explorer.explore_page", return_value=_PAGE_HOME):
            result = explore_app("https://example.com", max_pages=1)
        for key in ("start_url", "visited_count", "pages", "errors"):
            assert key in result, f"missing key: {key}"

    def test_start_url_preserved(self):
        with patch("services.multi_page_explorer.explore_page", return_value=_PAGE_HOME):
            result = explore_app("https://example.com", max_pages=1)
        assert result["start_url"] == "https://example.com"

    def test_pages_is_list(self):
        with patch("services.multi_page_explorer.explore_page", return_value=_PAGE_HOME):
            result = explore_app("https://example.com", max_pages=1)
        assert isinstance(result["pages"], list)

    def test_errors_is_list(self):
        with patch("services.multi_page_explorer.explore_page", return_value=_PAGE_HOME):
            result = explore_app("https://example.com", max_pages=1)
        assert isinstance(result["errors"], list)

    def test_visited_count_matches_pages_when_no_errors(self):
        with patch("services.multi_page_explorer.explore_page", return_value=_PAGE_HOME):
            result = explore_app("https://example.com", max_pages=1)
        assert result["visited_count"] == len(result["pages"])


# ══════════════════════════════════════════════════════════════
# 7. explore_app — max_pages enforcement
# ══════════════════════════════════════════════════════════════

def _make_page(url: str, links=None) -> dict:
    return {
        "url": url, "title": url.split("/")[-1] or "home",
        "links": links or [],
        "inputs": [], "buttons": [], "forms": [],
    }


class TestMaxPages:

    def test_max_pages_1_visits_only_start(self):
        home = _make_page("https://example.com", links=[
            {"text": "A", "selector": 'a[href="/a"]'},
            {"text": "B", "selector": 'a[href="/b"]'},
        ])
        with patch("services.multi_page_explorer.explore_page", return_value=home):
            result = explore_app("https://example.com", max_pages=1)
        assert result["visited_count"] == 1
        assert len(result["pages"]) == 1

    def test_max_pages_respected(self):
        pages = {
            "https://example.com":       _make_page("https://example.com",       [{"text":"A","selector":'a[href="/a"]'}, {"text":"B","selector":'a[href="/b"]'}]),
            "https://example.com/a":     _make_page("https://example.com/a",     [{"text":"C","selector":'a[href="/c"]'}]),
            "https://example.com/b":     _make_page("https://example.com/b",     []),
            "https://example.com/c":     _make_page("https://example.com/c",     []),
        }
        def fake_explore(url):
            return pages.get(url, _make_page(url))

        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            result = explore_app("https://example.com", max_pages=3)

        assert result["visited_count"] <= 3
        assert len(result["pages"]) <= 3

    def test_no_pages_beyond_max(self):
        home = _make_page("https://example.com", links=[
            {"text": str(i), "selector": f'a[href="/{i}"]'} for i in range(20)
        ])
        def fake_explore(url):
            return _make_page(url)
        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            result = explore_app("https://example.com", max_pages=4)
        assert result["visited_count"] <= 4


# ══════════════════════════════════════════════════════════════
# 8. explore_app — deduplication
# ══════════════════════════════════════════════════════════════

class TestDeduplication:

    def test_same_url_not_visited_twice(self):
        home = _make_page("https://example.com", links=[
            {"text": "Self", "selector": 'a[href="/"]'},
            {"text": "Login", "selector": 'a[href="/login"]'},
        ])
        login = _make_page("https://example.com/login", links=[
            {"text": "Home", "selector": 'a[href="/"]'},
        ])
        def fake_explore(url):
            return login if "login" in url else home

        call_count = {"n": 0}
        def counting_explore(url):
            call_count["n"] += 1
            return fake_explore(url)

        with patch("services.multi_page_explorer.explore_page", side_effect=counting_explore):
            result = explore_app("https://example.com", max_pages=10)

        # Only 2 unique URLs should have been explored
        assert result["visited_count"] == 2
        assert call_count["n"] == 2

    def test_pages_list_has_no_duplicate_urls(self):
        home = _make_page("https://example.com", links=[
            {"text": "Login", "selector": 'a[href="/login"]'},
        ])
        login = _make_page("https://example.com/login")

        def fake_explore(url):
            return login if "login" in url else home

        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            result = explore_app("https://example.com", max_pages=5)

        urls = [p["url"] for p in result["pages"]]
        assert len(urls) == len(set(urls))


# ══════════════════════════════════════════════════════════════
# 9. explore_app — error handling
# ══════════════════════════════════════════════════════════════

class TestErrorHandling:

    def test_failed_page_recorded_in_errors(self):
        def fake_explore(url):
            if "bad" in url:
                raise RuntimeError("timeout")
            return _make_page(url, links=[{"text": "Bad", "selector": 'a[href="/bad"]'}])

        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            result = explore_app("https://example.com", max_pages=5)

        assert any("bad" in e["url"] for e in result["errors"])

    def test_failed_page_does_not_abort_exploration(self):
        def fake_explore(url):
            if "bad" in url:
                raise RuntimeError("connection refused")
            if "good" in url:
                return _make_page(url)
            # start page: has both /bad and /good links
            return _make_page(url, [
                {"text": "Bad",  "selector": 'a[href="/bad"]'},
                {"text": "Good", "selector": 'a[href="/good"]'},
            ])

        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            result = explore_app("https://example.com", max_pages=5)

        # /good should still have been visited
        visited_urls = [p["url"] for p in result["pages"]]
        assert any("good" in u for u in visited_urls)

    def test_error_entry_has_url_and_error_fields(self):
        def fake_explore(url):
            raise ValueError("DNS failure")

        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            result = explore_app("https://example.com", max_pages=1)

        assert result["errors"]
        err = result["errors"][0]
        assert "url" in err
        assert "error" in err
        assert "DNS" in err["error"]

    def test_invalid_start_url_returns_error(self):
        result = explore_app("not_a_url", max_pages=1)
        assert result["visited_count"] == 0
        assert result["pages"] == []
        assert result["errors"]


# ══════════════════════════════════════════════════════════════
# 10. explore_app — external links not followed
# ══════════════════════════════════════════════════════════════

class TestExternalLinksNotFollowed:

    def test_external_links_not_enqueued(self):
        home = _make_page("https://example.com", links=[
            {"text": "External", "selector": 'a[href="https://google.com"]'},
            {"text": "Internal", "selector": 'a[href="/about"]'},
        ])
        about = _make_page("https://example.com/about")

        visited_urls = []
        def fake_explore(url):
            visited_urls.append(url)
            if "about" in url:
                return about
            return home

        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            explore_app("https://example.com", max_pages=5)

        assert not any("google.com" in u for u in visited_urls)
        assert any("about" in u for u in visited_urls)

    def test_mailto_links_not_followed(self):
        home = _make_page("https://example.com", links=[
            {"text": "Contact", "selector": 'a[href="mailto:contact@example.com"]'},
        ])
        visited = []
        def fake_explore(url):
            visited.append(url)
            return home

        with patch("services.multi_page_explorer.explore_page", side_effect=fake_explore):
            explore_app("https://example.com", max_pages=5)

        assert len(visited) == 1  # only start page
