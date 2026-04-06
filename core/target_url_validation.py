# core/target_url_validation.py
"""
SSRF hardening: only http(s) targets, no private/link-local IPs, optional host allowlist.

Environment:
  ALLOWED_TARGET_HOSTS — comma-separated hostname suffixes (e.g. demoqa.com,herokuapp.com).
  If unset/empty, any public resolvable host is allowed after safety checks.
"""
from __future__ import annotations

import concurrent.futures
import ipaddress
import os
import socket
from typing import Optional
from urllib.parse import urljoin, urlparse

_MAX_URL_LEN = 2048
_DNS_TIMEOUT_S = 3.0


class TargetURLNotAllowed(ValueError):
    """Raised when a navigation URL must not be loaded server-side."""

    def __init__(self, message: str = "Target URL not allowed"):
        super().__init__(message)


def _allowed_host_suffixes() -> frozenset[str]:
    raw = (os.getenv("ALLOWED_TARGET_HOSTS") or "").strip()
    if not raw:
        return frozenset()
    return frozenset(p.strip().lower() for p in raw.split(",") if p.strip())


def _host_matches_policy(hostname: str) -> bool:
    allowed = _allowed_host_suffixes()
    if not allowed:
        return True
    h = hostname.lower().rstrip(".")
    for suffix in allowed:
        if h == suffix or h.endswith("." + suffix):
            return True
    return False


def _extract_hostname(netloc: str) -> str:
    if not netloc:
        return ""
    host = netloc
    if "@" in host:
        host = host.split("@")[-1]
    if host.startswith("["):
        idx = host.find("]")
        if idx != -1:
            return host[1:idx].strip().lower()
        return ""
    if ":" in host:
        parts = host.rsplit(":", 1)
        if parts[1].isdigit():
            host = parts[0]
    return host.strip().lower()


def _check_resolved_ip(addr: str) -> None:
    """Raise TargetURLNotAllowed if addr is loopback, private, link-local, etc."""
    base = addr.split("%")[0]
    try:
        ip = ipaddress.ip_address(base)
    except ValueError:
        raise TargetURLNotAllowed()
    if (
        ip.is_private
        or ip.is_loopback
        or ip.is_link_local
        or ip.is_multicast
        or ip.is_reserved
        or (ip.version == 4 and int(ip) == 0)
    ):
        raise TargetURLNotAllowed()


def _blocked_hostname_label(host: str) -> bool:
    h = host.lower().rstrip(".")
    return h in (
        "localhost",
        "metadata.google.internal",
        "metadata",
        "metadata.goog",
    )


def _dns_resolve_check(hostname: str) -> None:
    try:
        ascii_host = hostname.encode("idna").decode("ascii")
    except (UnicodeError, UnicodeDecodeError):
        raise TargetURLNotAllowed() from None

    with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool:
        fut = pool.submit(
            socket.getaddrinfo,
            ascii_host,
            None,
            type=socket.SOCK_STREAM,
        )
        try:
            infos = fut.result(timeout=_DNS_TIMEOUT_S)
        except concurrent.futures.TimeoutError:
            raise TargetURLNotAllowed() from None
        except socket.gaierror:
            raise TargetURLNotAllowed() from None

    if not infos:
        raise TargetURLNotAllowed()
    for info in infos:
        addr = info[4][0]
        _check_resolved_ip(addr)


def _validate_hostname(host: str) -> None:
    if not host:
        raise TargetURLNotAllowed()
    if _blocked_hostname_label(host):
        raise TargetURLNotAllowed()
    if not _host_matches_policy(host):
        raise TargetURLNotAllowed()
    try:
        ipaddress.ip_address(host.split("%")[0])
        _check_resolved_ip(host)
        return
    except ValueError:
        pass
    _dns_resolve_check(host)


def _absolute_candidate(raw: str, base_url: Optional[str]) -> str:
    raw = raw.strip()
    if not raw:
        raise TargetURLNotAllowed()
    if len(raw) > _MAX_URL_LEN:
        raise TargetURLNotAllowed()

    if raw.startswith("//"):
        if not base_url:
            raise TargetURLNotAllowed()
        bu = urlparse(base_url)
        if bu.scheme not in ("http", "https"):
            raise TargetURLNotAllowed()
        raw = f"{bu.scheme}:{raw}"

    p = urlparse(raw)
    if p.scheme in ("http", "https") and p.netloc:
        return raw
    if not base_url:
        raise TargetURLNotAllowed()
    candidate = urljoin(base_url, raw)
    p2 = urlparse(candidate)
    if (p2.scheme or "").lower() not in ("http", "https") or not p2.netloc:
        raise TargetURLNotAllowed()
    return candidate


def validate_target_url(url: str, *, base_url: Optional[str] = None) -> str:
    """
    Validate a URL before server-side navigation (Playwright goto, explorers).

    Returns the absolute URL string to use. Raises TargetURLNotAllowed if unsafe.
    """
    candidate = _absolute_candidate(url, base_url)
    p = urlparse(candidate)
    if (p.scheme or "").lower() not in ("http", "https"):
        raise TargetURLNotAllowed()
    host = _extract_hostname(p.netloc)
    _validate_hostname(host)
    return candidate


def validate_steps_navigation_urls(steps: list, base_url: Optional[str]) -> None:
    """Validate every goto step; raises TargetURLNotAllowed on first violation."""
    from runners.common import _normalize_action, _url_from_step

    for s in steps:
        if not isinstance(s, dict):
            continue
        if _normalize_action(s) != "goto":
            continue
        u = (_url_from_step(s, base_url) or "").strip()
        if not u:
            continue
        validate_target_url(u, base_url=base_url)
