"""Local-agent URL rules (Phase 4C): http(s) only; optional localhost/private with flags/capabilities."""
from __future__ import annotations

import ipaddress
import re
from typing import FrozenSet, Optional
from urllib.parse import urlparse

_JAVASCRIPT_RE = re.compile(r"^\s*javascript\s*:", re.I)


class LocalAgentURLRejected(ValueError):
    """URL must not be loaded by the local agent."""


def _host_from_netloc(netloc: str) -> str:
    if not netloc:
        return ""
    host = netloc.rsplit("@", 1)[-1]
    if host.startswith("["):
        end = host.find("]")
        if end != -1:
            return host[1:end].strip().lower()
        return ""
    if ":" in host:
        parts = host.rsplit(":", 1)
        if parts[1].isdigit():
            host = parts[0]
    return host.strip().lower()


def _is_loopback_host(host: str) -> bool:
    h = (host or "").strip().lower()
    if h in ("localhost", "127.0.0.1", "::1"):
        return True
    if h.startswith("127."):
        try:
            ip = ipaddress.ip_address(h.split("%")[0])
            return bool(ip.is_loopback)
        except ValueError:
            return False
    return False


def _is_literal_private_or_link_local(host: str) -> bool:
    base = (host or "").split("%")[0]
    try:
        ip = ipaddress.ip_address(base)
    except ValueError:
        return False
    return bool(ip.is_private or ip.is_loopback or ip.is_link_local or ip.is_reserved or ip.is_multicast)


def _hostname_policy_flags(host: str) -> tuple[bool, bool]:
    """Return (needs_localhost_access, needs_intranet_access)."""
    if _is_loopback_host(host):
        return True, False
    if _is_literal_private_or_link_local(host):
        return False, True
    return False, False


def validate_job_navigation_url(
    url: str,
    *,
    allow_localhost: bool,
    allow_private_ips: bool,
    agent_capabilities: Optional[FrozenSet[str]] = None,
) -> str:
    """
    Validate URL for local Playwright navigation.

    - Only http/https, reasonable length.
    - Blocks ``file:``, ``ftp:``, ``javascript:``, data:, etc.
    - Loopback hosts require ``allow_localhost`` or ``localhost_access`` capability.
    - Literal private/link-local IPs require ``allow_private_ips`` or ``intranet_access`` capability.
    """
    caps = agent_capabilities or frozenset()
    u = (url or "").strip()
    if not u or len(u) > 2048:
        raise LocalAgentURLRejected("url missing or too long")

    low = u.lower()
    if _JAVASCRIPT_RE.search(low) or "javascript:" in low:
        raise LocalAgentURLRejected("javascript URLs are not allowed")

    parsed = urlparse(u)
    scheme = (parsed.scheme or "").lower()
    if scheme not in ("http", "https"):
        raise LocalAgentURLRejected("only http and https URLs are allowed")

    host = _host_from_netloc(parsed.netloc or "")
    if not host:
        raise LocalAgentURLRejected("URL must include a host")

    need_lo, need_intra = _hostname_policy_flags(host)
    if need_lo:
        if not (allow_localhost or "localhost_access" in caps):
            raise LocalAgentURLRejected(
                "localhost / loopback URL requires --allow-localhost / VANYA_AGENT_ALLOW_LOCALHOST=1 "
                "or agent capability localhost_access"
            )
    if need_intra:
        if not (allow_private_ips or "intranet_access" in caps):
            raise LocalAgentURLRejected(
                "private or link-local IP targets require --allow-private-ips / VANYA_AGENT_ALLOW_PRIVATE_IPS=1 "
                "or agent capability intranet_access"
            )

    return u
