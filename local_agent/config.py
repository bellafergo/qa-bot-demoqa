"""Configuration for Vanya Local Agent CLI (env + argparse)."""
from __future__ import annotations

import os
from dataclasses import dataclass
from typing import Any, Optional
from urllib.parse import urlparse


@dataclass(frozen=True)
class AgentConfig:
    base_url: str
    agent_id: str
    agent_token: str
    poll_interval_s: float
    http_timeout_s: float
    dry_run: bool
    once: bool
    agent_version: str
    browser_enabled: bool = False
    allow_localhost: bool = False
    allow_private_ips: bool = False
    browser_headless: bool = True
    browser_timeout_ms_default: int = 15_000

    def token_log_label(self) -> str:
        """Safe label for logs (fingerprint + last 4 chars; never full token)."""
        from local_agent.security import token_log_label

        return token_log_label(self.agent_token)


def _env(name: str, default: Optional[str] = None) -> Optional[str]:
    v = os.getenv(name)
    if v is None or str(v).strip() == "":
        return default
    return str(v).strip()


def _env_bool(name: str, default: bool = False) -> bool:
    v = _env(name)
    if v is None:
        return default
    return v.lower() in ("1", "true", "yes", "on")


def _parse_float(s: Optional[str], *, fallback: float) -> float:
    if s is None or str(s).strip() == "":
        return fallback
    return float(str(s).strip())


def _parse_int_env(name: str, *, fallback: int, lo: int, hi: int) -> int:
    raw = _env(name)
    if raw is None:
        return fallback
    try:
        v = int(str(raw).strip())
    except Exception:
        return fallback
    return max(lo, min(v, hi))


def _merge_bool(cli_val: Optional[bool], env_name: str, *, default: bool = False) -> bool:
    if cli_val is not None:
        return bool(cli_val)
    return _env_bool(env_name, default)


def validate_base_url(url: str) -> str:
    u = (url or "").strip().rstrip("/")
    if not u:
        raise ValueError("base URL is required (CLI --base-url or VANYA_CLOUD_URL)")
    p = urlparse(u)
    if p.scheme not in ("http", "https"):
        raise ValueError("base URL must start with http:// or https://")
    if not p.netloc:
        raise ValueError("base URL must include a host")
    return u


def build_config(
    *,
    base_url: Optional[str],
    agent_id: Optional[str],
    agent_token: Optional[str],
    poll_interval_s: Optional[float],
    http_timeout_s: Optional[float],
    dry_run: bool,
    once: bool,
    agent_version: Optional[str] = None,
    browser_enabled: Optional[bool] = None,
    allow_localhost: Optional[bool] = None,
    allow_private_ips: Optional[bool] = None,
    browser_headless: Optional[bool] = None,
    browser_timeout_ms_default: Optional[int] = None,
) -> AgentConfig:
    bu = validate_base_url((base_url or _env("VANYA_CLOUD_URL") or "").strip())
    aid = (agent_id or _env("VANYA_AGENT_ID") or "").strip()
    tok = (agent_token or _env("VANYA_AGENT_TOKEN") or "").strip()
    if not aid:
        raise ValueError("agent id is required (CLI --agent-id or VANYA_AGENT_ID)")
    if not tok:
        raise ValueError("agent token is required (CLI --agent-token or VANYA_AGENT_TOKEN)")
    if len(tok) < 8:
        raise ValueError("agent token looks too short; refusing to run")

    poll = poll_interval_s
    if poll is None:
        poll = _parse_float(_env("VANYA_AGENT_POLL_INTERVAL"), fallback=10.0)
    if poll < 1.0:
        raise ValueError("poll interval must be >= 1 second")

    timeout = http_timeout_s
    if timeout is None:
        timeout = _parse_float(_env("VANYA_AGENT_HTTP_TIMEOUT"), fallback=30.0)
    if timeout < 5.0:
        raise ValueError("HTTP timeout must be >= 5 seconds")

    av = (agent_version or "vanya-local-agent/4c").strip() or "vanya-local-agent/4c"

    be = _merge_bool(browser_enabled, "VANYA_AGENT_BROWSER_ENABLED", default=False)
    al = _merge_bool(allow_localhost, "VANYA_AGENT_ALLOW_LOCALHOST", default=False)
    ap = _merge_bool(allow_private_ips, "VANYA_AGENT_ALLOW_PRIVATE_IPS", default=False)
    bh = _merge_bool(browser_headless, "VANYA_AGENT_BROWSER_HEADLESS", default=True)
    btm = browser_timeout_ms_default
    if btm is None:
        btm = _parse_int_env("VANYA_AGENT_BROWSER_TIMEOUT_MS", fallback=15_000, lo=3000, hi=60_000)

    return AgentConfig(
        base_url=bu,
        agent_id=aid,
        agent_token=tok,
        poll_interval_s=float(poll),
        http_timeout_s=float(timeout),
        dry_run=dry_run,
        once=once,
        agent_version=av,
        browser_enabled=be,
        allow_localhost=al,
        allow_private_ips=ap,
        browser_headless=bh,
        browser_timeout_ms_default=int(btm),
    )


def argparse_namespace_to_kwargs(ns: Any) -> dict:
    """Map argparse.Namespace to build_config kwargs (None = use env / defaults)."""
    av = getattr(ns, "agent_version", None)

    def _ob(name: str) -> Optional[bool]:
        if not hasattr(ns, name):
            return None
        v = getattr(ns, name)
        return v if v is not None else None

    return dict(
        base_url=getattr(ns, "base_url", None),
        agent_id=getattr(ns, "agent_id", None),
        agent_token=getattr(ns, "agent_token", None),
        poll_interval_s=getattr(ns, "poll_interval", None),
        http_timeout_s=getattr(ns, "http_timeout", None),
        dry_run=bool(getattr(ns, "dry_run", False)),
        once=bool(getattr(ns, "once", False)),
        agent_version=av if av is not None else None,
        browser_enabled=_ob("browser_enabled"),
        allow_localhost=_ob("allow_localhost"),
        allow_private_ips=_ob("allow_private_ips"),
        browser_headless=_ob("browser_headless"),
        browser_timeout_ms_default=getattr(ns, "browser_timeout_ms", None),
    )
