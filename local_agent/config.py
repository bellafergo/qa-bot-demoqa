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

    def token_log_label(self) -> str:
        """Safe label for logs (fingerprint + last 4 chars; never full token)."""
        from local_agent.security import token_log_label

        return token_log_label(self.agent_token)


def _env(name: str, default: Optional[str] = None) -> Optional[str]:
    v = os.getenv(name)
    if v is None or str(v).strip() == "":
        return default
    return str(v).strip()


def _parse_float(s: Optional[str], *, fallback: float) -> float:
    if s is None or str(s).strip() == "":
        return fallback
    return float(str(s).strip())


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

    av = (agent_version or "vanya-local-agent/4b").strip() or "vanya-local-agent/4b"

    return AgentConfig(
        base_url=bu,
        agent_id=aid,
        agent_token=tok,
        poll_interval_s=float(poll),
        http_timeout_s=float(timeout),
        dry_run=dry_run,
        once=once,
        agent_version=av,
    )


def argparse_namespace_to_kwargs(ns: Any) -> dict:
    """Map argparse.Namespace to build_config kwargs (None = use env / defaults)."""
    av = getattr(ns, "agent_version", None)
    return dict(
        base_url=getattr(ns, "base_url", None),
        agent_id=getattr(ns, "agent_id", None),
        agent_token=getattr(ns, "agent_token", None),
        poll_interval_s=getattr(ns, "poll_interval", None),
        http_timeout_s=getattr(ns, "http_timeout", None),
        dry_run=bool(getattr(ns, "dry_run", False)),
        once=bool(getattr(ns, "once", False)),
        agent_version=av if av is not None else None,
    )
