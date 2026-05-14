"""Process ``browser_inspection`` jobs on the local agent (Phase 4C)."""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Tuple

from local_agent.config import AgentConfig
from local_agent.inspect_local import run_local_browser_inspection
from local_agent.result_packager import pack_browser_inspection_result_ref
from local_agent.url_guard import LocalAgentURLRejected, validate_job_navigation_url

logger = logging.getLogger("vanya.local_agent.browser_job")


def _caps_set(agent_capabilities: Optional[List[str]]) -> frozenset[str]:
    return frozenset(str(x).strip().lower() for x in (agent_capabilities or []) if str(x).strip())


def _payload_url(job: Dict[str, Any]) -> str:
    p = job.get("payload") if isinstance(job.get("payload"), dict) else {}
    u = str(p.get("url") or "").strip()
    if u:
        return u
    return str(job.get("target_url") or "").strip()


def _payload_timeout_ms(job: Dict[str, Any], default_ms: int) -> int:
    p = job.get("payload") if isinstance(job.get("payload"), dict) else {}
    try:
        v = int(p.get("timeout_ms", default_ms))
    except Exception:
        v = int(default_ms)
    return max(3000, min(v, 60_000))


def _payload_execution_mode(job: Dict[str, Any]) -> str:
    p = job.get("payload") if isinstance(job.get("payload"), dict) else {}
    return str(p.get("execution_mode") or "").strip().lower()


def execute_browser_inspection_job(
    job: Dict[str, Any],
    cfg: AgentConfig,
    *,
    agent_capabilities: Optional[List[str]] = None,
) -> Tuple[str, Optional[str], Optional[str]]:
    """
    Returns ``(status, result_ref, error)`` for ``submit_job_result``.
    ``status`` is ``succeeded`` or ``failed``.
    """
    caps = _caps_set(agent_capabilities)
    jid = str(job.get("job_id") or "").strip()
    jt = str(job.get("job_type") or "").strip().lower()
    if jt != "browser_inspection":
        return "failed", None, f"unsupported job_type: {jt!r} (only browser_inspection in Phase 4C)"

    if not cfg.browser_enabled:
        return "failed", None, "browser execution disabled — set VANYA_AGENT_BROWSER_ENABLED=1 or pass --browser-enabled"

    if _payload_execution_mode(job) != "local_agent":
        return "failed", None, "payload.execution_mode must be local_agent for local browser runs"

    raw_url = _payload_url(job)
    if not raw_url:
        return "failed", None, "missing url in job.payload.url and job.target_url"

    try:
        safe_url = validate_job_navigation_url(
            raw_url,
            allow_localhost=cfg.allow_localhost,
            allow_private_ips=cfg.allow_private_ips,
            agent_capabilities=caps,
        )
    except LocalAgentURLRejected as e:
        return "failed", None, str(e)[:500]

    timeout_ms = _payload_timeout_ms(job, cfg.browser_timeout_ms_default)
    try:
        result, raw = run_local_browser_inspection(
            url=safe_url,
            timeout_ms=timeout_ms,
            headless=cfg.browser_headless,
        )
        ref = pack_browser_inspection_result_ref(result, raw_runner=raw)
        st = "succeeded" if result.inspection_succeeded else "failed"
        err: Optional[str] = None
        if not result.inspection_succeeded:
            err = (result.warnings[0] if result.warnings else "inspection_succeeded=false")[:500]
        return st, ref, err
    except Exception as e:
        logger.exception("browser_inspection job failed job_id=%s", jid)
        return "failed", None, f"playwright_error: {type(e).__name__}: {e}"[:500]
