"""Process ``browser_inspection`` jobs on the local agent (Phase 4C + 4D cloud persist)."""
from __future__ import annotations

import base64
import logging
from typing import Any, Dict, List, Optional, Tuple

from local_agent.client import AgentClientError
from local_agent.config import AgentConfig
from local_agent.inspect_local import run_local_browser_inspection
from local_agent.result_packager import pack_browser_inspection_result_ref
from local_agent.url_guard import LocalAgentURLRejected, validate_job_navigation_url
from services.browser_inspection_persistence import merge_persist_fields_into_inspection
from services.local_agent_evidence_service import sniff_image_content_type

logger = logging.getLogger("vanya.local_agent.browser_job")

_MIME_EXT = {
    "image/png": ("png", "image/png"),
    "image/jpeg": ("jpg", "image/jpeg"),
    "image/webp": ("webp", "image/webp"),
}


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


def _payload_watch_id(job: Dict[str, Any]) -> Optional[str]:
    p = job.get("payload") if isinstance(job.get("payload"), dict) else {}
    w = str(p.get("watch_id") or "").strip()
    return w or None


def decode_runner_screenshot_bytes(raw_runner: Dict[str, Any]) -> Tuple[Optional[bytes], Optional[str]]:
    """Decode ``screenshot_b64`` / data-URL from runner output. Returns ``(bytes, error)``."""
    b64 = raw_runner.get("screenshot_b64") if isinstance(raw_runner, dict) else None
    if not b64 or not str(b64).strip():
        return None, None
    s = str(b64).strip()
    try:
        if "base64," in s:
            s = s.split("base64,", 1)[1]
        pad = (-len(s)) % 4
        if pad:
            s += "=" * pad
        return base64.b64decode(s, validate=False), None
    except Exception as e:
        return None, f"screenshot_decode_failed: {type(e).__name__}"


def execute_browser_inspection_job(
    job: Dict[str, Any],
    cfg: AgentConfig,
    *,
    agent_capabilities: Optional[List[str]] = None,
    client: Optional[Any] = None,
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
        raw = raw if isinstance(raw, dict) else {}

        merged = result
        artifact_upload_warning: Optional[str] = None
        artifact_sha: Optional[str] = None
        evidence_url = result.screenshot_url

        if client is not None:
            img_bytes, dec_err = decode_runner_screenshot_bytes(raw)
            if dec_err:
                artifact_upload_warning = dec_err
            elif img_bytes:
                mime = sniff_image_content_type(img_bytes)
                if not mime or mime not in _MIME_EXT:
                    artifact_upload_warning = "artifact_upload_skipped: unsupported image bytes"
                else:
                    ext, ct = _MIME_EXT[mime]
                    try:
                        up = client.upload_browser_inspection_artifact(
                            result.inspection_id,
                            img_bytes,
                            filename=f"screenshot.{ext}",
                            content_type=ct,
                        )
                        evidence_url = str(up.get("evidence_url") or "").strip() or evidence_url
                        artifact_sha = str(up.get("sha256") or "").strip() or None
                    except AgentClientError as e:
                        artifact_upload_warning = str(e)[:400]

            if evidence_url and evidence_url != (result.screenshot_url or ""):
                merged = result.model_copy(update={"screenshot_url": evidence_url})

            project_id = str(job.get("project_id") or "").strip() or None
            pres: Dict[str, Any] = {}
            try:
                pres = client.persist_browser_inspection_to_cloud(
                    inspection=merged,
                    job_id=jid or None,
                    watch_id=_payload_watch_id(job),
                    project_id=project_id,
                    artifact_sha256=artifact_sha,
                )
            except AgentClientError as e:
                pres = {"persisted_run_id": None, "persisted": False, "persistence_warning": str(e)[:400]}

            pw_parts = [x for x in [pres.get("persistence_warning"), artifact_upload_warning] if x]
            merged = merge_persist_fields_into_inspection(
                merged,
                persisted_run_id=pres.get("persisted_run_id"),
                persisted=bool(pres.get("persisted")),
                persistence_warning="; ".join(str(x) for x in pw_parts) if pw_parts else None,
            )

        ref = pack_browser_inspection_result_ref(
            merged,
            raw_runner=raw,
            cloud_evidence_url=merged.screenshot_url,
            persisted_run_id=merged.persisted_run_id,
            persisted=merged.persisted,
            persistence_warning=merged.persistence_warning,
        )
        st = "succeeded" if result.inspection_succeeded else "failed"
        err: Optional[str] = None
        if not result.inspection_succeeded:
            err = (result.warnings[0] if result.warnings else "inspection_succeeded=false")[:500]
        return st, ref, err
    except Exception as e:
        logger.exception("browser_inspection job failed job_id=%s", jid)
        return "failed", None, f"playwright_error: {type(e).__name__}: {e}"[:500]
