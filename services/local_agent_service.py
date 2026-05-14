# services/local_agent_service.py
"""Phase 4A — Local agent admin + agent API (no job execution)."""
from __future__ import annotations

import hashlib
import hmac
import json
import logging
import os
import secrets
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from fastapi import HTTPException, Request, UploadFile

from models.local_agent_models import (
    LOCAL_AGENT_MAX_METADATA_JSON,
    LocalAgent,
    LocalAgentArtifactUploadResponse,
    LocalAgentBrowserInspectionPersistRequest,
    LocalAgentBrowserInspectionPersistResponse,
    LocalAgentDetail,
    LocalAgentHeartbeat,
    LocalAgentJob,
    LocalAgentJobBrief,
    LocalAgentJobResultSubmit,
    LocalAgentPatchRequest,
    LocalAgentPollRequest,
    LocalAgentPollResponse,
    LocalAgentRegistrationRequest,
    LocalAgentRegistrationResponse,
    normalize_capabilities,
)
from services.db.local_agent_repository import local_agent_repo

logger = logging.getLogger("vanya.local_agent")


def _register_secret() -> str:
    return (os.getenv("LOCAL_AGENT_REGISTER_SECRET") or "").strip()


def require_local_agent_admin(request: Request) -> None:
    """
    Protect /local-agents/* admin surface when LOCAL_AGENT_REGISTER_SECRET is set.
    Accepts X-Vanya-Local-Agent-Register-Key or X-Service-Token (VANYA_SERVICE_TOKEN).
    """
    sec = _register_secret()
    if not sec:
        return
    hdr = (request.headers.get("x-vanya-local-agent-register-key") or "").strip()
    if hmac.compare_digest(hdr, sec):
        return
    from core.vanya_auth import machine_request_authorized

    if machine_request_authorized(request.headers):
        return
    raise HTTPException(status_code=403, detail="Missing or invalid local agent admin key")


def hash_agent_token(raw_token: str) -> tuple[str, str]:
    """Return (sha256_hex, short fingerprint for UI)."""
    raw = (raw_token or "").encode("utf-8")
    h = hashlib.sha256(raw).hexdigest()
    return h, h[:12]


def _public_agent_status(row: Dict[str, Any]) -> str:
    if not row.get("enabled"):
        return "disabled"
    return str(row.get("status") or "offline")


def _row_to_local_agent(row: Dict[str, Any]) -> LocalAgent:
    d = dict(row)
    d.pop("token_hash", None)
    caps = normalize_capabilities(list(d.get("capabilities") or []))
    st = _public_agent_status(row)
    return LocalAgent(
        agent_id=d["agent_id"],
        project_id=d.get("project_id"),
        name=d["name"],
        status=st,  # type: ignore[arg-type]
        capabilities=caps,
        version=d.get("version"),
        last_seen_at=d.get("last_seen_at"),
        created_at=d.get("created_at") or "",
        updated_at=d.get("updated_at") or "",
        enabled=bool(d.get("enabled")),
        token_fingerprint=str(d.get("token_fingerprint") or ""),
        metadata=d.get("metadata") if isinstance(d.get("metadata"), dict) else {},
    )


def _row_to_job(row: Dict[str, Any]) -> LocalAgentJob:
    return LocalAgentJob(
        job_id=row["job_id"],
        project_id=row["project_id"],
        agent_id=row.get("agent_id"),
        job_type=row.get("job_type") or "browser_inspection",  # type: ignore[arg-type]
        target_url=row.get("target_url") or "",
        payload=row.get("payload") if isinstance(row.get("payload"), dict) else {},
        status=row.get("status") or "queued",  # type: ignore[arg-type]
        created_at=row.get("created_at") or "",
        claimed_at=row.get("claimed_at"),
        completed_at=row.get("completed_at"),
        result_ref=row.get("result_ref"),
    )


def register_agent(body: LocalAgentRegistrationRequest, request: Request) -> LocalAgentRegistrationResponse:
    require_local_agent_admin(request)
    caps = list(body.capabilities)
    caps_json = json.dumps(caps, separators=(",", ":"))
    meta_json = json.dumps(body.metadata or {}, separators=(",", ":"), default=str)
    raw = f"vla_{secrets.token_urlsafe(32)}"
    th, fp = hash_agent_token(raw)
    aid = str(uuid.uuid4())
    local_agent_repo.insert_agent(
        agent_id=aid,
        project_id=body.project_id,
        name=body.name,
        capabilities_json=caps_json,
        version=body.version,
        agent_meta_json=meta_json,
        token_hash=th,
        token_fingerprint=fp,
    )
    return LocalAgentRegistrationResponse(
        agent_id=aid,
        agent_token=raw,
        token_fingerprint=fp,
        project_id=body.project_id.strip(),
        name=body.name.strip(),
        capabilities=caps,
        version=body.version,
    )


def list_agents(*, project_id: Optional[str], limit: int, request: Request) -> List[LocalAgent]:
    require_local_agent_admin(request)
    rows = local_agent_repo.list_agents(project_id=project_id, limit=limit)
    return [_row_to_local_agent(r) for r in rows]


def get_agent(agent_id: str, request: Request) -> LocalAgentDetail:
    require_local_agent_admin(request)
    row = local_agent_repo.get_agent(agent_id)
    if not row:
        raise HTTPException(status_code=404, detail="agent not found")
    base = _row_to_local_agent(row)
    briefs: List[LocalAgentJobBrief] = []
    pid = (row.get("project_id") or "").strip()
    if pid:
        for j in local_agent_repo.list_recent_jobs_for_project(pid, limit=20):
            briefs.append(
                LocalAgentJobBrief(
                    job_id=j["job_id"],
                    job_type=str(j.get("job_type") or "browser_inspection"),
                    status=str(j.get("status") or ""),
                    target_url=str(j.get("target_url") or "")[:512],
                    created_at=str(j.get("created_at") or ""),
                    completed_at=j.get("completed_at"),
                )
            )
    return LocalAgentDetail(
        agent_id=base.agent_id,
        project_id=base.project_id,
        name=base.name,
        status=base.status,
        capabilities=base.capabilities,
        version=base.version,
        last_seen_at=base.last_seen_at,
        created_at=base.created_at,
        updated_at=base.updated_at,
        enabled=base.enabled,
        token_fingerprint=base.token_fingerprint,
        metadata=base.metadata,
        recent_jobs=briefs,
    )


def patch_agent(agent_id: str, body: LocalAgentPatchRequest, request: Request) -> LocalAgent:
    require_local_agent_admin(request)
    row = local_agent_repo.get_agent(agent_id)
    if not row:
        raise HTTPException(status_code=404, detail="agent not found")
    if body.name is not None:
        local_agent_repo.update_agent(agent_id, name=body.name)
    if body.version is not None:
        local_agent_repo.update_agent(agent_id, version=body.version)
    if body.capabilities is not None:
        caps_json = json.dumps(body.capabilities, separators=(",", ":"))
        local_agent_repo.update_agent(agent_id, capabilities_json=caps_json)
    if body.metadata is not None:
        meta_json = json.dumps(body.metadata, separators=(",", ":"), default=str)
        if len(meta_json.encode("utf-8")) > LOCAL_AGENT_MAX_METADATA_JSON:
            raise HTTPException(status_code=400, detail="metadata too large")
        local_agent_repo.update_agent(agent_id, agent_meta_json=meta_json)
    if body.enabled is not None:
        if not body.enabled:
            local_agent_repo.update_agent(agent_id, enabled=False, status="disabled")
        else:
            local_agent_repo.update_agent(agent_id, enabled=True, status="offline")
    row2 = local_agent_repo.get_agent(agent_id)
    return _row_to_local_agent(row2 or row)


def disable_agent(agent_id: str, request: Request) -> LocalAgent:
    require_local_agent_admin(request)
    row = local_agent_repo.get_agent(agent_id)
    if not row:
        raise HTTPException(status_code=404, detail="agent not found")
    local_agent_repo.update_agent(agent_id, enabled=False, status="disabled")
    row2 = local_agent_repo.get_agent(agent_id)
    return _row_to_local_agent(row2 or row)


def _auth_agent(agent_id: str, authorization: Optional[str]) -> Dict[str, Any]:
    if not authorization or not authorization.lower().startswith("bearer "):
        raise HTTPException(status_code=401, detail="Authorization Bearer token required")
    token = authorization.split(" ", 1)[1].strip()
    if not token:
        raise HTTPException(status_code=401, detail="Empty Bearer token")
    th, _ = hash_agent_token(token)
    row = local_agent_repo.get_agent_by_token_hash(th)
    if not row:
        raise HTTPException(status_code=401, detail="Invalid agent token")
    if row["agent_id"] != (agent_id or "").strip():
        raise HTTPException(status_code=403, detail="agent_id does not match token")
    return row


def heartbeat(agent_id: str, body: LocalAgentHeartbeat, authorization: Optional[str]) -> LocalAgent:
    row = _auth_agent(agent_id, authorization)
    if not row.get("enabled"):
        raise HTTPException(status_code=403, detail="agent is disabled")
    now = datetime.now(timezone.utc).isoformat()
    ver = body.agent_version if body.agent_version is not None else row.get("version")
    local_agent_repo.update_agent(agent_id, last_seen_at=now, status="online", version=ver)
    row2 = local_agent_repo.get_agent(agent_id)
    return _row_to_local_agent(row2 or row)


def poll_jobs(agent_id: str, body: LocalAgentPollRequest, authorization: Optional[str]) -> LocalAgentPollResponse:
    row = _auth_agent(agent_id, authorization)
    if not row.get("enabled"):
        raise HTTPException(status_code=403, detail="agent is disabled")
    pid = (row.get("project_id") or "").strip()
    if not pid:
        raise HTTPException(status_code=400, detail="agent has no project_id")
    raw_jobs = local_agent_repo.list_queued_jobs_for_project(pid, limit=body.limit)
    caps = normalize_capabilities(list(row.get("capabilities") or []))
    return LocalAgentPollResponse(
        jobs=[_row_to_job(j) for j in raw_jobs],
        agent_capabilities=[str(x) for x in caps],
    )


def submit_job_result(
    agent_id: str,
    job_id: str,
    body: LocalAgentJobResultSubmit,
    authorization: Optional[str],
) -> LocalAgentJob:
    agent = _auth_agent(agent_id, authorization)
    if not agent.get("enabled"):
        raise HTTPException(status_code=403, detail="agent is disabled")
    job = local_agent_repo.get_job(job_id)
    if not job:
        raise HTTPException(status_code=404, detail="job not found")
    ap = (agent.get("project_id") or "").strip()
    jp = (job.get("project_id") or "").strip()
    if ap and jp and ap != jp:
        raise HTTPException(status_code=403, detail="job does not belong to agent project")
    now = datetime.now(timezone.utc).isoformat()
    st = body.status
    local_agent_repo.update_job(
        job_id,
        status=st,
        completed_at=now,
        result_ref=body.result_ref,
        error_message=(body.error or "")[:500] or None,
    )
    row2 = local_agent_repo.get_job(job_id)
    return _row_to_job(row2 or job)


async def upload_browser_inspection_artifact(
    agent_id: str,
    inspection_id: str,
    upload: UploadFile,
    authorization: Optional[str],
) -> LocalAgentArtifactUploadResponse:
    row = _auth_agent(agent_id, authorization)
    if not row.get("enabled"):
        raise HTTPException(status_code=403, detail="agent is disabled")
    from services.local_agent_evidence_service import upload_browser_inspection_artifact as _upload

    return await _upload(agent_id=agent_id, inspection_id=inspection_id, upload=upload)


def persist_browser_inspection_from_agent(
    agent_id: str,
    body: LocalAgentBrowserInspectionPersistRequest,
    authorization: Optional[str],
) -> LocalAgentBrowserInspectionPersistResponse:
    row = _auth_agent(agent_id, authorization)
    if not row.get("enabled"):
        raise HTTPException(status_code=403, detail="agent is disabled")
    from services.local_agent_evidence_service import persist_browser_inspection_from_local_agent

    return persist_browser_inspection_from_local_agent(
        agent_id=agent_id,
        agent_project_id=(row.get("project_id") or "").strip() or None,
        body=body,
    )
