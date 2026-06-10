# services/local_agent_service.py
"""Phase 4A — Local agent admin + agent API (no job execution)."""
from __future__ import annotations

import hashlib
import hmac
import json
import logging
import os
import re
import secrets
import uuid
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional

from fastapi import HTTPException, Request, UploadFile

from models.local_agent_models import (
    LOCAL_AGENT_MAX_METADATA_JSON,
    AgentCapability,
    AgentInventory,
    FoundationAgentStatus,
    LocalAgent,
    LocalAgentArtifactUploadResponse,
    LocalAgentBrowserInspectionPersistRequest,
    LocalAgentBrowserInspectionPersistResponse,
    LocalAgentDetail,
    LocalAgentFoundationHeartbeat,
    LocalAgentFoundationRegistrationRequest,
    LocalAgentFoundationRegistrationResponse,
    LocalAgentFoundationView,
    LocalAgentHeartbeat,
    LocalAgentJob,
    LocalAgentJobBrief,
    LocalAgentJobResultSubmit,
    LocalAgentMockHeartbeatRequest,
    LocalAgentPatchRequest,
    LocalAgentPollRequest,
    LocalAgentPollResponse,
    LocalAgentRegistrationRequest,
    LocalAgentRegistrationResponse,
    LocalAgentReport,
    normalize_capabilities,
)
from services.db.local_agent_repository import local_agent_repo

logger = logging.getLogger("vanya.local_agent")

FOUNDATION_HEARTBEAT_ONLINE_SECONDS = 300
FOUNDATION_CAPABILITY_CATALOG: List[AgentCapability] = [
    AgentCapability(
        capability_id="database_validation",
        name="Database Validation",
        description="Prepare read-only database validation checks from incident intelligence.",
    ),
    AgentCapability(
        capability_id="contract_validation",
        name="Contract Validation",
        description="Model and compare API contracts without live endpoint execution.",
    ),
    AgentCapability(
        capability_id="browser_probe",
        name="Browser Probe",
        description="Observe browser signals from the customer network when approved.",
    ),
    AgentCapability(
        capability_id="filesystem_inventory",
        name="Filesystem Inventory",
        description="Report declared filesystem inventory metadata only.",
    ),
    AgentCapability(
        capability_id="repo_inventory",
        name="Repository Inventory",
        description="Report declared repository inventory metadata only.",
    ),
    AgentCapability(
        capability_id="network_inventory",
        name="Network Inventory",
        description="Report declared network service inventory metadata only.",
    ),
]


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


# ── INT-03A Local Agents Foundation (registration, heartbeat, inventory) ─────


def build_deterministic_agent_id(project_id: str, name: str) -> str:
    slug_proj = re.sub(r"[^a-z0-9]+", "_", (project_id or "").strip().lower()).strip("_") or "project"
    slug_name = re.sub(r"[^a-z0-9]+", "_", (name or "").strip().lower()).strip("_") or "agent"
    return f"agent:{slug_proj}:{slug_name}"


def build_deterministic_agent_token(agent_id: str) -> str:
    secret = _register_secret() or "vanya-foundation"
    digest = hashlib.sha256(f"{agent_id}:{secret}".encode("utf-8")).hexdigest()[:16]
    token_id = re.sub(r"[^a-zA-Z0-9_]+", "_", agent_id)
    return f"agent_{token_id}_{digest}"


def _parse_iso(ts: Optional[str]) -> Optional[datetime]:
    if not ts:
        return None
    try:
        return datetime.fromisoformat(str(ts).replace("Z", "+00:00"))
    except ValueError:
        return None


def resolve_foundation_status(row: Dict[str, Any], *, now: Optional[datetime] = None) -> FoundationAgentStatus:
    if not row.get("enabled"):
        return "OFFLINE"
    db_status = str(row.get("status") or "").lower()
    if db_status == "disabled":
        return "OFFLINE"
    seen = _parse_iso(row.get("last_seen_at"))
    if not seen:
        return "UNKNOWN"
    ref = now or datetime.now(timezone.utc)
    if seen.tzinfo is None:
        seen = seen.replace(tzinfo=timezone.utc)
    age = (ref - seen).total_seconds()
    if age <= FOUNDATION_HEARTBEAT_ONLINE_SECONDS:
        return "ONLINE"
    return "OFFLINE"


def _inventory_from_metadata(meta: Dict[str, Any], agent_id: str) -> AgentInventory:
    inv = meta.get("inventory") if isinstance(meta.get("inventory"), dict) else {}
    return AgentInventory(
        agent_id=agent_id,
        databases_detected=[str(x) for x in (inv.get("databases_detected") or []) if str(x).strip()],
        repositories_detected=[str(x) for x in (inv.get("repositories_detected") or []) if str(x).strip()],
        services_detected=[str(x) for x in (inv.get("services_detected") or []) if str(x).strip()],
    )


def _merge_metadata(row: Dict[str, Any], **updates: Any) -> Dict[str, Any]:
    meta = row.get("metadata") if isinstance(row.get("metadata"), dict) else {}
    merged = dict(meta)
    merged.update({k: v for k, v in updates.items() if v is not None})
    return merged


def row_to_foundation_view(row: Dict[str, Any], *, now: Optional[datetime] = None) -> LocalAgentFoundationView:
    meta = row.get("metadata") if isinstance(row.get("metadata"), dict) else {}
    return LocalAgentFoundationView(
        agent_id=row["agent_id"],
        name=row.get("name") or row["agent_id"],
        version=str(row.get("version") or meta.get("version") or "1.0.0"),
        status=resolve_foundation_status(row, now=now),
        registered_at=str(row.get("created_at") or ""),
        last_heartbeat_at=row.get("last_seen_at"),
        environment=str(meta.get("environment") or "unknown"),
        capabilities=[str(x) for x in (row.get("capabilities") or [])],
        metadata=meta,
    )


def list_foundation_capabilities() -> List[AgentCapability]:
    return list(FOUNDATION_CAPABILITY_CATALOG)


def register_foundation_agent(
    body: LocalAgentFoundationRegistrationRequest,
    request: Request,
) -> LocalAgentFoundationRegistrationResponse:
    require_local_agent_admin(request)
    agent_id = build_deterministic_agent_id(body.project_id, body.name)
    if local_agent_repo.get_agent(agent_id):
        raise HTTPException(status_code=409, detail="agent already registered for this project and name")
    duplicate = local_agent_repo.get_agent_by_project_and_name(body.project_id, body.name)
    if duplicate and duplicate.get("agent_id") != agent_id:
        raise HTTPException(status_code=409, detail="agent name already registered for this project")

    caps = list(body.capabilities)
    caps_json = json.dumps(caps, separators=(",", ":"))
    inventory = body.inventory.model_dump() if body.inventory else {}
    meta = _merge_metadata(
        {"metadata": body.metadata or {}},
        environment=(body.environment or "production").strip().lower(),
        inventory=inventory,
        foundation=True,
    )
    meta_json = json.dumps(meta, separators=(",", ":"), default=str)
    if len(meta_json.encode("utf-8")) > LOCAL_AGENT_MAX_METADATA_JSON:
        raise HTTPException(status_code=400, detail="metadata too large")

    raw_token = build_deterministic_agent_token(agent_id)
    token_hash, fingerprint = hash_agent_token(raw_token)
    local_agent_repo.insert_agent(
        agent_id=agent_id,
        project_id=body.project_id,
        name=body.name,
        capabilities_json=caps_json,
        version=body.version,
        agent_meta_json=meta_json,
        token_hash=token_hash,
        token_fingerprint=fingerprint,
    )
    row = local_agent_repo.get_agent(agent_id) or {}
    return LocalAgentFoundationRegistrationResponse(
        agent_id=agent_id,
        agent_token=raw_token,
        token_fingerprint=fingerprint,
        project_id=body.project_id.strip(),
        name=body.name.strip(),
        environment=str(meta.get("environment") or "production"),
        version=body.version,
        capabilities=caps,
        registered_at=str(row.get("created_at") or datetime.now(timezone.utc).isoformat()),
    )


def _apply_foundation_heartbeat(
    *,
    agent_id: str,
    version: Optional[str],
    capabilities: Optional[List[str]],
    timestamp: Optional[str],
    inventory: Optional[AgentInventory],
) -> LocalAgentFoundationView:
    row = local_agent_repo.get_agent(agent_id)
    if not row:
        raise HTTPException(status_code=404, detail="agent not found")
    if not row.get("enabled"):
        raise HTTPException(status_code=403, detail="agent is disabled")

    now = (timestamp or datetime.now(timezone.utc).isoformat()).strip()
    meta = _merge_metadata(row)
    if inventory is not None:
        meta["inventory"] = inventory.model_dump(exclude={"agent_id"})
    meta_json = json.dumps(meta, separators=(",", ":"), default=str)
    updates: Dict[str, Any] = {"last_seen_at": now, "status": "online", "agent_meta_json": meta_json}
    if version:
        updates["version"] = version
    if capabilities is not None:
        updates["capabilities_json"] = json.dumps(capabilities, separators=(",", ":"))
    local_agent_repo.update_agent(agent_id, **updates)
    row2 = local_agent_repo.get_agent(agent_id) or row
    return row_to_foundation_view(row2)


def process_foundation_heartbeat(
    body: LocalAgentFoundationHeartbeat,
    authorization: Optional[str],
) -> LocalAgentFoundationView:
    row = _auth_agent(body.agent_id, authorization)
    if body.agent_id != row.get("agent_id"):
        raise HTTPException(status_code=403, detail="agent_id mismatch")
    caps = [str(x) for x in body.capabilities] if body.capabilities is not None else None
    return _apply_foundation_heartbeat(
        agent_id=body.agent_id,
        version=body.version,
        capabilities=caps,
        timestamp=body.timestamp,
        inventory=body.inventory,
    )


def mock_foundation_heartbeat(
    body: LocalAgentMockHeartbeatRequest,
    request: Request,
) -> LocalAgentFoundationView:
    require_local_agent_admin(request)
    caps = [str(x) for x in body.capabilities] if body.capabilities is not None else None
    return _apply_foundation_heartbeat(
        agent_id=body.agent_id,
        version=body.version,
        capabilities=caps,
        timestamp=body.timestamp,
        inventory=body.inventory,
    )


def refresh_offline_agents(*, project_id: Optional[str] = None) -> int:
    """Mark stale agents offline in storage (no remote polling)."""
    rows = local_agent_repo.list_agents(project_id=project_id, limit=500)
    now = datetime.now(timezone.utc)
    updated = 0
    for row in rows:
        if not row.get("enabled"):
            continue
        status = resolve_foundation_status(row, now=now)
        db_status = "online" if status == "ONLINE" else "offline"
        if str(row.get("status") or "").lower() != db_status:
            local_agent_repo.update_agent(row["agent_id"], status=db_status)
            updated += 1
    return updated


def build_local_agent_report(*, project_id: Optional[str], limit: int = 200) -> LocalAgentReport:
    refresh_offline_agents(project_id=project_id)
    rows = local_agent_repo.list_agents(project_id=project_id, limit=limit)
    now = datetime.now(timezone.utc)
    agents = [row_to_foundation_view(row, now=now) for row in rows]
    inventory = [_inventory_from_metadata(row.get("metadata") or {}, row["agent_id"]) for row in rows]
    online = sum(1 for a in agents if a.status == "ONLINE")
    summary = (
        f"{len(agents)} local agent(s) registered; {online} online."
        if agents
        else "No local agents registered."
    )
    return LocalAgentReport(agents=agents, inventory=inventory, summary=summary)


def get_local_agent_report(project_id: Optional[str], limit: int, request: Request) -> LocalAgentReport:
    require_local_agent_admin(request)
    return build_local_agent_report(project_id=project_id, limit=limit)
