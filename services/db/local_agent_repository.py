# services/db/local_agent_repository.py
"""SQLite persistence for Vanya Local Agent (Phase 4A)."""
from __future__ import annotations

import json
import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, Integer, String, Text, and_, desc

from models.local_agent_models import LOCAL_AGENT_MAX_JOB_PAYLOAD_JSON
from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.local_agent")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class LocalAgentRow(Base):
    __tablename__ = "local_agents"

    agent_id = Column(String, primary_key=True)
    project_id = Column(String, nullable=True, index=True)
    name = Column(String, nullable=False)
    status = Column(String, nullable=False, default="offline")
    capabilities_json = Column(Text, nullable=False, default="[]")
    version = Column(String, nullable=True)
    last_seen_at = Column(String, nullable=True)
    created_at = Column(String, nullable=False)
    updated_at = Column(String, nullable=False)
    enabled = Column(Integer, nullable=False, default=1)
    agent_meta_json = Column(Text, nullable=True)
    token_hash = Column(String, nullable=False, unique=True, index=True)
    token_fingerprint = Column(String, nullable=False)


class LocalAgentJobRow(Base):
    __tablename__ = "local_agent_jobs"

    job_id = Column(String, primary_key=True)
    project_id = Column(String, nullable=False, index=True)
    agent_id = Column(String, nullable=True, index=True)
    job_type = Column(String, nullable=False, default="browser_inspection")
    target_url = Column(String, nullable=False)
    payload_json = Column(Text, nullable=True)
    status = Column(String, nullable=False, default="queued")
    created_at = Column(String, nullable=False)
    claimed_at = Column(String, nullable=True)
    completed_at = Column(String, nullable=True)
    result_ref = Column(String, nullable=True)
    error_message = Column(Text, nullable=True)


def _agent_to_dict(row: LocalAgentRow) -> Dict[str, Any]:
    caps: List[str] = []
    try:
        caps = json.loads(row.capabilities_json or "[]")
        if not isinstance(caps, list):
            caps = []
    except Exception:
        caps = []
    meta: Dict[str, Any] = {}
    try:
        m = json.loads(row.agent_meta_json or "{}")
        meta = m if isinstance(m, dict) else {}
    except Exception:
        meta = {}
    return {
        "agent_id": row.agent_id,
        "project_id": row.project_id,
        "name": row.name,
        "status": row.status,
        "capabilities": caps,
        "version": row.version,
        "last_seen_at": row.last_seen_at,
        "created_at": row.created_at,
        "updated_at": row.updated_at,
        "enabled": bool(row.enabled),
        "metadata": meta,
        "token_hash": row.token_hash,
        "token_fingerprint": row.token_fingerprint,
    }


def _job_to_dict(row: LocalAgentJobRow) -> Dict[str, Any]:
    payload: Dict[str, Any] = {}
    try:
        p = json.loads(row.payload_json or "{}")
        payload = p if isinstance(p, dict) else {}
    except Exception:
        payload = {}
    return {
        "job_id": row.job_id,
        "project_id": row.project_id,
        "agent_id": row.agent_id,
        "job_type": row.job_type,
        "target_url": row.target_url,
        "payload": payload,
        "status": row.status,
        "created_at": row.created_at,
        "claimed_at": row.claimed_at,
        "completed_at": row.completed_at,
        "result_ref": row.result_ref,
        "error_message": row.error_message,
    }


class LocalAgentRepository:
    def insert_agent(
        self,
        *,
        agent_id: str,
        project_id: Optional[str],
        name: str,
        capabilities_json: str,
        version: Optional[str],
        agent_meta_json: Optional[str],
        token_hash: str,
        token_fingerprint: str,
    ) -> None:
        now = _utc_iso()
        row = LocalAgentRow(
            agent_id=agent_id,
            project_id=(project_id or "").strip() or None,
            name=name.strip(),
            status="offline",
            capabilities_json=capabilities_json,
            version=(version or "").strip() or None,
            last_seen_at=None,
            created_at=now,
            updated_at=now,
            enabled=1,
            agent_meta_json=agent_meta_json,
            token_hash=token_hash,
            token_fingerprint=token_fingerprint,
        )
        with get_session() as s:
            s.add(row)

    def get_agent(self, agent_id: str) -> Optional[Dict[str, Any]]:
        aid = (agent_id or "").strip()
        if not aid:
            return None
        with get_session() as s:
            row = s.query(LocalAgentRow).filter_by(agent_id=aid).first()
            return _agent_to_dict(row) if row else None

    def get_agent_by_token_hash(self, token_hash: str) -> Optional[Dict[str, Any]]:
        th = (token_hash or "").strip().lower()
        if not th:
            return None
        with get_session() as s:
            row = s.query(LocalAgentRow).filter_by(token_hash=th).first()
            return _agent_to_dict(row) if row else None

    def list_agents(self, *, project_id: Optional[str], limit: int) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = s.query(LocalAgentRow).order_by(LocalAgentRow.created_at.desc())
            if project_id is not None and str(project_id).strip():
                q = q.filter(LocalAgentRow.project_id == str(project_id).strip())
            rows = q.limit(lim).all()
            return [_agent_to_dict(r) for r in rows]

    def update_agent(self, agent_id: str, **fields: Any) -> bool:
        aid = (agent_id or "").strip()
        if not aid:
            return False
        with get_session() as s:
            row = s.query(LocalAgentRow).filter_by(agent_id=aid).first()
            if not row:
                return False
            if "name" in fields and fields["name"] is not None:
                row.name = str(fields["name"]).strip()
            if "capabilities_json" in fields and fields["capabilities_json"] is not None:
                row.capabilities_json = str(fields["capabilities_json"])
            if "version" in fields and fields["version"] is not None:
                row.version = str(fields["version"]).strip() or None
            if "enabled" in fields and fields["enabled"] is not None:
                row.enabled = 1 if bool(fields["enabled"]) else 0
            if "status" in fields and fields["status"] is not None:
                row.status = str(fields["status"])
            if "last_seen_at" in fields:
                row.last_seen_at = fields["last_seen_at"]
            if "agent_meta_json" in fields and fields["agent_meta_json"] is not None:
                row.agent_meta_json = fields["agent_meta_json"]
            row.updated_at = _utc_iso()
            return True

    def insert_job(
        self,
        *,
        job_id: str,
        project_id: str,
        agent_id: Optional[str],
        job_type: str,
        target_url: str,
        payload_json: Optional[str],
        status: str = "queued",
    ) -> None:
        pj = payload_json or "{}"
        if len(pj.encode("utf-8")) > LOCAL_AGENT_MAX_JOB_PAYLOAD_JSON:
            raise ValueError(f"payload_json exceeds {LOCAL_AGENT_MAX_JOB_PAYLOAD_JSON} bytes")
        now = _utc_iso()
        row = LocalAgentJobRow(
            job_id=job_id,
            project_id=(project_id or "").strip(),
            agent_id=(agent_id or "").strip() or None,
            job_type=(job_type or "browser_inspection").strip(),
            target_url=(target_url or "").strip(),
            payload_json=pj,
            status=status,
            created_at=now,
            claimed_at=None,
            completed_at=None,
            result_ref=None,
            error_message=None,
        )
        with get_session() as s:
            s.add(row)

    def get_job(self, job_id: str) -> Optional[Dict[str, Any]]:
        jid = (job_id or "").strip()
        if not jid:
            return None
        with get_session() as s:
            row = s.query(LocalAgentJobRow).filter_by(job_id=jid).first()
            return _job_to_dict(row) if row else None

    def list_queued_jobs_for_project(self, project_id: str, *, limit: int) -> List[Dict[str, Any]]:
        pid = (project_id or "").strip()
        if not pid:
            return []
        lim = max(1, min(int(limit), 50))
        with get_session() as s:
            rows = (
                s.query(LocalAgentJobRow)
                .filter(
                    and_(
                        LocalAgentJobRow.project_id == pid,
                        LocalAgentJobRow.status == "queued",
                    )
                )
                .order_by(LocalAgentJobRow.created_at.asc())
                .limit(lim)
                .all()
            )
            return [_job_to_dict(r) for r in rows]

    def list_recent_jobs_for_project(self, project_id: str, *, limit: int = 25) -> List[Dict[str, Any]]:
        """Recent jobs for a project (admin / UI), newest first."""
        pid = (project_id or "").strip()
        if not pid:
            return []
        lim = max(1, min(int(limit), 100))
        with get_session() as s:
            rows = (
                s.query(LocalAgentJobRow)
                .filter(LocalAgentJobRow.project_id == pid)
                .order_by(desc(LocalAgentJobRow.created_at))
                .limit(lim)
                .all()
            )
            return [_job_to_dict(r) for r in rows]

    def update_job(self, job_id: str, **fields: Any) -> bool:
        jid = (job_id or "").strip()
        if not jid:
            return False
        with get_session() as s:
            row = s.query(LocalAgentJobRow).filter_by(job_id=jid).first()
            if not row:
                return False
            if "status" in fields and fields["status"] is not None:
                row.status = str(fields["status"])
            if "claimed_at" in fields:
                row.claimed_at = fields["claimed_at"]
            if "completed_at" in fields:
                row.completed_at = fields["completed_at"]
            if "result_ref" in fields:
                row.result_ref = fields["result_ref"]
            if "error_message" in fields:
                row.error_message = fields["error_message"]
            return True


local_agent_repo = LocalAgentRepository()
