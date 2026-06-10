# services/db/internal_api_connector_repository.py
"""SQLite persistence for INT-03C internal API connectors."""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, Integer, String, Text

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.internal_api_connector")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class InternalApiConnectorRow(Base):
    __tablename__ = "internal_api_connectors"

    connector_id = Column(String, primary_key=True)
    agent_id = Column(String, nullable=False, index=True)
    name = Column(String, nullable=False)
    api_type = Column(String, nullable=False)
    environment = Column(String, nullable=False)
    status = Column(String, nullable=False, default="UNKNOWN")
    base_url_label = Column(String, nullable=False)
    read_only = Column(Integer, nullable=False, default=1)
    created_at = Column(String, nullable=False)


class InternalApiEndpointRow(Base):
    __tablename__ = "internal_api_endpoints"

    endpoint_id = Column(String, primary_key=True)
    connector_id = Column(String, nullable=False, index=True)
    method = Column(String, nullable=False)
    path_label = Column(String, nullable=False)
    category = Column(String, nullable=False, default="")
    read_only = Column(Integer, nullable=False, default=1)
    created_at = Column(String, nullable=False)


class InternalApiValidationRow(Base):
    __tablename__ = "internal_api_validation_requests"

    request_id = Column(String, primary_key=True)
    connector_id = Column(String, nullable=False, index=True)
    endpoint_id = Column(String, nullable=False, index=True)
    agent_id = Column(String, nullable=False, index=True)
    validation_type = Column(String, nullable=False)
    status = Column(String, nullable=False, default="PENDING")
    requires_user_approval = Column(Integer, nullable=False, default=1)
    created_at = Column(String, nullable=False)


def _connector_to_dict(row: InternalApiConnectorRow) -> Dict[str, Any]:
    return {
        "connector_id": row.connector_id,
        "agent_id": row.agent_id,
        "name": row.name,
        "api_type": row.api_type,
        "environment": row.environment,
        "status": row.status,
        "base_url_label": row.base_url_label,
        "read_only": bool(row.read_only),
        "created_at": row.created_at,
    }


def _endpoint_to_dict(row: InternalApiEndpointRow) -> Dict[str, Any]:
    return {
        "endpoint_id": row.endpoint_id,
        "connector_id": row.connector_id,
        "method": row.method,
        "path_label": row.path_label,
        "category": row.category or "",
        "read_only": bool(row.read_only),
        "created_at": row.created_at,
    }


def _validation_to_dict(row: InternalApiValidationRow) -> Dict[str, Any]:
    return {
        "request_id": row.request_id,
        "connector_id": row.connector_id,
        "endpoint_id": row.endpoint_id,
        "agent_id": row.agent_id,
        "validation_type": row.validation_type,
        "status": row.status,
        "requires_user_approval": bool(row.requires_user_approval),
        "created_at": row.created_at,
    }


class InternalApiConnectorRepository:
    def insert_connector(self, **fields: Any) -> None:
        row = InternalApiConnectorRow(
            connector_id=fields["connector_id"],
            agent_id=fields["agent_id"],
            name=fields["name"],
            api_type=fields["api_type"],
            environment=fields["environment"],
            status=fields.get("status") or "UNKNOWN",
            base_url_label=fields["base_url_label"],
            read_only=1 if fields.get("read_only", True) else 0,
            created_at=fields.get("created_at") or _utc_iso(),
        )
        with get_session() as s:
            s.add(row)

    def get_connector(self, connector_id: str) -> Optional[Dict[str, Any]]:
        cid = (connector_id or "").strip()
        if not cid:
            return None
        with get_session() as s:
            row = s.query(InternalApiConnectorRow).filter_by(connector_id=cid).first()
            return _connector_to_dict(row) if row else None

    def list_connectors(self, *, agent_id: Optional[str] = None, limit: int = 200) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = s.query(InternalApiConnectorRow).order_by(InternalApiConnectorRow.created_at.desc())
            if agent_id and str(agent_id).strip():
                q = q.filter(InternalApiConnectorRow.agent_id == str(agent_id).strip())
            return [_connector_to_dict(r) for r in q.limit(lim).all()]

    def list_connectors_for_agents(self, agent_ids: List[str], *, limit: int = 200) -> List[Dict[str, Any]]:
        ids = [str(a).strip() for a in agent_ids if str(a).strip()]
        if not ids:
            return []
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = (
                s.query(InternalApiConnectorRow)
                .filter(InternalApiConnectorRow.agent_id.in_(ids))
                .order_by(InternalApiConnectorRow.created_at.desc())
            )
            return [_connector_to_dict(r) for r in q.limit(lim).all()]

    def insert_endpoint(self, **fields: Any) -> None:
        row = InternalApiEndpointRow(
            endpoint_id=fields["endpoint_id"],
            connector_id=fields["connector_id"],
            method=fields["method"],
            path_label=fields["path_label"],
            category=fields.get("category") or "",
            read_only=1 if fields.get("read_only", True) else 0,
            created_at=fields.get("created_at") or _utc_iso(),
        )
        with get_session() as s:
            s.add(row)

    def get_endpoint(self, endpoint_id: str) -> Optional[Dict[str, Any]]:
        eid = (endpoint_id or "").strip()
        if not eid:
            return None
        with get_session() as s:
            row = s.query(InternalApiEndpointRow).filter_by(endpoint_id=eid).first()
            return _endpoint_to_dict(row) if row else None

    def list_endpoints(self, *, connector_id: Optional[str] = None, limit: int = 500) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 1000))
        with get_session() as s:
            q = s.query(InternalApiEndpointRow).order_by(InternalApiEndpointRow.created_at.asc())
            if connector_id and str(connector_id).strip():
                q = q.filter(InternalApiEndpointRow.connector_id == str(connector_id).strip())
            return [_endpoint_to_dict(r) for r in q.limit(lim).all()]

    def count_endpoints(self, connector_id: str) -> int:
        cid = (connector_id or "").strip()
        if not cid:
            return 0
        with get_session() as s:
            return (
                s.query(InternalApiEndpointRow)
                .filter(InternalApiEndpointRow.connector_id == cid)
                .count()
            )

    def insert_validation(self, **fields: Any) -> None:
        row = InternalApiValidationRow(
            request_id=fields["request_id"],
            connector_id=fields["connector_id"],
            endpoint_id=fields["endpoint_id"],
            agent_id=fields["agent_id"],
            validation_type=fields["validation_type"],
            status=fields.get("status") or "PENDING",
            requires_user_approval=1 if fields.get("requires_user_approval", True) else 0,
            created_at=fields.get("created_at") or _utc_iso(),
        )
        with get_session() as s:
            s.add(row)

    def list_validations(
        self,
        *,
        connector_id: Optional[str] = None,
        limit: int = 200,
    ) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = s.query(InternalApiValidationRow).order_by(InternalApiValidationRow.created_at.desc())
            if connector_id and str(connector_id).strip():
                q = q.filter(InternalApiValidationRow.connector_id == str(connector_id).strip())
            return [_validation_to_dict(r) for r in q.limit(lim).all()]


internal_api_connector_repo = InternalApiConnectorRepository()
