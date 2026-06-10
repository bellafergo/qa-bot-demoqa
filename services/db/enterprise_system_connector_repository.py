# services/db/enterprise_system_connector_repository.py
"""SQLite persistence for INT-03D enterprise system connectors."""
from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, Integer, String

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.enterprise_system_connector")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class EnterpriseSystemConnectorRow(Base):
    __tablename__ = "enterprise_system_connectors"

    connector_id = Column(String, primary_key=True)
    agent_id = Column(String, nullable=False, index=True)
    system_name = Column(String, nullable=False)
    system_type = Column(String, nullable=False)
    environment = Column(String, nullable=False)
    status = Column(String, nullable=False, default="UNKNOWN")
    read_only = Column(Integer, nullable=False, default=1)
    created_at = Column(String, nullable=False)


class EnterpriseSystemModuleRow(Base):
    __tablename__ = "enterprise_system_modules"

    module_id = Column(String, primary_key=True)
    connector_id = Column(String, nullable=False, index=True)
    module_name = Column(String, nullable=False)
    category = Column(String, nullable=False, default="")
    criticality = Column(String, nullable=False, default="MEDIUM")
    created_at = Column(String, nullable=False)


class EnterpriseSystemValidationRow(Base):
    __tablename__ = "enterprise_system_validation_requests"

    request_id = Column(String, primary_key=True)
    connector_id = Column(String, nullable=False, index=True)
    module_id = Column(String, nullable=False, index=True)
    agent_id = Column(String, nullable=False, index=True)
    validation_type = Column(String, nullable=False)
    status = Column(String, nullable=False, default="PENDING")
    requires_user_approval = Column(Integer, nullable=False, default=1)
    created_at = Column(String, nullable=False)


def _connector_to_dict(row: EnterpriseSystemConnectorRow) -> Dict[str, Any]:
    return {
        "connector_id": row.connector_id,
        "agent_id": row.agent_id,
        "system_name": row.system_name,
        "system_type": row.system_type,
        "environment": row.environment,
        "status": row.status,
        "read_only": bool(row.read_only),
        "created_at": row.created_at,
    }


def _module_to_dict(row: EnterpriseSystemModuleRow) -> Dict[str, Any]:
    return {
        "module_id": row.module_id,
        "connector_id": row.connector_id,
        "module_name": row.module_name,
        "category": row.category or "",
        "criticality": row.criticality or "MEDIUM",
        "created_at": row.created_at,
    }


def _validation_to_dict(row: EnterpriseSystemValidationRow) -> Dict[str, Any]:
    return {
        "request_id": row.request_id,
        "connector_id": row.connector_id,
        "module_id": row.module_id,
        "agent_id": row.agent_id,
        "validation_type": row.validation_type,
        "status": row.status,
        "requires_user_approval": bool(row.requires_user_approval),
        "created_at": row.created_at,
    }


class EnterpriseSystemConnectorRepository:
    def insert_connector(self, **fields: Any) -> None:
        row = EnterpriseSystemConnectorRow(
            connector_id=fields["connector_id"],
            agent_id=fields["agent_id"],
            system_name=fields["system_name"],
            system_type=fields["system_type"],
            environment=fields["environment"],
            status=fields.get("status") or "UNKNOWN",
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
            row = s.query(EnterpriseSystemConnectorRow).filter_by(connector_id=cid).first()
            return _connector_to_dict(row) if row else None

    def list_connectors(self, *, agent_id: Optional[str] = None, limit: int = 200) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = s.query(EnterpriseSystemConnectorRow).order_by(EnterpriseSystemConnectorRow.created_at.desc())
            if agent_id and str(agent_id).strip():
                q = q.filter(EnterpriseSystemConnectorRow.agent_id == str(agent_id).strip())
            return [_connector_to_dict(r) for r in q.limit(lim).all()]

    def list_connectors_for_agents(self, agent_ids: List[str], *, limit: int = 200) -> List[Dict[str, Any]]:
        ids = [str(a).strip() for a in agent_ids if str(a).strip()]
        if not ids:
            return []
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = (
                s.query(EnterpriseSystemConnectorRow)
                .filter(EnterpriseSystemConnectorRow.agent_id.in_(ids))
                .order_by(EnterpriseSystemConnectorRow.created_at.desc())
            )
            return [_connector_to_dict(r) for r in q.limit(lim).all()]

    def insert_module(self, **fields: Any) -> None:
        row = EnterpriseSystemModuleRow(
            module_id=fields["module_id"],
            connector_id=fields["connector_id"],
            module_name=fields["module_name"],
            category=fields.get("category") or "",
            criticality=fields.get("criticality") or "MEDIUM",
            created_at=fields.get("created_at") or _utc_iso(),
        )
        with get_session() as s:
            s.add(row)

    def get_module(self, module_id: str) -> Optional[Dict[str, Any]]:
        mid = (module_id or "").strip()
        if not mid:
            return None
        with get_session() as s:
            row = s.query(EnterpriseSystemModuleRow).filter_by(module_id=mid).first()
            return _module_to_dict(row) if row else None

    def list_modules(self, *, connector_id: Optional[str] = None, limit: int = 500) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 1000))
        with get_session() as s:
            q = s.query(EnterpriseSystemModuleRow).order_by(EnterpriseSystemModuleRow.module_name.asc())
            if connector_id and str(connector_id).strip():
                q = q.filter(EnterpriseSystemModuleRow.connector_id == str(connector_id).strip())
            return [_module_to_dict(r) for r in q.limit(lim).all()]

    def insert_validation(self, **fields: Any) -> None:
        row = EnterpriseSystemValidationRow(
            request_id=fields["request_id"],
            connector_id=fields["connector_id"],
            module_id=fields["module_id"],
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
            q = s.query(EnterpriseSystemValidationRow).order_by(
                EnterpriseSystemValidationRow.created_at.desc()
            )
            if connector_id and str(connector_id).strip():
                q = q.filter(EnterpriseSystemValidationRow.connector_id == str(connector_id).strip())
            return [_validation_to_dict(r) for r in q.limit(lim).all()]


enterprise_system_connector_repo = EnterpriseSystemConnectorRepository()
