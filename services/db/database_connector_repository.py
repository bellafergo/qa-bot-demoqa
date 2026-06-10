# services/db/database_connector_repository.py
"""SQLite persistence for INT-03B database connections and validation executions."""
from __future__ import annotations

import json
import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, Integer, String, Text

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.database_connector")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


class DatabaseConnectionRow(Base):
    __tablename__ = "database_connections"

    connection_id = Column(String, primary_key=True)
    agent_id = Column(String, nullable=False, index=True)
    name = Column(String, nullable=False)
    database_type = Column(String, nullable=False)
    host_label = Column(String, nullable=False)
    database_name = Column(String, nullable=False)
    status = Column(String, nullable=False, default="UNKNOWN")
    created_at = Column(String, nullable=False)


class DatabaseValidationExecutionRow(Base):
    __tablename__ = "database_validation_executions"

    execution_id = Column(String, primary_key=True)
    check_id = Column(String, nullable=False, index=True)
    connection_id = Column(String, nullable=False, index=True)
    agent_id = Column(String, nullable=False, index=True)
    executed_at = Column(String, nullable=False)
    status = Column(String, nullable=False)
    row_count = Column(Integer, nullable=False, default=0)
    summary = Column(Text, nullable=False, default="")
    confidence = Column(String, nullable=False, default="0.0")
    requires_user_approval = Column(Integer, nullable=False, default=1)
    check_json = Column(Text, nullable=True)


class ApprovalSimulationRow(Base):
    __tablename__ = "database_validation_approval_simulations"

    check_id = Column(String, primary_key=True)
    approval_id = Column(String, nullable=False)
    status = Column(String, nullable=False, default="PENDING")
    updated_at = Column(String, nullable=False)


def _connection_to_dict(row: DatabaseConnectionRow) -> Dict[str, Any]:
    return {
        "connection_id": row.connection_id,
        "agent_id": row.agent_id,
        "name": row.name,
        "database_type": row.database_type,
        "host_label": row.host_label,
        "database_name": row.database_name,
        "status": row.status,
        "created_at": row.created_at,
    }


def _execution_to_dict(row: DatabaseValidationExecutionRow) -> Dict[str, Any]:
    return {
        "execution_id": row.execution_id,
        "check_id": row.check_id,
        "connection_id": row.connection_id,
        "agent_id": row.agent_id,
        "executed_at": row.executed_at,
        "status": row.status,
        "row_count": int(row.row_count or 0),
        "summary": row.summary or "",
        "confidence": float(row.confidence or 0.0),
        "requires_user_approval": bool(row.requires_user_approval),
        "check_json": row.check_json,
    }


class DatabaseConnectorRepository:
    def insert_connection(self, **fields: Any) -> None:
        row = DatabaseConnectionRow(
            connection_id=fields["connection_id"],
            agent_id=fields["agent_id"],
            name=fields["name"],
            database_type=fields["database_type"],
            host_label=fields["host_label"],
            database_name=fields["database_name"],
            status=fields.get("status") or "UNKNOWN",
            created_at=fields.get("created_at") or _utc_iso(),
        )
        with get_session() as s:
            s.add(row)

    def get_connection(self, connection_id: str) -> Optional[Dict[str, Any]]:
        cid = (connection_id or "").strip()
        if not cid:
            return None
        with get_session() as s:
            row = s.query(DatabaseConnectionRow).filter_by(connection_id=cid).first()
            return _connection_to_dict(row) if row else None

    def list_connections(self, *, agent_id: Optional[str] = None, limit: int = 200) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = s.query(DatabaseConnectionRow).order_by(DatabaseConnectionRow.created_at.desc())
            if agent_id and str(agent_id).strip():
                q = q.filter(DatabaseConnectionRow.agent_id == str(agent_id).strip())
            return [_connection_to_dict(r) for r in q.limit(lim).all()]

    def update_connection_status(self, connection_id: str, status: str) -> bool:
        cid = (connection_id or "").strip()
        if not cid:
            return False
        with get_session() as s:
            row = s.query(DatabaseConnectionRow).filter_by(connection_id=cid).first()
            if not row:
                return False
            row.status = status
            return True

    def insert_execution(self, **fields: Any) -> None:
        row = DatabaseValidationExecutionRow(
            execution_id=fields["execution_id"],
            check_id=fields["check_id"],
            connection_id=fields["connection_id"],
            agent_id=fields["agent_id"],
            executed_at=fields.get("executed_at") or _utc_iso(),
            status=fields["status"],
            row_count=int(fields.get("row_count") or 0),
            summary=str(fields.get("summary") or ""),
            confidence=str(fields.get("confidence") or 0.0),
            requires_user_approval=1 if fields.get("requires_user_approval", True) else 0,
            check_json=fields.get("check_json"),
        )
        with get_session() as s:
            s.add(row)

    def get_execution(self, execution_id: str) -> Optional[Dict[str, Any]]:
        eid = (execution_id or "").strip()
        if not eid:
            return None
        with get_session() as s:
            row = s.query(DatabaseValidationExecutionRow).filter_by(execution_id=eid).first()
            return _execution_to_dict(row) if row else None

    def list_executions(self, *, connection_id: Optional[str] = None, limit: int = 100) -> List[Dict[str, Any]]:
        lim = max(1, min(int(limit), 500))
        with get_session() as s:
            q = s.query(DatabaseValidationExecutionRow).order_by(
                DatabaseValidationExecutionRow.executed_at.desc()
            )
            if connection_id and str(connection_id).strip():
                q = q.filter(DatabaseValidationExecutionRow.connection_id == str(connection_id).strip())
            return [_execution_to_dict(r) for r in q.limit(lim).all()]

    def upsert_approval_simulation(self, *, check_id: str, approval_id: str, status: str) -> None:
        now = _utc_iso()
        with get_session() as s:
            row = s.query(ApprovalSimulationRow).filter_by(check_id=check_id).first()
            if row:
                row.approval_id = approval_id
                row.status = status
                row.updated_at = now
            else:
                s.add(
                    ApprovalSimulationRow(
                        check_id=check_id,
                        approval_id=approval_id,
                        status=status,
                        updated_at=now,
                    )
                )

    def get_approval_simulation(self, check_id: str) -> Optional[Dict[str, Any]]:
        cid = (check_id or "").strip()
        if not cid:
            return None
        with get_session() as s:
            row = s.query(ApprovalSimulationRow).filter_by(check_id=cid).first()
            if not row:
                return None
            return {
                "check_id": row.check_id,
                "approval_id": row.approval_id,
                "status": row.status,
                "updated_at": row.updated_at,
            }


database_connector_repo = DatabaseConnectorRepository()
