# services/db/audit_event_repository.py
"""SQLite persistence for centralized audit events (SEC-01E)."""
from __future__ import annotations

import json
import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from sqlalchemy import Column, String, Text, desc

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.audit_event")


def _utc_now() -> datetime:
    return datetime.now(timezone.utc)


class AuditEventRow(Base):
    __tablename__ = "audit_events"

    event_id = Column(String, primary_key=True)
    timestamp = Column(String, nullable=False, index=True)
    user_id = Column(String, nullable=False, index=True)
    user_email = Column(String, nullable=True)
    event_type = Column(String, nullable=False, index=True)
    resource_type = Column(String, nullable=False, index=True)
    resource_id = Column(String, nullable=False, index=True)
    action = Column(String, nullable=False)
    result = Column(String, nullable=False, default="SUCCESS")
    metadata_json = Column(Text, nullable=False, default="{}")


class AuditEventRepository:
    def insert(
        self,
        *,
        event_id: str,
        timestamp: datetime,
        user_id: str,
        user_email: Optional[str],
        event_type: str,
        resource_type: str,
        resource_id: str,
        action: str,
        result: str,
        metadata: Dict[str, Any],
    ) -> None:
        row = AuditEventRow(
            event_id=event_id,
            timestamp=timestamp.isoformat(),
            user_id=(user_id or "system").strip() or "system",
            user_email=(user_email or "").strip() or None,
            event_type=event_type,
            resource_type=resource_type,
            resource_id=(resource_id or "").strip() or "unknown",
            action=action,
            result=result,
            metadata_json=json.dumps(metadata or {}, ensure_ascii=False),
        )
        with get_session() as session:
            session.add(row)
            session.commit()

    def list_events(
        self,
        *,
        event_type: Optional[str] = None,
        resource_type: Optional[str] = None,
        user_id: Optional[str] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        with get_session() as session:
            query = session.query(AuditEventRow)
            if event_type:
                query = query.filter(AuditEventRow.event_type == event_type)
            if resource_type:
                query = query.filter(AuditEventRow.resource_type == resource_type)
            if user_id:
                query = query.filter(AuditEventRow.user_id == user_id)
            rows = (
                query.order_by(desc(AuditEventRow.timestamp))
                .limit(max(1, min(limit, 500)))
                .all()
            )
            return [self._row_to_dict(row) for row in rows]

    def list_by_resource(
        self,
        *,
        resource_type: str,
        resource_id: str,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        with get_session() as session:
            rows = (
                session.query(AuditEventRow)
                .filter(
                    AuditEventRow.resource_type == resource_type,
                    AuditEventRow.resource_id == resource_id,
                )
                .order_by(desc(AuditEventRow.timestamp))
                .limit(max(1, min(limit, 500)))
                .all()
            )
            return [self._row_to_dict(row) for row in rows]

    def count_all(self) -> int:
        with get_session() as session:
            return session.query(AuditEventRow).count()

    def event_type_counts(self) -> Dict[str, int]:
        with get_session() as session:
            rows = session.query(AuditEventRow.event_type).all()
        counts: Dict[str, int] = {}
        for (event_type,) in rows:
            counts[event_type] = counts.get(event_type, 0) + 1
        return counts

    def latest_event(self) -> Optional[Dict[str, Any]]:
        with get_session() as session:
            row = (
                session.query(AuditEventRow)
                .order_by(desc(AuditEventRow.timestamp))
                .first()
            )
            return self._row_to_dict(row) if row else None

    @staticmethod
    def _row_to_dict(row: AuditEventRow) -> Dict[str, Any]:
        metadata: Dict[str, Any] = {}
        try:
            metadata = json.loads(row.metadata_json or "{}")
        except Exception:
            metadata = {}
        return {
            "event_id": row.event_id,
            "timestamp": row.timestamp,
            "user_id": row.user_id,
            "user_email": row.user_email,
            "event_type": row.event_type,
            "resource_type": row.resource_type,
            "resource_id": row.resource_id,
            "action": row.action,
            "result": row.result,
            "metadata": metadata,
        }


audit_event_repo = AuditEventRepository()
