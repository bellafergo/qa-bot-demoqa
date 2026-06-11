# services/audit_event_service.py
"""
Enterprise SEC-01E — Centralized audit event recording (read-only trail).

Records activity only — no permission enforcement or compliance workflows.
"""
from __future__ import annotations

import logging
import uuid
from contextlib import contextmanager
from contextvars import ContextVar
from datetime import datetime, timezone
from typing import Any, Dict, Iterator, List, Optional, Tuple

from models.audit_models import AuditEvent, AuditEventSummary, AuditEventsResponse

logger = logging.getLogger("vanya.audit_event")

_audit_actor: ContextVar[Optional[Dict[str, Optional[str]]]] = ContextVar("audit_actor", default=None)


def _utc_now() -> datetime:
    return datetime.now(timezone.utc)


def _parse_timestamp(value: str) -> datetime:
    try:
        return datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    except Exception:
        return _utc_now()


def set_audit_actor(*, user_id: Optional[str] = None, user_email: Optional[str] = None) -> None:
    _audit_actor.set(
        {
            "user_id": (user_id or "").strip() or "anonymous",
            "user_email": (user_email or "").strip() or None,
        }
    )


@contextmanager
def audit_actor_scope(
    *,
    user_id: Optional[str] = None,
    user_email: Optional[str] = None,
) -> Iterator[None]:
    token = _audit_actor.set(
        {
            "user_id": (user_id or "").strip() or "anonymous",
            "user_email": (user_email or "").strip() or None,
        }
    )
    try:
        yield
    finally:
        _audit_actor.reset(token)


def current_audit_actor() -> Tuple[str, Optional[str]]:
    actor = _audit_actor.get()
    if actor:
        return actor.get("user_id") or "anonymous", actor.get("user_email")
    return "system", None


def record_event(
    *,
    event_type: str,
    resource_type: str,
    resource_id: str,
    action: str,
    result: str = "SUCCESS",
    user_id: Optional[str] = None,
    user_email: Optional[str] = None,
    metadata: Optional[Dict[str, Any]] = None,
) -> AuditEvent:
    """Persist a deterministic audit event."""
    from services.db.audit_event_repository import audit_event_repo

    actor_id, actor_email = current_audit_actor()
    resolved_user_id = (user_id or actor_id or "system").strip() or "system"
    resolved_email = (user_email or actor_email or "").strip() or None
    now = _utc_now()
    event_id = str(uuid.uuid4())

    audit_event_repo.insert(
        event_id=event_id,
        timestamp=now,
        user_id=resolved_user_id,
        user_email=resolved_email,
        event_type=event_type,
        resource_type=resource_type,
        resource_id=(resource_id or "").strip() or "unknown",
        action=action,
        result=result,
        metadata=metadata or {},
    )

    resolved_resource_id = (resource_id or "").strip() or "unknown"
    return AuditEvent(
        event_id=event_id,
        timestamp=now,
        user_id=resolved_user_id,
        user_email=resolved_email,
        event_type=event_type,  # type: ignore[arg-type]
        resource_type=resource_type,  # type: ignore[arg-type]
        resource_id=resolved_resource_id,
        action=action,
        result=result,  # type: ignore[arg-type]
        metadata=metadata or {},
    )


def list_events(
    *,
    event_type: Optional[str] = None,
    resource_type: Optional[str] = None,
    user_id: Optional[str] = None,
    limit: int = 100,
) -> AuditEventsResponse:
    from services.db.audit_event_repository import audit_event_repo

    rows = audit_event_repo.list_events(
        event_type=(event_type or "").strip() or None,
        resource_type=(resource_type or "").strip() or None,
        user_id=(user_id or "").strip() or None,
        limit=limit,
    )
    events = [_row_to_event(row) for row in rows]
    return AuditEventsResponse(events=events, total=len(events))


def list_events_by_resource(
    *,
    resource_type: str,
    resource_id: str,
    limit: int = 100,
) -> AuditEventsResponse:
    from services.db.audit_event_repository import audit_event_repo

    rows = audit_event_repo.list_by_resource(
        resource_type=resource_type,
        resource_id=resource_id,
        limit=limit,
    )
    events = [_row_to_event(row) for row in rows]
    return AuditEventsResponse(events=events, total=len(events))


def build_audit_summary() -> AuditEventSummary:
    from services.db.audit_event_repository import audit_event_repo

    latest_row = audit_event_repo.latest_event()
    return AuditEventSummary(
        total_events=audit_event_repo.count_all(),
        event_types=audit_event_repo.event_type_counts(),
        latest_event=_row_to_event(latest_row) if latest_row else None,
    )


def safe_record_event(**kwargs: Any) -> None:
    """Best-effort audit recording that never raises."""
    try:
        record_event(**kwargs)
    except Exception as exc:
        logger.debug("audit_event: record failed: %s", exc)


def _row_to_event(row: Dict[str, Any]) -> AuditEvent:
    return AuditEvent(
        event_id=row["event_id"],
        timestamp=_parse_timestamp(row["timestamp"]),
        user_id=row["user_id"],
        user_email=row.get("user_email"),
        event_type=row["event_type"],  # type: ignore[arg-type]
        resource_type=row["resource_type"],  # type: ignore[arg-type]
        resource_id=row["resource_id"],
        action=row["action"],
        result=row.get("result") or "SUCCESS",  # type: ignore[arg-type]
        metadata=row.get("metadata") or {},
    )
