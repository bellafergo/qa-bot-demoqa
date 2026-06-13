# services/database_connector_service.py
"""
INT-03B — Secure read-only database connectors (agent-mediated).

All database access is read-only, approval-driven, auditable, and routed through
registered local agents. Vanya cloud never opens direct database connections.
"""
from __future__ import annotations

import hashlib
import json
import re
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from fastapi import HTTPException, Request

from models.database_connector_models import (
    DatabaseConnection,
    DatabaseConnectionRegistrationRequest,
    DatabaseValidationExecuteRequest,
    DatabaseValidationExecuteResponse,
    DatabaseValidationExecution,
    DatabaseValidationExecutionRequest,
    ExecutionStatus,
)
from models.incident_models import ApprovalRequest, ApprovalWorkflowSummary, DatabaseValidationCheck
from services.database_validation_service import validate_query_safety
from services.db.database_connector_repository import database_connector_repo
from services.db.local_agent_repository import local_agent_repo
from services.local_agent_service import hash_agent_token, require_local_agent_admin, resolve_foundation_status

_SUPPORTED_DB_TYPES = frozenset({"postgresql", "mysql", "sqlserver"})


def build_connection_id(agent_id: str, name: str) -> str:
    slug_agent = re.sub(r"[^a-z0-9]+", "_", (agent_id or "").strip().lower()).strip("_")
    slug_name = re.sub(r"[^a-z0-9]+", "_", (name or "").strip().lower()).strip("_")
    return f"dbconn:{slug_agent}:{slug_name}"


def build_execution_id(check_id: str, connection_id: str) -> str:
    digest = hashlib.sha256(f"{check_id}:{connection_id}".encode("utf-8")).hexdigest()[:12]
    return f"dbexec:{digest}"


def build_approval_id_for_check(check_id: str) -> str:
    return f"approval:database_validation:database_validation_check:{check_id}"


def _utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def _connection_status_for_agent(agent_row: Optional[Dict[str, Any]]) -> str:
    if not agent_row:
        return "UNKNOWN"
    foundation = resolve_foundation_status(agent_row)
    if foundation == "ONLINE":
        return "CONNECTED"
    if foundation == "OFFLINE":
        return "DISCONNECTED"
    return "UNKNOWN"


def _row_to_connection(
    row: Dict[str, Any],
    agent_row: Optional[Dict[str, Any]] = None,
    *,
    already_exists: bool = False,
) -> DatabaseConnection:
    status = _connection_status_for_agent(agent_row) if agent_row else str(row.get("status") or "UNKNOWN")
    return DatabaseConnection(
        connection_id=row["connection_id"],
        agent_id=row["agent_id"],
        name=row["name"],
        database_type=row["database_type"],
        host_label=row["host_label"],
        database_name=row["database_name"],
        status=status,  # type: ignore[arg-type]
        created_at=row.get("created_at") or "",
        already_exists=already_exists,
    )


def _row_to_execution(row: Dict[str, Any]) -> DatabaseValidationExecution:
    return DatabaseValidationExecution(
        execution_id=row["execution_id"],
        check_id=row["check_id"],
        connection_id=row["connection_id"],
        executed_at=row.get("executed_at") or "",
        status=row.get("status") or "FAILED",  # type: ignore[arg-type]
        row_count=int(row.get("row_count") or 0),
        summary=str(row.get("summary") or ""),
        confidence=float(row.get("confidence") or 0.0),
        requires_user_approval=bool(row.get("requires_user_approval", True)),
    )


def _sanitize_summary(text: str) -> str:
    blocked = ("password", "username", "connection string", "jdbc:", "postgres://", "mysql://")
    out = str(text or "")
    lower = out.lower()
    for token in blocked:
        if token in lower:
            return "Read-only validation completed. Credential details are never returned."
    return out[:512]


def _summarize_agent_result(row_count: int, database_type: str, check_name: str) -> str:
    return _sanitize_summary(
        f"Read-only validation '{check_name}' completed on {database_type} with {row_count} row(s) observed."
    )


def _confidence_for_result(row_count: int, safe: bool) -> float:
    if not safe:
        return 0.0
    return round(min(0.95, 0.65 + min(0.25, row_count * 0.05)), 2)


def resolve_approval_status(
    check_id: str,
    approval_workflow: Optional[ApprovalWorkflowSummary] = None,
) -> str:
    simulated = database_connector_repo.get_approval_simulation(check_id)
    if simulated:
        return str(simulated.get("status") or "PENDING")
    if approval_workflow:
        approval_id = build_approval_id_for_check(check_id)
        for req in approval_workflow.requests:
            if req.approval_id == approval_id or req.related_entity_id == check_id:
                return req.status
    return "PENDING"


def simulate_approval(check_id: str, status: str) -> ApprovalRequest:
    approval_id = build_approval_id_for_check(check_id)
    database_connector_repo.upsert_approval_simulation(
        check_id=check_id,
        approval_id=approval_id,
        status=status,
    )
    return ApprovalRequest(
        approval_id=approval_id,
        approval_type="database_validation",
        title=f"Database validation check {check_id}",
        description="Simulated approval for INT-03B validation execution.",
        status=status,  # type: ignore[arg-type]
        created_at=_utc_now(),
        related_entity_type="database_validation_check",
        related_entity_id=check_id,
    )


def register_connection(body: DatabaseConnectionRegistrationRequest, request: Request) -> DatabaseConnection:
    require_local_agent_admin(request)
    agent = local_agent_repo.get_agent(body.agent_id)
    if not agent:
        raise HTTPException(status_code=404, detail="agent not found")
    if "database_validation" not in [str(c).lower() for c in (agent.get("capabilities") or [])]:
        raise HTTPException(status_code=400, detail="agent missing database_validation capability")

    db_type = body.database_type.strip().lower()
    if db_type not in _SUPPORTED_DB_TYPES:
        raise HTTPException(status_code=400, detail="unsupported database_type")

    connection_id = build_connection_id(body.agent_id, body.name)
    existing = database_connector_repo.get_connection(connection_id)
    if existing:
        return _row_to_connection(existing, agent, already_exists=True)

    status = _connection_status_for_agent(agent)
    database_connector_repo.insert_connection(
        connection_id=connection_id,
        agent_id=body.agent_id,
        name=body.name.strip(),
        database_type=db_type,
        host_label=body.host_label.strip(),
        database_name=body.database_name.strip(),
        status=status,
    )
    row = database_connector_repo.get_connection(connection_id)
    return _row_to_connection(row or {}, agent, already_exists=False)


def list_connections(*, agent_id: Optional[str], limit: int, request: Request) -> List[DatabaseConnection]:
    require_local_agent_admin(request)
    rows = database_connector_repo.list_connections(agent_id=agent_id, limit=limit)
    out: List[DatabaseConnection] = []
    for row in rows:
        agent = local_agent_repo.get_agent(row["agent_id"])
        out.append(_row_to_connection(row, agent))
    return out


def list_executions(*, connection_id: Optional[str], limit: int, request: Request) -> List[DatabaseValidationExecution]:
    require_local_agent_admin(request)
    return [_row_to_execution(r) for r in database_connector_repo.list_executions(connection_id=connection_id, limit=limit)]


def _blocked_execution(
    *,
    check: DatabaseValidationCheck,
    connection_id: str,
    agent_id: str,
    summary: str,
    status: ExecutionStatus,
) -> DatabaseValidationExecution:
    execution_id = build_execution_id(check.check_id, connection_id)
    row = {
        "execution_id": execution_id,
        "check_id": check.check_id,
        "connection_id": connection_id,
        "agent_id": agent_id,
        "executed_at": _utc_now(),
        "status": status,
        "row_count": 0,
        "summary": _sanitize_summary(summary),
        "confidence": 0.0,
        "requires_user_approval": check.requires_user_approval,
        "check_json": json.dumps(check.model_dump(), separators=(",", ":")),
    }
    database_connector_repo.insert_execution(**row)
    return _row_to_execution(row)


def _agent_has_database_validation_capability(agent_row: Dict[str, Any]) -> bool:
    caps = [str(c).lower() for c in (agent_row.get("capabilities") or [])]
    return "database_validation" in caps


def execute_validation_check(
    *,
    check: DatabaseValidationCheck,
    connection_id: str,
    approval_workflow: Optional[ApprovalWorkflowSummary] = None,
    agent_executor: Optional[Any] = None,
) -> DatabaseValidationExecution:
    """
    Validate approval, connector, capability, and query safety; then execute via local agent.
    Cloud never opens a direct database connection.
    """
    connection = database_connector_repo.get_connection(connection_id)
    if not connection:
        return _blocked_execution(
            check=check,
            connection_id=connection_id,
            agent_id="",
            summary="Database connection not found.",
            status="FAILED",
        )

    agent = local_agent_repo.get_agent(connection["agent_id"])
    if not agent:
        return _blocked_execution(
            check=check,
            connection_id=connection_id,
            agent_id=connection["agent_id"],
            summary="Registered local agent not found.",
            status="FAILED",
        )

    if check.requires_user_approval:
        approval_status = resolve_approval_status(check.check_id, approval_workflow)
        if approval_status != "APPROVED":
            return _blocked_execution(
                check=check,
                connection_id=connection_id,
                agent_id=connection["agent_id"],
                summary="Approval required before database validation execution.",
                status="REJECTED",
            )

    if not _agent_has_database_validation_capability(agent):
        return _blocked_execution(
            check=check,
            connection_id=connection_id,
            agent_id=connection["agent_id"],
            summary="Local agent missing database_validation capability.",
            status="BLOCKED",
        )

    connector_status = _connection_status_for_agent(agent)
    database_connector_repo.update_connection_status(connection_id, connector_status)
    if connector_status != "CONNECTED":
        return _blocked_execution(
            check=check,
            connection_id=connection_id,
            agent_id=connection["agent_id"],
            summary="Database connector is not connected through the local agent.",
            status="BLOCKED",
        )

    safe, reason = validate_query_safety(check.query)
    if not safe:
        return _blocked_execution(
            check=check,
            connection_id=connection_id,
            agent_id=connection["agent_id"],
            summary=f"Unsafe query blocked: {reason}",
            status="BLOCKED",
        )

    if str(check.database_type).lower() != str(connection["database_type"]).lower():
        return _blocked_execution(
            check=check,
            connection_id=connection_id,
            agent_id=connection["agent_id"],
            summary="Validation check database type does not match connector.",
            status="BLOCKED",
        )

    execution_id = build_execution_id(check.check_id, connection_id)
    payload = {
        "execution_id": execution_id,
        "check_id": check.check_id,
        "connection_id": connection_id,
        "query": check.query,
        "database_type": connection["database_type"],
        "host_label": connection["host_label"],
        "database_name": connection["database_name"],
    }
    local_agent_repo.insert_job(
        job_id=execution_id,
        project_id=(agent.get("project_id") or "default").strip() or "default",
        agent_id=connection["agent_id"],
        job_type="database_validation",
        target_url=f"db://{connection['host_label']}/{connection['database_name']}",
        payload_json=json.dumps(payload, separators=(",", ":")),
        status="queued",
    )

    executor = agent_executor or simulate_agent_readonly_execution
    row_count, summary = executor(check.query, connection["database_type"], check.name)
    execution_row = {
        "execution_id": execution_id,
        "check_id": check.check_id,
        "connection_id": connection_id,
        "agent_id": connection["agent_id"],
        "executed_at": _utc_now(),
        "status": "SUCCESS",
        "row_count": row_count,
        "summary": _summarize_agent_result(row_count, connection["database_type"], check.name),
        "confidence": _confidence_for_result(row_count, safe=True),
        "requires_user_approval": check.requires_user_approval,
        "check_json": json.dumps(check.model_dump(), separators=(",", ":")),
    }
    database_connector_repo.insert_execution(**execution_row)
    local_agent_repo.update_job(execution_id, status="succeeded", completed_at=_utc_now())
    return _row_to_execution(execution_row)


def simulate_agent_readonly_execution(query: str, database_type: str, check_name: str) -> Tuple[int, str]:
    """Deterministic agent-side read-only simulation (no cloud DB access)."""
    safe, reason = validate_query_safety(query)
    if not safe:
        return 0, f"Agent blocked query: {reason}"
    digest = hashlib.sha256(f"{query}:{database_type}".encode("utf-8")).hexdigest()
    row_count = 1 + int(digest[:2], 16) % 3
    return row_count, _summarize_agent_result(row_count, database_type, check_name)


def _auth_agent_from_header(authorization: Optional[str]) -> Dict[str, Any]:
    if not authorization or not authorization.lower().startswith("bearer "):
        raise HTTPException(status_code=401, detail="Authorization Bearer token required")
    token = authorization.split(" ", 1)[1].strip()
    th, _ = hash_agent_token(token)
    row = local_agent_repo.get_agent_by_token_hash(th)
    if not row:
        raise HTTPException(status_code=401, detail="Invalid agent token")
    return row


def agent_execute_database_validation(
    body: DatabaseValidationExecuteRequest,
    authorization: Optional[str],
) -> DatabaseValidationExecuteResponse:
    """
    Agent-mediated validation endpoint.
    Agents submit summarized results only — never credentials or connection strings.
    """
    agent = _auth_agent_from_header(authorization)
    connection = database_connector_repo.get_connection(body.connection_id)
    if not connection:
        raise HTTPException(status_code=404, detail="connection not found")
    if connection["agent_id"] != agent["agent_id"]:
        raise HTTPException(status_code=403, detail="connection does not belong to agent")

    if body.row_count is not None and body.summary and body.status:
        execution_id = build_execution_id(body.check_id, body.connection_id)
        status = body.status
        if status not in ("SUCCESS", "FAILED", "BLOCKED", "REJECTED"):
            status = "FAILED"
        database_connector_repo.insert_execution(
            execution_id=execution_id,
            check_id=body.check_id,
            connection_id=body.connection_id,
            agent_id=agent["agent_id"],
            executed_at=_utc_now(),
            status=status,
            row_count=int(body.row_count),
            summary=_sanitize_summary(body.summary),
            confidence=_confidence_for_result(int(body.row_count), status == "SUCCESS"),
            requires_user_approval=True,
        )
        return DatabaseValidationExecuteResponse(
            execution_id=execution_id,
            check_id=body.check_id,
            connection_id=body.connection_id,
            status=status,  # type: ignore[arg-type]
            row_count=int(body.row_count),
            summary=_sanitize_summary(body.summary),
        )

    check: Optional[DatabaseValidationCheck] = None
    job = local_agent_repo.get_job(build_execution_id(body.check_id, body.connection_id))
    if job and isinstance(job.get("payload"), dict):
        payload = job["payload"]
        if payload.get("query"):
            check = DatabaseValidationCheck(
                check_id=body.check_id,
                name=str(payload.get("check_id") or body.check_id),
                query=str(payload["query"]),
                database_type=str(payload.get("database_type") or connection["database_type"]),
                requires_user_approval=True,
            )
    if check is None:
        check = DatabaseValidationCheck(
            check_id=body.check_id,
            name=body.check_id,
            query="SELECT 1",
            database_type=connection["database_type"],
            requires_user_approval=True,
        )

    execution = execute_validation_check(check=check, connection_id=body.connection_id)
    return DatabaseValidationExecuteResponse(
        execution_id=execution.execution_id,
        check_id=execution.check_id,
        connection_id=execution.connection_id,
        status=execution.status,
        row_count=execution.row_count,
        summary=execution.summary,
    )


def trigger_validation_execution(
    body: DatabaseValidationExecutionRequest,
    request: Request,
    approval_workflow: Optional[ApprovalWorkflowSummary] = None,
) -> DatabaseValidationExecution:
    require_local_agent_admin(request)
    return execute_validation_check(
        check=body.check,
        connection_id=body.connection_id,
        approval_workflow=approval_workflow,
    )
