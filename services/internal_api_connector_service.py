# services/internal_api_connector_service.py
"""
INT-03C — Internal API connectors (agent-mediated, read-only foundation).

Metadata registration and validation request preparation only.
No automatic discovery, enterprise API calls, or mutating HTTP methods.
"""
from __future__ import annotations

import hashlib
import re
from typing import Any, Dict, List, Optional

from fastapi import HTTPException, Request

from models.internal_api_connector_models import (
    ApiConnectorReport,
    ApiEndpoint,
    ApiValidationCreateRequest,
    ApiValidationRequest,
    InternalApiConnector,
    InternalApiConnectorRegistrationRequest,
)
from services.db.internal_api_connector_repository import internal_api_connector_repo
from services.db.local_agent_repository import local_agent_repo
from services.local_agent_service import require_local_agent_admin, resolve_foundation_status

_SUPPORTED_API_TYPES = frozenset({"REST", "SOAP", "GRAPHQL", "RPC", "OTHER"})
_READ_ONLY_METHODS = frozenset({"GET"})
_MUTATING_METHODS = frozenset({"POST", "PUT", "PATCH", "DELETE"})


def build_connector_id(agent_id: str, name: str) -> str:
    slug_agent = re.sub(r"[^a-z0-9]+", "_", (agent_id or "").strip().lower()).strip("_")
    slug_name = re.sub(r"[^a-z0-9]+", "_", (name or "").strip().lower()).strip("_")
    return f"apiconn:{slug_agent}:{slug_name}"


def build_endpoint_id(connector_id: str, method: str, path_label: str) -> str:
    slug_path = re.sub(r"[^a-z0-9]+", "_", (path_label or "").strip().lower()).strip("_")[:48]
    method_slug = re.sub(r"[^a-z0-9]+", "_", (method or "").strip().lower()).strip("_")
    digest = hashlib.sha256(f"{connector_id}:{method_slug}:{slug_path}".encode("utf-8")).hexdigest()[:10]
    return f"apiendpoint:{digest}"


def build_validation_request_id(connector_id: str, endpoint_id: str, validation_type: str) -> str:
    digest = hashlib.sha256(f"{connector_id}:{endpoint_id}:{validation_type}".encode("utf-8")).hexdigest()[:12]
    return f"apivalid:{digest}"


def _connector_status_for_agent(agent_row: Optional[Dict[str, Any]], requested: str) -> str:
    if str(requested or "").upper() == "INACTIVE":
        return "INACTIVE"
    if not agent_row:
        return "UNKNOWN"
    foundation = resolve_foundation_status(agent_row)
    if foundation == "ONLINE":
        return "ACTIVE"
    if foundation == "OFFLINE":
        return "INACTIVE"
    return "UNKNOWN"


def _endpoint_read_only(method: str) -> bool:
    return str(method or "").upper() in _READ_ONLY_METHODS


def _agent_has_api_capability(agent_row: Dict[str, Any]) -> bool:
    caps = {str(c).lower() for c in (agent_row.get("capabilities") or [])}
    return bool(caps.intersection({"contract_validation", "intranet_access", "network_inventory"}))


def _row_to_connector(row: Dict[str, Any], agent_row: Optional[Dict[str, Any]] = None) -> InternalApiConnector:
    status = _connector_status_for_agent(agent_row, str(row.get("status") or "UNKNOWN"))
    endpoint_count = internal_api_connector_repo.count_endpoints(row["connector_id"])
    return InternalApiConnector(
        connector_id=row["connector_id"],
        agent_id=row["agent_id"],
        name=row["name"],
        api_type=row["api_type"],  # type: ignore[arg-type]
        environment=row["environment"],
        status=status,  # type: ignore[arg-type]
        base_url_label=row["base_url_label"],
        endpoint_count=endpoint_count,
        read_only=bool(row.get("read_only", True)),
    )


def _row_to_endpoint(row: Dict[str, Any]) -> ApiEndpoint:
    return ApiEndpoint(
        endpoint_id=row["endpoint_id"],
        connector_id=row["connector_id"],
        method=row["method"],  # type: ignore[arg-type]
        path_label=row["path_label"],
        category=row.get("category") or "",
        read_only=bool(row.get("read_only", _endpoint_read_only(row["method"]))),
    )


def _row_to_validation(row: Dict[str, Any]) -> ApiValidationRequest:
    return ApiValidationRequest(
        request_id=row["request_id"],
        connector_id=row["connector_id"],
        endpoint_id=row["endpoint_id"],
        validation_type=row["validation_type"],  # type: ignore[arg-type]
        status=row.get("status") or "PENDING",  # type: ignore[arg-type]
        requires_user_approval=bool(row.get("requires_user_approval", True)),
    )


def register_internal_api_connector(
    body: InternalApiConnectorRegistrationRequest,
    request: Request,
) -> InternalApiConnector:
    require_local_agent_admin(request)
    agent = local_agent_repo.get_agent(body.agent_id)
    if not agent:
        raise HTTPException(status_code=404, detail="agent not found")
    if not _agent_has_api_capability(agent):
        raise HTTPException(status_code=400, detail="agent missing internal API capability")

    api_type = str(body.api_type or "REST").upper()
    if api_type not in _SUPPORTED_API_TYPES:
        raise HTTPException(status_code=400, detail="unsupported api_type")
    if not body.endpoints:
        raise HTTPException(status_code=400, detail="endpoint inventory is required")

    connector_id = build_connector_id(body.agent_id, body.name)
    if internal_api_connector_repo.get_connector(connector_id):
        raise HTTPException(status_code=409, detail="internal API connector already registered")

    status = _connector_status_for_agent(agent, body.status)
    internal_api_connector_repo.insert_connector(
        connector_id=connector_id,
        agent_id=body.agent_id,
        name=body.name.strip(),
        api_type=api_type,
        environment=body.environment.strip(),
        status=status,
        base_url_label=body.base_url_label.strip(),
        read_only=True,
    )

    for endpoint in body.endpoints:
        method = str(endpoint.method or "").upper()
        if method not in _READ_ONLY_METHODS | _MUTATING_METHODS:
            raise HTTPException(status_code=400, detail=f"unsupported HTTP method: {method}")
        endpoint_id = build_endpoint_id(connector_id, method, endpoint.path_label)
        internal_api_connector_repo.insert_endpoint(
            endpoint_id=endpoint_id,
            connector_id=connector_id,
            method=method,
            path_label=endpoint.path_label.strip(),
            category=(endpoint.category or "").strip(),
            read_only=_endpoint_read_only(method),
        )

    row = internal_api_connector_repo.get_connector(connector_id)
    return _row_to_connector(row or {}, agent)


def list_internal_api_connectors(
    *,
    agent_id: Optional[str],
    limit: int,
    request: Request,
) -> List[InternalApiConnector]:
    require_local_agent_admin(request)
    rows = internal_api_connector_repo.list_connectors(agent_id=agent_id, limit=limit)
    out: List[InternalApiConnector] = []
    for row in rows:
        agent = local_agent_repo.get_agent(row["agent_id"])
        out.append(_row_to_connector(row, agent))
    return out


def create_validation_request(body: ApiValidationCreateRequest, request: Request) -> ApiValidationRequest:
    require_local_agent_admin(request)
    connector = internal_api_connector_repo.get_connector(body.connector_id)
    if not connector:
        raise HTTPException(status_code=404, detail="connector not found")

    endpoint = internal_api_connector_repo.get_endpoint(body.endpoint_id)
    if not endpoint or endpoint["connector_id"] != connector["connector_id"]:
        raise HTTPException(status_code=404, detail="endpoint not found")

    method = str(endpoint.get("method") or "").upper()
    request_id = build_validation_request_id(
        connector["connector_id"],
        endpoint["endpoint_id"],
        body.validation_type,
    )

    if method in _MUTATING_METHODS:
        row = {
            "request_id": request_id,
            "connector_id": connector["connector_id"],
            "endpoint_id": endpoint["endpoint_id"],
            "agent_id": connector["agent_id"],
            "validation_type": body.validation_type,
            "status": "BLOCKED",
            "requires_user_approval": True,
        }
        internal_api_connector_repo.insert_validation(**row)
        return _row_to_validation(row)

    row = {
        "request_id": request_id,
        "connector_id": connector["connector_id"],
        "endpoint_id": endpoint["endpoint_id"],
        "agent_id": connector["agent_id"],
        "validation_type": body.validation_type,
        "status": "PENDING",
        "requires_user_approval": True,
    }
    internal_api_connector_repo.insert_validation(**row)
    return _row_to_validation(row)


def list_validation_requests(
    *,
    connector_id: Optional[str],
    limit: int,
    request: Request,
) -> List[ApiValidationRequest]:
    require_local_agent_admin(request)
    rows = internal_api_connector_repo.list_validations(connector_id=connector_id, limit=limit)
    return [_row_to_validation(row) for row in rows]


def build_api_connector_report(*, project_id: Optional[str] = None, limit: int = 200) -> ApiConnectorReport:
    pid = (project_id or "").strip() or None
    if pid:
        agent_rows = local_agent_repo.list_agents(project_id=pid, limit=limit)
        agent_ids = [str(row.get("agent_id") or "").strip() for row in agent_rows]
        agent_ids = [aid for aid in agent_ids if aid]
        connector_rows = internal_api_connector_repo.list_connectors_for_agents(agent_ids, limit=limit)
    else:
        connector_rows = internal_api_connector_repo.list_connectors(limit=limit)

    connectors: List[InternalApiConnector] = []
    connector_ids: List[str] = []
    for row in connector_rows:
        agent = local_agent_repo.get_agent(row["agent_id"])
        connector = _row_to_connector(row, agent)
        connectors.append(connector)
        connector_ids.append(connector.connector_id)

    endpoints: List[ApiEndpoint] = []
    for cid in connector_ids:
        for row in internal_api_connector_repo.list_endpoints(connector_id=cid, limit=500):
            endpoints.append(_row_to_endpoint(row))

    validations: List[ApiValidationRequest] = []
    if connector_ids:
        connector_id_set = set(connector_ids)
        for row in internal_api_connector_repo.list_validations(limit=limit):
            if row["connector_id"] in connector_id_set:
                validations.append(_row_to_validation(row))

    if not connectors:
        summary = "No internal API connectors registered."
    else:
        blocked = sum(1 for v in validations if v.status == "BLOCKED")
        summary = (
            f"{len(connectors)} internal API connector(s), {len(endpoints)} endpoint(s), "
            f"{len(validations)} validation request(s); {blocked} blocked mutating request(s)."
        )

    return ApiConnectorReport(
        connectors=connectors,
        endpoints=endpoints,
        validations=validations,
        summary=summary,
    )
