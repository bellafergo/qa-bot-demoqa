# services/enterprise_system_connector_service.py
"""
INT-03D — Enterprise systems connectors (agent-mediated, read-only foundation).

Metadata registration and validation preparation only.
No ERP execution, auto-discovery, SAP RFC, or Oracle transactions.
"""
from __future__ import annotations

import hashlib
import re
from typing import Any, Dict, List, Optional

from fastapi import HTTPException, Request

from models.enterprise_system_connector_models import (
    EnterpriseSystemConnector,
    EnterpriseSystemModule,
    EnterpriseSystemRegistrationRequest,
    EnterpriseSystemReport,
    EnterpriseSystemValidationCreateRequest,
    EnterpriseValidationRequest,
)
from services.db.enterprise_system_connector_repository import enterprise_system_connector_repo
from services.db.local_agent_repository import local_agent_repo
from services.local_agent_service import require_local_agent_admin, resolve_foundation_status

_SUPPORTED_SYSTEM_TYPES = frozenset(
    {
        "SAP_ECC",
        "SAP_S4",
        "ORACLE_EBS",
        "ORACLE_FUSION",
        "NETSUITE",
        "DYNAMICS",
        "MAINFRAME",
        "LEGACY",
        "OTHER",
    }
)


def build_connector_id(agent_id: str, system_name: str) -> str:
    slug_agent = re.sub(r"[^a-z0-9]+", "_", (agent_id or "").strip().lower()).strip("_")
    slug_name = re.sub(r"[^a-z0-9]+", "_", (system_name or "").strip().lower()).strip("_")
    return f"erpconn:{slug_agent}:{slug_name}"


def build_module_id(connector_id: str, module_name: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "_", (module_name or "").strip().lower()).strip("_")[:48]
    digest = hashlib.sha256(f"{connector_id}:{slug}".encode("utf-8")).hexdigest()[:10]
    return f"erpmodule:{digest}"


def build_validation_request_id(connector_id: str, module_id: str, validation_type: str) -> str:
    digest = hashlib.sha256(f"{connector_id}:{module_id}:{validation_type}".encode("utf-8")).hexdigest()[:12]
    return f"erpvalid:{digest}"


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


def _agent_has_enterprise_capability(agent_row: Dict[str, Any]) -> bool:
    caps = {str(c).lower() for c in (agent_row.get("capabilities") or [])}
    return bool(
        caps.intersection(
            {"database_validation", "contract_validation", "intranet_access", "network_inventory"}
        )
    )


def _row_to_connector(row: Dict[str, Any], agent_row: Optional[Dict[str, Any]] = None) -> EnterpriseSystemConnector:
    status = _connector_status_for_agent(agent_row, str(row.get("status") or "UNKNOWN"))
    return EnterpriseSystemConnector(
        connector_id=row["connector_id"],
        agent_id=row["agent_id"],
        system_name=row["system_name"],
        system_type=row["system_type"],  # type: ignore[arg-type]
        environment=row["environment"],
        status=status,  # type: ignore[arg-type]
        read_only=bool(row.get("read_only", True)),
    )


def _row_to_module(row: Dict[str, Any]) -> EnterpriseSystemModule:
    return EnterpriseSystemModule(
        module_id=row["module_id"],
        connector_id=row["connector_id"],
        module_name=row["module_name"],
        category=row.get("category") or "",
        criticality=row.get("criticality") or "MEDIUM",
    )


def _row_to_validation(row: Dict[str, Any]) -> EnterpriseValidationRequest:
    return EnterpriseValidationRequest(
        request_id=row["request_id"],
        connector_id=row["connector_id"],
        module_id=row["module_id"],
        validation_type=row["validation_type"],  # type: ignore[arg-type]
        status=row.get("status") or "PENDING",  # type: ignore[arg-type]
        requires_user_approval=bool(row.get("requires_user_approval", True)),
    )


def register_enterprise_system(
    body: EnterpriseSystemRegistrationRequest,
    request: Request,
) -> EnterpriseSystemConnector:
    require_local_agent_admin(request)
    agent = local_agent_repo.get_agent(body.agent_id)
    if not agent:
        raise HTTPException(status_code=404, detail="agent not found")
    if not _agent_has_enterprise_capability(agent):
        raise HTTPException(status_code=400, detail="agent missing enterprise systems capability")

    system_type = str(body.system_type or "OTHER").upper()
    if system_type not in _SUPPORTED_SYSTEM_TYPES:
        raise HTTPException(status_code=400, detail="unsupported system_type")
    if not body.modules:
        raise HTTPException(status_code=400, detail="module inventory is required")

    connector_id = build_connector_id(body.agent_id, body.system_name)
    if enterprise_system_connector_repo.get_connector(connector_id):
        raise HTTPException(status_code=409, detail="enterprise system already registered")

    status = _connector_status_for_agent(agent, body.status)
    enterprise_system_connector_repo.insert_connector(
        connector_id=connector_id,
        agent_id=body.agent_id,
        system_name=body.system_name.strip(),
        system_type=system_type,
        environment=body.environment.strip(),
        status=status,
        read_only=True,
    )

    for module in body.modules:
        module_id = build_module_id(connector_id, module.module_name)
        enterprise_system_connector_repo.insert_module(
            module_id=module_id,
            connector_id=connector_id,
            module_name=module.module_name.strip(),
            category=(module.category or "").strip(),
            criticality=(module.criticality or "MEDIUM").strip().upper(),
        )

    row = enterprise_system_connector_repo.get_connector(connector_id)
    return _row_to_connector(row or {}, agent)


def list_enterprise_systems(
    *,
    agent_id: Optional[str],
    limit: int,
    request: Request,
) -> List[EnterpriseSystemConnector]:
    require_local_agent_admin(request)
    rows = enterprise_system_connector_repo.list_connectors(agent_id=agent_id, limit=limit)
    out: List[EnterpriseSystemConnector] = []
    for row in rows:
        agent = local_agent_repo.get_agent(row["agent_id"])
        out.append(_row_to_connector(row, agent))
    return out


def create_enterprise_validation(
    body: EnterpriseSystemValidationCreateRequest,
    request: Request,
) -> EnterpriseValidationRequest:
    require_local_agent_admin(request)
    connector = enterprise_system_connector_repo.get_connector(body.connector_id)
    if not connector:
        raise HTTPException(status_code=404, detail="connector not found")

    module = enterprise_system_connector_repo.get_module(body.module_id)
    if not module or module["connector_id"] != connector["connector_id"]:
        raise HTTPException(status_code=404, detail="module not found")

    request_id = build_validation_request_id(
        connector["connector_id"],
        module["module_id"],
        body.validation_type,
    )
    row = {
        "request_id": request_id,
        "connector_id": connector["connector_id"],
        "module_id": module["module_id"],
        "agent_id": connector["agent_id"],
        "validation_type": body.validation_type,
        "status": "PENDING",
        "requires_user_approval": True,
    }
    enterprise_system_connector_repo.insert_validation(**row)
    return _row_to_validation(row)


def list_enterprise_validations(
    *,
    connector_id: Optional[str],
    limit: int,
    request: Request,
) -> List[EnterpriseValidationRequest]:
    require_local_agent_admin(request)
    rows = enterprise_system_connector_repo.list_validations(connector_id=connector_id, limit=limit)
    return [_row_to_validation(row) for row in rows]


def build_enterprise_system_report(*, project_id: Optional[str] = None, limit: int = 200) -> EnterpriseSystemReport:
    pid = (project_id or "").strip() or None
    if pid:
        agent_rows = local_agent_repo.list_agents(project_id=pid, limit=limit)
        agent_ids = [str(row.get("agent_id") or "").strip() for row in agent_rows]
        agent_ids = [aid for aid in agent_ids if aid]
        connector_rows = enterprise_system_connector_repo.list_connectors_for_agents(agent_ids, limit=limit)
    else:
        connector_rows = enterprise_system_connector_repo.list_connectors(limit=limit)

    connectors: List[EnterpriseSystemConnector] = []
    connector_ids: List[str] = []
    for row in connector_rows:
        agent = local_agent_repo.get_agent(row["agent_id"])
        connector = _row_to_connector(row, agent)
        connectors.append(connector)
        connector_ids.append(connector.connector_id)

    modules: List[EnterpriseSystemModule] = []
    for cid in connector_ids:
        for row in enterprise_system_connector_repo.list_modules(connector_id=cid, limit=500):
            modules.append(_row_to_module(row))

    validations: List[EnterpriseValidationRequest] = []
    if connector_ids:
        connector_id_set = set(connector_ids)
        for row in enterprise_system_connector_repo.list_validations(limit=limit):
            if row["connector_id"] in connector_id_set:
                validations.append(_row_to_validation(row))

    if not connectors:
        summary = "No enterprise systems registered."
    else:
        summary = (
            f"{len(connectors)} enterprise system(s), {len(modules)} module(s), "
            f"{len(validations)} validation request(s) — read-only inventory only."
        )

    return EnterpriseSystemReport(
        connectors=connectors,
        modules=modules,
        validations=validations,
        summary=summary,
    )
