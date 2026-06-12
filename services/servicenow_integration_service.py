# services/servicenow_integration_service.py
"""
Read-only ServiceNow integration service (SNOW-01A).

Discovery only — no incident/change creation, updates, approvals, or CMDB writes.
"""
from __future__ import annotations

import logging
import os
from datetime import datetime, timezone
from typing import List, Optional

from models.servicenow_models import (
    ServiceNowCMDBResponse,
    ServiceNowChange,
    ServiceNowChangesResponse,
    ServiceNowCI,
    ServiceNowConnectionStatus,
    ServiceNowIncident,
    ServiceNowIncidentsResponse,
    ServiceNowService,
    ServiceNowServicesResponse,
)
from services.integration_service import integration_service
from services.servicenow_repository_service import (
    ServiceNowAPIError,
    ServiceNowHttpConfig,
    count_table,
    list_table,
    validate_connection,
)

logger = logging.getLogger("vanya.servicenow_integration")

_LAST_SYNC: Optional[datetime] = None

_INCIDENT_FIELDS = "number,short_description,state,priority,assignment_group,opened_at"
_CHANGE_FIELDS = "number,short_description,state,risk,planned_start_date,planned_end_date"
_SERVICE_FIELDS = "name,business_criticality,operational_status"
_CMDB_FIELDS = "name,sys_class_name,operational_status"


def _touch_sync() -> datetime:
    global _LAST_SYNC
    now = datetime.now(timezone.utc)
    _LAST_SYNC = now
    return now


def _resolve_http_config() -> Optional[ServiceNowHttpConfig]:
    try:
        cfg = integration_service.get_config("servicenow")
    except KeyError:
        return None

    if not cfg.enabled:
        return None

    instance_url = (
        cfg.base_url
        or os.getenv("SERVICENOW_INSTANCE_URL")
        or ""
    ).strip().rstrip("/")
    username = (
        cfg.workspace
        or os.getenv("SERVICENOW_USERNAME")
        or ""
    ).strip()
    password = (
        integration_service.get_connector_secret("servicenow", "token")
        or os.getenv("SERVICENOW_PASSWORD")
        or ""
    ).strip()

    has_password = cfg.token_present or bool(password)
    if not instance_url or not username or not has_password or not password:
        return None

    return ServiceNowHttpConfig(
        instance_url=instance_url,
        username=username,
        password=password,
    )


def _empty_status(*, instance_url: Optional[str] = None) -> ServiceNowConnectionStatus:
    return ServiceNowConnectionStatus(
        connected=False,
        instance_url=instance_url,
        last_sync=_LAST_SYNC,
    )


def _ref_text(raw: object) -> str:
    if raw is None:
        return ""
    if isinstance(raw, dict):
        return str(raw.get("display_value") or raw.get("value") or "").strip()
    return str(raw).strip()


def validate_servicenow_connection() -> ServiceNowConnectionStatus:
    """Validate connectivity and return aggregate discovery counts."""
    http = _resolve_http_config()
    if http is None:
        try:
            cfg = integration_service.get_config("servicenow")
            return _empty_status(instance_url=cfg.base_url)
        except KeyError:
            return _empty_status()

    try:
        validate_connection(http)
        incident_count = count_table(http, "incident")
        change_count = count_table(http, "change_request")
        service_count = count_table(http, "cmdb_ci_service")
        cmdb_count = count_table(http, "cmdb_ci")
        sync = _touch_sync()
        return ServiceNowConnectionStatus(
            connected=True,
            instance_url=http.instance_url,
            incident_count=incident_count,
            change_count=change_count,
            service_count=service_count,
            cmdb_count=cmdb_count,
            last_sync=sync,
        )
    except ServiceNowAPIError as exc:
        logger.info("servicenow validation failed: %s", exc.code)
        return _empty_status(instance_url=http.instance_url)
    except Exception as exc:
        logger.debug("servicenow validation error: %s", exc)
        return _empty_status(instance_url=http.instance_url)


def list_incidents(*, limit: int = 50) -> ServiceNowIncidentsResponse:
    """Return up to limit incidents; response total is len(incidents), not global table count."""
    http = _resolve_http_config()
    if http is None:
        return ServiceNowIncidentsResponse()

    try:
        rows = list_table(http, table="incident", fields=_INCIDENT_FIELDS, limit=limit)
        incidents: List[ServiceNowIncident] = []
        for row in rows:
            number = _ref_text(row.get("number"))
            if not number:
                continue
            incidents.append(
                ServiceNowIncident(
                    number=number,
                    short_description=_ref_text(row.get("short_description")),
                    state=_ref_text(row.get("state")),
                    priority=_ref_text(row.get("priority")),
                    assignment_group=_ref_text(row.get("assignment_group")) or None,
                    opened_at=_ref_text(row.get("opened_at")) or None,
                )
            )
        _touch_sync()
        return ServiceNowIncidentsResponse(incidents=incidents, total=len(incidents))
    except ServiceNowAPIError:
        return ServiceNowIncidentsResponse()


def list_changes(*, limit: int = 50) -> ServiceNowChangesResponse:
    """Return up to limit changes; response total is len(changes), not global table count."""
    http = _resolve_http_config()
    if http is None:
        return ServiceNowChangesResponse()

    try:
        rows = list_table(http, table="change_request", fields=_CHANGE_FIELDS, limit=limit)
        changes: List[ServiceNowChange] = []
        for row in rows:
            number = _ref_text(row.get("number"))
            if not number:
                continue
            changes.append(
                ServiceNowChange(
                    number=number,
                    short_description=_ref_text(row.get("short_description")),
                    state=_ref_text(row.get("state")),
                    risk=_ref_text(row.get("risk")),
                    planned_start=_ref_text(row.get("planned_start_date")) or None,
                    planned_end=_ref_text(row.get("planned_end_date")) or None,
                )
            )
        _touch_sync()
        return ServiceNowChangesResponse(changes=changes, total=len(changes))
    except ServiceNowAPIError:
        return ServiceNowChangesResponse()


def list_services(*, limit: int = 50) -> ServiceNowServicesResponse:
    """Return up to limit services; response total is len(services), not global table count."""
    http = _resolve_http_config()
    if http is None:
        return ServiceNowServicesResponse()

    try:
        rows = list_table(http, table="cmdb_ci_service", fields=_SERVICE_FIELDS, limit=limit)
        services: List[ServiceNowService] = []
        for row in rows:
            name = _ref_text(row.get("name"))
            if not name:
                continue
            services.append(
                ServiceNowService(
                    name=name,
                    business_criticality=_ref_text(row.get("business_criticality")),
                    operational_status=_ref_text(row.get("operational_status")),
                )
            )
        _touch_sync()
        return ServiceNowServicesResponse(services=services, total=len(services))
    except ServiceNowAPIError:
        return ServiceNowServicesResponse()


def list_cmdb_items(*, limit: int = 50) -> ServiceNowCMDBResponse:
    """Return up to limit CMDB items; response total is len(items), not global table count."""
    http = _resolve_http_config()
    if http is None:
        return ServiceNowCMDBResponse()

    try:
        rows = list_table(http, table="cmdb_ci", fields=_CMDB_FIELDS, limit=limit)
        items: List[ServiceNowCI] = []
        for row in rows:
            name = _ref_text(row.get("name"))
            if not name:
                continue
            items.append(
                ServiceNowCI(
                    name=name,
                    class_name=_ref_text(row.get("sys_class_name")),
                    operational_status=_ref_text(row.get("operational_status")),
                )
            )
        _touch_sync()
        return ServiceNowCMDBResponse(items=items, total=len(items))
    except ServiceNowAPIError:
        return ServiceNowCMDBResponse()
