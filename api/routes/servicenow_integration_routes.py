# api/routes/servicenow_integration_routes.py
"""Read-only ServiceNow discovery routes (SNOW-01A)."""
from __future__ import annotations

import logging

from fastapi import APIRouter, Query

from models.servicenow_models import (
    ServiceNowCMDBResponse,
    ServiceNowChangesResponse,
    ServiceNowConnectionStatus,
    ServiceNowIncidentsResponse,
    ServiceNowServicesResponse,
)
from services.servicenow_integration_service import (
    list_changes,
    list_cmdb_items,
    list_incidents,
    list_services,
    validate_servicenow_connection,
)

logger = logging.getLogger("vanya.servicenow_integration_routes")

router = APIRouter(prefix="/integrations/servicenow", tags=["servicenow-integration"])


@router.get("/status", response_model=ServiceNowConnectionStatus)
def get_servicenow_status():
    """ServiceNow discovery aggregate — connectivity plus entity counts."""
    return validate_servicenow_connection()


@router.get("/incidents", response_model=ServiceNowIncidentsResponse)
def get_servicenow_incidents(
    limit: int = Query(50, ge=1, le=100, description="Maximum incidents to return"),
):
    """List ServiceNow incidents (read-only)."""
    return list_incidents(limit=limit)


@router.get("/changes", response_model=ServiceNowChangesResponse)
def get_servicenow_changes(
    limit: int = Query(50, ge=1, le=100, description="Maximum changes to return"),
):
    """List ServiceNow change requests (read-only)."""
    return list_changes(limit=limit)


@router.get("/services", response_model=ServiceNowServicesResponse)
def get_servicenow_services(
    limit: int = Query(50, ge=1, le=100, description="Maximum services to return"),
):
    """List ServiceNow business services (read-only)."""
    return list_services(limit=limit)


@router.get("/cmdb", response_model=ServiceNowCMDBResponse)
def get_servicenow_cmdb(
    limit: int = Query(50, ge=1, le=100, description="Maximum CMDB items to return"),
):
    """List ServiceNow CMDB configuration items (read-only)."""
    return list_cmdb_items(limit=limit)
