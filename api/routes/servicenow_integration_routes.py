# api/routes/servicenow_integration_routes.py
"""
Read-only ServiceNow discovery routes (SNOW-01A).

Route semantics (do not conflate with generic connector routes):
  GET /integrations/servicenow/status    — ServiceNow discovery aggregate (counts, last_sync, connected)
  GET /integrations/servicenow/incidents   — incident discovery (read-only)
  GET /integrations/servicenow/changes    — change request discovery (read-only)
  GET /integrations/servicenow/services    — business service discovery (read-only)
  GET /integrations/servicenow/cmdb          — CMDB CI discovery (read-only)
  GET /integrations/servicenow/intelligence    — operational correlation report (read-only)

List endpoints return up to `limit` rows; response `total` is the number of items
in that response (page size), not the global ServiceNow table count. Use
GET /integrations/servicenow/status for aggregate entity counts.

Generic connector framework status (health, enabled, config_summary):
  GET  /integrations/servicenow             — ConnectorStatus via integrations_routes
  POST /integrations/servicenow/health-check

Do not conflate GET /integrations/servicenow with GET /integrations/servicenow/status —
same split as Jira and QMetry (framework status vs discovery aggregate).

All ServiceNow discovery endpoints are GET-only. Vanya does not create incidents,
modify changes, approve workflows, or update CMDB records in ServiceNow.
"""
from __future__ import annotations

import logging

from typing import Optional

from fastapi import APIRouter, Query

from models.servicenow_intelligence_models import ServiceNowIntelligenceReport
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
from services.servicenow_intelligence_service import build_servicenow_intelligence_report

logger = logging.getLogger("vanya.servicenow_integration_routes")

router = APIRouter(prefix="/integrations/servicenow", tags=["servicenow-integration"])


@router.get("/status", response_model=ServiceNowConnectionStatus)
def get_servicenow_status():
    """ServiceNow discovery aggregate — connectivity plus entity counts (no credentials)."""
    return validate_servicenow_connection()


@router.get("/incidents", response_model=ServiceNowIncidentsResponse)
def get_servicenow_incidents(
    limit: int = Query(50, ge=1, le=100, description="Maximum incidents to return"),
):
    """List ServiceNow incidents (read-only). `total` is items returned, not global count."""
    return list_incidents(limit=limit)


@router.get("/changes", response_model=ServiceNowChangesResponse)
def get_servicenow_changes(
    limit: int = Query(50, ge=1, le=100, description="Maximum changes to return"),
):
    """List ServiceNow change requests (read-only). `total` is items returned, not global count."""
    return list_changes(limit=limit)


@router.get("/services", response_model=ServiceNowServicesResponse)
def get_servicenow_services(
    limit: int = Query(50, ge=1, le=100, description="Maximum services to return"),
):
    """List ServiceNow business services (read-only). `total` is items returned, not global count."""
    return list_services(limit=limit)


@router.get("/cmdb", response_model=ServiceNowCMDBResponse)
def get_servicenow_cmdb(
    limit: int = Query(50, ge=1, le=100, description="Maximum CMDB items to return"),
):
    """List ServiceNow CMDB configuration items (read-only). `total` is items returned, not global count."""
    return list_cmdb_items(limit=limit)


@router.get("/intelligence", response_model=ServiceNowIntelligenceReport)
def get_servicenow_intelligence(
    project_id: Optional[str] = Query(None, description="Project id for incident intelligence context"),
):
    """Correlate ServiceNow operational data with existing Vanya intelligence (read-only)."""
    return build_servicenow_intelligence_report(project_id=project_id)
