# api/routes/local_agent_routes.py
"""Phase 4A — Local agent admin (/local-agents) and agent API (/agent-api)."""
from __future__ import annotations

from typing import List, Optional

from fastapi import APIRouter, File, Header, Query, Request, UploadFile

from models.local_agent_models import (
    AgentCapability,
    LocalAgent,
    LocalAgentArtifactUploadResponse,
    LocalAgentBrowserInspectionPersistRequest,
    LocalAgentBrowserInspectionPersistResponse,
    LocalAgentDetail,
    LocalAgentFoundationHeartbeat,
    LocalAgentFoundationRegistrationRequest,
    LocalAgentFoundationRegistrationResponse,
    LocalAgentFoundationView,
    LocalAgentHeartbeat,
    LocalAgentJob,
    LocalAgentJobResultSubmit,
    LocalAgentMockHeartbeatRequest,
    LocalAgentPatchRequest,
    LocalAgentPollRequest,
    LocalAgentPollResponse,
    LocalAgentRegistrationRequest,
    LocalAgentRegistrationResponse,
    LocalAgentReport,
)
from models.database_connector_models import DatabaseValidationExecuteRequest, DatabaseValidationExecuteResponse
from services import database_connector_service, local_agent_service

admin_router = APIRouter(prefix="/local-agents", tags=["local-agents"])
agent_router = APIRouter(prefix="/agent-api", tags=["local-agent-api"])


@admin_router.post("/register", response_model=LocalAgentRegistrationResponse)
def register_local_agent(body: LocalAgentRegistrationRequest, request: Request) -> LocalAgentRegistrationResponse:
    return local_agent_service.register_agent(body, request)


@admin_router.post("/foundation/register", response_model=LocalAgentFoundationRegistrationResponse)
def register_foundation_local_agent(
    body: LocalAgentFoundationRegistrationRequest,
    request: Request,
) -> LocalAgentFoundationRegistrationResponse:
    return local_agent_service.register_foundation_agent(body, request)


@admin_router.get("/foundation/report", response_model=LocalAgentReport)
def get_foundation_local_agent_report(
    request: Request,
    project_id: Optional[str] = None,
    limit: int = 200,
) -> LocalAgentReport:
    return local_agent_service.get_local_agent_report(project_id=project_id, limit=limit, request=request)


@admin_router.get("/foundation/capabilities", response_model=List[AgentCapability])
def list_foundation_capabilities(request: Request) -> List[AgentCapability]:
    local_agent_service.require_local_agent_admin(request)
    return local_agent_service.list_foundation_capabilities()


@admin_router.post("/foundation/mock-heartbeat", response_model=LocalAgentFoundationView)
def mock_foundation_local_agent_heartbeat(
    body: LocalAgentMockHeartbeatRequest,
    request: Request,
) -> LocalAgentFoundationView:
    return local_agent_service.mock_foundation_heartbeat(body, request)


@admin_router.get("", response_model=List[LocalAgent])
def list_local_agents(
    request: Request,
    project_id: Optional[str] = None,
    limit: int = 100,
) -> List[LocalAgent]:
    return local_agent_service.list_agents(project_id=project_id, limit=limit, request=request)


@admin_router.get("/{agent_id}", response_model=LocalAgentDetail)
def get_local_agent(agent_id: str, request: Request) -> LocalAgentDetail:
    return local_agent_service.get_agent(agent_id, request)


@admin_router.patch("/{agent_id}", response_model=LocalAgent)
def patch_local_agent(agent_id: str, body: LocalAgentPatchRequest, request: Request) -> LocalAgent:
    return local_agent_service.patch_agent(agent_id, body, request)


@admin_router.post("/{agent_id}/disable", response_model=LocalAgent)
def disable_local_agent(agent_id: str, request: Request) -> LocalAgent:
    return local_agent_service.disable_agent(agent_id, request)


@agent_router.post("/{agent_id}/heartbeat", response_model=LocalAgent)
def agent_heartbeat(
    agent_id: str,
    body: LocalAgentHeartbeat,
    request: Request,
    authorization: Optional[str] = Header(default=None, alias="Authorization"),
) -> LocalAgent:
    return local_agent_service.heartbeat(agent_id, body, authorization)


@agent_router.post("/foundation/heartbeat", response_model=LocalAgentFoundationView)
def agent_foundation_heartbeat(
    body: LocalAgentFoundationHeartbeat,
    authorization: Optional[str] = Header(default=None, alias="Authorization"),
) -> LocalAgentFoundationView:
    return local_agent_service.process_foundation_heartbeat(body, authorization)


@agent_router.post("/database-validation/execute", response_model=DatabaseValidationExecuteResponse)
def agent_database_validation_execute(
    body: DatabaseValidationExecuteRequest,
    authorization: Optional[str] = Header(default=None, alias="Authorization"),
) -> DatabaseValidationExecuteResponse:
    return database_connector_service.agent_execute_database_validation(body, authorization)


@agent_router.post("/{agent_id}/poll", response_model=LocalAgentPollResponse)
def agent_poll(
    agent_id: str,
    body: LocalAgentPollRequest,
    request: Request,
    authorization: Optional[str] = Header(default=None, alias="Authorization"),
) -> LocalAgentPollResponse:
    _ = request
    return local_agent_service.poll_jobs(agent_id, body, authorization)


@agent_router.post("/{agent_id}/jobs/{job_id}/result", response_model=LocalAgentJob)
def agent_job_result(
    agent_id: str,
    job_id: str,
    body: LocalAgentJobResultSubmit,
    request: Request,
    authorization: Optional[str] = Header(default=None, alias="Authorization"),
) -> LocalAgentJob:
    _ = request
    return local_agent_service.submit_job_result(agent_id, job_id, body, authorization)


@agent_router.post(
    "/{agent_id}/artifacts/upload",
    response_model=LocalAgentArtifactUploadResponse,
    summary="Upload one browser inspection screenshot (PNG/JPEG/WEBP, bounded size).",
)
async def agent_upload_browser_artifact(
    agent_id: str,
    inspection_id: str = Query(..., min_length=8, max_length=128),
    file: UploadFile = File(...),
    authorization: Optional[str] = Header(default=None, alias="Authorization"),
) -> LocalAgentArtifactUploadResponse:
    return await local_agent_service.upload_browser_inspection_artifact(
        agent_id, inspection_id, file, authorization
    )


@agent_router.post(
    "/{agent_id}/browser-inspections/persist",
    response_model=LocalAgentBrowserInspectionPersistResponse,
    summary="Persist a local browser inspection run (no screenshot_b64).",
)
def agent_persist_browser_inspection(
    agent_id: str,
    body: LocalAgentBrowserInspectionPersistRequest,
    authorization: Optional[str] = Header(default=None, alias="Authorization"),
) -> LocalAgentBrowserInspectionPersistResponse:
    return local_agent_service.persist_browser_inspection_from_agent(agent_id, body, authorization)
