# api/routes/local_agent_routes.py
"""Phase 4A — Local agent admin (/local-agents) and agent API (/agent-api)."""
from __future__ import annotations

from typing import List, Optional

from fastapi import APIRouter, Header, Request

from models.local_agent_models import (
    LocalAgent,
    LocalAgentHeartbeat,
    LocalAgentJob,
    LocalAgentJobResultSubmit,
    LocalAgentPatchRequest,
    LocalAgentPollRequest,
    LocalAgentPollResponse,
    LocalAgentRegistrationRequest,
    LocalAgentRegistrationResponse,
)
from services import local_agent_service

admin_router = APIRouter(prefix="/local-agents", tags=["local-agents"])
agent_router = APIRouter(prefix="/agent-api", tags=["local-agent-api"])


@admin_router.post("/register", response_model=LocalAgentRegistrationResponse)
def register_local_agent(body: LocalAgentRegistrationRequest, request: Request) -> LocalAgentRegistrationResponse:
    return local_agent_service.register_agent(body, request)


@admin_router.get("", response_model=List[LocalAgent])
def list_local_agents(
    request: Request,
    project_id: Optional[str] = None,
    limit: int = 100,
) -> List[LocalAgent]:
    return local_agent_service.list_agents(project_id=project_id, limit=limit, request=request)


@admin_router.get("/{agent_id}", response_model=LocalAgent)
def get_local_agent(agent_id: str, request: Request) -> LocalAgent:
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
