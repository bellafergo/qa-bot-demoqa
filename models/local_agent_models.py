# models/local_agent_models.py
"""Phase 4A — Vanya Local Agent: cloud ↔ agent contracts (no Playwright execution yet)."""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field, field_validator

LocalAgentStatus = Literal["online", "offline", "disabled"]
LocalAgentCapability = Literal["browser_inspection", "playwright", "localhost_access", "intranet_access"]
LocalAgentJobStatus = Literal["queued", "running", "succeeded", "failed", "cancelled"]
LocalAgentJobType = Literal["browser_inspection"]

LOCAL_AGENT_MAX_NAME_LEN = 256
LOCAL_AGENT_MAX_METADATA_JSON = 2048
LOCAL_AGENT_MAX_JOB_PAYLOAD_JSON = 4096
LOCAL_AGENT_MAX_VERSION_LEN = 64
LOCAL_AGENT_MAX_TARGET_URL_LEN = 2048
LOCAL_AGENT_MAX_RESULT_REF_LEN = 512

_CAPABILITIES_ALLOWED = frozenset(
    {"browser_inspection", "playwright", "localhost_access", "intranet_access"}
)


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def normalize_capabilities(caps: Optional[List[str]]) -> List[LocalAgentCapability]:
    """Dedupe and validate capability strings."""
    out: List[LocalAgentCapability] = []
    seen: set[str] = set()
    for c in caps or ["browser_inspection"]:
        k = str(c).strip().lower()
        if k not in _CAPABILITIES_ALLOWED:
            raise ValueError(f"unknown capability: {c}")
        if k in seen:
            continue
        seen.add(k)
        out.append(k)  # type: ignore[assignment]
    return out


class LocalAgentRegistrationRequest(BaseModel):
    project_id: str = Field(..., min_length=1, max_length=256)
    name: str = Field(..., min_length=1, max_length=LOCAL_AGENT_MAX_NAME_LEN)
    capabilities: List[LocalAgentCapability] = Field(
        default_factory=lambda: normalize_capabilities(["browser_inspection"])
    )
    version: Optional[str] = Field(default=None, max_length=LOCAL_AGENT_MAX_VERSION_LEN)
    metadata: Dict[str, Any] = Field(default_factory=dict, description="Small JSON-serializable metadata only.")

    @field_validator("capabilities", mode="before")
    @classmethod
    def _caps(cls, v: Any) -> Any:
        if v is None:
            return normalize_capabilities(["browser_inspection"])
        if not isinstance(v, list):
            raise TypeError("capabilities must be a list of strings")
        return normalize_capabilities([str(x) for x in v])

    @field_validator("metadata", mode="before")
    @classmethod
    def _metadata_size(cls, v: Any) -> Any:
        if v is None:
            return {}
        import json

        raw = json.dumps(v, separators=(",", ":"), default=str)
        if len(raw.encode("utf-8")) > LOCAL_AGENT_MAX_METADATA_JSON:
            raise ValueError(f"metadata JSON must be <= {LOCAL_AGENT_MAX_METADATA_JSON} bytes")
        return v


class LocalAgentHeartbeat(BaseModel):
    """Agent → cloud heartbeat (no heavy payloads)."""

    agent_version: Optional[str] = Field(default=None, max_length=LOCAL_AGENT_MAX_VERSION_LEN)
    notes: Optional[str] = Field(default=None, max_length=512)


class LocalAgentPollRequest(BaseModel):
    limit: int = Field(default=10, ge=1, le=50)


class LocalAgentJobResultSubmit(BaseModel):
    status: Literal["succeeded", "failed", "cancelled"]
    result_ref: Optional[str] = Field(default=None, max_length=LOCAL_AGENT_MAX_RESULT_REF_LEN)
    error: Optional[str] = Field(default=None, max_length=500)


class LocalAgent(BaseModel):
    agent_id: str
    project_id: Optional[str] = None
    name: str
    status: LocalAgentStatus
    capabilities: List[LocalAgentCapability]
    version: Optional[str] = None
    last_seen_at: Optional[str] = None
    created_at: str
    updated_at: str
    enabled: bool
    token_fingerprint: str = Field(description="Non-secret prefix of stored token hash for support/UI.")
    metadata: Dict[str, Any] = Field(default_factory=dict)


class LocalAgentRegistrationResponse(BaseModel):
    """Returned once at registration; includes plaintext agent token."""

    agent_id: str
    agent_token: str = Field(description="Bearer secret for /agent-api/* — store securely; not shown again.")
    token_fingerprint: str
    project_id: str
    name: str
    capabilities: List[LocalAgentCapability]
    version: Optional[str] = None
    created_at: str = Field(default_factory=_utc_now_iso)


class LocalAgentJob(BaseModel):
    job_id: str
    project_id: str
    agent_id: Optional[str] = None
    job_type: LocalAgentJobType = "browser_inspection"
    target_url: str
    payload: Dict[str, Any] = Field(default_factory=dict)
    status: LocalAgentJobStatus
    created_at: str
    claimed_at: Optional[str] = None
    completed_at: Optional[str] = None
    result_ref: Optional[str] = None


class LocalAgentPollResponse(BaseModel):
    jobs: List[LocalAgentJob] = Field(default_factory=list)
    agent_capabilities: List[str] = Field(
        default_factory=list,
        description="Capabilities registered for this agent (Phase 4C+). Empty if unknown.",
    )


class LocalAgentPatchRequest(BaseModel):
    name: Optional[str] = Field(default=None, min_length=1, max_length=LOCAL_AGENT_MAX_NAME_LEN)
    enabled: Optional[bool] = None
    capabilities: Optional[List[LocalAgentCapability]] = None
    version: Optional[str] = Field(default=None, max_length=LOCAL_AGENT_MAX_VERSION_LEN)
    metadata: Optional[Dict[str, Any]] = None

    @field_validator("capabilities", mode="before")
    @classmethod
    def _caps_patch(cls, v: Any) -> Any:
        if v is None:
            return v
        if not isinstance(v, list):
            raise TypeError("capabilities must be a list of strings")
        return normalize_capabilities([str(x) for x in v])

    @field_validator("metadata", mode="before")
    @classmethod
    def _metadata_size(cls, v: Any) -> Any:
        if v is None:
            return v
        import json

        raw = json.dumps(v, separators=(",", ":"), default=str)
        if len(raw.encode("utf-8")) > LOCAL_AGENT_MAX_METADATA_JSON:
            raise ValueError(f"metadata JSON must be <= {LOCAL_AGENT_MAX_METADATA_JSON} bytes")
        return v
