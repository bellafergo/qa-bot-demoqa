# models/local_agent_models.py
"""Phase 4A — Vanya Local Agent: cloud ↔ agent contracts (no Playwright execution yet)."""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field, field_validator, model_validator

from models.browser_inspection_models import BrowserInspectionResult
from models.internal_api_connector_models import ApiConnectorReport

LocalAgentStatus = Literal["online", "offline", "disabled"]
LocalAgentCapability = Literal[
    "browser_inspection",
    "playwright",
    "localhost_access",
    "intranet_access",
    "database_validation",
    "contract_validation",
    "browser_probe",
    "filesystem_inventory",
    "repo_inventory",
    "network_inventory",
]
FoundationAgentStatus = Literal["ONLINE", "OFFLINE", "UNKNOWN"]
LocalAgentJobStatus = Literal["queued", "running", "succeeded", "failed", "cancelled"]
LocalAgentJobType = Literal["browser_inspection", "database_validation"]

LOCAL_AGENT_MAX_NAME_LEN = 256
LOCAL_AGENT_MAX_METADATA_JSON = 2048
LOCAL_AGENT_MAX_JOB_PAYLOAD_JSON = 4096
LOCAL_AGENT_MAX_VERSION_LEN = 64
LOCAL_AGENT_MAX_TARGET_URL_LEN = 2048
LOCAL_AGENT_MAX_RESULT_REF_LEN = 512
LOCAL_AGENT_MAX_ARTIFACT_BYTES = 8 * 1024 * 1024  # Phase 4D — single screenshot per inspection

_CAPABILITIES_ALLOWED = frozenset(
    {
        "browser_inspection",
        "playwright",
        "localhost_access",
        "intranet_access",
        "database_validation",
        "contract_validation",
        "browser_probe",
        "filesystem_inventory",
        "repo_inventory",
        "network_inventory",
    }
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


class LocalAgentJobBrief(BaseModel):
    """Lightweight job row for admin UI (Phase 4E)."""

    job_id: str
    job_type: str = "browser_inspection"
    status: str
    target_url: str = Field(..., max_length=512)
    created_at: str
    completed_at: Optional[str] = None


class LocalAgentDetail(LocalAgent):
    """GET /local-agents/{id} — includes recent project jobs (newest first)."""

    recent_jobs: List[LocalAgentJobBrief] = Field(default_factory=list)


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


class LocalAgentArtifactUploadResponse(BaseModel):
    """Phase 4D — Cloudinary secure URL + integrity hash (no raw bytes returned)."""

    evidence_url: str = Field(..., min_length=8, max_length=2048)
    sha256: str = Field(..., min_length=64, max_length=64, description="Hex SHA-256 of uploaded bytes.")
    content_type: str = Field(..., max_length=64)


class LocalAgentBrowserInspectionPersistRequest(BaseModel):
    """Agent → cloud: persist a bounded BrowserInspectionResult (no screenshot_b64)."""

    job_id: Optional[str] = Field(default=None, max_length=128)
    watch_id: Optional[str] = Field(default=None, max_length=128)
    project_id: Optional[str] = Field(default=None, max_length=256)
    artifact_sha256: Optional[str] = Field(default=None, max_length=64)
    inspection: BrowserInspectionResult

    @field_validator("artifact_sha256", mode="before")
    @classmethod
    def _artifact_sha(cls, v: Any) -> Any:
        if v is None or v == "":
            return None
        s = str(v).strip().lower()
        if len(s) != 64 or any(c not in "0123456789abcdef" for c in s):
            raise ValueError("artifact_sha256 must be 64 lowercase hex characters")
        return s

    @model_validator(mode="before")
    @classmethod
    def _strip_heavy_inspection(cls, data: Any) -> Any:
        if not isinstance(data, dict):
            return data
        out = dict(data)
        ins = out.get("inspection")
        if isinstance(ins, dict):
            ins = dict(ins)
            for k in ("screenshot_b64", "raw_html", "dom", "dom_snapshot", "html"):
                ins.pop(k, None)
            out["inspection"] = ins
        return out


class LocalAgentBrowserInspectionPersistResponse(BaseModel):
    persisted_run_id: Optional[str] = None
    persisted: bool = False
    persistence_warning: Optional[str] = Field(default=None, max_length=512)
    evidence_url: Optional[str] = Field(default=None, max_length=2048)


# ── INT-03A Local Agents Foundation ───────────────────────────────────────────


class AgentCapability(BaseModel):
    capability_id: str
    name: str
    description: str = ""


class AgentInventory(BaseModel):
    agent_id: str = ""
    databases_detected: List[str] = Field(default_factory=list)
    repositories_detected: List[str] = Field(default_factory=list)
    services_detected: List[str] = Field(default_factory=list)


class LocalAgentFoundationView(BaseModel):
    """Read-only foundation view for admin UI and reports."""

    agent_id: str
    name: str
    version: str = ""
    status: FoundationAgentStatus = "UNKNOWN"
    registered_at: str
    last_heartbeat_at: Optional[str] = None
    environment: str = "unknown"
    capabilities: List[str] = Field(default_factory=list)
    metadata: Dict[str, Any] = Field(default_factory=dict)


class LocalAgentReport(BaseModel):
    agents: List[LocalAgentFoundationView] = Field(default_factory=list)
    inventory: List[AgentInventory] = Field(default_factory=list)
    summary: str = ""
    internal_api_connectors: Optional[ApiConnectorReport] = None


class LocalAgentFoundationRegistrationRequest(BaseModel):
    project_id: str = Field(..., min_length=1, max_length=256)
    name: str = Field(..., min_length=1, max_length=LOCAL_AGENT_MAX_NAME_LEN)
    environment: str = Field(default="production", max_length=64)
    version: str = Field(default="1.0.0", max_length=LOCAL_AGENT_MAX_VERSION_LEN)
    capabilities: List[LocalAgentCapability] = Field(
        default_factory=lambda: normalize_capabilities(["database_validation", "contract_validation"])
    )
    inventory: Optional[AgentInventory] = None
    metadata: Dict[str, Any] = Field(default_factory=dict)

    @field_validator("capabilities", mode="before")
    @classmethod
    def _foundation_caps(cls, v: Any) -> Any:
        if v is None:
            return normalize_capabilities(["database_validation", "contract_validation"])
        if not isinstance(v, list):
            raise TypeError("capabilities must be a list of strings")
        return normalize_capabilities([str(x) for x in v])

    @field_validator("metadata", mode="before")
    @classmethod
    def _foundation_metadata_size(cls, v: Any) -> Any:
        if v is None:
            return {}
        import json

        raw = json.dumps(v, separators=(",", ":"), default=str)
        if len(raw.encode("utf-8")) > LOCAL_AGENT_MAX_METADATA_JSON:
            raise ValueError(f"metadata JSON must be <= {LOCAL_AGENT_MAX_METADATA_JSON} bytes")
        return v


class LocalAgentFoundationRegistrationResponse(BaseModel):
    agent_id: str
    agent_token: str = Field(description="Bearer secret for /agent-api/* — store securely; not shown again.")
    token_fingerprint: str
    project_id: str
    name: str
    environment: str
    version: str
    capabilities: List[LocalAgentCapability]
    registered_at: str = Field(default_factory=_utc_now_iso)


class LocalAgentFoundationHeartbeat(BaseModel):
    """Agent → cloud foundation heartbeat (no remote commands)."""

    agent_id: str = Field(..., min_length=1, max_length=256)
    version: Optional[str] = Field(default=None, max_length=LOCAL_AGENT_MAX_VERSION_LEN)
    capabilities: Optional[List[LocalAgentCapability]] = None
    timestamp: Optional[str] = None
    inventory: Optional[AgentInventory] = None

    @field_validator("capabilities", mode="before")
    @classmethod
    def _heartbeat_caps(cls, v: Any) -> Any:
        if v is None:
            return v
        if not isinstance(v, list):
            raise TypeError("capabilities must be a list of strings")
        return normalize_capabilities([str(x) for x in v])


class LocalAgentMockHeartbeatRequest(BaseModel):
    """Admin-only mock heartbeat for connectivity verification (INT-03A)."""

    agent_id: str = Field(..., min_length=1, max_length=256)
    version: Optional[str] = Field(default=None, max_length=LOCAL_AGENT_MAX_VERSION_LEN)
    capabilities: Optional[List[LocalAgentCapability]] = None
    timestamp: Optional[str] = None
    inventory: Optional[AgentInventory] = None

    @field_validator("capabilities", mode="before")
    @classmethod
    def _mock_caps(cls, v: Any) -> Any:
        if v is None:
            return v
        if not isinstance(v, list):
            raise TypeError("capabilities must be a list of strings")
        return normalize_capabilities([str(x) for x in v])
