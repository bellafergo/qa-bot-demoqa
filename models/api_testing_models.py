# models/api_testing_models.py
"""
Pydantic models for the OpenAPI / Swagger-based API Test Generation block.
"""
from __future__ import annotations

import uuid
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field, model_validator


def _new_draft_id() -> str:
    return f"DRAFT-API-{uuid.uuid4().hex[:8].upper()}"


# ── OpenAPI parsing ───────────────────────────────────────────────────────────

class OpenAPISpecRequest(BaseModel):
    """Input for the spec parser.  Provide spec_text or spec_url (or both)."""
    spec_url:        Optional[str] = None
    spec_text:       Optional[str] = None
    format_hint:     Optional[Literal["json", "yaml"]] = None
    base_url_override: Optional[str] = None

    @model_validator(mode="after")
    def at_least_one_source(self) -> "OpenAPISpecRequest":
        if not self.spec_url and not self.spec_text:
            raise ValueError("At least one of 'spec_url' or 'spec_text' must be provided.")
        return self


class EndpointInfo(BaseModel):
    """A single API endpoint discovered from an OpenAPI / Swagger spec."""
    operation_id:        Optional[str] = None
    path:                str
    method:              str            # GET | POST | PUT | PATCH | DELETE | …
    summary:             str = ""
    description:         str = ""
    tags:                List[str] = Field(default_factory=list)
    parameters:          List[Dict[str, Any]] = Field(default_factory=list)
    request_body_schema: Optional[Dict[str, Any]] = None
    response_schema:     Optional[Dict[str, Any]] = None
    requires_auth:       bool = False
    security_schemes:    List[str] = Field(default_factory=list)


# ── API test generation ───────────────────────────────────────────────────────

class APITestGenerationRequest(BaseModel):
    """Input for the API test generation service."""
    endpoints:          Optional[List[EndpointInfo]] = None
    spec_url:           Optional[str] = None
    spec_text:          Optional[str] = None
    include_negative_tests: bool = True
    include_auth_tests:     bool = True
    max_drafts:             int  = 20
    module_override:    Optional[str] = None
    priority_override:  Optional[str] = None


class DraftAPITest(BaseModel):
    """A single draft API test case, pending approval into the catalog."""
    draft_id:      str = Field(default_factory=_new_draft_id)
    name:          str
    module:        str
    endpoint:      str           # e.g. "/users/{id}"
    method:        str           # GET | POST | PUT | PATCH | DELETE
    type:          str = "smoke"
    priority:      str = "medium"
    rationale:     str = ""
    steps:         List[Dict[str, Any]] = Field(default_factory=list)
    assertions:    List[Dict[str, Any]] = Field(default_factory=list)
    confidence:    Literal["low", "medium", "high"] = "medium"
    source_signal: str = ""


class APITestGenerationResponse(BaseModel):
    """Result of an API test generation request."""
    discovered_endpoints: int = 0
    drafts:               List[DraftAPITest] = Field(default_factory=list)
    total_endpoints:      int = 0
    total_drafts:         int = 0
    generation_notes:     List[str] = Field(default_factory=list)


# ── Approval ──────────────────────────────────────────────────────────────────

class ApproveAPITestsRequest(BaseModel):
    """Approve drafts into the catalog."""
    drafts:   List[DraftAPITest]
    activate: bool = True


class ApproveAPITestsResponse(BaseModel):
    created_test_case_ids: List[str] = Field(default_factory=list)
    skipped_draft_ids:     List[str] = Field(default_factory=list)
    total_created:         int = 0
    total_skipped:         int = 0


# ── Execution ─────────────────────────────────────────────────────────────────

class APITestExecutionRequest(BaseModel):
    """Run API tests — from catalog or directly from drafts."""
    test_case_ids: Optional[List[str]] = None
    drafts:        Optional[List[DraftAPITest]] = None
    environment:   Optional[str] = "default"
    project_id:    Optional[str] = None
    auth_config:   Optional[Dict[str, Any]] = None

    @model_validator(mode="after")
    def at_least_one_source(self) -> "APITestExecutionRequest":
        if not self.test_case_ids and not self.drafts:
            raise ValueError("At least one of 'test_case_ids' or 'drafts' must be provided.")
        return self
