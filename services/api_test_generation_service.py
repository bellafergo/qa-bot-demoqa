# services/api_test_generation_service.py
"""
API Test Generation Service
==============================

Generates draft API test cases from discovered OpenAPI endpoints using
deterministic templates. No LLM dependency.

Generation rules per endpoint:
  1. Happy path  (always)
  2. Missing required field  (if request body with required fields exists)
  3. Invalid parameter  (if path/query parameters exist)
  4. Unauthorized  (if endpoint requires_auth and include_auth_tests=True)
"""
from __future__ import annotations

import logging
import re
from typing import Any, Dict, List, Optional, Tuple

from models.api_testing_models import (
    APITestGenerationRequest,
    APITestGenerationResponse,
    ApproveAPITestsRequest,
    ApproveAPITestsResponse,
    DraftAPITest,
    EndpointInfo,
    OpenAPISpecRequest,
)

logger = logging.getLogger("vanya.api_test_generation")


# ── Module inference ──────────────────────────────────────────────────────────

_MODULE_KEYWORD_MAP: List[Tuple[List[str], str]] = [
    (["auth", "login", "logout", "session", "token", "oauth", "signin", "signup", "password", "credential"], "auth"),
    (["checkout", "payment", "pay", "cart", "order", "purchase", "billing", "invoice", "subscription"], "checkout"),
    (["product", "catalog", "category", "search", "inventory", "item", "sku", "stock", "listing"], "catalog"),
    (["user", "profile", "account", "settings", "preference", "customer", "member", "me"], "account"),
    (["admin", "management", "config", "configuration", "dashboard", "analytics", "report"], "admin"),
    (["file", "upload", "download", "media", "asset", "image", "attachment", "document"], "files"),
    (["notification", "email", "sms", "message", "alert", "webhook", "event"], "notifications"),
    (["address", "shipping", "delivery", "location", "geo"], "shipping"),
]


def _infer_module(endpoint: EndpointInfo, override: Optional[str] = None) -> str:
    if override:
        return override

    # Combine tags, path, summary into one text blob
    text = " ".join([
        " ".join(endpoint.tags),
        endpoint.path,
        endpoint.summary,
        endpoint.description,
    ]).lower()

    for keywords, module in _MODULE_KEYWORD_MAP:
        if any(kw in text for kw in keywords):
            return module

    return "api"


# ── Priority inference ────────────────────────────────────────────────────────

def _infer_priority(module: str, test_type: str, override: Optional[str] = None) -> str:
    if override and override in ("low", "medium", "high", "critical"):
        return override
    if module in ("auth", "checkout") and test_type == "smoke":
        return "critical"
    if module in ("auth", "checkout"):
        return "high"
    if test_type == "smoke":
        return "high"
    return "medium"


# ── Example value builders ────────────────────────────────────────────────────

_FIELD_EXAMPLES: Dict[str, Any] = {
    "email":    "test@example.com",
    "password": "TestPass123!",
    "username": "testuser",
    "name":     "Test User",
    "first_name": "Test",
    "last_name":  "User",
    "phone":    "+1-555-555-0100",
    "age":      25,
    "count":    1,
    "quantity": 1,
    "amount":   9.99,
    "price":    9.99,
    "id":       "test-id-001",
    "title":    "Test Title",
    "description": "Test description",
    "url":      "https://example.com",
    "status":   "active",
    "type":     "standard",
    "role":     "user",
    "token":    "test-token-abc123",
    "code":     "TEST123",
    "limit":    10,
    "offset":   0,
    "page":     1,
    "sort":     "created_at",
    "order":    "asc",
}


def _example_value(field_name: str, schema_info: Dict[str, Any]) -> Any:
    """Return a sensible example value for a field based on name + schema type."""
    fn = field_name.lower()

    # Exact match first
    if fn in _FIELD_EXAMPLES:
        return _FIELD_EXAMPLES[fn]

    # Partial match
    for key, val in _FIELD_EXAMPLES.items():
        if key in fn or fn in key:
            return val

    # Fall back to type-based default
    schema_type = schema_info.get("type", "string")
    if schema_type == "integer":
        return 1
    if schema_type == "number":
        return 1.0
    if schema_type == "boolean":
        return True
    if schema_type == "array":
        return []
    if schema_type == "object":
        return {}
    return f"test_{field_name}"


def _build_example_body(schema: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    """Build a minimal valid body from a JSON schema."""
    if not schema:
        return {}
    props = schema.get("properties", {})
    required = set(schema.get("required", []))
    body: Dict[str, Any] = {}
    # Always include required fields; include a sample of optional ones
    for field, field_schema in props.items():
        if field in required or len(body) < 3:
            body[field] = _example_value(field, field_schema)
    return body


def _get_required_fields(schema: Optional[Dict[str, Any]]) -> List[str]:
    if not schema:
        return []
    return list(schema.get("required", []))


def _path_example(path: str) -> str:
    """Replace path params like {id} with example values."""
    return re.sub(r"\{(\w+)\}", lambda m: f"test-{m.group(1)}-001", path)


# ── Step / assertion builders ─────────────────────────────────────────────────

def _api_request_step(
    method: str,
    endpoint: str,
    headers: Optional[Dict[str, str]] = None,
    params: Optional[Dict[str, Any]] = None,
    body: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    step: Dict[str, Any] = {
        "action":   "api_request",
        "method":   method.upper(),
        "endpoint": endpoint,
    }
    if headers:
        step["headers"] = headers
    if params:
        step["params"] = params
    if body is not None:
        step["body"] = body
    return step


def _status_assertion(code: int) -> Dict[str, Any]:
    return {"type": "status_code_equals", "value": str(code)}


def _response_contains(target: str, value: Any) -> Dict[str, Any]:
    return {"type": "response_contains", "target": target, "value": str(value)}


def _schema_match_assertion() -> Dict[str, Any]:
    return {"type": "response_schema_match"}


def _header_exists_assertion(header: str) -> Dict[str, Any]:
    return {"type": "header_exists", "value": header}


# ── Draft builders ────────────────────────────────────────────────────────────

def _happy_path_draft(ep: EndpointInfo, module: str, priority: str) -> DraftAPITest:
    """Generate a happy-path (smoke) draft for an endpoint."""
    path_ex = _path_example(ep.path)
    body = _build_example_body(ep.request_body_schema) if ep.request_body_schema else None
    headers: Dict[str, str] = {}
    if body is not None:
        headers["Content-Type"] = "application/json"

    step = _api_request_step(ep.method, path_ex, headers=headers or None, body=body)
    assertions = [_status_assertion(200 if ep.method != "POST" else 201)]
    if ep.response_schema:
        assertions.append(_schema_match_assertion())

    summary = ep.summary or f"{ep.method} {ep.path}"
    return DraftAPITest(
        name          = f"[Happy Path] {summary}",
        module        = module,
        endpoint      = ep.path,
        method        = ep.method,
        type          = "smoke",
        priority      = priority,
        rationale     = f"Verify {ep.method} {ep.path} returns a successful response with valid input.",
        steps         = [step],
        assertions    = assertions,
        confidence    = "high",
        source_signal = f"{ep.method} {ep.path}",
    )


def _missing_field_draft(ep: EndpointInfo, module: str, required_field: str) -> DraftAPITest:
    """Generate a negative test that omits a required field."""
    path_ex = _path_example(ep.path)
    full_body = _build_example_body(ep.request_body_schema)
    # Remove the required field
    body = {k: v for k, v in full_body.items() if k != required_field}
    headers = {"Content-Type": "application/json"}

    step = _api_request_step(ep.method, path_ex, headers=headers, body=body)
    assertions = [_status_assertion(400)]

    summary = ep.summary or f"{ep.method} {ep.path}"
    return DraftAPITest(
        name          = f"[Missing Field] {summary} — missing '{required_field}'",
        module        = module,
        endpoint      = ep.path,
        method        = ep.method,
        type          = "negative",
        priority      = "medium",
        rationale     = f"Verify {ep.method} {ep.path} returns 400 when required field '{required_field}' is absent.",
        steps         = [step],
        assertions    = assertions,
        confidence    = "high",
        source_signal = f"required field: {required_field}",
    )


def _invalid_param_draft(ep: EndpointInfo, module: str) -> Optional[DraftAPITest]:
    """Generate a negative test with an invalid parameter value."""
    # Find a path or query param to corrupt
    target_param = None
    for p in ep.parameters:
        if p.get("in") in ("path", "query") and p.get("name"):
            target_param = p
            break
    if target_param is None:
        return None

    param_name = target_param["name"]
    param_in   = target_param.get("in", "query")
    path_ex    = ep.path

    if param_in == "path":
        # Replace path param with obviously invalid value
        path_ex = re.sub(rf"\{{{param_name}\}}", "!!invalid!!", ep.path)
        step = _api_request_step(ep.method, path_ex)
    else:
        params = {param_name: "!!invalid!!"}
        step   = _api_request_step(ep.method, _path_example(ep.path), params=params)

    assertions = [_status_assertion(400)]

    summary = ep.summary or f"{ep.method} {ep.path}"
    return DraftAPITest(
        name          = f"[Invalid Param] {summary} — invalid '{param_name}'",
        module        = module,
        endpoint      = ep.path,
        method        = ep.method,
        type          = "negative",
        priority      = "medium",
        rationale     = f"Verify {ep.method} {ep.path} returns 400/422 when '{param_name}' contains an invalid value.",
        steps         = [step],
        assertions    = assertions,
        confidence    = "medium",
        source_signal = f"param: {param_name} ({param_in})",
    )


def _unauthorized_draft(ep: EndpointInfo, module: str) -> DraftAPITest:
    """Generate an unauthorized access test (no auth header)."""
    path_ex = _path_example(ep.path)
    body = _build_example_body(ep.request_body_schema) if ep.request_body_schema else None
    # Explicitly send empty/no Authorization header
    headers: Dict[str, str] = {"Authorization": ""}

    step = _api_request_step(ep.method, path_ex, headers=headers, body=body)
    assertions = [{"type": "status_code_equals", "value": "401"}]

    summary = ep.summary or f"{ep.method} {ep.path}"
    return DraftAPITest(
        name          = f"[Unauthorized] {summary}",
        module        = module,
        endpoint      = ep.path,
        method        = ep.method,
        type          = "negative",
        priority      = "high",
        rationale     = f"Verify {ep.method} {ep.path} returns 401/403 when called without valid credentials.",
        steps         = [step],
        assertions    = assertions,
        confidence    = "high",
        source_signal = f"security: {', '.join(ep.security_schemes)}",
    )


# ── ID allocation ─────────────────────────────────────────────────────────────

def _allocate_api_ids(count: int) -> List[str]:
    """Allocate TC-API-XXXX IDs from the existing catalog."""
    from services.db.catalog_repository import catalog_repo
    existing = catalog_repo.list_test_cases(status=None, limit=10000)
    max_num = 0
    for tc in existing:
        m = re.match(r"TC-API-(\d+)", tc.test_case_id)
        if m:
            max_num = max(max_num, int(m.group(1)))
    return [f"TC-API-{max_num + i + 1:04d}" for i in range(count)]


# ── Service ───────────────────────────────────────────────────────────────────

class APITestGenerationService:

    def generate(self, req: APITestGenerationRequest) -> APITestGenerationResponse:
        """Generate draft API tests from an endpoint list or a raw spec."""
        endpoints = req.endpoints or []

        # If no endpoints provided but spec given, parse it first
        if not endpoints and (req.spec_text or req.spec_url):
            from services.openapi_parser_service import openapi_parser_service
            from models.api_testing_models import OpenAPISpecRequest as OASReq
            spec_req = OASReq(spec_text=req.spec_text, spec_url=req.spec_url)
            endpoints = openapi_parser_service.parse(spec_req)

        if not endpoints:
            return APITestGenerationResponse(
                generation_notes=["No endpoints discovered — provide endpoints or a valid spec."]
            )

        notes: List[str] = []
        drafts: List[DraftAPITest] = []

        for ep in endpoints:
            module   = _infer_module(ep, req.module_override)
            ep_drafts: List[DraftAPITest] = []

            # 1. Happy path
            prio = _infer_priority(module, "smoke", req.priority_override)
            ep_drafts.append(_happy_path_draft(ep, module, prio))

            if req.include_negative_tests:
                # 2. Missing required field
                required = _get_required_fields(ep.request_body_schema)
                if required and ep.method.upper() in ("POST", "PUT", "PATCH"):
                    ep_drafts.append(_missing_field_draft(ep, module, required[0]))

                # 3. Invalid parameter
                inv = _invalid_param_draft(ep, module)
                if inv:
                    ep_drafts.append(inv)

            # 4. Unauthorized test
            if req.include_auth_tests and ep.requires_auth:
                ep_drafts.append(_unauthorized_draft(ep, module))

            drafts.extend(ep_drafts)

        # Enforce max_drafts cap
        if len(drafts) > req.max_drafts:
            notes.append(f"Capped drafts from {len(drafts)} to {req.max_drafts}.")
            drafts = drafts[: req.max_drafts]

        logger.info(
            "api_test_generation: generated %d drafts from %d endpoints",
            len(drafts), len(endpoints),
        )

        return APITestGenerationResponse(
            discovered_endpoints = len(endpoints),
            drafts               = drafts,
            total_endpoints      = len(endpoints),
            total_drafts         = len(drafts),
            generation_notes     = notes,
        )

    def approve(self, req: ApproveAPITestsRequest) -> ApproveAPITestsResponse:
        """Persist approved drafts into the catalog as API test cases."""
        from models.test_case import TestCaseCreate
        from services.test_catalog_service import catalog_service

        ids = _allocate_api_ids(len(req.drafts))
        created: List[str] = []
        skipped: List[str] = []
        status = "active" if req.activate else "inactive"

        for tc_id, draft in zip(ids, req.drafts):
            try:
                payload = TestCaseCreate(
                    test_case_id = tc_id,
                    name         = draft.name,
                    module       = draft.module,
                    type         = draft.type if draft.type in (
                        "smoke", "regression", "functional", "negative", "e2e"
                    ) else "smoke",
                    priority     = draft.priority if draft.priority in (
                        "low", "medium", "high", "critical"
                    ) else "medium",
                    status       = status,
                    test_type    = "api",
                    tags         = ["api-generated", draft.method.lower()],
                    base_url     = None,
                    steps        = draft.steps,
                    assertions   = draft.assertions,
                )
                catalog_service.create_test_case(payload)
                created.append(tc_id)
                logger.info("api_test_generation: approved draft → %s", tc_id)
            except Exception as exc:
                logger.warning("api_test_generation: skipped draft %s — %s", draft.draft_id, exc)
                skipped.append(draft.draft_id)

        return ApproveAPITestsResponse(
            created_test_case_ids = created,
            skipped_draft_ids     = skipped,
            total_created         = len(created),
            total_skipped         = len(skipped),
        )


api_test_generation_service = APITestGenerationService()
